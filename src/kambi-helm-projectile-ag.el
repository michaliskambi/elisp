;;;; Michalis configuration for helm, projectile and ag.

;;; Commentary:
;;
;; Configuration for this is maintainer together,
;; since they do cooperate with each other, in particular
;; we use helm-projectile that connects helm+projectile.

(require 'kambi-utils)

;; projectile ----------------------------------------------------------------

(when (require 'projectile nil 'noerror)
  ;; see https://github.com/bbatsov/projectile
  ;; installable through melpa added above
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-find-dir-includes-top-level t)

  ;; setting projectile-mode-line through customize doesn't work reliably
  ;; (on Debian river)
  (setq projectile-mode-line
    '(:eval (if (file-remote-p default-directory)
              " Projectile"
            (format " P[%s]" (projectile-project-name)))))

  (defun kam-optional-projectile-find-file ()
    (interactive)
    "Open file in projectile project, or just open any file if outside project."
    (if (projectile-project-p)
        (call-interactively 'projectile-find-file)
      (call-interactively 'find-file)))
  (defun kam-optional-projectile-find-dir ()
    (interactive)
    "Open dir in projectile project, or just open any file if outside project."
    (if (projectile-project-p)
        (call-interactively 'projectile-find-dir)
      (call-interactively 'dired)))

  (define-key projectile-mode-map (kbd "M-f") 'kam-optional-projectile-find-file)
  (define-key projectile-mode-map (kbd "M-d") 'kam-optional-projectile-find-dir)
  (define-key projectile-mode-map (kbd "M-s") 'projectile-switch-project)
  (define-key svn-status-mode-map (kbd "M-s") 'projectile-switch-project) ;; also override svn shortcut
  (define-key projectile-mode-map (kbd "M-g") 'projectile-grep)

  (when (require 'ag nil 'noerror)

    (when kam-is-windows
      ;; highlighting on Windows always causes weird bug: the first component
      ;; (filename)is eaten. Happens even if you run 'ag' from shell in Emacs.
      ;; See https://github.com/Wilfred/ag.el/issues/101
      ;; https://github.com/Wilfred/ag.el/issues/97
      (setq ag-highlight-search nil))

    (defun kam-projectile-ag (search-term &optional arg default-search-term)
      (interactive
       ;; Note: do not use default-search-term of read-from-minibuffer,
       ;; because it has brain-dead specification.
       ;; Others rant about it too: http://xahlee.info/comp/lisp_read-from-minibuffer_propels_deep_questions.html
       (list (read-from-minibuffer
              (projectile-prepend-project-name (format "Ag %ssearch for (default \"%s\"): " (if current-prefix-arg "regexp " "") (projectile-symbol-or-selection-at-point))))
             current-prefix-arg
             (projectile-symbol-or-selection-at-point)))
      "Like `projectile-ag', but without uncomfortable \"insert thing at point\" behavior.
It only uses \"thing at point\" if you just press enter.

Longer explanation:

projectile-ag by default suggests (projectile-symbol-or-selection-at-point) value.

While this is often useful (often you want to search for this),
it's uncomfortable when you need to search for something else,
because then you have to remember to presss C-a C-k
before you start typing your phrase in the minibuffer.

Other GUIs often preselect the default input box contents, to allow you
to start typing to automatically delete the default contents.

In Emacs, the solution to this is that many commands have a default value,
like \"Search [WordUnderPoint]: \" and if you input nothing (just press enter)
-- then the default \"WordUnderPoint\" is used.

(This is also not a perfect solution, as you cannot modify the default value,
e.g. if you want to search for \"WordUnderPointBlah\", then you have to type
it whole. But it's better than the current I think, because I seldom want to
extend the thing under point, and if I want -- I can eventually copy it before
entering projectile-ag, and paste then. But at least the case when
\"my cursor was over something irrelevant, I want to type what to search\"
is fast.
"
      (if (equal search-term "")
          (projectile-ag default-search-term arg)
        (projectile-ag search-term arg)))

    (defun kam-optional-projectile-ag ()
      (interactive)
      "Search using AG in projectile project, or just in current dir if outside project."
      (if (projectile-project-p)
          (call-interactively 'kam-projectile-ag)
        (call-interactively 'ag)))

    (define-key projectile-mode-map (kbd "M-g") 'kam-optional-projectile-ag))

  ;; customize project name, in case of xxx/trunk/ dir.
  ;; Based on https://github.com/bbatsov/projectile/pull/928
  ;; I added $ at end, so that subprojects (with .projectile) inside larger xxx/trunk
  ;; get their own names correctly.
  (defun kam-projectile-custom-project-name (project-root)
     (cond
      ((string-match "/\\([^/]+\\)/\\(?:branches\\|tags\\)\\(?:.*\\)/\\([^/]+\\)/?$" project-root)
       (let* ((product-name (match-string 1 project-root))
              (branch-name (match-string 2 project-root)))
         (concat product-name "/" branch-name))) ;

      ((string-match "/\\([^/]+\\)/trunk/?$" project-root)
             (let* ((product-name (match-string 1 project-root)))
               (concat product-name "/trunk")))

      ;; do the default
      (t
       (projectile-default-project-name project-root))))
  (setq projectile-project-name-function 'kam-projectile-custom-project-name)

  ;; unfortunately, ctags for Pascal projects is not much useful
  ;; (only procedures/functions)
  ;; (define-key projectile-mode-map (kbd "<s-f12>") 'projectile-find-tag)

  ;; projectile with CGE interaction
  (projectile-register-project-type 'castle-engine '("CastleEngineManifest.xml") "castle-engine compile --mode=debug && castle-engine run" nil)

  (when kam-is-windows
    ;; makes projectile scan projects much faster on Windows.
    ;; For some projects, like FPC, it's a *must*, as the default value on Windows
    ;; (alien) is much too slow.
    (setq projectile-indexing-method 'alien)
    (setq projectile-generic-command "c:/cygwin/bin/find . -type f -print0")
  )

  ;; remove from the "known projects" list the non-existing projects
  (projectile-cleanup-known-projects)
)

;; projectile + compilation --------------------------------------------------
;;
;; This allows compilation command to be remembered for a project, not for a file,
;; which is nice if you like to run compilation from any file belonging to a project,
;; and expect to run the same command as previous.

(defvar-local kam-force-compilation-not-in-project nil
  "When non-nil (you usually want to only set it buffer-local) then
force our compilation commands `kam-compile' and `kam-compile-immediate'
to use non-project compilation.")

(defun kam-compile ()
  "Compile, asking for command, possibly using projectile.

Uses projectile if we're inside a project (thus using (proposing/editing/saving)
a project-wide compilation command), unless kam-force-compilation-not-in-project
is defined."
  (interactive)
  (if (and (projectile-project-p) (not kam-force-compilation-not-in-project))
      (let ((saved-compilation-read-command compilation-read-command))
        (setq compilation-read-command t)
        (call-interactively 'projectile-compile-project)
        (setq compilation-read-command saved-compilation-read-command))
    (kam-compile-prompt compile-command)))

(defun kam-compile-immediate ()
  "Compile, withot asking for a command, possibly using projectile.

Uses projectile if we're inside a project (thus using (proposing/editing/saving)
a project-wide compilation command), unless kam-force-compilation-not-in-project
is defined."
  (interactive)
  (if (and (projectile-project-p) (not kam-force-compilation-not-in-project))
      (let ((saved-compilation-read-command compilation-read-command))
        (setq compilation-read-command nil)
        (call-interactively 'projectile-compile-project)
        (setq compilation-read-command saved-compilation-read-command))
    (compile compile-command)))

;; helm ----------------------------------------------------------------------

;; parts based on http://tuhdo.github.io/helm-intro.html
(when (require 'helm-config nil 'noerror)
  (helm-mode 1)
  (setq helm-completion-mode-string "") ;; do not show " Helm" in modeline
  ;; (setq helm-full-frame t)

  ;; we should not do this, as it's internal.
  ;; But it seems the only way to have helm-ag results with truncated lines by
  ;; default?
  ;; Also makes helm-recentf have truncated lines, fine by me.
  (setq helm-truncate-lines t)

  (defun kam-helm-find-files-no-ffap ()
    "Like helm-find-files, but never try to guess filename using ffap.
This is useful for me, because my ffap is quite slow (it tries hard
to search various dirs with Pascal units), so I prefer to request
is explicitly. The default C-x C-f should be fast.

Also, it's sometimes undesired. When your cursor is over something resembling
a file, or an URL, but you don't want to open *that*, it's undesired to autoselect
that."
    (interactive)
    (let ((previous-helm-ff-guess-ffap-filenames helm-ff-guess-ffap-filenames)
          (previous-helm-ff-guess-ffap-urls      helm-ff-guess-ffap-urls)
          ;; (previous-helm-ff-no-preselect         helm-ff-no-preselect)
         )
      (setq helm-ff-guess-ffap-filenames nil)
      (setq helm-ff-guess-ffap-urls      nil)
      ;; (setq helm-ff-no-preselect         t)
      (unwind-protect ;; see https://curiousprogrammer.wordpress.com/2009/06/08/error-handling-in-emacs-lisp/
          (call-interactively 'helm-find-files)
        (setq helm-ff-guess-ffap-filenames previous-helm-ff-guess-ffap-filenames)
        (setq helm-ff-guess-ffap-urls      previous-helm-ff-guess-ffap-urls)
        ;; (setq helm-ff-no-preselect         previous-helm-ff-no-preselect)
      )
    )
  )

  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-x k") 'helm-show-kill-ring)
  ;; (global-set-key (kbd "M-0") 'helm-mini) ;; not needed, kam-buffer-menu already uses helm
  ;; Note: should ignore stuff listed on completion-ignored-extensions .
  ;; Although it has also it's own "Helm Boring File Regexp List",
  ;; but we should not set it (unless completion-ignored-extensions
  ;; is not enough, like for *~ files)
  (global-set-key (kbd "C-x C-f") 'kam-helm-find-files-no-ffap)
  (global-set-key (kbd "C-h a") 'helm-apropos)
  (global-set-key (kbd "C-x C-/") 'helm-find)

  (defun kam-helm-recentf ()
    "Customized `helm' for `recentf', to show basename by default."
    (interactive)
    (helm :sources 'helm-source-recentf
          :ff-transformer-show-only-basename t
          :buffer "*helm recentf*"))
  (global-set-key (kbd "C-x C-r") 'kam-helm-recentf)
  ;;(global-set-key (kbd "C-x C-r") 'helm-recentf)

  ;; following helm-buffer sources, but kills buffers without asking

  (defun kam-helm-buffer-run-kill-persistent ()
    "Kill buffer without quitting helm."
    (interactive)
    (with-helm-alive-p
      (helm-attrset 'kill-action '(kam-helm-buffers-persistent-kill . never-split))
      (helm-execute-persistent-action 'kill-action)))
  (put 'kam-helm-buffer-run-kill-persistent 'helm-only t)

  (defun kam-helm-buffers-persistent-kill (_buffer)
    (let ((marked (helm-marked-candidates)))
      (unwind-protect
           (cl-loop for b in marked
                 do (progn (helm-buffers-persistent-kill-1 b)
                           (message nil)))
        (with-helm-buffer
          (setq helm-marked-candidates nil
                helm-visible-mark-overlays nil))
        (helm-force-update (helm-buffers--quote-truncated-buffer (helm-get-selection))))))

  ;;  (defun kam-helm-kill-selection-and-quit ()
  ;;    "Like helm-kill-selection-and-quit, but by default copy real value
  ;; (not display value)"
  ;;    (interactive)
  ;;    (helm-kill-selection-and-quit 1))

  (define-key helm-buffer-map (kbd "<delete>") 'kam-helm-buffer-run-kill-persistent)

  (define-key helm-map (kbd "C-t") 'helm-toggle-truncate-line)

  ;; key shortcuts more consistent with normal line editing
  (define-key helm-map (kbd "C-v") 'cua-paste)
  (define-key helm-map (kbd "C-z") 'undo)
  (define-key helm-map (kbd "<home>") 'beginning-of-line)
  (define-key helm-map (kbd "<end>") 'end-of-line)
  (define-key helm-map (kbd "C-k") 'helm-kill-selection-and-quit)
  (define-key helm-map (kbd "<C-home>") 'helm-beginning-of-buffer)
  (define-key helm-map (kbd "<C-end>") 'helm-end-of-buffer)

  ;; these do not really work, as there's no easy way to select region within helm
  ;; (define-key helm-map (kbd "C-x") 'cua-cut-region)
  ;; (define-key helm-map (kbd "C-c") 'cua-copy-region)
  ;; (define-key helm-map (kbd "C-x") 'kill-region)
  ;; (define-key helm-map (kbd "C-c") 'kill-ring-save)
  ;; (define-key helm-map (kbd "<C-return>") 'helm-kill-selection-and-quit)

  ;; swap the TAB and C-j meanings, this makes TAB again act as completion
  ;; TODO: assigning these doesn't work for some reason?
  ;; (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
  ;; (define-key helm-map (kbd "TAB") 'helm-select-action)
)

(defun kam-find-file-at-point ()
  (interactive)
  "Find file at point, possibly using helm."
  (if (require 'helm-config nil 'noerror)
      (progn
        (setq helm-ff-guess-ffap-filenames t)
        (setq helm-ff-guess-ffap-urls      t)
        ;; when helm is available, it's better to use standard helm-find-files
        ;; than find-file-at-point. find-file-at-point would also be
        ;; completed using helm, but it would have less options.
        ;; See https://groups.google.com/forum/#!topic/emacs-helm/Y-RKJGLxNu4
        ;; https://github.com/emacs-helm/helm/issues/984
        (call-interactively 'helm-find-files))
    (call-interactively 'find-file-at-point)))

;; helm-projectile -----------------------------------------------------------

;; See http://tuhdo.github.io/helm-projectile.html ,
;; https://github.com/bbatsov/helm-projectile

(when (require 'helm-projectile nil 'noerror)
  (setq helm-projectile-fuzzy-match nil)
  (setq projectile-completion-system 'helm)
  ;; (helm-projectile-on) ;; rather useless for me, I don't use default projectile keybindings anyway?

  ;; By default, helm-projectile-find-file/dir includes results
  ;; from current dired, which I don't like to see.
  ;; So define my own commands, similar to helm-projectile-find-file/dir,
  ;; that don't have them.
  (helm-projectile-command "kam-find-file" helm-source-projectile-files-list "Find file: ")
  (helm-projectile-command "kam-find-dir" helm-source-projectile-directories-list "Find dir: ")

  (defun kam-optional-helm-projectile-find-file ()
    (interactive)
    "Open file in projectile project, or just open any file if outside project."
    (if (projectile-project-p)
        (call-interactively 'helm-projectile-kam-find-file)
      (call-interactively 'kam-helm-find-files-no-ffap)))
  (defun kam-optional-helm-projectile-find-dir ()
    (interactive)
    "Open dir in projectile project, or just open any file if outside project."
    (if (projectile-project-p)
        (call-interactively 'helm-projectile-kam-find-dir)
      (call-interactively 'dired)))

  (define-key projectile-mode-map (kbd "M-f") 'kam-optional-helm-projectile-find-file)
  (define-key projectile-mode-map (kbd "M-d") 'kam-optional-helm-projectile-find-dir)
  (define-key projectile-mode-map (kbd "M-s") 'helm-projectile-switch-project)
  (define-key svn-status-mode-map (kbd "M-s") 'helm-projectile-switch-project) ;; also override svn shortcut
  (setq projectile-switch-project-action 'projectile-dired)

  ;; Hm, while it is cool to have incremental results, you cannot easily
  ;; leave the results buffer, and jump using next-error, previous-error.
  ;; Saving results C-x C-s, or making them editable C-c C-e, look ugly
  ;; and are not as functional as normal grep (or ag) results).
  ;;
  ;; Don't use helm-ag for now.
  ;;
  ;; (when (require 'helm-ag nil 'noerror)
  ;;   (define-key projectile-mode-map (kbd "M-g") 'helm-projectile-ag))
)

;; ag (outside helm/projectile) ----------------------------------------------

(global-set-key (kbd "C-g") 'ag)

;; ---------------------------------------------------------------------------

(provide 'kambi-helm-projectile-ag)

;;; kambi-helm-projectile-ag.el ends here