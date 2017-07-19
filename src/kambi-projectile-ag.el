;;;; Configuration for helm (related to also projectile and ag).
;;
;; See https://github.com/bbatsov/projectile
;; Installable through MELPA.

(require 'kambi-utils)

;; projectile ----------------------------------------------------------------

(when (require 'projectile nil 'noerror)
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-find-dir-includes-top-level t)

  ;; setting projectile-mode-line through customize doesn't work reliably
  ;; (on Debian river), so do it here.
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

  ;; projectile with CGE interaction.
  ;; Can be tested executing (projectile-project-type) .
  (projectile-register-project-type 'castle-engine '("CastleEngineManifest.xml")
    :compile "castle-engine compile --mode=debug && castle-engine run"
    :test "castle-engine run"
    :run "castle-engine run"
  )

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

(defvar kam-force-compilation-not-in-project nil
  "When non-nil (you usually want to only set it buffer-local) then
force our compilation commands `kam-compile' and `kam-compile-immediate'
to use non-project compilation.")
(make-variable-buffer-local 'kam-force-compilation-not-in-project)

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
  "Compile, without asking for a command, possibly using projectile.

Uses projectile if we're inside a project (thus using (proposing/editing/saving)
a project-wide compilation command), unless kam-force-compilation-not-in-project
is defined."
  (interactive)

  ;; do not use this when compile-command is nil
  (unless (stringp compile-command)
    ;;(error "compile-command not defined")
    ;; Test: It seems that projectile-compile-project only defines compile-command at 1st run?
    (call-interactively 'projectile-compile-project)
  )

  (if (and (projectile-project-p)
           (not kam-force-compilation-not-in-project))

      (let ((saved-compilation-read-command compilation-read-command))
        (message (concat "Compiling using projectile with " compile-command))
        (setq compilation-read-command nil)
        (call-interactively 'projectile-compile-project)
        (setq compilation-read-command saved-compilation-read-command)
      )

    (compile compile-command)
  )
)

;; ag (outside projectile) ---------------------------------------------------

(global-set-key (kbd "C-g") 'ag)

;; ---------------------------------------------------------------------------

(provide 'kambi-projectile-ag)

;;; kambi-projectile-ag.el ends here
