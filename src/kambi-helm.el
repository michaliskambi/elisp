;;;; Michalis configuration for helm (and projectile and ag, when related to helm).

(require 'kambi-utils)

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

;; (add-hook 'helm-ag-mode-hook 'kam-no-wrap-lines) ;; doesn't have any effect?

;; ---------------------------------------------------------------------------

(provide 'kambi-helm)

;;; kambi-helm.el ends here
