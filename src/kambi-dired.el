;;;; Configuration for dired and related dired-xxx packages.

;; Make it possible to change omit mode
;; See https://www.emacswiki.org/emacs/DiredOmitMode
(when (or (> emacs-major-version 24)
          (and (= emacs-major-version 24) (>= emacs-minor-version 4)))
  (require 'dired-x)
)

(defvar kam-current-dired-omit t
  "Global variable controlling whether we hide some files in Dired")
(defun kam-dired-toggle-omit-mode ()
  "Toggle direct omit mode."
  (interactive)
  (setq kam-current-dired-omit (not kam-current-dired-omit))
  (dired-omit-mode (if kam-current-dired-omit t 0))
)

(defvar kam-current-dired-details nil
  "Global variable controlling whether we prefer new Dired buffer to show details")
(defun kam-toggle-dired-details ()
  (interactive)
  (setq kam-current-dired-details (not kam-current-dired-details))
  (dired-hide-details-mode (if kam-current-dired-details 0 t))
  (message (concat "Dired show details: " (if kam-current-dired-details "YES" "NO")))
)

(defun kam-dired-du-mode ()
  (interactive)
  (when (dired-du-mode 'toggle)
    ;; when entering dired-du-mode (it returns t then),
    ;; also show details (since dired-du-mode is useless when they are hidden)
    (unless kam-current-dired-details
      (kam-toggle-dired-details)
    )
  )
)

(defun kam-dired-start ()
  ;; redefine it to Kambi standard shortcut
  (local-set-key (kbd "M-o") 'other-window)

  ;; consistent with helm find-files
  (local-set-key (kbd "C-l") 'dired-jump)
  ;; consistent with caja
  (local-set-key (kbd "M-<up>") 'dired-jump)
  (local-set-key (kbd "M-<down>") 'dired-find-file)
  ;; consistent with ido and ivy
  (local-set-key (kbd "<backspace>") 'dired-jump)

  ;; h to hide / unhide
  (local-set-key (kbd "h") 'kam-dired-toggle-omit-mode)

  ;; D to toddle "du" mode.
  ;; TODO: if I could sort by this size, it would be best...
  ;; But I can't, "s" only toggles name/date sorting.
  ;; dired-quick-sort allows to sort by size.. but not by "du" output.
  (local-set-key (kbd "D") 'kam-dired-du-mode)

  (local-set-key (kbd "C-1") 'kam-toggle-dired-details)
  (local-set-key (kbd "C-2") 'kam-toggle-dired-details)

  ;; c = collapse, not done by default as it costs a bit of time sometimes,
  ;; and also is not possible when something not readable (e.g. cannot enter /
  ;; because /lost+found not readable)
  (local-set-key (kbd "c") 'dired-collapse-mode)

  (local-set-key (kbd "i") 'dired-subtree-insert)

  (when (fboundp 'dired-omit-mode)
    ;; every change of dired-omit-mode makes (kam-dired-refresh)
    ;; call, because it activates dired-after-readin-hook,
    ;; so don't place this in (kam-dired-refresh)
    (dired-omit-mode (if kam-current-dired-omit t 0))
  )

  (kam-dired-refresh)
)
(add-hook 'dired-mode-hook 'kam-dired-start t)

(defun kam-dired-refresh ()
  ;; Refresh dired-hide-details-mode status to refresh
  ;; current kam-current-dired-details variable.
  (when (fboundp 'dired-hide-details-mode)
    (dired-hide-details-mode (if kam-current-dired-details 0 t))
  )
)
;; We depend that dired-after-readin-hook actually happens every time
;; you go back to dired buffer, which is true in my configuration,
;; with "always auto revert".
(add-hook 'dired-after-readin-hook 'kam-dired-refresh)

(when (require 'dired-quick-sort nil 'noerror)
  (dired-quick-sort-setup))

(defun set-dired-backup-overwrite ()
  ;; Although I set this by customize, but still in dired mode
  ;; moving files said
  ;; "dired-rename-file: Symbol's value as variable is void: dired-backup-overwrite"
  ;; Setting this in hook helps.
  (setq dired-backup-overwrite t))
(add-hook 'dired-mode-hook 'set-dired-backup-overwrite)

;; wdired --------------------------------------------------------------------

;; Use 'r' key in dired and then you can edit filenames and permissions
;; (toggle permissions with space). C-x C-s commits, C-c C-k kills (aborts)
;; changes.
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
(setq wdired-allow-to-change-permissions 't)

;; ---------------------------------------------------------------------------
;; delete-by-moving-to-trash setup, somewhat related to dired

(defun kam-delete-by-moving-to-trash (directory)
  (when (and (file-accessible-directory-p directory)
              (>= emacs-major-version 23))
    (setq delete-by-moving-to-trash t)
    (setq trash-directory directory)))
(kam-delete-by-moving-to-trash "~/tmp")

;; ---------------------------------------------------------------------------
;; workaround http://stackoverflow.com/questions/4076360/error-in-dired-sorting-on-os-x
;; on Mac OS X

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

;; This can be uncommented to use Cygwin's "ls" on Windows, makes dired output better
;; (sorted, with dirs first).
;; Unfortunately it's also slower.
;; (when (eq system-type 'windows-nt)
;;   (require 'ls-lisp)
;;   (setq ls-lisp-use-insert-directory-program t))

;; ---------------------------------------------------------------------------

(provide 'kambi-dired)

;;; kambi-dired.el ends here
