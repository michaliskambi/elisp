;; Note: migrated psvn -> dsvn,
;; because dsvn is in Emacs ELPA / MELPA,
;; and it doesn't include 'cl, so it may be more maintained.
;; See also https://github.com/oliyh/masvn

(when (require 'dsvn nil 'noerror)

  ;; Not needed with dsvn
  ;; (defun kam-svn-status-force-kill ()
  ;;   "Run `svn rm --force' on all selected files.
  ;; See `svn-status-marked-files' for what counts as selected."
  ;;   (interactive)
  ;;   (svn-status-rm t))

    ;; (interactive "P")
    ;; (message "Forcefully removing: %S" (svn-status-get-file-list-names t))
    ;; (svn-status-create-arg-file (svn-status-get-file-list t))
    ;; (svn-run t t 'rm "rm" "--force" "--targets" svn-status-temp-arg-file))

  ;; "k" removes file under point, including non-versioned files.
  ;; Consistent with magit.
  (define-key svn-status-mode-map (kbd "k") 'svn-remove-file)
)

(when (require 'diff-mode nil 'noerror)
  ;; Customizations to diff-mode.
  ;; In theory independent from dsvn customizations, but in practice I use diff-mode
  ;; only with diff-mode.
  ;; Make these keys consistent with my magit keybindings.
  (define-key diff-mode-map (kbd "e") 'diff-refine-hunk)
  (define-key diff-mode-map (kbd "C-c") 'cua-copy-region)
)

(provide 'kambi-svn)
