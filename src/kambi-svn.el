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
  (defun kam-svn-show-diff ()
    (svn-diff ".")
  )
  (setq log-edit-diff-function 'kam-svn-show-diff)
)

(provide 'kambi-svn)
