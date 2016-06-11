(add-to-list 'load-path (concat kambi-elisp-path "contrib/psvn/"))

(require 'psvn)

(defun kam-svn-status-force-kill ()
  "Run `svn rm --force' on all selected files.
See `svn-status-marked-files' for what counts as selected."
  (interactive)
  (svn-status-rm t))

  ;; (interactive "P")
  ;; (message "Forcefully removing: %S" (svn-status-get-file-list-names t))
  ;; (svn-status-create-arg-file (svn-status-get-file-list t))
  ;; (svn-run t t 'rm "rm" "--force" "--targets" svn-status-temp-arg-file))

;; "k" removes file under point, including non-versioned files.
;; Consistent wit magit.
(define-key svn-status-mode-map (kbd "k") 'kam-svn-status-force-kill)

(provide 'kambi-svn)
