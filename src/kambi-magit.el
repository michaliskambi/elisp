(defun kam-magit-mode-hook ()
  ;; redefine back to Kambi standard shortcuts
  (local-set-key (kbd "M-1") 'delete-other-windows)
  (local-set-key (kbd "M-2") 'split-window-vertically)
  (local-set-key (kbd "M-3") 'split-window-horizontally)
  (local-set-key (kbd "C-w") 'kill-this-buffer)
  ;; use C-xxx to invoke magit-show-level-XXX (by default under M-xxx in Magit)
  (local-set-key (kbd "C-1") 'magit-show-level-1-all)
  (local-set-key (kbd "C-2") 'magit-show-level-2-all)
  (local-set-key (kbd "C-3") 'magit-show-level-3-all)
  (local-set-key (kbd "C-4") 'magit-show-level-4-all)
  ;; this is the usual way to view diffs in magit
  (local-set-key (kbd "=") 'magit-show-level-4-all)
)
(add-hook 'magit-mode-hook 'kam-magit-mode-hook t)

(provide 'kambi-magit)
