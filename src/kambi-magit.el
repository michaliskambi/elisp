(defun kam-magit-mode-hook ()

  ;; redefine back to Kambi standard shortcuts
  (local-set-key (kbd "M-1") 'delete-other-windows)
  (local-set-key (kbd "M-2") 'split-window-vertically)
  (local-set-key (kbd "M-3") 'split-window-horizontally)
  (local-set-key (kbd "C-w") 'kill-this-buffer)

  ;; use XXX to invoke magit-show-level-XXX
  ;; (by default under M-XXX in Magit, at least on Magit version in Elementary OS;
  ;; in Debian testing, with Magit 2.1.0, these functions have different names
  ;; and are already assigned to shortcuts 1/2/3/4)
  (when (functionp 'magit-show-level-1-all) (local-set-key (kbd "1") 'magit-show-level-1-all))
  (when (functionp 'magit-show-level-2-all) (local-set-key (kbd "2") 'magit-show-level-2-all))
  (when (functionp 'magit-show-level-3-all) (local-set-key (kbd "3") 'magit-show-level-3-all))
  (when (functionp 'magit-show-level-4-all) (local-set-key (kbd "4") 'magit-show-level-4-all))
  ;; this is the usual way to view diffs in magit
  (if (functionp 'magit-show-level-4-all)
      (local-set-key (kbd "=") 'magit-show-level-4-all)
    (local-set-key (kbd "=") 'magit-section-show-level-4-all))
)
(add-hook 'magit-mode-hook 'kam-magit-mode-hook t)

(provide 'kambi-magit)
