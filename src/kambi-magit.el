(defun kam-magit-mode-hook ()

  ;; redefine back to Kambi standard shortcuts
  (local-set-key (kbd "M-1") 'delete-other-windows)
  (local-set-key (kbd "M-2") 'split-window-vertically)
  (local-set-key (kbd "M-3") 'split-window-horizontally)
  (local-set-key (kbd "C-w") 'kill-this-buffer)

  ;; redefine back to Kambi standard shortcuts - these are overridden in diff view
  ;; when committing
  (local-set-key (kbd "<C-tab>") 'switch-buf)
  (local-set-key (kbd "<C-return>") 'kam-find-file-at-point)

  ;; consistent with "e" in diff-mode (e.g. when viewing psvn/dsvn diffs)
  (if (functionp 'magit-diff-toggle-refine-hunk)
      (local-set-key (kbd "e") 'magit-diff-toggle-refine-hunk)
    ;; older function name, for older magit in older Emacs versions
    (local-set-key (kbd "e") 'magit-toggle-diff-refine-hunk))

  ;; How to find the keymap? Execute (current-active-maps),
  ;; and compare with contents of "describe-variable" for various "magit*map" variables.
  ;; (define-key magit-hunk-section-map (kbd "C-c C-c") 'cua-copy-region)
  (define-key magit-hunk-section-map (kbd "C-c") 'cua-copy-region)
  (define-key magit-file-section-map (kbd "C-c") 'cua-copy-region) ;; make C-c work on the line "modified   FILENAME"
  ;; Do not override C-x, as it would override ability for C-x C-f, C-x C-r etc.
  ;; (define-key magit-hunk-section-map (kbd "C-x") 'cua-cut-region)
  (define-key magit-hunk-section-map (kbd "C-v") 'cua-paste) ;; actually this is already OK

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
      (local-set-key (kbd "`") 'magit-show-level-4-all)
    (local-set-key (kbd "`") 'magit-section-show-level-4-all))

  ;; fix colors for terminals (like for ssh session in castle-engine.io),
  ;; otherwise default colors are hardly visible on terminals.
  (unless (display-graphic-p)
    (set-face-foreground 'magit-diff-removed "black")
    (set-face-background 'magit-diff-removed "red")
    (set-face-foreground 'magit-diff-removed-highlight "white")
    (set-face-background 'magit-diff-removed-highlight "red")

    (set-face-foreground 'magit-diff-added "black")
    (set-face-background 'magit-diff-added "green")
    (set-face-foreground 'magit-diff-added-highlight "white")
    (set-face-background 'magit-diff-added-highlight "green")
  )
)
(add-hook 'magit-mode-hook 'kam-magit-mode-hook t)

(provide 'kambi-magit)
