(use-package csharp-mode
  :config

  (defun kam-indent-block-4-spaces ()
    "Indents current region by 1 space."
    (interactive)
    (increase-left-margin (region-beginning) (region-end) 4)
  )

  (defun kam-unindent-block-4-spaces ()
    "Unindents current region by 1 space."
    (interactive)
    (decrease-left-margin (region-beginning) (region-end) 4)
  )

  (add-hook 'csharp-mode-hook
    (lambda ()
      ;; tab width = 4 like in MonoDevelop
      (setq c-basic-offset 4)
      (setq tab-width 4)
      ;; tab key just inserts tab
      (local-set-key (kbd "TAB") 'self-insert-command)
      ;; indent by 4 spaces
      (local-set-key (kbd "M-i") 'kam-indent-block-4-spaces)
      (local-set-key (kbd "M-u") 'kam-unindent-block-4-spaces)
    )
    t)

  ;; Set the extra indentation before a substatement (e.g. the opening brace in
  ;; the consequent block of an if statement) to 0 (instead of '+).
  ;; Probably useless now since we disable automatic indentation in kambi-csharp-mode.
  ;; (c-set-offset 'substatement-open 0)

  (define-derived-mode
    kambi-csharp-mode csharp-mode "K-C#"
    "Kambi's version of `csharp-mode' -- removed automatic indentation.")

  (define-keys-to-nil kambi-csharp-mode-map
    '("{" "}" ";" "#" ":" "(" ")" "\t" "\C-d" "\177" "," "*" "/"))

  (add-to-list 'auto-mode-alist '("\\.cs\\'" . kambi-csharp-mode))
)

;; Following https://joshwolfe.ca/posts/emacs-csharp-development/ ,
;; omnisharp gives us code completion in C#.
;;
;; Note: This doesn't use lsp-mode, it implements some things on its own.
;;
;; Useful:
;;   omnisharp-solution-errors
;;
;; Also
;; use-package docs: https://github.com/jwiegley/use-package
(use-package omnisharp
  :after company
  :config
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-to-list 'company-backends 'company-omnisharp)
  (define-key csharp-mode-map (kbd "<C-return>") 'omnisharp-go-to-definition)
  (define-key csharp-mode-map (kbd "<M-left>") 'pop-tag-mark)
)

(provide 'kambi-csharp)
