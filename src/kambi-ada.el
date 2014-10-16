(require 'compile)
(require 'ada-mode)

;; kambi-ada-mode -------------------------------------------------

(define-derived-mode
  kambi-ada-mode ada-mode "Kambi-Ada"
  "Kambi's version of `ada-mode' -- removed automatic indentation.")

(define-keys-to-nil kambi-ada-mode-map '("\C-j" "\C-m"))

;; List of Ada extensions below is based on grepping ada-mode.el for
;; `ada-add-extensions'
(add-to-list 'auto-mode-alist '("\\.ads\\'"      . kambi-ada-mode))
(add-to-list 'auto-mode-alist '("\\.adb\\'"      . kambi-ada-mode))
(add-to-list 'auto-mode-alist '("\\.ads\\.dg\\'" . kambi-ada-mode))
(add-to-list 'auto-mode-alist '("\\.adb\\.dg\\'" . kambi-ada-mode))

;; other kambi customizations for ada ----------------------------------------

(add-hook 'ada-mode-hook
  (lambda ()
    (set-local-compile-command
      (concat "gnat make " (file-name-nondirectory buffer-file-name)))
  ) t)

;; ------------------------------------------------------------

(provide 'kambi-ada)

;; eof ------------------------------------------------------------
