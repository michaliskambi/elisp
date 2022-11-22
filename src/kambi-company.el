;; Configure company code completion.
;; On 2022-11 I'm in the process of switch auto-complete -> company,
;; because company is integrated with lsp-mode.

(when (require 'company nil 'noerror)

  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "<tab>") 'company-complete)
    (define-key company-mode-map (kbd "<C-prior>") 'lsp-find-declaration)
    (define-key company-mode-map (kbd "<C-next>") 'lsp-find-definition)

    ;; otherwise aborting is bound to C-g, unintuitive
    (define-key company-active-map (kbd "<escape>") #'company-abort)
    (define-key company-active-map (kbd "<tab>") #'company-complete-selection)

    ;; Testing options from https://company-mode.github.io/manual/Getting-Started.html#Getting-Started
    ;; (define-key company-active-map (kbd "C-t") #'company-complete-common-or-cycle)
    ;; (define-key company-active-map (kbd "C-c")
    ;;   (lambda ()
    ;;     (interactive)
    ;;     (company-complete-common-or-cycle -1)))
  )

  ;;(add-hook 'after-init-hook 'company-tng-mode)

  ;; Company completion can be configured to ignore case. E.g. `Event.is` -> should offer completion for `IsKey`.
  ;; We do this in kambi-customizations.el :
  ;;   (company-keywords-ignore-case t)

  ;; do not use company in shell-mode, as "../" completing to "../../" is confusing my typing
  (setq company-global-modes '(not shell-mode magit-mode))

  ;; use company in all modes (except listed above)
  (add-hook 'after-init-hook 'global-company-mode)

  ;; Disable AC (autocomplete), to make sure I only use company for completion
  ;; (and I prefer company, as company is integrated with lsp-mode).
  ;;
  ;; They actually can work somewhat together (some UI is weird sometimes, but workable).
  ;; But for now I want to make sure I exercise company.
  ;; Also UI is actually a bit weird (try e.g. on EmacsLisp).
  ;;
  ;; Note: for some (subjective) comparison, see https://github.com/company-mode/company-mode/issues/68 .
  ;;
  ;;(setq ac-modes (delete 'kambi-pascal-mode ac-modes))
  (setq ac-modes nil)
)

(provide 'kambi-company)
