;; see https://github.com/emacs-lsp/lsp-mode
(setq lsp-keymap-prefix "<C-f11>") ;; must be before (require 'lsp-mode)
(require 'lsp-mode)

;; configure lsp-pascal
(require 'lsp-pascal)

;; configure lsp-pascal variables
(setq lsp-pascal-command "/home/michalis/sources/lsp/castle-pascal-language-server/server/lib/x86_64-linux/pasls")
;;(setq lsp-pascal-command "/home/michalis/sources/lsp/genericptr-pascal-language-server/lib/x86_64-linux/pasls")
 ;; Should this lead to /home/michalis/installed/fpclazarus/current/fpc or fpcsrc?
 ;; Description suggests it's for source, name suggests it's passed to FPC so it should contain just compiled units.
 ;; Later: Message on FPC mailing lists confirms it's for source.
(setq lsp-pascal-fpcdir "/home/michalis/installed/fpclazarus/current/fpcsrc/")
(setq lsp-pascal-lazarusdir "/home/michalis/installed/fpclazarus/current/lazarus")
(setq lsp-pascal-pp "/home/michalis/installed/fpclazarus/current/fpc/bin/x86_64-linux/fpc.sh")

;; enable lsp-pascal for my kambi-pascal-mode
(add-hook 'kambi-pascal-mode-hook #'lsp)

;; Lines below somewhat repeat job done by lsp-pascal,
;; but I need to set it up for my kambi-pascal-mode.
;;
;; Without this, lsp-mode will report
;;   Warning (lsp-mode): Unable to calculate the languageId for buffer ‘a.pas’. Take a look at ‘lsp-language-id-configuration’. The ‘major-mode’ is kambi-pascal-mode
(add-to-list 'lsp-language-id-configuration '(kambi-pascal-mode . "pascal"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection (lambda ()
                                                          lsp-pascal-command))
                  :major-modes '(opascal-mode pascal-mode kambi-pascal-mode)
                  :environment-fn (lambda ()
                                    '(("FPCDIR" . lsp-pascal-fpcdir)
                                      ("PP" . lsp-pascal-pp)
                                      ("LAZARUSDIR" . lsp-pascal-lazarusdir)
                                      ("FPCTARGET" . lsp-pascal-fpctarget)
                                      ("FPCTARGETCPU" . lsp-pascal-fpctargetcpu)))
                  :server-id 'pasls))

;; Disable AC (autocomplete), to make sure I only use company for completion using LSP.
;; They actually can work nicely together -- but for now I want to make sure I exercise company.
;; Note: for some (subjective) comparison, see https://github.com/company-mode/company-mode/issues/68 .
(setq ac-modes (delete 'kambi-pascal-mode ac-modes))

(defun kambi-pascal-lsp-config ()
  (require 'company)
  ;; use company autocompletion, that uses LSP
  (local-set-key (kbd "<tab>") 'company-complete)
  (local-set-key (kbd "<C-prior>") 'lsp-find-declaration)
  (local-set-key (kbd "<C-next>") 'lsp-find-definition)
)
(add-hook 'kambi-pascal-mode-hook 'kambi-pascal-lsp-config)

;; otherwise aborting is bound to C-g, unintuitive
(with-eval-after-load 'company
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

;; use company e.g. also in shell mode
(add-hook 'after-init-hook 'global-company-mode)

(provide 'kambi-pascal-lsp)
