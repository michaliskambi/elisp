(kam-install-if-needed (list
  'yasnippet
  'company
  'lsp-mode
  'lsp-ui
  'lsp-pascal
))

;; see https://github.com/emacs-lsp/lsp-mode
(setq lsp-keymap-prefix "<C-f11>") ;; must be before (require 'lsp-mode)
(require 'lsp-mode)

;; configure lsp-pascal
(require 'lsp-pascal)

;; configure lsp-pascal variables

(if kam-is-windows
    (progn
      (setq lsp-pascal-command "D:/cygwin64/home/michalis/sources/castle-engine/pascal-language-server/server/lib/x86_64-win64/pasls.exe")
      ;;(setq lsp-pascal-command "c:/Program Files (x86)/Embarcadero/Studio/22.0/bin/DelphiLSP.exe")

      (setq lsp-pascal-fpcdir "D:/cygwin64/home/michalis/installed/fpclazarus/3.2.2-lazarus2.2/fpcsrc/")
      (setq lsp-pascal-lazarusdir "D:/cygwin64/home/michalis/installed/fpclazarus/3.2.2-lazarus2.2/lazarus")
      (setq lsp-pascal-pp "D:/cygwin64/home/michalis/installed/fpclazarus/3.2.2-lazarus2.2/fpc/bin/x86_64-win64/fpc.exe")
      (setq lsp-pascal-fpctarget "win64") ;;/ TODO should be autodetected
      (setq lsp-pascal-fpctargetcpu "x86_64") ;;/ TODO should be autodetected
    )

  (progn
    ;;(setq lsp-pascal-command "/home/michalis/sources/lsp/pascal-language-server/server/lib/x86_64-linux/pasls")
    (setq lsp-pascal-command "/home/michalis/sources/lsp/castle-isopod-pascal-language-server/server/lib/x86_64-linux/pasls")
    ;;(setq lsp-pascal-command "/home/michalis/sources/lsp/castle-genericptr-pascal-language-server/lib/x86_64-linux/pasls")

    ;; Should this lead to /home/michalis/installed/fpclazarus/current/fpc or fpcsrc?
    ;; Description suggests it's for source, name suggests it's passed to FPC so it should contain just compiled units.
    ;; Later: Message on FPC mailing lists confirms it's for source.
    (setq lsp-pascal-fpcdir "/home/michalis/installed/fpclazarus/current/fpcsrc/")
    (setq lsp-pascal-lazarusdir "/home/michalis/installed/fpclazarus/current/lazarus")
    (setq lsp-pascal-pp "/home/michalis/installed/fpclazarus/current/fpc/bin/x86_64-linux/fpc.sh")
    (setq lsp-pascal-fpctarget "linux") ;;/ TODO should be autodetected
    (setq lsp-pascal-fpctargetcpu "x86_64") ;;/ TODO should be autodetected
  )
)

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
                  :initialization-options
                    (lambda ()
                      `(
                        ;; initialization options handled by Ryan Joseph LSP server
                        :showSyntaxErrors t
                        :overloadPolicy 3
                        :maximumCompletions 100
                        :insertCompletionsAsSnippets t
                        :insertCompletionProcedureBrackets nil
                        :includeWorkspaceFoldersAsUnitPaths t
                        :includeWorkspaceFoldersAsIncludePaths t
                        :checkSyntax t
                        :publishDiagnostics t
                        :documentSymbols t
                        ;; initialization options handled by CGE fork of Philip Zander LSP server, see https://github.com/Isopod/pascal-language-server/pull/2
;;                        :syntaxErrorReportingMode 1
                       )
                    )
                  :server-id 'pasls))

;; Filter messages, to avoid obscuring LSP messages (e.g. Pascal syntax errors).
;;
;; Adjusted from https://www.emacswiki.org/emacs/EchoArea#h5o-3 ,
;; (removed from it hacky way to extend *Messages* buffer directly
;; that doesn't work in Emacs 27.1).
(defvar kam-message-filter-regexp-list '("^No completion found$")
  "Filter messages, to avoid obscuring LSP messages (e.g. Pascal syntax errors).")
(defadvice message (around message-filter-by-regexp activate)
  (if (not (ad-get-arg 0))
      ad-do-it
    (let ((formatted-string (apply 'format (ad-get-args 0))))
      (unless (and (stringp formatted-string)
               (some (lambda (re) (string-match re formatted-string)) kam-message-filter-regexp-list))
        (progn
          (ad-set-args 0 `("%s" ,formatted-string))
          ad-do-it)))))

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
  (yas-minor-mode-on)
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
