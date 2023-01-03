(kam-install-if-needed (list
  'yasnippet
  'company
  'lsp-mode
  'lsp-ui
  'lsp-pascal
))

;; configure company autocompletion, it is used by lsp-mode
(require 'kambi-company)

;; used by lsp-mode to display routine parameters
(yas-global-mode 1)

;; see https://github.com/emacs-lsp/lsp-mode
(setq lsp-keymap-prefix "<C-f11>") ;; must be before (require 'lsp-mode)
(require 'lsp-mode)

(define-key lsp-mode-map (kbd "<C-return>") 'lsp-find-definition)

;; configure lsp-pascal
(require 'lsp-pascal)

;; configure lsp-pascal variables
;;
;; Note: CGE pasls will autodetect OS/CPU, if they are not specified explicitly, based on current platform.
;; Note: Philip Zander LSP will try to autodetect it looking at Lazarus config too.
;; So you don't strictly need to specify them.
(if kam-is-windows
    (progn
      (setq lsp-pascal-command "D:/cygwin64/home/michalis/sources/castle-engine/castle-engine/bin/pasls.exe")
      ;;(setq lsp-pascal-command "c:/Program Files (x86)/Embarcadero/Studio/22.0/bin/DelphiLSP.exe")

      (setq lsp-pascal-fpcdir "D:/cygwin64/home/michalis/installed/fpclazarus/3.2.2-lazarus2.2/fpcsrc/")
      (setq lsp-pascal-lazarusdir "D:/cygwin64/home/michalis/installed/fpclazarus/3.2.2-lazarus2.2/lazarus")
      (setq lsp-pascal-pp "D:/cygwin64/home/michalis/installed/fpclazarus/3.2.2-lazarus2.2/fpc/bin/x86_64-win64/fpc.exe")
      (setq lsp-pascal-fpctarget "win64")
      (setq lsp-pascal-fpctargetcpu "x86_64")
    )

  (progn
    ;;(setq lsp-pascal-command "/home/michalis/sources/lsp/pascal-language-server/server/lib/x86_64-linux/pasls")
    (setq lsp-pascal-command "/home/michalis/sources/lsp/castle-isopod-pascal-language-server/server/pasls")
    ;;(setq lsp-pascal-command "/home/michalis/sources/lsp/castle-genericptr-pascal-language-server/lib/x86_64-linux/pasls")

    ;; Should this lead to /home/michalis/installed/fpclazarus/current/fpc or fpcsrc?
    ;; Description suggests it's for source, name suggests it's passed to FPC so it should contain just compiled units.
    ;; Later: Message on FPC mailing lists confirms it's for source.
    (setq lsp-pascal-fpcdir "/home/michalis/installed/fpclazarus/current/fpcsrc/")
    (setq lsp-pascal-lazarusdir "/home/michalis/installed/fpclazarus/current/lazarus")
    (setq lsp-pascal-pp "/home/michalis/installed/fpclazarus/current/fpc/bin/x86_64-linux/fpc.sh")
    (setq lsp-pascal-fpctarget "linux")
    (setq lsp-pascal-fpctargetcpu "x86_64")
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

(provide 'kambi-pascal-lsp)
