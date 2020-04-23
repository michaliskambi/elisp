Testing https://microsoft.github.io/language-server-protocol/
with
https://github.com/arjanadriaanse/pascal-language-server
https://github.com/arjanadriaanse/lsp-pascal/blob/master/lsp-pascal.el

# Install

lsp-ivy
lsp-ui lsp-
mode

# Customize

```
 '(lsp-pascal-command
   "/home/michalis/elisp/lsp/pascal-language-server/lib/x86_64-linux/pasls")
 ;; TODO: or should this lead to /home/michalis/installed/fpclazarus/current/fpc?
 ;; Description suggests it's for source, name suggests it's passed to FPC so it should contain just compiled units.
 '(lsp-pascal-fpcdir "/home/michalis/installed/fpclazarus/current/fpcsrc/")
 '(lsp-pascal-lazarusdir "/home/michalis/installed/fpclazarus/current/lazarus")
 '(lsp-pascal-pp
   "/home/michalis/installed/fpclazarus/current/fpc/bin/x86_64-linux/fpc.sh")
```

# Add this to ~/.emacs

```
;; see https://github.com/emacs-lsp/lsp-mode
(setq lsp-keymap-prefix "<C-f11>") ;; must be before (require 'lsp-mode)
(require 'lsp-mode)

;; configure lsp-pascal
(add-to-list 'load-path (concat kambi-elisp-path "lsp/lsp-pascal/"))
(require 'lsp-pascal)
(add-hook 'kambi-pascal-mode-hook #'lsp)
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

;; TODO:
;; - M-x company-capf doesn't complete
;; - F11 T h says it doesn't do anything
;; - at exit: Error processing message (lsp-unknown-message-type #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("jsonrpc" "2.0" "error" #s(hash-table size 65 test equal rehash-size 1.5 rehash-threshold 0.8125 data ("code" -32603 "message" "Access violation")) "id" nil))).
;; Am I doing something wrong?
```
