# Goal

Trying to get Pascal intelligent completion in Emacs using LSP.

LSP is cross-editor protocol, known from VS Code but useful in other editors alike,
see https://microsoft.github.io/language-server-protocol/ .

## Testing various LSP Pascal servers in VS Code

- LSP server:

    fork 1 https://github.com/genericptr/pascal-language-server

      ```
      git clone https://github.com/genericptr/pascal-language-server genericptr-pascal-language-server
      cd genericptr-pascal-language-server
      fix in options.pas: constructor Create(_commands: TStringArray);
      sudo apt install libsqlite3-dev
      lazbuild pasls.lpi

      TODO: cannot make it to work in VS Code, VS Code reports (when opening any Pascal file):

      [Error - 02:25:51] Server initialization failed.
        Message: TFPCUnitToSrcCache.GetConfigCache missing CompilerFilename
        Code: -32603
      [Error - 02:25:51] Starting client failed
        Message: TFPCUnitToSrcCache.GetConfigCache missing CompilerFilename
        Code: -32603
      ```

    fork 2: https://github.com/Isopod/pascal-language-server
            https://github.com/Kagamma/pascal-language-server

      ```
      git clone https://github.com/Isopod/pascal-language-server
      cd pascal-language-server/
      git submodule update --init --recursive
      cd server
      lazbuild pasls.lpi

      DONE: Works in VS Code, for simple Pascal programs.

      TODO: Make it aware of CGE paths, make it do completion in CGE units like gamestatemain.pas
      ```

- VS Code Extension (not useful for Emacs users, just mentioning for completeness):

    https://github.com/genericptr/pasls-vscode

    ```
    git clone https://github.com/genericptr/pasls-vscode
    install vsix
    config:
    - FPC sources:
    - Laz sources:
    - FPC exe:
    - pasls exe: the one you got from above
    ```

## Use from Emacs

- lsp-pascal from https://github.com/arjanadriaanse/lsp-pascal is in Melpa now.
  So just install it.

- lsp-mode will be installed as dependency of lsp-pascal

- lsp-ivy (because I like Ivy completion)

- lsp-ui

## Customize

```
 '(lsp-pascal-command
   "/home/michalis/elisp/lsp/pascal-language-server/lib/x86_64-linux/pasls")
 ;; Should this lead to /home/michalis/installed/fpclazarus/current/fpc or fpcsrc?
 ;; Description suggests it's for source, name suggests it's passed to FPC so it should contain just compiled units.
 ;; Later: Message on FPC mailing lists confirms it's for source.
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
