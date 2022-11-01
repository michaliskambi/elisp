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

- company (completion framework that lsp needs to work)

## Add this to ~/.emacs

```
(add-to-list 'load-path (concat kambi-elisp-path "lsp/"))
(require 'kambi-pascal-lsp)
```

## Test

Open a new Pascal file.

Declare instance of some known class from used unit, e.g. TList.

Type `MyInstance.` and then M-x company-capf.
This should be intelligent completion, listing TList properties/methods now.

TODO:

- Explore how to configure it best from Emacs.
  For now I just bound "Tab" to company-capf.

  Any more functionality from company autocompletion?
    read https://company-mode.github.io/
  Any more functionality from LSP?

- Anything like "code complete" (Ctrl Shift C in Lazarus?)

- Anything like "just to interface" / "jump to implementation" from Lazarus

- See previous TODO: how to make pasls aware of CGE units
