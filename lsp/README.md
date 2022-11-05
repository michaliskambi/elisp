# Goal

Trying to get Pascal intelligent completion in Emacs (and VS Code) using LSP.

LSP is cross-editor protocol, known from VS Code but useful in other editors alike,
see https://microsoft.github.io/language-server-protocol/ .

## Various LSP Pascal servers

## Arjan Adriaanse repo (original, unmaintained)

https://github.com/arjanadriaanse/pascal-language-server started it all, but after 4 commits in 2020 it seems unmaintained.

I found 2 maintained forks of it that I got to work (by creating forks of my own, through once I figure out more I'll want to contribute back).

### Ryan Joseph (genericptr) fork

https://github.com/genericptr/pascal-language-server

Michalis fork: https://github.com/michaliskambi/pascal-language-server-genericptr

Installation:

```
sudo apt install libsqlite3-dev
git clone https://github.com/michaliskambi/pascal-language-server-genericptr
cd pascal-language-server-genericptr
lazbuild pasls.lpi
create $HOME/.config/pasls/castle-pasls.ini following https://github.com/michaliskambi/pascal-language-server-genericptr docs
```

Notes:

* DONE: initially failed to work, VS Code reports (when opening any Pascal file):

    ```
    [Error - 02:25:51] Server initialization failed.
      Message: TFPCUnitToSrcCache.GetConfigCache missing CompilerFilename
      Code: -32603
    [Error - 02:25:51] Starting client failed
      Message: TFPCUnitToSrcCache.GetConfigCache missing CompilerFilename
      Code: -32603
    ```

    From Emacs lsp-pascal, failed with the same message.

    Fixed by https://github.com/michaliskambi/pascal-language-server-genericptr/commit/210f131f9cd32a9441f90d4613938c503fa1ec03

* Nice: It exposes extra FPC options, which allows me to add CGE paths from VS Code.

    Still, in my fork I added ability to read `~/.config/pasls/castle-pasls.ini` (compatible with other fork). This makes it easier to provide all CGE paths, and this works with all text editors (Emacs, VS Code) with a single configuration.

### Philip Zander (Isopod) fork

https://github.com/Isopod/pascal-language-server

Kagamma fork: https://github.com/Kagamma/pascal-language-server

Michalis fork: https://github.com/castle-engine/pascal-language-server

Installation:

```
git clone https://github.com/castle-engine/pascal-language-server cge-pascal-language-server
cd cge-pascal-language-server/
git submodule update --init --recursive
cd server
lazbuild pasls.lpi
create $HOME/.config/pasls/castle-pasls.ini following https://github.com/castle-engine/pascal-language-server docs
```

Notes:

* DONE: Works in VS Code, for simple Pascal programs.

* TODO: Make it aware of even LCL units? Seems like it cannot find any LCL unit, despite setting Lazarus dir.

* DONE: Make it aware of CGE paths, make it do completion in CGE units like gamestatemain.pas.
  Done in https://github.com/castle-engine/pascal-language-server by special option in config file.

* TODO: extremely fragile when unit on uses clause not found,
  and its poor in finding such units.
  Needs
  - config to read units in Lazarus automatically?
  - read units in current project automatically.

## Other editors than Emacs (mentioning it here for completeness)

### VS Code Extension

https://github.com/genericptr/pasls-vscode

```
git clone https://github.com/genericptr/pasls-vscode
install vsix in VS Code
config:
- FPC sources:
- Laz sources:
- FPC exe:
- pasls exe: the one you got from above
```

Tested, works nicely, with both pasls forks.

### Neovim

Available in https://github.com/Isopod/pascal-language-server repo in client/nvim ( https://github.com/Isopod/pascal-language-server/tree/master/client/nvim ).

## Installing necessary packages in Emacs

- `lsp-pascal` from https://github.com/arjanadriaanse/lsp-pascal is in Melpa now.
  So just install it as a normal Emacs packages (e.g. choosing in `M-x package-list-packages`).

- `lsp-mode` will be installed as dependency of `lsp-pascal`, good

- `lsp-ivy` (because I like Ivy completion)

- `lsp-ui`

- `company` (completion framework that `lsp-mode` needs to work)

- `yasnippet` (necessary to show nicely routine parameters)

## Add this to ~/.emacs

```
(add-to-list 'load-path (concat kambi-elisp-path "lsp/"))
(require 'kambi-pascal-lsp)
```

Read and customize `kambi-pascal-lsp.el`.

## What works

* Completion aware of methods/properties in each namespace.

    Demo:
    * Open a new Pascal file.
    * Declare instance of some known class from used unit, e.g. `TList`.
    * Type `MyInstance.` and then `M-x company-complete`.
    * This should be intelligent completion, listing `TList` properties/methods now.

* Ctrll+clicking on identifier jumps to declaration (works in VS Code and Emacs alike).

* When you start `(`, you see parameters of method/routine.

    TODO: How to show them all in Emacs?
    Currently I only see 1st parameters (in case of overloads) in Emacs, with prefix like "1/2", I still don't know how to view them all.
    And/or how to see all possible parameters using a tooltop, like in VS Code.
    So functionality works in Emacs, but presentation is poor.

* "Go to Definition" / "Go to Declaration"

    In VS Code: context menu (right click), ctrl + click, F12.

    In Emacs: context menu (right click), ctrl + click, shortcuts mentioned there.
    lsp-find-declaration
    lsp-find-definition

## TODO (in general, for both VS Code and Emacs)

- Is there anything like "Code complete" (Ctrl+Shift+C in Lazarus) available in any fork?

    So that e.g. writing "MyButton.OnClick := @Foo", pressing Ctrl+Shift+C would
    automatically create empty Foo implementation.

## TODO (specifically for Emacs)

- How to see CGE docs in company mode?

  company-mode in Emacs can show docs in F1.
  How to configure it to show docs of CGE routine?
  It is useful from VS Code too?

- How to make company-show-location work?

  company docs say:

  """
  C-w ¶
  Display a buffer with the definition of the selected candidate (company-show-location).
  """

  but it does nothing for me.

- In Emacs: make completion not case sensitive, e.g. Event.is should complete IsKey.
