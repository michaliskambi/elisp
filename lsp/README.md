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

* Tested in both VS Code and Emacs.

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

* (NICE): It exposes extra FPC options as LSP initialization options, which allows me to add CGE paths from VS Code.

     This also means I could, in principle, use it with CGE without my mods to add `~/.config/pasls/castle-pasls.ini`. Though `~/.config/pasls/castle-pasls.ini` still makes it easier by allowing me to provide just single CGE path.

* (SOLVED IN MY FORK, BUT WITH A HACK): On really invalid syntax (e.g. open LFM and try to use kambi-pascal-mode) it can send invalid JSON to LSP (`{"result":,"id":85,"jsonrpc":"2.0"}`), and Emacs will spam console with errors.

    ```
    Warning (lsp-mode): Failed to parse the following chunk:
    ’’’
    Content-Type: application/vscode-jsonrpc; charset=utf-8
    Content-Length: 35

    {"result":,"id":85,"jsonrpc":"2.0"}Content-Type: application/vscode-jsonrpc; charset=utf-8
    Content-Length: 39

    {"result":null,"id":86,"jsonrpc":"2.0"}Content-Type: application/vscode-jsonrpc; charset=utf-8
    Content-Length: 39

    {"result":null,"id":87,"jsonrpc":"2.0"}
    ’’’
    with message (json-parse-error unexpected token near ',' <callback> 1 11 11)
    ```

* TODO: (possibly Emacs lsp-mode or lsp-pascal problem, not related to this LSP server -- but the Philip Zander's fork is better in this regard): When a unit is missing, it fails silently in Emacs, saying "no completions". But in VS Code there is a clear message "unit not found: Xxx".

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

Notes specific to this fork:

* Tested in both VS Code and Emacs.

* DONE: Make it aware of CGE paths, make it do completion in CGE units like gamestatemain.pas.
  Done in https://github.com/castle-engine/pascal-language-server by special option in config file.

* TODO: It doesn't take extra FPC options as LSP initialization options, though author is open to add this functionality ( https://github.com/michaliskambi/elisp/issues/1 ).

* As an extra feature, can read configuration from Lazarus options in home directory. This is completely optional though.

* Nice: When the unit is missing on the `uses` clause (this makes it impossible to do code completion, in both LSP server forks and in Lazarus IDE) it results in clear error so you know _which unit is missing_.

### Notes about both:

* Their capabilites are really quite even, due to them both enabling just Lazarus CodeTools as LSP server.

* Both are active.

     https://github.com/genericptr/pascal-language-server has last commit on October 16th, 2022.

     https://github.com/Isopod/pascal-language-server has last commit on November 2021, but the maintainer was quick to find + address my findings on https://github.com/michaliskambi/elisp/issues/1 , and encourage merge requests. Much appreciated.

* Both LSP server forks **and Lazarus IDE too** are rather "fragile" when it comes to having non-existing units on the `uses` clause. The code completion fails until the CodeTools can find the unit.

    Testcase:

    - I opened CGE unit https://github.com/castle-engine/castle-engine/blob/master/examples/creature_behaviors/code/gamestatemenu.pas .

    - I tested my forks that add simple CGE support ( https://github.com/michaliskambi/pascal-language-server-genericptr , https://github.com/castle-engine/pascal-language-server ) -- this just results in adding a bunch of `-Fuxxx`, `-Fixxx` to FPC options. I tested also Lazarus IDE 2.2.2.

    - If I edit the `TStateMenu.Start` to say `But` and try to complete...

        ```delphi
        begin
          inherited;
          ButtonPlay.OnClick := {$ifdef FPC}@{$endif} ClickPlay;
          ButtonQuit.OnClick := {$ifdef FPC}@{$endif} ClickQuit;
          // Hide "Quit" button on mobile/console platforms, where users don't expect such button
          ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;

          But
        end;
        ```

        -> this works great. It says to me that it can complete `But` to `ButtonPlay` or `ButtonQuit`.

    - But if I now mess up the `uses` clause, adding there non-existent name...

        ```delphi
        uses CastleApplicationProperties, CastleWindow,
          GameStatePlay, Foobar;
        ```

        -> now completion completely fails. `But` cannot be completed to anything. Which is a shame, it would be better IMHO if CodeTools would just ignore the missing `Foobar` in the `uses` clause.

* TODO: We have to make LSP server find and understand `CastleEngineManifest.xml` in containing directory, and extract extra file paths from it. Alternatively: extract extra paths from LPI.

    This is extra important in the light of above (CodeTools fail if unit cannot be found). E.g. right now code completion fails on CGE `tests/code/testcases/testcastlecomponentserialize.pas`, because it cannot find `CastleTestCase` which is in `tests/code/tester-fpcunit/`. And LSP cannot guess by itself to search in `../tester-fpcunit/` for this. Only project files (we maintain both `CastleEngineManifest.xml` and LPI for this) contain the necessary information to find all units.

* Both LSP servers fail at finding LCL units (unless I misconfigured them).

    Testcase:

    - editing https://github.com/michaliskambi/pascal-language-server-genericptr/blob/castle-master/references.pas

    - go to line 91, https://github.com/michaliskambi/pascal-language-server-genericptr/blob/castle-master/references.pas#L91 , after `StartSrcCode:=CodeToolBoss.LoadFile(Filename,false,false);` assignment

    - type `Start` and try to complete.

         Philip Zander's LSP server will fail to complete anything, answering that `Laz_AVL_Tree` cannot be found (error visible in Emacs).

         Ryan Joseph's LSP server will fail to complete anything. Without any message in Emacs (so it is a bit worse), though there is a clear message in VS Code.

         Lazarus IDE is better: it can do code completion, so it evidently finds `Laz_AVL_Tree` (since we know it would fail to do code completion when units are missing).

    - Removing `Laz_AVL_Tree`, Philip Zander's LSP server reports it cannot find `LazFileUtils`. Then `CTUnitGraph`, and then `CodeCache`. Code completion starts to work only once I hack `uses` clause to be this:

        ```delphi
        uses
          { RTL }
          SysUtils, Classes,
          { CodeTools }
          URIParser, //CodeToolManager, //CodeCache, {CTUnitGraph,}
          { LazUtils }
          //LazFileUtils, Laz_AVL_Tree,
          { LSP }
          lsp, basic, general;
        ```

        It will not compile of course, I removed all LCL units, but I can complete `Start` identifier now :)

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
