;;;; Kambi Pascal mode and many utils to operate on Pascal code.

;; 2004-12-26: finally I'm shifting from pascal-mode to delphi-mode.
;;
;; Main advantage of delphi-mode for me is support for Delphi
;; 1-line comments "//", this often screws up syntax coloring in
;; pascal mode (e.g. "// don't do this" would make pascal-mode
;; into believing that following text is inside string).
;; Actually, I think that this was a main reason for me to avoid
;; such "//"-style comments in my pascal code, when I started using
;; Emacs long time ago...
;;
;; Moreover I just consider delphi-mode better
;; - it doesn't have SO pissing off indentation:
;;   see kambi-pascal-mode-map,
;;   I only needed to turn off 3 keys there to turn off indentation
;;   while I needed to turn off 7 keys with pascal-mode :)
;; - it has a complete list of ObjectPascal keywords and directives,
;;   while with pascal-mode I needed to add many keywords to make
;;   my Pascal sources really coloured.
;;
;;   Well, actually, it also forced me to give up on colouring some
;;   specials that are not keywords but I like them to be coloured
;;   like "string" and "integer".

(if (featurep 'xemacs)
    (progn
      (load "pascal")
      (defconst use-delphi-mode nil))
  (progn
    (require 'delphi)
    (defconst use-delphi-mode t))
)

(require 'kambi-utils)
(require 'ffap)
(require 'compile)

;; definicja kambi-pascal-mode ----------------------------------------

(defun kambi-pascal-mode-ancestor ()
  (if use-delphi-mode (delphi-mode) (pascal-mode))
)

(define-derived-mode kambi-pascal-mode kambi-pascal-mode-ancestor
  "Kambi-Pascal"
  "Kambi mode for editing Pascal files.

It's derived from delphi-mode (or, if it's not available,
from pascal-mode), just to removes automatic
 (read \"stupid and pissing me off\") indentation.
Well, actually, now that I switched from pascal-mode to delphi-mode,
this default indentation is not so awful, but I still prefer to
use my own simple insert-newline-indented-as-prev.

Note that I cleaned here some code specific only to the case
when use-delphi-mode = nil. Saying it explicitly:
while kambi-pascal-mode works when use-delphi-mode = nil,
it doesn't really work as good as it could.
*The* strongly suggested way is to set use-delphi-mode to non-nil
if only delphi-mode is available.

Derived from delphi-mode, so e.g. hooks in
delphi-mode-hook are also called by this mode."
  (setq comment-start "//")
  (setq comment-end "")
  (setq comment-start-skip comment-start) ;; same thing works best
)

;; wylacz w kambi-pascal-mode-map robienie indentation jakie domyslnie
;; odziedziczylismy po delphi-mode
(define-keys-to-nil kambi-pascal-mode-map '("\r" "\t" "\177"))

;; kilka stalych do operowania do filenames --------------------

(defconst castle-engine-path-base
  (let
    ((env-value (getenv "CASTLE_ENGINE_PATH")))
    ;; <rant>
    ;; file-accessible-directory-p is a brain-dead name for "directory exists"
    ;; function. Why does everything in Emacs and EmacsLisp must have
    ;; weird and/or outdated names, that do not match any nomenclature
    ;; used in modern programming languages and editors?
    ;; </rant>
    (if (and (stringp env-value) (file-accessible-directory-p env-value))
        (if (file-accessible-directory-p (kam-file-name-in-directory env-value "castle_game_engine"))
            (kam-file-name-in-directory env-value "castle_game_engine")
          (if (file-accessible-directory-p (kam-file-name-in-directory env-value "castle-engine"))
              (kam-file-name-in-directory env-value "castle-engine")
            env-value))
      (kam-file-name-in-directory kam-home-directory "/sources/castle-engine/trunk/castle_game_engine")
    )
  )
  "Path to Castle Game Engine units. From $CASTLE_ENGINE_PATH
environment variable, if possible."
)

(unless (fboundp 'castle-engine-path)
  ;; Do not define `castle-engine-path' if it's already there.
  ;; This allows to override this in ~/.emacs before executing kambi-startup.
  (defun castle-engine-path (s)
    (kam-file-name-in-directory castle-engine-path-base s)))

(defun fpc-source-path (s)
  (concat kam-home-directory "/installed/fpc/current/src/" s))

(defun kam-private-pascal-lib-path (s)
  (concat kam-home-directory "/sources/kambi/pas/" s))

(defconst pascal-units-recursive-paths
  (list
    ;; fpc sources paths that contains *only* things not specific
    ;; to any OS/processor go here.
    (fpc-source-path "rtl/inc/")
    (fpc-source-path "rtl/objpas/")
    (fpc-source-path "packages/")
  )
  "List of strigs, directories searched recursively for Pascal unit files.
Must be absolute (since they may be used from various directories).
Each entry must be terminated by / (or \\ on Windows)."
)

(defconst pascal-units-dirs-not-descend
  (list ".svn" ".git")
  "`kam-find-pascal-file' when descending recursively into directories mentioned
in `pascal-units-recursive-paths', does not descend into directories with
names mentioned here."
)

(defconst pascal-units-paths
  (append
    (list
      ;; castle-engine.
      ;; Includes platform-specific paths
      ;; (added to search path regardless of kam-is-windows, kam-is-unix,
      ;; because filenames must be unique (for Lazarus packages) anyway).
      (castle-engine-path "src/base/")
      (castle-engine-path "src/base/android/")
      (castle-engine-path "src/base/unix/")
      (castle-engine-path "src/base/windows/")
      (castle-engine-path "src/base/opengl/")
      (castle-engine-path "src/opengl/")
      (castle-engine-path "src/opengl/windows/")
      (castle-engine-path "src/opengl/unix/")
      (castle-engine-path "src/images/")
      (castle-engine-path "src/images/opengl/")
      (castle-engine-path "src/3d/")
      (castle-engine-path "src/3d/opengl/")
      (castle-engine-path "src/window/")
      (castle-engine-path "src/window/gtk/")
      (castle-engine-path "src/window/unix/")
      (castle-engine-path "src/window/windows/")
      (castle-engine-path "src/x3d/")
      (castle-engine-path "src/x3d/opengl/")
      (castle-engine-path "src/x3d/opengl/glsl/")
      (castle-engine-path "src/audio/")
      (castle-engine-path "src/fonts/")
      (castle-engine-path "src/fonts/opengl/")
      (castle-engine-path "src/fonts/windows/")
      (castle-engine-path "src/castlescript/")
      (castle-engine-path "src/castlescript/opengl/")
      (castle-engine-path "src/ui/")
      (castle-engine-path "src/ui/opengl/")
      (castle-engine-path "src/game/")
      (castle-engine-path "src/net/")
      (castle-engine-path "src/services/")
      ;; pasdoc
      ;; (concat kam-home-directory "/sources/pasdoc/trunk/source/component/")
      ;; (concat kam-home-directory "/sources/pasdoc/trunk/source/console/")
      ;; (concat kam-home-directory "/sources/pasdoc/trunk/source/tools/")
      ;; Kambi private library
      ;; (kam-private-pascal-lib-path "dbase/units/")
      ;; (kam-private-pascal-lib-path "net/units/")
      ;; (kam-private-pascal-lib-path "units/regexpr/")
      ;; (kam-private-pascal-lib-path "video/units/")
      ;; (kam-private-pascal-lib-path "sdl/units/")
      ;; (kam-private-pascal-lib-path "units/bigint/")
      ;; (kam-private-pascal-lib-path "units/bigint/unpacked/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDL/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDL_Mixer/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDL_Image/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDL_Net/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDL_ttf/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/smpeg/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SFont/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDL_Sound/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDLMonoFonts/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDLSpriteEngine/Pas/")
      ;; (kam-private-pascal-lib-path "units/sdl/jedi-sdl/JEDI-SDLv1.0/SDLCtrls/Pas/")
      ;; (kam-private-pascal-lib-path "graph/units/")
      ;; (kam-private-pascal-lib-path "units/synapse/source/lib/")
      ;; (kam-private-pascal-lib-path "units/macroprocessor/")
      ;; (kam-private-pascal-lib-path "mpi/units/")
      ;; (kam-private-pascal-lib-path "delphi/units/components/units/")
      ;; (kam-private-pascal-lib-path "delphi/units/base/")
      ;; (kam-private-pascal-lib-path "delphi/units/dbase/")
      ;; (kam-private-pascal-lib-path "delphi/units/dbase/components/")
      ;; (kam-private-pascal-lib-path "delphi/units/graphics/")
      ;; (kam-private-pascal-lib-path "delphi/units/mdi_mimic/components/")
      ;; fpc source
      (fpc-source-path "rtl/i386/")
      (fpc-source-path "rtl/inc/")
    )
    (when kam-is-windows
      (list
        ;; fpc source (windows-specific)
        (fpc-source-path "rtl/win/")
        (fpc-source-path "rtl/win/wininc/")
      )
    )
    (when kam-is-unix
      (list
        ;; fpc source (Unix-specific)
        (fpc-source-path "rtl/unix/")
      )
    )
    (when kam-is-linux
      (list
        ;; fpc source
        (fpc-source-path "rtl/linux/")
        (fpc-source-path "fcl/linux/")
      )
    )
    (when kam-is-freebsd
      (list
        ;; fpc source
        (fpc-source-path "rtl/bsd/")
        (fpc-source-path "rtl/freebsd/")
      )
    )
  )
  "Directories where Pascal units may be found.
Each entry must be terminated by a slash (on Windows, also backslash is allowed)."
)

(defconst pascal-exts-implicit
  '(".pas" ".pp" ".PAS" ".p")
  "Lista rozszerzen (z poczatkowa kropka, pusty string oznacza brak rozszerzenia)
ktore oznaczaja pliki Pascalowe i ktore niekiedy moga byc pomijane gdy
nazwa pliku Pascalowego gdzies sie pojawia (np. deklaraujac moduly przez \"uses\"
pomijasz rozszerzenie .pas). Powinny byc uporzadkowane w takiej kolejnosci
w jakiej powinny byc szukane - gdy bedziemy dopasowywac do pliku takie
rozszerzenie implicit bedziemy dopasowywac najpierw pierwsze rozszerzenie na
liscie, potem drugie itd.")

(defconst pascal-exts-explicit
  '( ".pasprogram"
     ".dpr" ".inc" ; (dpr is actually Delphi, but I treat is as general Pascal extension)
     ".xfm" ".dfm" ".dpk" ; Delphi
     ".lfm" ".lrs" ".lpr" ; Lazarus
   )
  "Lista rozszerzen (z poczatkowa kropka, pusty string oznacza brak rozszerzenia)
ktore oznaczaja pliki Pascalowe i ktore nigdy nie beda pomijane przy
podawaniu jakiegos pliku. Tzn. te rozszerzenia nie beda nigdy uzywane aby
odgadnac jakis plik w rodzaju tego co robimy w kam-ffap-find-pascal-file,
te rozszerzenia sa uzywane tylko aby rozpoznawac typ pliku.")

(defconst pascal-exts
  (append pascal-exts-implicit pascal-exts-explicit))

(defconst pascal-exts-regexps
  ;; Zmieniajac wartosc tej stalej pamietaj : te wyrazenia moga
  ;; (chociaz nie wszystkie musza) byc tak zbudowane zeby pasowac
  ;; tylko do konca stringa, uzywajac konstrukcji \\' regexp'ow.
  ;; Wtedy nie zmyli ich np. plik o nazwie .passwd
  ;; ktory przeciez pasowalby do regexpa \\.pas
  ;;
  ;; value of constant below is now simply calculated from pascal-exts
  (let (result)
    (dolist (elt pascal-exts result)
      (setq result (cons (concat (regexp-quote elt) "\\'") result))
    )
  )
  "Lista rozszerzen Pascala przedstawiona jako wyrazenia regularne.
Jezeli nazwa pliku pasuje do tego wyrazenia regularnego to mozna wnioskowac
ze ten plik jest w Pascalu." )

;; ------------------------------------------------------------
;; zdefiniuj rozszerzenia dla ktorych domyslnie otworzy mode kambi-pascal

(dolist (ext pascal-exts-regexps)
  (add-to-list 'auto-mode-alist `(,ext . kambi-pascal-mode)))

;; ---------------------------------------------------------------------------
;; Extensions for ffap-file-at-point to search for Pascal files.
;; Searching for Pascal source name like `SysUtils.pas' should find it
;; (in FPC sources, in Kambi sources etc.). Inside Pascal mode,
;; you can even just search for `SysUtils' and it will automatically look
;; for a unit, trying to append various common units' extensions.

(defun nondir-file-readable-p (fname)
  "Return non-nil if the file exists, is readable and is not a directory."
  (and (file-readable-p fname) (not (eq (car (file-attributes fname)) t)))
)

(defun kam-find-pascal-file (str)
  "Search for file in various directories where Pascal units are kept.
Filename to search (sans directory) is extracted from the STR.
The first directory where to search is also extracted from STR.
This way it will work Ok if STR already contains a valid (relative or not)
path to the existing file (in particular STR without a path means
that path is an empty string --- which indicates to start looking in current dir).

Afterwards searches all paths on `pascal-units-paths', `pascal-units-recursive-paths'
 (that hold various my units, FPC sources and such).

It doesn't try to add any file extension. So make sure STR contains the full
name of the file (with extension). See `kam-ffap-kambi-pascal-mode'
for a function that tries to guess an extension when searching for file.

Searches for STR with both original case and lowercase.
FPC suggests (and I always follow this convention,
as it matches Unix conventions) to use lowercase for Pascal unit names
on case-sensitive filesystems.

This is nice to install on `ffap-alist' when you get something that already
has a Pascal extension."
  (block func-block

    (let (dir-to-search
          file-name
          (search-name (file-name-nondirectory str))
          (search-name-lower (downcase (file-name-nondirectory str)))
         )

      ;; although we could implement it nicer, by first trying
      ;; search-name with original case, then with lowercase,
      ;; it would be less optimal: we want to search first on current path,
      ;; then on other non-recursive paths, and only when it fails:
      ;; on recursive paths.

      (dolist
        ;; search 1st inside (extract-file-path str),
        ;; then in every item on pascal-units-paths.
        ;;
        ;; Note the need for extract-file-path, instead of file-name-directory,
        ;; below: we need empty string in case there's no directory.
        (dir-to-search (cons (extract-file-path str) pascal-units-paths))

        (setq file-name (concat dir-to-search search-name))
        (when (nondir-file-readable-p file-name) (return-from func-block file-name))
        (setq file-name (concat dir-to-search search-name-lower))
        (when (nondir-file-readable-p file-name) (return-from func-block file-name))
      )

      (let ((dirs-to-search-recursively pascal-units-recursive-paths)
            (project-dir (kam-project-dir str)))

        (when project-dir
          (setq dirs-to-search-recursively (cons project-dir dirs-to-search-recursively))
        )
        ;; search dir of STR 1st
        (setq dirs-to-search-recursively (cons (extract-file-path str) dirs-to-search-recursively))

        (dolist (dir-to-search dirs-to-search-recursively)

          (setq file-name (kam-search-for-file dir-to-search search-name
            pascal-units-dirs-not-descend t))
          (when file-name (return-from func-block file-name))
          (setq file-name (kam-search-for-file dir-to-search search-name-lower
            pascal-units-dirs-not-descend t))
          (when file-name (return-from func-block file-name))
        )
      )
    )

    nil ; not found, if you didn't returned using (return-from ...) by now
  )
)

;; tests:
;; (kam-find-pascal-file "ibconnection.pp")
;; (kam-find-pascal-file "IBConnection.pp")
;; (kam-find-pascal-file "unix.pp") ; should not work under non-unix
;; (kam-find-pascal-file "windows.pp") ; should not work under non-windows
;; (kam-find-pascal-file "KambiIBFPC.pas")

(defun kam-ffap-kambi-pascal-mode (str)
  "Search for file in various directories where Pascal units are kept,
and try to append various Pascal sources extensions.
Uses `kam-find-pascal-file' to find STR, first unmodified,
then with extensions like `.pas', `.pp' and others added.

The effect is that it magically finds a Pascal source even if you forgot
to provide an extension.

This is nice to install in `ffap-alist' for mode `kambi-pascal-mode',
this way you can use ffap when standing over \"uses\" clauses of your units."
  (let (ext
        (str-ext (extract-file-ext str))
        (buffer-ext (if buffer-file-name (extract-file-ext buffer-file-name) nil))
        (exts-list (cons "" pascal-exts-implicit))
       )

    ;; optimization: if current buffer filename already has a Pascal extension,
    ;; and we don't have any extension in STR,
    ;; then look for the same extension first. This is useful to look for .pp
    ;; units from other .pp units.

    (when (and
        buffer-ext ;; current buffer has some extension
        (eq "" str-ext) ;; STR doesn't contain any extension
        (member buffer-ext exts-list) ;; current buffer extension indicates Pascal
      )
      ;; move buffer-ext to the beginning of exts-list
      (setq exts-list (cons buffer-ext (remove buffer-ext exts-list)))
    )

    ;; (message "str-ext %s buffer-ext %s First extension to look for %s" str-ext buffer-ext (car exts-list))

    (block func-block
      (dolist (ext exts-list)
        (setq result (kam-find-pascal-file (concat str ext)))
        (when result (return-from func-block result))
      )
      nil ; not found
    )
  )
)

;; tests
;; (kam-ffap-kambi-pascal-mode "KambiIBFPC")

(dolist (ext pascal-exts-regexps)
  (add-to-list 'ffap-alist `(,ext . kam-find-pascal-file)))
(add-to-list 'ffap-alist '(kambi-pascal-mode . kam-ffap-kambi-pascal-mode))

;; compiling-related things ----------------------------------------

(defun kam-is-castle-engine-project-p (file-name)
  "Is the file inside a castle-engine project, that is inside
a project with CastleEngineManifest.xml."
  (or
    (file-exists-p (concat (file-name-directory file-name) "CastleEngineManifest.xml"))
    (and
      (not (equal (file-name-directory file-name) "/"))
      (kam-is-castle-engine-project-p
        (file-name-directory (directory-file-name (file-name-directory file-name))))
    )
  )
)

(defun kam-pascal-compile-command (file-name)
  "Return compile-command for file-name calculated the way I like
for Pascal sources. Detects my various projects and their compilation setup."
  (let
    (
      ;; for buffer-file-name like
      ;; .../castle_game_engine/examples/3d_rendering_processing/multiple_viewports.lpr
      ;; return "multiple_viewports".
      (file-base-name (file-name-sans-extension
        (file-name-nondirectory file-name)))

      ;; for buffer-file-name like
      ;; .../castle_game_engine/examples/3d_rendering_processing/multiple_viewports.lpr
      ;; return "3d_rendering_processing".
      ;; (last-dir-name (file-name-nondirectory
      ;;   (directory-file-name (file-name-directory file-name))))

      ;; for buffer-file-name like
      ;; .../castle_game_engine/examples/3d_rendering_processing/multiple_viewports.lpr
      ;; return ".../castle_game_engine/examples/3d_rendering_processing/".
      (dir-name (file-name-directory file-name))

      ;; for buffer-file-name like
      ;; .../castle_game_engine/examples/3d_rendering_processing/multiple_viewports.lpr
      ;; return ".../castle_game_engine/examples/".
      (dir-parent (file-name-directory (directory-file-name (file-name-directory file-name))))

      ;; for buffer-file-name like
      ;; .../castle_game_engine/examples/3d_rendering_processing/multiple_viewports.lpr
      ;; return ".../castle_game_engine/examples/3d_rendering_processing/multiple_viewports_compile.sh".
      (compile-script (concat
        (file-name-sans-extension file-name)
        "_compile.sh"))

      (is-runnable (or
        (string-match-p ".pasprogram$" file-name)
        (string-match-p ".dpr$" file-name)
        (string-match-p ".lpr$" file-name)
      ))
    )

    (if (string-is-suffix "pasdoc/trunk/source/" dir-parent)
        (concat "cd ../.. && make")
      (if (string-is-suffix "tools/build-tool/" dir-name)
          (concat "sh castle-engine_compile.sh && /bin/mv castle-engine" kam-os-exe-extension " ~/bin/")
        (if (string-is-suffix "castle_game_engine/tests/" dir-name)
            (concat "./compile_console.sh && ./test_castle_game_engine -a")
          (if (string-is-suffix "castle-engine/tests/" dir-name)
              (concat "./compile_console.sh && ./test_castle_game_engine -a")
            (if (kam-is-castle-engine-project-p file-name)
                (concat "castle-engine compile --mode=debug && castle-engine run")
              (if (file-exists-p compile-script)
                  (concat "sh " file-base-name "_compile.sh && ./" file-base-name kam-os-exe-extension)
                (if (string-match-p "castle_game_engine" dir-name)
                    (concat "castle-engine simple-compile --mode=debug " (file-name-nondirectory file-name) (when is-runnable (concat " && ./" file-base-name)))
                  (if (string-match-p "castle-engine" dir-name)
                      (concat "castle-engine simple-compile --mode=debug " (file-name-nondirectory file-name) (when is-runnable (concat " && ./" file-base-name)))
                    (concat "fpc " (file-name-nondirectory file-name) (when is-runnable (concat " && ./" file-base-name)))
                  )
                )
              )
            )
          )
        )
      )
    )
  ) ;; let
) ;; defun

(defun kam-clean-cge ()
  "Clear CGE (useful to recompile it cleanly, or after FPC internal error)."
  (interactive)
  (async-shell-command "cc"))

(defun kam-pascal-compilation-filter-remove-lines (line-regexp)
  "Use within compilation-filter-hook implementations.
From https://github.com/haskell/haskell-mode/blob/master/haskell-compile.el ."
  ;; because delete-matching-lines changes point, it seems
  ;; without this, only 1st kam-pascal-compilation-filter-remove-lines
  ;; call works. save-excursion around goto-char is *not* enough.
  (save-excursion
    (delete-matching-lines line-regexp
      (if (boundp 'compilation-filter-start) ;; available since Emacs 24.2
          (save-excursion (goto-char compilation-filter-start)
                          (line-beginning-position))
        (point-min))
      (point)))
)

(defun kam-pascal-compilation-filter ()
  "Filter FPC output.

Note: It's unhandy to do it in compile-command using grep --invert-match,
because

1.It breaks exit status of command, thus breaking sequence like
  ./foo_compile.sh fpc-filter && ./aaa
  Unless you use pipefail or other solutions
  http://unix.stackexchange.com/questions/14270/get-exit-status-of-process-thats-piped-to-another

2.But this all makes your compile-command awfully long, which is bas
  because it's often useful to adjust it when running from a particular
  buffer. So it's not a good idea to make it ultra-complicated.

I also don't want to move this into some private compilation script.
My castle-engine tool should handle all stuff and eliminate the need
for a special compilation script... But I don't workaround there FPC output
problems, at least for now."
  (kam-pascal-compilation-filter-remove-lines "^$")
  (kam-pascal-compilation-filter-remove-lines "contains output sections")
  (kam-pascal-compilation-filter-remove-lines "not found, this will probably cause a linking failure")
  ;; 1 less extra line from output, to make it shorter
  ;; (but still see FPC version by fpc -l).
  ;; FPC is the basis of my game engine, which is the coolest thing I ever did,
  ;; and I love you Florian Klaempfl --- but I don't need to see this line
  ;; on every compilation.
  (kam-pascal-compilation-filter-remove-lines "Copyright (c) .* by Florian Klaempfl and others")
)

;; I could add this to kambi-pascal-mode function, instead of defining
;; my hook. But I simply prefer to use hooks as far as I can.
;; Moreover adjustments below are my personal preferences, so they should
;; be in hook, not in kambi-pascal-mode.
(add-hook 'kambi-pascal-mode-hook
  (lambda ()
    (set-local-compile-command (kam-pascal-compile-command (buffer-file-name)))
    (add-hook 'compilation-filter-hook 'kam-pascal-compilation-filter t)
  ) t)

;; FPC kiedy drukuje komunikat error/warning/itp. podaje nazwe pliku
;; ale bez sciezki. Wiec ustawiamy tutaj zeby Emacs search in all our
;; unit paths.
(add-to-list 'compilation-error-regexp-alist
  (list "^\\([^(\n]+\\)(\\([0-9]+\\),\\([0-9]+\\))" 1 2 3))
(setq compilation-search-path
  (append compilation-search-path pascal-units-paths))

;; KambiScript hacks ---------------------------------------------------------

;; load kambi-pascal-mode on KambiScript
;; (http://castle-engine.sourceforge.net/kambi_script.php)
(add-to-list 'auto-mode-alist '("\\.kscript\\'" . kambi-pascal-mode))

;; auto-complete -------------------------------------------------------------

;; All Pascal keywords to auto-complete in kambi-pascal-mode-hook.
;; Following example for C++ on http://www.emacswiki.org/emacs/AutoComplete

(defconst kam-ac-pascal-keywords
  (sort
    ;; keywords and directives, as strings.
    (mapcar 'symbol-name delphi-keywords)
    #'(lambda (a b) (> (length a) (length b)))))

(defvar kam-ac-source-pascal
  '((candidates
     . (lambda ()
         (all-completions ac-target kam-ac-pascal-keywords))))
  "Source for Pascal keywords.")

(add-hook 'kambi-pascal-mode-hook
  (lambda ()
    (make-local-variable 'ac-sources)
    (add-to-list 'ac-sources 'kam-ac-source-pascal)))

;; ---------------------------------------------------------------------------
;; just some Pascal helpers, to be invoked explicitly

(defun kam-insert-guid ()
  (interactive)
  (call-process "gen_guid" nil t)
)

(defun kam-clear-pascal-method-implementation ()
  "Remove Pascal functions bodies. Useful when simplifying multi-unit
testcase for FPC bug report, and trimming away code that doesn't affect bug."
  (interactive)
  ;; [^`] is used just as "any character including newline"
  ;; [^e] is added to not catch "begin end; another proc begin end;"

  ;; remove first function bodies with preceding "var" sections
;;  (query-replace-regexp "\nvar\n[^`]+?\nbegin\n" "\\nbegin\n")

  (query-replace-regexp "\nbegin\n[^e][^`]+?\nend;\n" "\nbegin { single line, to not match anymore } end;\n")
)

;; ------------------------------------------------------------

(provide 'kambi-pascal)

;; eof ------------------------------------------------------------
