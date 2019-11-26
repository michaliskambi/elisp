;;;; Utilities related to developing Castle Game Engine.

(require 'kambi-pascal)

;; Filenames constants --------------------------------------------------

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
      (kam-file-name-in-directory kam-home-directory "/sources/castle-engine/castle-engine")
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
      (castle-engine-path "src/ui/")
      (castle-engine-path "src/ui/opengl/")
      (castle-engine-path "src/game/")
      (castle-engine-path "src/files/")
      (castle-engine-path "src/services/")
      (castle-engine-path "src/services/opengl/")
      ;; pasdoc
      ;; (concat kam-home-directory "/sources/pasdoc/trunk/source/component/")
      ;; (concat kam-home-directory "/sources/pasdoc/trunk/source/console/")
      ;; (concat kam-home-directory "/sources/pasdoc/trunk/source/tools/")
    )
  )
  "Directories where CGE and other Pascal units may be found.
Each entry must be terminated by a slash (on Windows, also backslash is allowed)."
)

(defun kam-find-pascal-file (str)
  "Search for file in various directories where Pascal units are kept.
Filename to search (sans directory) is extracted from the STR.
The first directory where to search is also extracted from STR.
This way it will work Ok if STR already contains a valid (relative or not)
path to the existing file (in particular STR without a path means
that path is an empty string --- which indicates to start looking in current dir).

(Not anymore:

  Afterwards searches known projectile files
  (this way it searches Castle Game Engine, FPC, Lazarus sources and such,
  as I use projectile for them all).
)

It doesn't try to add any file extension. So make sure STR contains the full
name of the file (with extension). See `kam-ffap-kambi-pascal-mode'
for a function that tries to guess an extension when searching for file.

Searches for STR ignoring case.
FPC suggests (and I always follow this convention,
as it matches Unix conventions) to use lowercase for Pascal unit names
on case-sensitive filesystems.

This is nice to install on `ffap-alist' when you get something that already
has a Pascal extension."
  (block func-block

    (let (file-name
          dir-to-search
          (search-name (file-name-nondirectory str))
          (search-name-lower (downcase (file-name-nondirectory str)))
         )

      (dolist
        ;; search 1st inside (extract-file-path str),
        ;; then in every item on pascal-units-paths.
        ;; Searching on pascal-units-paths here is not necessary
        ;; (CGE is anyway known by projectile), but it's faster than
        ;; relying on projectile when moving around CGE units.
        ;;
        ;; Note the need for extract-file-path, instead of file-name-directory,
        ;; below: we need empty string in case there's no directory.
        (dir-to-search (cons (extract-file-path str) pascal-units-paths))

        ;; although we could implement it nicer, by first trying
        ;; whole algorithm with original case, then with lowercase,
        ;; it would be less optimal: we want to search first on current path,
        ;; then on other non-recursive paths, and only when it fails:
        ;; on recursive paths.

        (setq file-name (concat dir-to-search search-name))
        (when (kam-nondir-file-readable-p file-name) (return-from func-block file-name))
        (setq file-name (concat dir-to-search search-name-lower))
        (when (kam-nondir-file-readable-p file-name) (return-from func-block file-name))
      )
    )

    ;; otherwise fallback on projectile knowledge

    ;; (kam-find-pascal-file-using-projectile str)
  )
)

;; tests:
;; (kam-find-pascal-file "unix.pp")
;; (kam-find-pascal-file "windows.pp")
;; (kam-find-pascal-file "CastleImages.pas")
;; (kam-find-pascal-file "castleimages.pas")
;; (kam-find-pascal-file "castleconf.inc")

;; Unused, because (projectile-all-project-files) does not work reliably,
;; in non-project or (later versions) in any project
;; (error "You're not in a project", although it should work regardless
;; of the current project).
;;
;; This in effect was causing errors from kam-ffap-kambi-pascal-mode
;;
;; (defun kam-find-pascal-file-using-projectile (str)
;;   "Search for Pascal file STR (must not contain any directory part,
;; must contain extension) in projects known to projectile.
;; This way we utilize projectile knowledge to search units in FPC, CGE sources.
;; Returns the full path (string) or nil if not found."
;;   (let ((all-known-files (projectile-all-project-files))
;;         (search-suffix (concat "/" str))
;;        )
;;     (block func-block
;;       (dolist (known-file all-known-files)
;;         (when (s-suffix-p search-suffix known-file t)
;;           (return-from func-block known-file))
;;       )
;;       nil ; not found
;;     )
;;   ))

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
;; (kam-ffap-kambi-pascal-mode "CastleImages")

(dolist (ext pascal-exts-regexps)
  (add-to-list 'ffap-alist `(,ext . kam-find-pascal-file)))
(add-to-list 'ffap-alist '(kambi-pascal-mode . kam-ffap-kambi-pascal-mode))

(add-to-list 'ffap-string-at-point-mode-alist
  '(kambi-pascal-mode "--:\\\\$+<>@-Z_[:alpha:]~*?" "<@" "@>;.,!:"))

;; Compilation adjustments ---------------------------------------------------

;; FPC kiedy drukuje komunikat error/warning/itp. podaje nazwe pliku
;; ale bez sciezki. Wiec ustawiamy tutaj zeby Emacs search in all our
;; unit paths.
(add-to-list 'compilation-error-regexp-alist
  (list "^\\([^(\n]+\\)(\\([0-9]+\\),\\([0-9]+\\))" 1 2 3))
(setq compilation-search-path
  (append compilation-search-path pascal-units-paths))

(defun kam-is-castle-engine-project-p (file-name)
  "Is the file inside a castle-engine project, that is inside
a project with CastleEngineManifest.xml."
  (or
    (file-exists-p (concat (file-name-directory file-name) "CastleEngineManifest.xml"))
    (and
      (not (member (file-name-directory file-name) '("/" "~/")))
      ;; avoid infinite recursion
      (not (equal file-name           (file-name-directory (directory-file-name (file-name-directory file-name)))))
      (kam-is-castle-engine-project-p (file-name-directory (directory-file-name (file-name-directory file-name))))
    )
  )
)

(defun kam-is-castle-engine-project-but-not-projectile-p (file-name)
  "Is the file inside a castle-engine project, that is inside
a project with CastleEngineManifest.xml, but the CastleEngineManifest.xml directory
is NOT the top-level directory for projectile."
  (or
    (and
      (file-exists-p (concat (file-name-directory file-name) "CastleEngineManifest.xml"))
      (not (file-exists-p (concat (file-name-directory file-name) ".git")))
      (not (file-exists-p (concat (file-name-directory file-name) ".svn")))
      (not (file-exists-p (concat (file-name-directory file-name) ".projectile")))
    )
    (and
      (not (member (file-name-directory file-name) '("/" "~/")))
      ;; avoid infinite recursion
      (not (equal file-name                              (file-name-directory (directory-file-name (file-name-directory file-name)))))
      (kam-is-castle-engine-project-but-not-projectile-p (file-name-directory (directory-file-name (file-name-directory file-name))))
    )
  )
)

(defun kam-install-exe (exe-name)
  "Command-line to move exe file to my ~/bin/ subdirectory."
(concat
  "mv "
  exe-name
  kam-os-exe-extension
  " "
  kam-home-directory
  "/bin/"
  exe-name
  kam-os-exe-extension)
)

(defun kam-pascal-compile-command (file-name)
  "Return compile-command for file-name calculated the way I like
for Pascal sources. Detects my various projects and their compilation setup.

Returns nil if compilation of this command is better controlled project-wide
by projectile."
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
      (if (string-is-suffix "tools/build-tool/code/" dir-name)
          (concat "cd .. && sh castle-engine_compile.sh && " (kam-install-exe "castle-engine"))
        (if (string-is-suffix "castle_game_engine/tests/" dir-name)
            (concat "./compile_console.sh && ./test_castle_game_engine -a")
          (if (string-is-suffix "castle-engine/tests/" dir-name)
              (concat "./compile_console.sh && ./test_castle_game_engine -a")
            (if (kam-is-castle-engine-project-but-not-projectile-p file-name)
                (concat "castle-engine compile --mode=debug && castle-engine run")
              (if (kam-is-castle-engine-project-p file-name)
                  ;;(concat "castle-engine compile --mode=debug && castle-engine run")
                  nil ;; return nil to leave compilation command under projectile (per-project) control
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
    )
  ) ;; let
) ;; defun

(defun kam-clean-cge ()
  "Clear CGE (useful to recompile it cleanly, or after FPC internal error)."
  (interactive)
  (async-shell-command "clean-cge"))

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
    (let ((com-command (kam-pascal-compile-command (buffer-file-name))))
      (when com-command
          (set-local-compile-command com-command)
          (setq kam-force-compilation-not-in-project t))
    )
    (add-hook 'compilation-filter-hook 'kam-pascal-compilation-filter t)

    ;; Workaround opascal mode problem, see
    ;; https://emacs.stackexchange.com/questions/20567/syntax-highlighting-strings-incorrectly-for-strings-in-opascal-mode
    ;; Don't escape backslashes in pascal
    (modify-syntax-entry ?\\ ".")
  ) t)

;; Useful replacements -------------------------------------------------------

(defun kam-cge-api-upgrades ()
  (interactive)
  (save-excursion
    ;; this needs to be early, otherwise the type replacement below with handle "Vector3Single" replacement
    (kam-beg-of-buf) (query-replace-regexp "Vector3SingleCut(\\([^-+*,()]+\\))" "\\1.XYZ")
    (kam-beg-of-buf) (query-replace-regexp "Vector3SingleCut(\\([^()]+\\))" "(\\1).XYZ")

    ;; types and constructor names

    (kam-beg-of-buf) (query-replace  "ZeroVector2Single"       "TVector2.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector3Single"       "TVector3.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector4Single"       "TVector4.Zero")

    (kam-beg-of-buf) (query-replace  "ZeroVector2Double"       "TVector2Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector3Double"       "TVector3Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector4Double"       "TVector4Double.Zero")

    (kam-beg-of-buf) (query-replace  "ZeroMatrix2Single"       "TMatrix2.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix3Single"       "TMatrix3.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix4Single"       "TMatrix4.Zero")

    (kam-beg-of-buf) (query-replace  "ZeroMatrix2Double"       "TMatrix2Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix3Double"       "TMatrix3Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix4Double"       "TMatrix4Double.Zero")

    (kam-beg-of-buf) (query-replace  "IdentityMatrix2Single"       "TMatrix2.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix3Single"       "TMatrix3.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix4Single"       "TMatrix4.Identity")

    (kam-beg-of-buf) (query-replace  "IdentityMatrix2Double"       "TMatrix2Double.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix3Double"       "TMatrix3Double.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix4Double"       "TMatrix4Double.Identity")

    (kam-beg-of-buf) (query-replace  "Vector2Single"       "Vector2")
    (kam-beg-of-buf) (query-replace  "Vector3Single"       "Vector3")
    (kam-beg-of-buf) (query-replace  "Vector4Single"       "Vector4")

    (kam-beg-of-buf) (query-replace  "Matrix2Single"       "Matrix2")
    (kam-beg-of-buf) (query-replace  "Matrix3Single"       "Matrix3")
    (kam-beg-of-buf) (query-replace  "Matrix4Single"       "Matrix4")

    (kam-beg-of-buf) (query-replace  "Triangle2Single"       "Triangle2")
    (kam-beg-of-buf) (query-replace  "Triangle3Single"       "Triangle3")
    (kam-beg-of-buf) (query-replace  "Triangle4Single"       "Triangle4")

    (kam-beg-of-buf) (query-replace  "UnitVector2Single" "TVector3.One")
    (kam-beg-of-buf) (query-replace  "UnitVector2" "TVector3.One")

    (kam-beg-of-buf) (query-replace  "UnitVector3Single" "TVector3.One")
    (kam-beg-of-buf) (query-replace  "UnitVector3" "TVector3.One")

    (kam-beg-of-buf) (query-replace  "UnitVector4Single" "TVector3.One")
    (kam-beg-of-buf) (query-replace  "UnitVector4" "TVector3.One")

    (kam-beg-of-buf) (query-replace  "EmptyBox3D" "TBox3D.Empty")

    (kam-beg-of-buf) (query-replace  "VectorProduct" "TVector3.CrossProduct")
    (kam-beg-of-buf) (query-replace  "VectorDotProduct" "TVector3.DotProduct")
    (kam-beg-of-buf) (query-replace  "VectorsPerfectlyEqual" "TVector3.PerfectlyEquals")
    (kam-beg-of-buf) (query-replace  "MatricesPerfectlyEqual" "TMatrix4.PerfectlyEquals")
    (kam-beg-of-buf) (query-replace  "FloatsEqual" "SameValue")
    (kam-beg-of-buf) (query-replace  "VectorsEqual" "TVector3.Equals")
    (kam-beg-of-buf) (query-replace  "TriangleDir(" "TriangleDirection(")

    ;; this is actually a change in CGE 6.2
    (kam-beg-of-buf) (query-replace  "PointInside" "Contains")
    (kam-beg-of-buf) (query-replace  "GLFadeRectangle(" "GLFadeRectangleDark(")

    ;; 0-argument vector/matrix methods
    (kam-beg-of-buf) (query-replace-regexp "VectorLenSqr(\\([^-+*,()]+\\))" "\\1.LengthSqr")
    (kam-beg-of-buf) (query-replace-regexp "VectorLenSqr(\\([^()]+\\))" "(\\1).LengthSqr")

    (kam-beg-of-buf) (query-replace-regexp "VectorLen(\\([^-+*,()]+\\))" "\\1.Length")
    (kam-beg-of-buf) (query-replace-regexp "VectorLen(\\([^()]+\\))" "(\\1).Length")

    (kam-beg-of-buf) (query-replace-regexp "Normalized(\\([^-+*,()]+\\))" "\\1.Normalize")
    (kam-beg-of-buf) (query-replace-regexp "Normalized(\\([^()]+\\))" "(\\1).Normalize")

    (kam-beg-of-buf) (query-replace-regexp "VectorToNiceStr(\\([^-+*,()]+\\))" "\\1.ToString")
    (kam-beg-of-buf) (query-replace-regexp "VectorToNiceStr(\\([^()]+\\))" "(\\1).ToString")

    (kam-beg-of-buf) (query-replace-regexp "VectorToRawStr(\\([^-+*,()]+\\))" "\\1.ToRawString")
    (kam-beg-of-buf) (query-replace-regexp "VectorToRawStr(\\([^()]+\\))" "(\\1).ToRawString")

    (kam-beg-of-buf) (query-replace-regexp "VectorAverage(\\([^-+*,()]+\\))" "\\1.Average")
    (kam-beg-of-buf) (query-replace-regexp "VectorAverage(\\([^()]+\\))" "(\\1).Average")

    (kam-beg-of-buf) (query-replace-regexp "PerfectlyZeroVector(\\([^-+*,()]+\\))" "\\1.IsPerfectlyZero")
    (kam-beg-of-buf) (query-replace-regexp "PerfectlyZeroVector(\\([^()]+\\))" "(\\1).IsPerfectlyZero")

    ;; after PerfectlyZeroVector
    (kam-beg-of-buf) (query-replace-regexp "ZeroVector(\\([^-+*,()]+\\))" "\\1.IsZero")
    (kam-beg-of-buf) (query-replace-regexp "ZeroVector(\\([^()]+\\))" "(\\1).IsZero")

    (kam-beg-of-buf) (query-replace-regexp "TriangleNormPlane(\\([^-+*,()]+\\))" "\\1.NormalizedPlane")
    (kam-beg-of-buf) (query-replace-regexp "TriangleNormPlane(\\([^()]+\\))" "(\\1).NormalizedPlane")

    (kam-beg-of-buf) (query-replace-regexp "FloatToNiceStr(\\([^-+*,()]+\\))" "Format('%f', [\\1])")
    (kam-beg-of-buf) (query-replace-regexp "FloatToNiceStr(\\([^()]+\\))" "Format('%f', [(\\1)])")

    ;; 2-argument vector/matrix methods
    (kam-beg-of-buf) (query-replace-regexp "VectorAdjustToLength(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.AdjustToLength(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixMultPoint(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.MultPoint(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixMultDirection(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.MultDirection(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "TryMatrixInverse(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.TryInverse(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixToNiceStr(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.ToString(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixToRawStr(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.ToRawString(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixRow(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.Row(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "TriangleTransform(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.Transform(\\2)")

    ;; misc changes
    (kam-beg-of-buf) (query-replace-regexp "Box.Data\\[\\(.\\), \\(.\\)\\]" "Box.Data[\\1].Data[\\2]")
    (kam-beg-of-buf) (query-replace-regexp "updatesecondspassed" "SecondsPassed")

    ;; other changes during 6.5 development
    (kam-beg-of-buf) (query-replace "palooping" "true")
    (kam-beg-of-buf) (query-replace "panotlooping" "false")
    (kam-beg-of-buf) (query-replace "paforcelooping" "true")
    (kam-beg-of-buf) (query-replace "paforcenotlooping" "false")
    (kam-beg-of-buf) (query-replace "castlegoogleplaygames" "CastleGameService")
    (kam-beg-of-buf) (query-replace "tgoogleplaygames" "TGameService")
    (kam-beg-of-buf) (query-replace "calculatedwidth" "EffectiveWidth")
    (kam-beg-of-buf) (query-replace "calculatedheight" "EffectiveHeight")
    (kam-beg-of-buf) (query-replace "calculatedrect" "EffectiveRect")
    (kam-beg-of-buf) (query-replace "screenrect" "RenderRect")
    (kam-beg-of-buf) (query-replace "tuicontrolsizeable" "TCastleUserInterface")
    (kam-beg-of-buf) (query-replace "tuicontrol" "TCastleUserInterface")
    (kam-beg-of-buf) (query-replace "tcastleuserinterfacerect" "TCastleUserInterface")
    (kam-beg-of-buf) (query-replace "tcastlewindowcustom" "TCastleWindowBase")
    (kam-beg-of-buf) (query-replace "tcastlecontrolcustom" "TCastleControlBase")

    (kam-beg-of-buf) (query-replace "rgbpixels" "Pixels")
    (kam-beg-of-buf) (query-replace "grayscalealphapixels" "Pixels")
    (kam-beg-of-buf) (query-replace "grayscalepixels" "Pixels")
    (kam-beg-of-buf) (query-replace "alphapixels" "Pixels")
    (kam-beg-of-buf) (query-replace ".fdchildren.add(" ".AddChildren(")
    (kam-beg-of-buf) (query-replace "castle3d" "CastleTransform")
    (kam-beg-of-buf) (query-replace "t3dcustomtransform" "TCastleTransform")
    (kam-beg-of-buf) (query-replace "t3dtransform" "TCastleTransform")
    (kam-beg-of-buf) (query-replace "t3dorient" "TCastleTransform")
    (kam-beg-of-buf) (query-replace-regexp "ApplicationData('\\([^']*\\)')" "'castle-data:/\\1'")
    (kam-beg-of-buf) (query-replace-regexp "ToX3DName(\\([^)]*\\))" "\\1")
    (kam-beg-of-buf) (query-replace "http://castle-engine.sourceforge.net" "https://castle-engine.io")
    (kam-beg-of-buf) (query-replace "https://castle-engine.sourceforge.io" "https://castle-engine.io")
    (kam-beg-of-buf) (query-replace "tglimageondemand" "TDrawableImage")
    (kam-beg-of-buf) (query-replace "tglimagecore" "TDrawableImage")
    (kam-beg-of-buf) (query-replace "tglimage" "TDrawableImage")

    (kam-beg-of-buf) (query-replace "tcamerainput" "TNavigationInput")
    ;; (kam-beg-of-buf) (query-replace "assigndefaultcamera" "AssignDefaultNavigation") ;; too risky
    (kam-beg-of-buf) (query-replace "TCamera" "TCastleNavigation") ;; too risky with tcamera lowercase
    (kam-beg-of-buf) (query-replace "twalkcamera" "TCastleWalkNavigation")
    (kam-beg-of-buf) (query-replace "texaminecamera" "TCastleExamineNavigation")

    (kam-beg-of-buf) (query-replace "currentprojectionwidth" "Camera.Orthographic.EffectiveWidth")
    (kam-beg-of-buf) (query-replace "currentprojectionheight" "Camera.Orthographic.EffectiveHeight")
    (kam-beg-of-buf) (query-replace "camerafromviewpoint" "UpdateCamera")
    (kam-beg-of-buf) (query-replace "camerafromnavigationinfo" "UpdateNavigation")
    (kam-beg-of-buf) (query-replace "load3d_filefilters" "LoadScene_FileFilters")
    (kam-beg-of-buf) (query-replace "load3d" "LoadNode")
    (kam-beg-of-buf) (query-replace "autodetectcamera" "AutoCamera")
    (kam-beg-of-buf) (query-replace "autodetectnavigation" "AutoNavigation")
    (kam-beg-of-buf) (query-replace "ciNormal" "niNormal")
    (kam-beg-of-buf) (query-replace "ciMouseDragging" "niMouseDragging")
    (kam-beg-of-buf) (query-replace "ciGesture" "niGesture")
    (kam-beg-of-buf) (query-replace "ci3dMouse" "ni3dMouse")
  ))

(defun kam-cge-delphi-upgrade ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "(specialize " "({$ifdef CASTLE_OBJFPC}specialize{$endif} ")
    (kam-beg-of-buf) (query-replace "= specialize " "= {$ifdef CASTLE_OBJFPC}specialize{$endif} ")
    (kam-beg-of-buf) (query-replace-regexp "^  generic " "  {$ifdef CASTLE_OBJFPC}generic{$endif}\n  ")
    (kam-beg-of-buf) (query-replace-regexp "^  generic$" "  {$ifdef CASTLE_OBJFPC}generic{$endif}")
    (kam-beg-of-buf) (query-replace-regexp "\\([-. (]\\)L\\[" "\\1List^[")
    (kam-beg-of-buf) (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\([-+*/]\\)= \\([^-+*/;]+\\);" " \\1 := \\1 \\3 \\4;")
    ;; if the expression on right-hand side has some +/*/etc then
    ;; for safety it needs parenthesis (for accuracy, in case of +=,
    ;; and for correctness in case of others)
    (kam-beg-of-buf) (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\([-+*/]\\)= \\([^;]+?\\)\\(;\\| else\\)"     " \\1 := \\1 \\3 (\\4)\\5")
    (kam-beg-of-buf) (query-replace-regexp "\\(T\\|P\\)Array_\\([a-zA-Z0-9_]+\\)" "\\1\\2Array")

    (kam-beg-of-buf) (query-replace "%00000001" "$01 { binary 00000001 }")
    (kam-beg-of-buf) (query-replace "%11111110" "$FE { binary 11111110 }")
    (kam-beg-of-buf) (query-replace "%00000011" "$03 { binary 00000011 }")
    (kam-beg-of-buf) (query-replace "%11111100" "$FC { binary 11111100 }")
    (kam-beg-of-buf) (query-replace "%00000111" "$07 { binary 00000111 }")
    (kam-beg-of-buf) (query-replace "%11111000" "$F8 { binary 11111000 }")
    (kam-beg-of-buf) (query-replace "%00001111" "$0F { binary 00001111 }")
    (kam-beg-of-buf) (query-replace "%11110000" "$F0 { binary 11110000 }")
    (kam-beg-of-buf) (query-replace "%00011111" "$1F { binary 00011111 }")
    (kam-beg-of-buf) (query-replace "%11100000" "$E0 { binary 11100000 }")
    (kam-beg-of-buf) (query-replace "%00111111" "$3F { binary 00111111 }")
    (kam-beg-of-buf) (query-replace "%11000000" "$C0 { binary 11000000 }")
    (kam-beg-of-buf) (query-replace "%01111111" "$7F { binary 01111111 }")
    (kam-beg-of-buf) (query-replace "%10000000" "$80 { binary 10000000 }")
    (kam-beg-of-buf) (query-replace "%11111111" "$FF { binary 11111111 }")
    (kam-beg-of-buf) (query-replace "%00000000" "$00 { binary 00000000 }")

    (kam-beg-of-buf) (query-replace-regexp " DepthRange" " RenderContext.DepthRange")
    (kam-beg-of-buf) (query-replace-regexp " ProjectionMatrix" " RenderContext.ProjectionMatrix")

    (kam-beg-of-buf) (query-replace-regexp "{$ifdef windows} *{$apptype gui} *{$endif}" "{$ifdef MSWINDOWS} {$apptype GUI} {$endif}")
    (kam-beg-of-buf) (query-replace-regexp "{$ifdef windows} *{$apptype console} *{$endif}" "{$ifdef MSWINDOWS} {$apptype CONSOLE} {$endif}")
    (kam-beg-of-buf) (query-replace-regexp "{$ifdef windows}" "{$ifdef MSWINDOWS}")
    (kam-beg-of-buf) (query-replace "{$ifndef BUGGY_ZERO_CONSTANT}" "{$ifdef ENABLE_SELF_RECORD_CONSTANTS}")
    (kam-beg-of-buf) (query-replace "{$ifdef BUGGY_ZERO_CONSTANT}" "{$ifndef ENABLE_SELF_RECORD_CONSTANTS}")
  ))

(defun kam-various-pascal-upgrades ()
  (interactive)
  (kam-cge-api-upgrades)
;;  (kam-cge-delphi-upgrade)
)
(global-set-key (kbd "<f5>") 'kam-various-pascal-upgrades)

(defun kam-cge-delphi-upgrade-dangerous ()
  (interactive)
  ;; replacements to negate sign for += and 0-, only for 1-argument right hand side
  (save-excursion
    (query-replace-regexp " \\+= -" " -= ")
  )
  (save-excursion
    (query-replace-regexp " -= -" " += ")
  )
  ;; replacements for += and -= to Inc and Dec, only for integers
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\+= \\([^-+*/;]+\\);" " Inc(\\1, \\3);")
  )
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) -= \\([^-+*/;]+\\);" " Dec(\\1, \\3);")
  )
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\+= \\([^;]+?\\)\\(;\\| else\\)"     " Inc(\\1, \\3)\\4")
  )
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) -= \\([^;]+?\\)\\(;\\| else\\)"     " Dec(\\1, \\3)\\4")
  )
  ;; replacement of @, only when used to callbacks
  (save-excursion
    (query-replace "@" "{$ifdef CASTLE_OBJFPC}@{$endif} ")
  )
)

(defun kam-various-pascal-upgrades-dangerous ()
  (interactive)
  (kam-cge-delphi-upgrade-dangerous)
  (kam-beg-of-buf) (query-replace "t2dscene" "TCastle2DScene")
)
;; (global-set-key (kbd "<f6>") 'kam-various-pascal-upgrades-dangerous)

(defun kam-www-fix ()
  (interactive)
  (save-excursion
    (query-replace-regexp "<a href=\"http://michalis.ii.uni.wroc.pl/cge-www-preview/apidoc/html/\\([^\"]+\\)\">\\([^<]+\\)</a>" "<?php api_link('\\2', '\\1'); ?>")
    (query-replace-regexp "<a href=\"https://castle-engine.io/apidoc/html/\\([^\"]+\\)\">\\([^<]+\\)</a>" "<?php api_link('\\2', '\\1'); ?>")
    (query-replace-regexp "<a href=\"https://castle-engine.io/apidoc-unstable/html/\\([^\"]+\\)\">\\([^<]+\\)</a>" "<?php api_link('\\2', '\\1'); ?>")
  )
)
(global-set-key (kbd "<f6>") 'kam-www-fix)

;; ------------------------------------------------------------

(provide 'kambi-castle-engine)

;; eof ------------------------------------------------------------
