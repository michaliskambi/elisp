;; Add this to ~/.emacs.
;;
;; Alternatively,
;; - if you have root permissions,
;; - and if you use "emacs --batch" in your private scripts,
;; then consider adding this to /etc/emacs/site-start.el .
;; Then Kambi elisp functions will be automatically available in batch mode
;; (when ~/.emacs is not read, but site elisp is).

(defconst kambi-elisp-path
  (concat (expand-file-name "~") "/elisp/")
  "Path (with final (back-)slash) to Michalis Kamburelis (Kambi) Emacs Lisp stuff.")
(add-to-list 'load-path (concat kambi-elisp-path "src/"))

;; Use customization file (things done by M-x customize) inside kambi-elisp-path.
;; Should be called before kambi-startup, to set cua-rectangle-mark
;; before activating cua in kambi-startup.
(setq custom-file (concat kambi-elisp-path "src/kambi-customizations.el"))
(load custom-file)

;; Initialize Kambi stuff.
(require 'kambi-startup)

;; Set default font.
;; Following https://www.emacswiki.org/emacs/SetFonts
;; and https://www.gnu.org/software/emacs/manual/html_node/emacs/Fonts.html
;; (useful on Windows)
;;(set-frame-font "Courier New-16")
;; (useful on Linux)
;;(set-default-font "DejaVu Sans Mono-12")
;; (useful on MacOSX)
;;(set-default-font "Menio-24")

;; Use tools from MacPorts, may be useful on Mac OS X if using MacPorts
;; (setq magit-git-executable "/opt/local/bin/git")
;; (setq projectile-generic-command "/opt/local/bin/find . -type f -print0")
;; (setq projectile-git-command "/opt/local/bin/git ls-files -zco --exclude-standard")
;; (setq projectile-git-ignored-command "/opt/local/bin/git ls-files -zcoi --exclude-standard")
;; (setq projectile-git-submodule-command  "/opt/local/bin/git submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'")
;; (setq svn-status-svn-executable "/opt/local/bin/svn")

;; Better dired on Mac OS X.
;; http://hocuspokus.net/2008/01/a-better-ls-for-mac-os-x/
;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Dired-ls.html
;; (setq ls-lisp-use-insert-directory-program t)      ;; use external ls
;; (setq insert-directory-program "/opt/local/bin/gls") ;; ls program name
