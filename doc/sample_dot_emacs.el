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

;; set Cygwin path on Windows
;;(setq cygwin-mount-cygwin-bin-directory "d:/cygwin64/bin")

;; use if your platform/Emacs version have problems with MELPA/ELPA through https
;;(setq kam-package-force-http t)

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
;;(set-default-font "Menlo-24")

;; maximize on startup?
;; https://emacs.stackexchange.com/questions/2999/how-to-maximize-my-emacs-frame-on-start-up
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
