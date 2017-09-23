;;;; This file, kambi-startup, configures Emacs for Kambi.
;;
;; This is the central file of Michalis EmacsLisp,
;; it includes all my other EmacsLisp customization files.
;; I try to keep my ~/.emacs minimal, to be able to easily
;; transfer my configuration to other system.

(add-to-list 'load-path (concat kambi-elisp-path "contrib/"))

(require 'kambi-utils)
(require 'kambi-various-personal)
(require 'kambi-dired)
(require 'kambi-svn)
(require 'kambi-projectile-ag)
;; helm unused for now, in favor of ivy
;; (require 'kambi-helm)
(require 'kambi-ivy)
(require 'kambi-cc-mode)
(require 'kambi-pascal)
(require 'kambi-vrmls)
(require 'kambi-www)
(require 'kambi-ocaml)
(require 'kambi-sql)
;; unused for now, assigns to .m files, which for I want as objective C
;; (require 'kambi-matlab)
(require 'kambi-makefile)
(require 'kambi-lisp-editing)
(require 'kambi-misc-convertions)
(require 'kambi-ada)
(require 'kambi-sgml)
(require 'kambi-log)
(require 'kambi-csharp)
(require 'kambi-shell)
(require 'kambi-magit)
(require 'kambi-search)
(require 'kambi-macosx)

(provide 'kambi-startup)
