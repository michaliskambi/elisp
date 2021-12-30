;;;; This file, kambi-startup, configures Emacs for Kambi.
;;
;; This is the central file of Michalis EmacsLisp,
;; it includes all my other EmacsLisp customization files.
;; I try to keep my ~/.emacs minimal, to be able to easily
;; transfer my configuration to other system.

(require 'kambi-utils)
(require 'kambi-various-personal)
(require 'kambi-dired)
(require 'kambi-svn)
(require 'kambi-projectile)
;; (require 'kambi-ag) ;; unused for now, in favor of ripgrep
(require 'kambi-ripgrep)
;; (require 'kambi-helm) ;; helm unused for now, in favor of ivy
(require 'kambi-ivy)
(require 'kambi-cc-mode)
(require 'kambi-pascal)
(require 'kambi-castle-engine)
(require 'kambi-vrmls)
(require 'kambi-www)
(require 'kambi-ocaml)
(require 'kambi-sql)
;; (require 'kambi-matlab) ;; unused for now, because assigns to .m files, which for now I want as objective C
(require 'kambi-makefile)
(require 'kambi-lisp-editing)
(require 'kambi-misc-convertions)
;; (require 'kambi-ada) ;; unused for now
(require 'kambi-sgml)
(require 'kambi-log)
(require 'kambi-csharp)
(require 'kambi-shell)
(require 'kambi-magit)
(require 'kambi-search)
(require 'kambi-macosx)
(require 'kambi-ffap)
(require 'kambi-adoc)

(provide 'kambi-startup)
