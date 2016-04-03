;;;; This file, kambi-startup, configures Emacs for Kambi.
;;
;; This is the central file of Michalis EmacsLisp,
;; it includes all my other EmacsLisp customization files.
;; I try to keep my ~/.emacs minimal, to be able to easily
;; transfer my configuration to other system.

(add-to-list 'load-path (concat kambi-elisp-path "contrib/"))

(require 'kambi-utils)
(require 'kambi-various-personal)
(require 'kambi-cc-mode)
(require 'kambi-pascal)
(require 'kambi-vrmls)
(require 'kambi-www)
(require 'kambi-ocaml)
(require 'kambi-sql)
(require 'kambi-matlab)
(require 'kambi-makefile)
(require 'kambi-lisp-editing)
(require 'kambi-misc-convertions)
(require 'kambi-ada)
(require 'kambi-sgml)
(require 'kambi-log)
(require 'kambi-csharp)
(require 'kambi-shell)
(require 'kambi-magit)

(provide 'kambi-startup)
