;; Add this to ~/.emacs.
;;
;; Alternatively, if it's your own system, and you have permissions,
;; and you use "emacs --batch" in your private scripts,
;; you may consider adding this to /etc/emacs/site-start.el.
;; Then Kambi elisp functions will be automatically available in batch mode
;; (when ~/.emacs is not read, but site elisp is).

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(defconst kambi-elisp-path "/home/michalis/elisp/"
  "Path (with final [back]slash) to Kambi Elisp directory.")
(add-to-list 'load-path (concat kambi-elisp-path "src/"))

;; Kambi: use my common customization file (things done by M-x customize)
;; Should be before kambi-startup, to set cua-rectangle-mark
;; before activating cua in kambi-startup.
(setq custom-file (concat kambi-elisp-path "src/kambi-customizations.el"))
(load custom-file)

;; Kambi: load my initialization
(require 'kambi-startup)

;; Delete to trash on emacs >= 23.
;; (kam-delete-by-moving-to-trash "~/tmp")
