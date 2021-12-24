;; Copyright 2021-2021 Michalis Kamburelis.
;; This file is part of "Michalis Kamburelis Emacs configuration".
;; See the file COPYING.txt,
;; included in this distribution, for details about the copyright.
;; No warranty.
;; ---------------------------------------------------------------------------
;;
;; Upgrade installed packages using paradox-upgrade-packages.
;; Doesn't assume that any other Lisp code here, including my default ~/.emacs,
;; is loaded, as it should work in batch mode with --no-init-file .
;;
;; Inspiration: https://gist.github.com/ksjogo/341c5af71c3268fca55ff28c66eab04a
;; ---------------------------------------------------------------------------

;; This solves melpa connectivity in Emacs < 26.3
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'package)
;; Add MELPA. Similar to /home/michalis/elisp/src/kambi-various-personal.el code,
;; but simplified, assumes HTTPS works OK (which should be the case now, actually
;; non-secure HTTP will fail).
(add-to-list 'package-archives `("melpa" . ,(concat "https://melpa.org/packages/")))

(package-initialize)

(require 'paradox)
;; avoids Paradox asking for GitHub token
(setq paradox-github-token t)
(paradox-upgrade-packages)

(message "All Emacs packages upgraded.")
