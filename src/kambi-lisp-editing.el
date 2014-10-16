;; This is Kambi's code to configure Emacs to be able to edit
;; *.el files comfortably.

(require 'kambi-utils)

;; ffap on *.el filenames
(defconst ffap-elisp-source-directories
  (list "/usr/share/emacs/site-lisp/")
  "See documentation of `ffap-elisp-source'.
Must contain only absolute directory paths.")

(defun ffap-elisp-source (name)
  "This is supposed to be registered in `ffap-alist' on *.el filenames.

This tries to find filenames by searching recursively
inside directories specified in `ffap-elisp-source-directories'.

This way it can sometimes find files that cannot be found
by `ffap-el' (the default function registered in ffap to *.el and *.elc
filenames). That's because `ffap-el' searches non-recursively paths
in `load-path'. E.g. on my Debian \"mmm-mode.elc\" is installed in
/usr/share/emacs21/site-lisp/, but \"mmm-mode.el\" is installed in
/usr/share/emacs/site-lisp/mmm-mode/ (and the last directory is not
listed on `load-path').

Note: it would be possible to simply append all
subdirectories (recursively) from `ffap-elisp-source-directories'
to `load-path'. But it would not be an elegant solution,
as `load-path' is intended to be used with `load' to load *.elc or *.el
files, while I want to ffap-elisp-source to only seek source files."
  (unless (file-name-absolute-p name)
    (block func-block
      (let ((name-no-dir (file-name-nondirectory name))
            result)
        (dolist (item ffap-elisp-source-directories)
          (setq result (kam-search-for-file item name-no-dir))
          (when result (return-from func-block result))
        ))
      nil ;; if noone calls "(return-from func-block" then return nil
    )))

;; tests:
;; (ffap-elisp-source "mmm-mode.el")
;; (ffap-elisp-source "php-mode.el")
;; (ffap-elisp-source "non-existing-mode.el")

(defun ffap-elisp-source-no-extension (name)
  "Like `ffap-elisp-source', but given NAME does not contain \".el\"
extension. In other words, this is equivalent to
  (ffap-elisp-source (concat name \".el\"))"
  (ffap-elisp-source (concat name ".el"))
)

;; tests:
;; (ffap-elisp-source-no-extension "mmm-mode")
;; (ffap-elisp-source-no-extension "php-mode")
;; (ffap-elisp-source-no-extension "non-existing-mode")

(setq ffap-alist (add-to-list-new-items ffap-alist
  '(("\\.el\\'" . ffap-elisp-source)
    (emacs-lisp-mode . ffap-elisp-source-no-extension)
    (lisp-interaction-mode . ffap-elisp-source-no-extension))))

;; provide ------------------------------------------------------------

(provide 'kambi-lisp-editing)

;; eof ------------------------------------------------------------