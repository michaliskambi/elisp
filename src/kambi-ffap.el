;;; kambi-ffap --- Find file at point customizations.

(require 'ffap)

(defun kam-ffap-string-at-point-with-line-number ()
  "Return a string of characters from around point.
Much like `ffap-string-at-point', but this doesn't look
at major mode or `ffap-string-at-point-mode-alist'.
It uses hardcoded characters to match something that
looks like a filename optionally followed by line/number."
  (let* ((args '("--:\\\\${}+<>@-Z_[:alpha:]~*?,\\(\\)" "<@" "@>;.,!:"))
	 (pt (point))
	 (beg (if (use-region-p)
		  (region-beginning)
		(save-excursion
		  (skip-chars-backward (car args))
		  (skip-chars-forward (nth 1 args) pt)
		  (point))))
	 (end (if (use-region-p)
		  (region-end)
		(save-excursion
		  (skip-chars-forward (car args))
		  (skip-chars-backward (nth 2 args) pt)
		  (point)))))
    (setq ffap-string-at-point
	  (buffer-substring-no-properties
	   (setcar ffap-string-at-point-region beg)
	   (setcar (cdr ffap-string-at-point-region) end)))))

(defun kam-find-file-at-point ()
  (interactive)
  "Find file at point."

  ;; Extract column and line number.
  ;; Based on Daniel Poersch code on
  ;; https://www.emacswiki.org/emacs/FindFileAtPoint .

  (let* ((string (kam-ffap-string-at-point-with-line-number)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or (condition-case nil
                  (and (not (string-match "//" string)) ; foo.com://bar
                       (substitute-in-file-name string))
                (error nil))
              string))
         (line-number-string nil)
         (line-number nil)
         (column-number-string nil)
         (column-number nil)
         (name-without-line-and-column nil)
        )

    ;; detect something like castleuicontrols.pas(3631,23)
    (when (string-match "^\\([^(]+\\)(\\([0-9]+\\),\\([0-9]+\\))$" name)
      (setq name-without-line-and-column (match-string 1 name))
      (setq line-number-string (match-string 2 name))
      (setq line-number (string-to-number line-number-string))
      (setq column-number-string (match-string 3 name))
      (setq column-number (string-to-number column-number-string))

      ;; debug
      ;; (message (concat "Complete name with line/column: " name))
      ;; (message (concat "Found line number: " line-number-string))
      ;; (message (concat "Found column number: " column-number-string))
      ;; (message (concat "New name " name-without-line-and-column))
    )

    ;; detect something like castleuicontrols.pas(3631)
    (when (string-match "^\\([^(]+\\)(\\([0-9]+\\))$" name)
      (setq name-without-line-and-column (match-string 1 name))
      (setq line-number-string (match-string 2 name))
      (setq line-number (string-to-number line-number-string))
    )

    (call-interactively 'find-file-at-point)

    (when line-number
      (goto-line line-number)
      (when column-number
        (move-to-column column-number)
      )
    )
  )
)

;; Old version of kam-find-file-at-point using Helm.
;;
;; (if (require 'helm-config nil 'noerror)
;;     (progn
;;       (setq helm-ff-guess-ffap-filenames t)
;;       (setq helm-ff-guess-ffap-urls      t)
;;       ;; when helm is available, it's better to use standard helm-find-files
;;       ;; than find-file-at-point. find-file-at-point would also be
;;       ;; completed using helm, but it would have less options.
;;       ;; See https://groups.google.com/forum/#!topic/emacs-helm/Y-RKJGLxNu4
;;       ;; https://github.com/emacs-helm/helm/issues/984
;;       (call-interactively 'helm-find-files))
;;   (call-interactively 'find-file-at-point)
;; )

;; ------------------------------------------------------------

(provide 'kambi-ffap)

;;; kambi-ffap.el ends here
