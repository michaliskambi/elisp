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

    ;; Temporarily disable ivy on URLs.
    ;;
    ;;ivy makes ffap on URLs broken -- contacting host,
    ;; then displaying useless list a files instead of just the URL.
    ;;
    ;; Like
    ;;   http://httpd.apache.org/
    ;;   https://projects.cat-astrophe-games.com/
    ;;   https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-WD3/
    ;;
    ;; It displays "Contacting host: xx" (which is already bad, I don't want ffap to
    ;; contact the host -- it wastes time and is a privacy concern,
    ;; I want ffap to only call browse-url and let WWW browser do the rest),
    ;; and then shows me a file list (with URL on it, but not default).
    ;;
    ;; Debugging it,
    ;; - (ffap-read-file-or-url "TEST prompt: " "https://duckduckgo.com/") is faulty.
    ;; - (url-http-find-free-connection ...) does actualy contacting.
    ;;   It should never be called by ffap-read-file-or-url, but it is.
    ;; - backtrace shows
    ;;
    ;;     url-http-find-free-connection("duckduckgo.com" 443 nil)
    ;;     ...
    ;;     url-file-handler(file-exists-p "https://duckduckgo.com/")
    ;;     file-exists-p("https://duckduckgo.com/")
    ;;     ivy--reset-state(#s(ivy-state "TEST prompt: " ffap-read-file-or-url-internal nil nil "https://duckduckgo.com/" file-name-history "~/common/doc/emacs/ffap.txt" nil nil nil #<frame ffap.txt - emacs 0x1203c30> #<window 3 on ffap.txt> #<buffer ffap.txt> nil (1 ("o" identity "default") ("w" kam-kill-new "copy whole") ("k" kam-kill-new-nondirectory "copy basename")) nil ivy--regex-plus nil nil nil "/home/michalis/common/doc/emacs/" ffap-read-file-or-url-internal nil "~/common/doc/emacs/ffap.txt" t nil (:caller ivy-completing-read)))
    ;;     ivy-read("TEST prompt: " ffap-read-file-or-url-internal :predicate nil :require-match nil :initial-input "https://duckduckgo.com/" :preselect "~/common/doc/emacs/ffap.txt" :def "~/common/doc/emacs/ffap.txt" :history file-name-history :keymap nil :dynamic-collection nil :extra-props (:caller ivy-completing-read) :caller ffap-read-file-or-url-internal)
    ;;     ivy-completing-read("TEST prompt: " ffap-read-file-or-url-internal nil nil "https://duckduckgo.com/" file-name-history "~/common/doc/emacs/ffap.txt" nil)
    ;;     completing-read("TEST prompt: " ffap-read-file-or-url-internal nil nil "https://duckduckgo.com/" file-name-history "~/common/doc/emacs/ffap.txt")
    ;;     ...
    ;;     ffap-read-file-or-url("TEST prompt: " "https://duckduckgo.com/")
    ;;
    ;; I didn't find any better solution than to disable Ivy here.
    ;;
    ;; Later: disable ivy on non-URLs too. It shows me one file in the dir (instead of others),
    ;; and changing it is more troublesome.

    ;;(if (and (functionp 'ivy-mode) (string-match ffap-url-regexp string))
    (if (functionp 'ivy-mode)
        (progn
          (ivy-mode 0)
          (call-interactively 'find-file-at-point)
          (ivy-mode 1)
        )
      (call-interactively 'find-file-at-point)
    )

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
