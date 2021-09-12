(require 'kambi-utils)

;; Support in compilation buffer for messages format produces by onsgmls

(add-to-list 'compilation-error-regexp-alist
  '("In entity [a-zA-Z0-9]+ included from \\([^ \t\n:]+\\):\\([0-9]+\\):\\([0-9]+\\)" 1 2 3))

(add-to-list 'compilation-error-regexp-alist
  '("/usr/bin/onsgmls:\\([^ \t\n:]+\\):\\([0-9]+\\):\\([0-9]+\\):.*" 1 2 3))

(defun kam-quote-xml-entities ()
  "Quote XML entities in the region. In other words, replace <
with &lt; and > with &gt; and & to &amp; in the region."
  (interactive)
  (kam-simple-replace-region "&" "&amp;")
  (kam-simple-replace-region "<" "&lt;")
  (kam-simple-replace-region ">" "&gt;")
)

(add-to-list 'auto-mode-alist
  (cons (concat "\\." (regexp-opt '("xml" "xsd" "sch" "rng" "xslt" "svg" "rss") t) "\\'")
  'nxml-mode))

(add-hook 'nxml-mode-hook
  (lambda ()
    ;; leave my global preferences unchanged for some keys in nxml
    (define-key nxml-mode-map (kbd "<C-return>") nil)
    (define-key nxml-mode-map (kbd "M-h") nil)
    ;; Set TAB to completion
    (local-set-key (kbd "TAB") 'nxml-complete)
  )
  t)

(add-hook 'html-mode-hook
  (lambda ()
    ;; Reset require-final-newline to nil, otherwise html/sgml mode sets this to t
    (setq require-final-newline nil)
  )
  t)

;; ------------------------------------------------------------

(provide 'kambi-sgml)

;; eof ------------------------------------------------------------
