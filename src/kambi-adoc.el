;;;; AsciiDoctor

(defun kam-open-cge-www-doc ()
  (interactive)
  (counsel-locate-action-extern
    (concat
      "http://localhost/~michalis/castle-engine/"
      (basename (buffer-file-name))
    )
  )
)

(when (require 'adoc-mode nil 'noerror)
  (add-hook 'adoc-mode-hook
    (lambda ()
      (local-set-key (kbd "M-b") 'kam-open-cge-www-doc)

      ;; adoc-mode sets require-final-newline to t,
      ;; but this causes warnings from ethan-wspace that supersedes the require-final-newline.
      ;;
      ;; The warnings about it are not possible to turn off in ethan-wspace,
      ;; and we cannot disable adoc-mode from setting require-final-newline...
      ;; so we manually reset require-final-newline to nil to avoid them.
      (set (make-local-variable 'require-final-newline) nil)
    ) t)

  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode))

  ;; fix colors for terminals (like for ssh session in castle-engine.io),
  ;; otherwise some AsciiDoc parts are invisible "black on black".
  (unless (display-graphic-p)
    (set-face-foreground 'markup-meta-face "green")
    (set-face-foreground 'markup-meta-hide-face "green")
  )
)

(defun kam-convert-html-to-adoc ()
  "Simple conversion HTML -> AsciiDoctor using a series of replacements.
Tailored to the needs of Castle Game Engine www."
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "<code>" "`")
    (kam-beg-of-buf) (query-replace "</code>" "`")
    (kam-beg-of-buf) (query-replace "<i>" "_")
    (kam-beg-of-buf) (query-replace "</i>" "_")
    (kam-beg-of-buf) (query-replace "<b>" "*")
    (kam-beg-of-buf) (query-replace "</b>" "*")
    ;; make CGE links relative
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"https://castle-engine.io/\\([^\"]+\\)\">\\([^<]+\\)</a>" "link:\\1[\\2]")
    ;; keep other links as they were
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>" "link:\\1[\\2]")
  )
)

(provide 'kambi-adoc)
