;;;; AsciiDoctor

(defun kam-open-cge-www-doc ()
  (interactive)
  (counsel-locate-action-extern
    (concat
      "http://localhost:8777/"
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
    (kam-beg-of-buf) (query-replace "<pre>" "```")
    (kam-beg-of-buf) (query-replace "</pre>" "```")
    (kam-beg-of-buf) (query-replace "<p>" "") ;; my HTML code usually already has a newline for paragraph
    (kam-beg-of-buf) (query-replace "</p>" "")
    (kam-beg-of-buf) (query-replace "<ul>" "")
    (kam-beg-of-buf) (query-replace "</ul>" "")
    (kam-beg-of-buf) (query-replace "<ol>" "")
    (kam-beg-of-buf) (query-replace "</ol>" "")
    (kam-beg-of-buf) (query-replace-regexp "^ *<li>" "- ")
    (kam-beg-of-buf) (query-replace "&lt;" "<")
    (kam-beg-of-buf) (query-replace "&gt;" ">")
    (kam-beg-of-buf) (query-replace "&amp;" "&")
    (kam-beg-of-buf) (query-replace "&mdash;" "--")
    ;; make CGE links relative
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"https://castle-engine.io/\\([^\"]+\\)\">\\([^<]+\\)</a>" "link:\\1[\\2]")
    ;; keep other links as they were
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"\\([^\"]+\\)\">\\([^<]+\\)</a>" "link:\\1[\\2]")
    ;; convert links using a_href_page
    (kam-beg-of-buf) (query-replace-regexp "<\\?php echo a_href_page(['\"]\\([^'\"]+\\)['\"], *['\"]\\([^'\"]+\\)['\"]); *\\?>" "link:\\2.php[\\1]")
    (kam-beg-of-buf) (query-replace-regexp "<dl>
  <dt>\\([^<]*\\)</dt>
  <dd>" "\\1:: ")
    (kam-beg-of-buf) (query-replace-regexp "  <dt>\\([^<]*\\)</dt>
  <dd>" "\\1:: ")
    (kam-beg-of-buf) (query-replace-regexp "<\\?php echo pascal_highlight_file('\\(.*\\)'); \\?>" "[source,pascal]
----
\\1
----")
    (kam-beg-of-buf) (query-replace-regexp "<\\?php api_link('\\([^']+\\)', '\\([^']+\\)'); \\?>" "cgeref:\\1[]")
    (kam-beg-of-buf) (query-replace-regexp "<\\?php echo cgeRef('\\([^']+\\)'); \\?>" "cgeref:\\1[]")
  )
)

(defun kam-adoc-upgrade-to-cgeref ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.\\([a-zA-Z_0-9]+\\)\\.html#\\([a-zA-Z_0-9]+\\)\\[\\([^]]+\\)\\]" "cgeref:\\2.\\3[\\4]")
    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.\\([a-zA-Z_0-9]+\\)\\.html\\[\\([^]]+\\)\\]" "cgeref:\\2[\\3]")
    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.html#\\([a-zA-Z_0-9]+\\)\\[\\([^]]+\\)\\]" "cgeref:\\2[\\3]")
    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.html\\[\\([^]]+\\)\\]" "cgeref:\\1[\\2]")

    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.\\([a-zA-Z_0-9]+\\)\\.html#\\([a-zA-Z_0-9]+\\)" "cgeref:\\2.\\3[]")
    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.\\([a-zA-Z_0-9]+\\)\\.html" "cgeref:\\2[]")
    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.html#\\([a-zA-Z_0-9]+\\)" "cgeref:\\2[]")
    (kam-beg-of-buf) (query-replace-regexp "https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.html" "cgeref:\\1[]")
  )
)

(defun kam-php-upgrade-to-cgeref ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.\\([a-zA-Z_0-9]+\\)\\.html#\\([a-zA-Z_0-9]+\\)\">\\([^<]+\\)</a>" "<?php echo cgeRef('\\2.\\3', '\\4'); ?>")
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.\\([a-zA-Z_0-9]+\\)\\.html\">\\([^<]+\\)</a>" "<?php echo cgeRef('\\2', '\\3'); ?>")
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.html#\\([a-zA-Z_0-9]+\\)\">\\([^<]+\\)</a>" "<?php echo cgeRef('\\2', '\\3'); ?>")
    (kam-beg-of-buf) (query-replace-regexp "<a href=\"https://castle-engine\\.io/apidoc-unstable/html/\\([a-zA-Z_0-9]+\\)\\.html\">\\([^<]+\\)</a>" "<?php echo cgeRef('\\1', '\\2'); ?>")

    (kam-beg-of-buf) (query-replace-regexp "<\\?php api_link('\\([^']+\\)', '\\([^']+\\)'); \\?>" "<?php echo cgeRef('\\1'); ?>")
  )
)

(provide 'kambi-adoc)
