;;;; Kambi "WWW content development" in Emacs
;;
;; These are things that configure parts of Emacs related to
;; "www content development", i.e. programming in PHP, HTML, CSS.
;; These are things specific for Kambi, some of them may be generally
;; usable but many of them change some global Emacs settings
;; that usual users may not want to change.
;;
;; I'm configuring PHP, HTML, CSS (and maybe others in the future)
;; (and call them as "www content development") in one place because
;; I use them together.

;; Czasem nie mam php-mode (NTEmacs nie ma, sam GNU emacs pod Linuxem tez
;; przez chwile nie mial bo php-mode nie kompilowal sie pod GNU emacsem
;; (tylko XEmacsem))
(defconst php-mode-available (fboundp 'php-mode))

;; php mode configuration (not dependent on availability of mmm-mode)
(when php-mode-available
  (require 'php-mode)

  ;; Define my own Kambi-PHP mode to get rid of stupid automatic indentation.
  (define-derived-mode
    kambi-php-mode php-mode "Kambi-PHP"
    "Kambi's version of `php-mode' -- removed automatic indentation.")
  ;; This is copied from my hacks for C modes (see kambi-cc-mode.el
  ;; in function remove-indentation-from-cc-mode-map)
  (define-keys-to-nil kambi-php-mode-map
    '("{" "}" ";" "#" ":" "(" ")" "\t" "\C-d" "\177" "," "*" "/"))
  (when (boundp 'delete-key-deletes-forward)
    (define-keys-to-nil kambi-php-mode-map '([delete] [backspace]))
  )
  (add-to-list 'auto-mode-alist '("\\.php\\'" . kambi-php-mode))
  ;; Drupal specials
  (add-to-list 'auto-mode-alist '("\\.module\\'" . kambi-php-mode))
  (add-to-list 'auto-mode-alist '("\\.test\\'" . kambi-php-mode))
  (add-to-list 'auto-mode-alist '("\\.install\\'" . kambi-php-mode))

  ;; I use .inc extension for Pascal include files.
  ;; So I don't want to execute php-mode for them.
  ;;
  ;; Usuwanie z php-file-patterns nie pomaga, zdaje sie ze zrobienie
  ;; samego (require 'php-mode) sprawia ze php-file-patterns sa dodane
  ;; do auto-mode-alist (wiec usuwanie ich pozniej z php-file-patterns
  ;; nie daje juz nic ciekawego).
  ;; A musze najpierw zrobic (require 'php-mode), inaczej Emacs
  ;; mowi ze php-file-patterns jest nieznana variable.
  ;; To pewnie mozna zrobic elegancko, nie orientuje sie jak
  ;; dziala to cale "autoload" Emacsa.
  ;;
  ;; Wiec chcac byc po bezpiecznej stronie, usuwam tez odpowiedni
  ;; element z auto-mode-alist.
  ;;
  ;; Note that later php-mode versions (since 1.13.5 in Debian?)
  ;; seem to remove php-file-patterns symbol (also removing .inc automatic
  ;; registration, BTW).
  (when (boundp 'php-file-patterns)
    (setq php-file-patterns (remove "\\.inc\\'" php-file-patterns)))
  (setq auto-mode-alist (remove '("\\.inc\\'" . php-mode) auto-mode-alist))
  (when kam-inc-extension-default-php
    (add-to-list 'auto-mode-alist '("\\.inc\\'" . kambi-php-mode)))

  (add-hook 'php-mode-hook 'php-suspicious-code-check-add-write-contents-hooks t)
)

;; html mode configuration (not dependent on availability of mmm-mode)
;;
;; Bez wzgledu na to czy php-mode-available, zawsze html-mode traktujemy
;; troche jak php mode (bo czesto pliki php edytuje w trybie html-mode).
;; Dlatego html-mode-hook zawsze dostaje php-suspicious-code-...
(add-hook 'html-mode-hook
  'php-suspicious-code-check-add-write-contents-hooks t)

(defun kam-bind-ctrl-z-undo () (local-set-key (kbd "C-z") 'undo))
(add-hook 'html-mode-hook 'kam-bind-ctrl-z-undo t)

;; If this is non-nil, then when you init html-mode on empty buffer
;; (e.g. when creating new buffer with filename *.html) then
;; some HTML template is automatically inserted.
;; I never use this (and I can't think about any sensible HTML template
;; that would suit my needs) so I turn this off.
(setq psgml-html-build-new-buffer nil)

;; ------------------------------------------------------------
;; configure-no-mmm-www and configure-mmm-www

(defun configure-no-mmm-www ()
  "This does necessary configuration to use php-mode and html-mode
(or maybe html-helper-mode) without mmm. Useful when you don't have
mmm-mode available.

This will also work even if you don't have php-mode."
  (interactive)
  (unless php-mode-available
    (add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode)))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-mode))
)

(defun configure-mmm-www ()
  "This configures MMM mode to combine HTML, PHP, CSS ...
Based closely on [http://www.emacswiki.org/cgi-bin/wiki/HtmlModeDeluxe].

Requires mmm-mode, php-mode, css-mode (others?) available."
  (interactive)

  (require 'mmm-mode)

  (setq mmm-global-mode 'maybe)

  ;; set up an mmm group for fancy html editing
  (mmm-add-group
    'fancy-html
    '(
      (html-php-tagged
              :submode kambi-php-mode
              :face mmm-code-submode-face
              :front "<[?]php"
              :back "[?]>")
      (html-css-attribute
              :submode css-mode
              :face mmm-declaration-submode-face
              :front "style=\""
              :back "\"")
    ))

  (mmm-add-mode-ext-class 'html-mode nil 'fancy-html)

  ;; Don't add html-php, instead I add fancy-html: this uses my
  ;; kambi-php-mode instead of default php-mode.
  ;; (mmm-add-mode-ext-class 'html-mode nil 'html-php)

  ;; Don't add html-js. I don't know why, but with mmm-mode 0.4.8-5
  ;; on Debian testting this causes the buffer to always be marked
  ;; modified on activation. I don't use JS too much anyway, so I just turned it off.
  ;; (mmm-add-mode-ext-class 'html-mode nil 'html-js)

  ;; temp commented out, on chantal this causes buffer marked modified?
  ;;(mmm-add-mode-ext-class 'html-mode nil 'embedded-css)

  ;; Invoke html-mode on all php and html files.
  (add-to-list 'auto-mode-alist '("\\.php\\'" . html-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-mode))
)

;; call one of configure-*-www
(if (and php-mode-available ;;mmm-available
      ;; For now: just don't use mmm-mode for php/html,
      ;; it really doesn't work smoothly (not always colors like it should),
      ;; and can make enormous slowdown on large files.
      nil)
    (configure-mmm-www)
  (configure-no-mmm-www)
)

;; php-suspicious-code-* ----------------------------------------

(defconst php-suspicious-code-regexp "\\?>\n[^ \n\t<]"
  "There is one php behaviour that I consider extremely stupid:
if the next character after php closing tag (like '?>') is newline
then that newline is stripped from the resulting output.
This is extremely stupid because it violates the extremely natural
assuption that \"everything outside of php tags is simply copied
to output\". In particular, you can easily make a mistake like that:

...some php code echoing string 'foo' ... ?>
bar

You would expect that in the resulting HTML words foo and bar will be
separate. But they will be not, because php stripped newline and the
resulting html did not contain anything between foo and bar.
So you have to change it to something like

...some php code echoing string 'foo' ... ?>
 bar

that looks very weird because at the first sight
the need for this one space before 'bar' is not clear.

This regexp is designed to catch such suspicious php code.

< should be removed to catch more suspicious cases, but most of the
time it would make false warnings, e.g. this regexp without < would
see suspicious code in:

  <li><?php ...output something...>
</ul>

but it is correct, there is no need for whitespace before </ul>."
)

(defun php-suspicious-code-check-write-hook ()
  "This function searches the buffer for php suspicious code.
It such code is found it shows this code (i.e. point changes
it's temporary position to suspicious code) and asks user whether
he really wants to save the buffer.
Returns t if user resigned from saving (returns nil if no suspicious
place was found or user not resigned from saving).

TODO - a better way for this would be to show _every_ such suspicious
occurence to user and ask for every occurence)

Add this function to write-contents-hooks when major mode is php-mode
or html-mode. Based on makefile-warn-suspicious-lines."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward php-suspicious-code-regexp nil t)
        (not (y-or-n-p
               (format "Suspicious line %d. Save anyway "
                      (count-lines (point-min) (point)))))))
)

(defun php-suspicious-code-check-add-write-contents-hooks ()
  ;; note : we're adding at the beginning of write-contents-hooks,
  ;; this should be usually safer
  (add-hook 'write-contents-hooks 'php-suspicious-code-check-write-hook)
)

(defun php-suspicious-code-message (&optional format-str)
  "For first suspicious php line in this buffer, output a message
about it. FORMAT-STR will be formatted with (buffer-name, line-number),
if nil then the default \"%s: Suspicious line %d\" will be used."
  (interactive)
  (if (not format-str) (setq format-str "%s: Suspicious line %d"))
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward php-suspicious-code-regexp nil t)
        (message format-str
          (buffer-name)
          (count-lines (point-min) (point))
        )
    )
  )
)

;; ----------------------------------------
;; Things specific for my camelot.homedns.org/~michalis www page

(defun kam-camelot-href-program-names ()
  (interactive)
  (query-replace-regexp (concat
    " \\(\\(view3dscene\\|glViewImage\\|bezier_curves\\|glplotter\\|"
    "lets_take_a_walk\\|rayhunter\\|malfunction\\|kulki\\|glcaps\\|"
    "gen_funkcja\\|edytorek\\)\\)")
  " <?php echo a_href_page('\\1', '\\1'); ?> ")
)

;; kambi-css-mode -------------------------------------------------

(define-derived-mode
  kambi-css-mode css-mode "Kambi-CSS"
  "Kambi's version of `css-mode' -- remove automatic completion of quotes
and brackets and indentation.")
(define-keys-to-nil kambi-css-mode-map '("\"" "(" "[" "{" "}"))
(add-to-list 'auto-mode-alist '("\\.css\\'" . kambi-css-mode))
(add-to-list 'auto-mode-alist '("\\.less\\'" . kambi-css-mode))
(add-to-list 'auto-mode-alist '("\\.scss\\'" . kambi-css-mode))

;; nxml mode -----------------------------------------------------------------

(add-hook 'nxml-mode-hook
  (lambda ()
    ;; nxml overrides this with it's utility, we override it back
    (local-set-key (kbd "C-M-u") 'uncomment-region)
  ) t)

;; provide ------------------------------------------------------------

(provide 'kambi-www)

;; eof ------------------------------------------------------------
