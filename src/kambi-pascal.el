;;;; Kambi Pascal mode and many utils to operate on Pascal code.

;; Require things we depend upon ---------------------------------------------

(require 'opascal)
(require 'kambi-utils)
(require 'ffap)
(require 'compile)

;; Define kambi-pascal-mode ----------------------------------------

;; We're using delphi-mode / opascal-mode instead of pascal-mode.
;; It is better for any "modern Object Pascal" source (whether you use Delphi or FPC).
;; - delphi-mode supports 1-line comments "//"
;; - It doesn't try to do *so much* indentation for me.
;;   Although eventually I turned it off completely anyway,
;;   see kambi-pascal-mode-map,
;;   but it was still easier to turn off in delphi-mode than pascal-mode.
;; - More complete list of Object Pascal keywords and directives.
;;   Although I needed to add some anyway.

(defun kambi-pascal-mode-ancestor ()
  (opascal-mode)
)

(define-derived-mode kambi-pascal-mode kambi-pascal-mode-ancestor
  "K-Pascal"
  "Kambi mode for editing Pascal files.

Removes automatic indentation
(part of the default Emacs delphi-mode / opascal-mode / pascal-mode),
which in my experience causes more distraction than help when editing code."
  (setq comment-start "//")
  (setq comment-end "")
  (setq comment-start-skip comment-start) ;; same thing works best
)

;; Turn off in kambi-pascal-mode-map indentation.
(define-keys-to-nil kambi-pascal-mode-map '("\r" "\t" "\177"))

;; Pascal files extensions --------------------------------------------------

(defconst pascal-exts-implicit
  '(".pas" ".pp" ".p")
  "Lista rozszerzen (z poczatkowa kropka, pusty string oznacza brak rozszerzenia)
ktore oznaczaja pliki Pascalowe i ktore niekiedy moga byc pomijane gdy
nazwa pliku Pascalowego gdzies sie pojawia (np. deklaraujac moduly przez \"uses\"
pomijasz rozszerzenie .pas). Powinny byc uporzadkowane w takiej kolejnosci
w jakiej powinny byc szukane - gdy bedziemy dopasowywac do pliku takie
rozszerzenie implicit bedziemy dopasowywac najpierw pierwsze rozszerzenie na
liscie, potem drugie itd.")

(defconst pascal-exts-explicit
  '( ".pasprogram" ; Michalis used it loooong time ago
     ".dpr" ".inc" ; (dpr is actually Delphi, but I treat is as general Pascal extension)
     ".xfm" ".dfm" ".dpk" ; Delphi
     ".lfm" ".lrs" ".lpr" ; Lazarus
   )
  "Lista rozszerzen (z poczatkowa kropka, pusty string oznacza brak rozszerzenia)
ktore oznaczaja pliki Pascalowe i ktore nigdy nie beda pomijane przy
podawaniu jakiegos pliku. Tzn. te rozszerzenia nie beda nigdy uzywane aby
odgadnac jakis plik w rodzaju tego co robimy w kam-ffap-find-pascal-file,
te rozszerzenia sa uzywane tylko aby rozpoznawac typ pliku.")

(defconst pascal-exts
  (append pascal-exts-implicit pascal-exts-explicit))

(defconst pascal-exts-regexps
  (let (result)
    (dolist (elt pascal-exts result)
      (setq result (cons (concat (regexp-quote elt) "\\'") result))
    )
  )
  "Lista rozszerzen Pascala przedstawiona jako wyrazenia regularne.
Jezeli nazwa pliku pasuje do tego wyrazenia regularnego to mozna wnioskowac
ze ten plik jest w Pascalu." )

(dolist (ext pascal-exts-regexps)
  (add-to-list 'auto-mode-alist `(,ext . kambi-pascal-mode)))

;; CastleScript --------------------------------------------------------------

;; load kambi-pascal-mode on KambiScript
;; http://castle-engine.sourceforge.net/castle_script.php
(add-to-list 'auto-mode-alist '("\\.kscript\\'" . kambi-pascal-mode))

;; auto-complete -------------------------------------------------------------

;; All Pascal keywords to auto-complete in kambi-pascal-mode-hook.
;; Following example for C++ on http://www.emacswiki.org/emacs/AutoComplete

(defconst kam-ac-pascal-keywords
  (sort
    ;; keywords and directives, as strings.
    (mapcar 'symbol-name opascal-keywords)
    #'(lambda (a b) (> (length a) (length b)))))

(defvar kam-ac-source-pascal
  '((candidates
     . (lambda ()
         (all-completions ac-target kam-ac-pascal-keywords))))
  "Source for Pascal keywords.")

(add-hook 'kambi-pascal-mode-hook
  (lambda ()
    (when (require 'auto-complete-config nil 'noerror)
      (make-local-variable 'ac-sources)
      (add-to-list 'ac-sources 'kam-ac-source-pascal)
    )
  )
)

;; ---------------------------------------------------------------------------
;; just some Pascal helpers, to be invoked explicitly

(defun kam-insert-guid ()
  (interactive)
  (call-process "gen_guid" nil t)
)

(defun kam-clear-pascal-method-implementation ()
  "Remove Pascal functions bodies. Useful when simplifying multi-unit
testcase for FPC bug report, and trimming away code that doesn't affect bug."
  (interactive)
  ;; [^`] is used just as "any character including newline"
  ;; [^e] is added to not catch "begin end; another proc begin end;"

  ;; remove first function bodies with preceding "var" sections
;;  (query-replace-regexp "\nvar\n[^`]+?\nbegin\n" "\\nbegin\n")

  (query-replace-regexp "\nbegin\n[^e][^`]+?\nend;\n" "\nbegin { single line, to not match anymore } end;\n")
)

;; So that e.g. /home/michalis/sources/castle-engine/castle-engine/examples/instantfpc/castle_open_dialog
;; opens in my Pascal mode
(add-to-list 'interpreter-mode-alist '("instantfpc" . kambi-pascal-mode))

;; ------------------------------------------------------------

(provide 'kambi-pascal)

;; eof ------------------------------------------------------------
