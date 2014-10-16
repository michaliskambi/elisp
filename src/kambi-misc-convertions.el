;;;; Various text convertions by Kambi.
;;
;; Usually meant to be applied to some specific kind of text (e.g. to C++ code).
;; These convertions are usually dangerous -- i.e., they are just simple tricks
;; that *usually* work but they can potentially screw up some text or something.
;; That's because they usually use regexps to do some things that really should
;; be done by writing some lexer or/and parser.
;; That's why these functions are always meant to be called explicitly by user.

;; You must call this before kam-c2pas-star-comment-to-wasaty-comment,
;; otherwise " + comment" regexps will not match.
(defun kam-c2pas-hash-define-to-int-const ()
  (interactive)
  ;; hexadecimal
  (kam-simple-re-replace-region "^#define[ \t]+\\([A-Za-z0-9_]+\\)[ \t]+0x\\([0-9A-Za-z]+\\)[ \t]*$" "  \\1 = $\\2;")
  ;; decimal
  (kam-simple-re-replace-region "^#define[ \t]+\\([A-Za-z0-9_]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]*$"       "  \\1 = \\2;")

  ;; hexadecimal + // comment
  (kam-simple-re-replace-region "^#define[ \t]+\\([A-Za-z0-9_]+\\)[ \t]+0x\\([0-9A-Za-z]+\\)[ \t]*\\(//.*\\)$" "  \\1 = $\\2; //< \\3")
  ;; decimal + // comment
  (kam-simple-re-replace-region "^#define[ \t]+\\([A-Za-z0-9_]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]*\\(//.*\\)$"       "  \\1 = \\2; //< \\3")

  ;; hexadecimal + /* */ comment
  (kam-simple-re-replace-region "^#define[ \t]+\\([A-Za-z0-9_]+\\)[ \t]+0x\\([0-9A-Za-z]+\\)[ \t]*/\\*\\(.*\\)\\*/[ \t]*$" "  \\1 = $\\2; //< \\3")
  ;; decimal + /* */ comment
  (kam-simple-re-replace-region "^#define[ \t]+\\([A-Za-z0-9_]+\\)[ \t]+\\(-?[0-9]+\\)[ \t]*/\\*\\(.*\\)\\*/[ \t]*$"       "  \\1 = \\2; //< \\3")
)

(defun kam-c2pas-star-comment-to-wasaty-comment ()
  (interactive)
  (kam-simple-replace-region "/*" "{")
  (kam-simple-replace-region "*/" "}")
)

(defun kam-convert-cpp-to-pascal-inregion ()
  "Probuje w dosc prosty sposob (za pomoca ciagu replace'ow)
zamienic program w C++ na program w Pascalu. Nie ma szans poradzic
sobie nawet z najprostszym programem C++ ale wykona przynajmniej
czesc roboty."
  (interactive)

  (kam-simple-replace-region "{" "begin")
  (kam-simple-replace-region "}" "end")
  (kam-c2pas-star-comment-to-wasaty-comment)
  (kam-simple-replace-region " = " " := ")
  (kam-simple-replace-region "==" "=")
  (kam-simple-replace-region "||" " or ")
  (kam-simple-replace-region "&&" " and ")
  (kam-simple-replace-region "|" " or ")
  (kam-simple-replace-region "&" "@")      ; & = "@" lub " and " - niby jak mam to wybrac ?
  (kam-simple-replace-region "\"" "'")
  (kam-simple-replace-region "->" "^.")
  (kam-simple-replace-region "!=" "<>")
  ;; (kam-simple-replace-region "!" "not") ; ! jest zbyt czesto uzywany w stringach / komentarzach
  (kam-simple-replace-region "exit" "halt")
  (kam-simple-replace-region "case" "")
  (kam-simple-replace-region "switch" "case")
  ;; wstaw "then" pomiedzy if () a begin
  (kam-simple-re-replace-region "if *\\((.*)[\n\t ]*\\)begin" "if \\1 then begin")
  (kam-simple-re-replace-region "for *( *\\([A-Za-z0-9_]*\\) *:= *\\([A-Za-z0-9_]*\\) *; *\\1 *< *\\([A-Za-z0-9_]*\\) *; *\\1\\+\\+ *)"
                             "for \\1 := \\2 to \\3-1 do")
  (kam-simple-re-replace-region "for *( *\\([A-Za-z0-9_]*\\) *:= *\\([A-Za-z0-9_]*\\) *; *\\1 *<= *\\([A-Za-z0-9_]*\\) *; *\\1\\+\\+ *)"
                             "for \\1 := \\2 to \\3 do")
  (kam-simple-re-replace-region "for *( *\\([A-Za-z0-9_]*\\) *:= *\\([A-Za-z0-9_]*\\) *; *\\1 *> *\\([A-Za-z0-9_]*\\) *; *\\1\\-\\- *)"
                             "for \\1 := \\2 downto \\3+1 do")
  (kam-simple-re-replace-region "for *( *\\([A-Za-z0-9_]*\\) *:= *\\([A-Za-z0-9_]*\\) *; *\\1 *>= *\\([A-Za-z0-9_]*\\) *; *\\1\\-\\- *)"
                             "for \\1 := \\2 downto \\3 do")
)

(defun kam-fuck-ms ()
  "Wycina z tekstu wszystkie znaki o kodzie 13 (widoczne jako ^M w Emacsie).

Te znaki pojawiaja sie gdy otworzymy w Emacsie plik z niekonsekwentnie
zapisanymi koncowkami linii - niektore UNIXowo #10 a niektore DOSowo
#13#10. Niektore glupsze od Emacsa edytory zapisuja takie pliki, zdarza
sie to takze gdy laczymy rozne pliki tekstowe w sposob binarny - wtedy
tez dostajemy w wyniku plik tekstowy ktory nie musi miec konsekwentnych
koncowek linii.

Emacs otwierajac taki plik uznaje ze bezpiecznie bedzie przyjac ze ten
plik ma UNIXowe zakonczenia linii #10 a znaki #13 wypisac jako czesc
tekstu. Emacs robi tu dobrze, starajac sie przedstawic mi dokladnie
to co mam w pliku. Ale ja zazwyczaj chce zignorowac takie znaki -
do tego przydaje sie ta funkcja."
  (interactive)
  (kam-simple-replace-buffer (string 13) "")
)

(defun kam-fuck-hs ()
  "Zamienia w tekscie wszystkie wystapienia postaci a^Ha na jedno a.
Potem wycina z tekstu wszystkie wystapienia _^H.
(^H to znak o kodzie 8 czyli backspace)"
  (interactive)
  (kam-simple-re-replace-buffer (concat "\\(.\\)" (string 8) "\\1") "\\1")
  (kam-simple-replace-buffer (concat "_" (string 8))  "")
)

(defun kam-fuck-logwrite ()
  "Wycina wszystkie LogWrite(*) z bufora."
  (interactive)
  (kam-simple-re-replace-buffer "LogWrite([^)]*);" "")
  (kam-simple-re-replace-buffer "LogWrite([^)]*)" "")
)

(defun kam-convert-dfm-to-pas ()
  "Probuje za pomoca ciagu replace'ow zamienic caly bufor w formacie
.dfm (uzywanym przez Delphi/Kylixa do zapisu Forma w postaci tekstowej)
na ciag przypisan Pascala. Ze wzgledu na prosty format jest to nawet
calkiem skuteczne.
  Prawdopodobnie jedyna rzecz nad jaka bedziesz
musial bardziej przytomnie czuwac to aby wszystkie kontrolki mialy
ustawionego Parenta dobrze (na nadrzedna kontrolke). Domyslnie
kazda kontrolka ma wpisane Parent:=?Self; a wiec wystarczy usunac znak
'?' aby otrzymac najczestszy przypadek Parent=Self.

Aby przerobic dfm na Pascala :
1. Wykonac replace'y robione przez ta funkcje
2. kolekcje przerobic, DesignSize, TextHeight formularza wyciac
3. zrobic odpowiednie Parent:= kazdej kontrolce"
  (interactive)
  (kam-simple-re-replace-buffer "\\(.*\\) = \\(.*\\)" "\\1 := \\2;'+")
  (kam-simple-re-replace-buffer "^\\([ \t]*\\)object \\(.*\\): \\(.*\\)" "\\1\\2 := \\3.Create(Self);\n\\1with \\2 do begin\n\\1  Parent := ?Self;'+")
  (kam-simple-replace-buffer "end" "end;'+")
)

(defun kam-convert-sqldate-sybase-to-ib ()
  "Zamienia daty zapisane w postaci Sybase SQL'a (rrrr-mm-dd) na InterBase'a (mm-dd-rrrr)"
  (interactive)
  (query-replace-regexp "200\\(.\\)-\\([0-9]*-[0-9]*\\)'" "\\2-200\\1''+")
)

(defun kam-add-defpars-clause ()
  ""
  (interactive)
  (query-replace-regexp "=\([^;)]*\);" " {$IFDEF DEFPARS}=\1{$ENDIF};")
  (query-replace-regexp "=\([^;)]*\))" " {$IFDEF DEFPARS}=\1{$ENDIF})")
)

(defun kam-remove-pas-comp-directive (STR)
  "Usun z aktualnego bufora wszystkie wystapienia dyrektywy kompilatora STR"
  ;; (kam-simple-re-replace-buffer (concat "^[ \\t]*{\\$" STR "[^}]*}[ \\t]*\\n") "")
  (kam-simple-re-replace-buffer (concat "{\\$" STR "[^}]*}") "")
  (kam-simple-re-replace-buffer (concat "(\\*\\$" STR "[^*]*\\*)") "")
)

(defun kam-convert-delphi-to-fpc ()
  "Probuje troche ulatwic zycie zamieniajac czesc rzeczy Pascalowych
z Delphi na FPC."
  (interactive)
  (kam-remove-pas-comp-directive "EXTERNALSYM")
  (kam-remove-pas-comp-directive "NODEFINE")
  (kam-remove-pas-comp-directive "HPPEMIT")
)

(defun kam-mcmenu-rule-upcase-region ()
  "Zamien regule /etc/mc/mc.menu tak zeby wyrazenie regularne uzywalo
duzych liter, np.
  f \.3ds$ | f \.geo$
zamien na
  f \.3DS$ | f \.GEO$
. Dziala na current region."
  (interactive)
  (re-function-replace-region "\\([^a-zA-Z0-9][fFdD][ \t]+\\)\\([^ \n\t]+\\)"
    (lambda () (concat (match-string 1) (upcase (match-string 2))))
  )
)

(defun kam-phpwiki-to-moinmoin ()
  (interactive)

  ;; list (must start with spaces + *)
  (kam-simple-re-replace-buffer "^-\\([^-]\\)" "  *\\1")
  (kam-simple-re-replace-buffer "^*" "  *")
  (kam-simple-re-replace-buffer "^;" "  *")

  ;; quote by `` inside
  (kam-simple-re-replace-buffer "~PasDoc" "Pas``Doc")

  ;; tt font (must be before headings)
  (kam-simple-re-replace-buffer "=\\([^=]*\\)=" "{{{\\1}}}")
  (kam-simple-re-replace-buffer "<pre>" "{{{")
  (kam-simple-re-replace-buffer "</pre>" "}}}")

  ;; headings (note that meaning is reversed,
  (kam-simple-re-replace-buffer "^!!\\([^!].*\\)$" "= \\1 =")
  (kam-simple-re-replace-buffer "^!\\([^!].*\\)$" "== \\1 ==")

  ;; links [] with custom label
  (kam-simple-re-replace-buffer "\\[\\(.*?\\)|\\(http.*?\\)\\]" "[\\2 \\1]")
  (kam-simple-re-replace-buffer "\\[\\(.*?\\)|\\(.*?\\)\\]" "[:\\2:\\1]")

  ;; using [a-zA-Z] to force page name
  (kam-simple-re-replace-buffer "\\[\\([a-zA-Z0-9]*\\)\\]" "[\"\\1\"]")

  ;; rules (while 4 - is accepted by moinmoin, it makes too thin rule)
  (kam-simple-re-replace-buffer "----" "-----")

  (kam-simple-re-replace-buffer "HomePage" "FrontPage")
)

(defun kam-phpwiki-changes-log-to-moinmoin ()
  (interactive)
  (kam-beg-of-buf)
  (insert "This page was converted from old pasdoc wiki that used phpwiki engine.
Below you can see page history from phpwiki.

")
  (kam-simple-re-replace-buffer "^" "## ")
)

;; ------------------------------------------------------------

(provide 'kambi-misc-convertions)

;; eof ------------------------------------------------------------
