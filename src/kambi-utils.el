;;; kambi-utils --- various useful EmacsLisp functions.

;;; Commentary:
;;
;; Ten plik zapewnia absolutnie podstawowe funkcje, tzn.  ten modul
;; EmacsLispa nie zalezy od zadnego innego mojego modulu w EmacsLispie
;; (dopisek pozniejszy: poza kambi-xemacs ktory jest jeszcze nizej).

;; Laduj cl-macs, jest tam zdefiniowanych kilka wartosciowych makr jak dotimes
;; czy dolist.  Nie sa ladowane automatycznie przez Emacsa 20.7 bo makra
;; nie sa potrzebne do uruchamiania skompilowanych plikow .elc (makra sa rozwijane
;; przed byte-dompile) (Emacs 21 juz wlacza cl-macs automatycznie, chyba ze w -batch
;; model wiec tak czy siak chcemy zaladowac cl-macs.
;;
;; Nie mozna zalaodowac cl-macs przez require (nie robia w nim provide 'cl-macs).
;; Przed zaladowaniem cl-macs TRZEBA zaladowac cl (to juz mozna zrobic przez require).

;;; Code:

;; TODO: migrate to use only cl-lib, see http://stackoverflow.com/questions/5019724/in-emacs-what-does-this-error-mean-warning-cl-package-required-at-runtime
(require 'cl)

(load "cl-macs" nil t)

(when (featurep 'xemacs)
  (require 'kambi-xemacs))

;; string operations ---------------------------------------------------

(defun string-repeat (STR COUNT)
  "Return STR repeated COUNT times."
  (let ((RESULT ""))
    (dotimes (i COUNT RESULT) (setq RESULT (concat RESULT STR))  )
  )
)

(defun string-pos (SUBSTR STR &optional CASE-SENS)
  "Search for substring SUBSTR in string STR, case sensitive if CASE-SENS.
Jesli nie podasz 3-go parametru albo podasz nil to porownywanie bedzie
ignorowalo duze/male litery, wpp.  nie.  Zwraca pierwsza pozycje SUBSTR
w STR (zero-based) lub nil jesli nie znajdzie."
  (let ((i 0)
        (endi (- (length STR) (length SUBSTR)) )
        (RESULT nil))
    (while (and (<= i endi) (not RESULT))
      (if (symbolp (compare-strings SUBSTR 0 nil
                                    STR i (+ i (length SUBSTR))
                                    (not CASE-SENS)))
        (setq RESULT i)
      )
      (setq i (+ i 1))
    )
    RESULT
  )
)

(defun same-text (STR1 STR2 &optional CASE-SENS)
  "Compares two strings STR1 and STR2, using current locale.
CASE-SENS = nil (default) means \"ignore case\".
Return nil if strings equal, nil otherwise.
This is just a simple wrapper around compare-strings functions."
  (eq (compare-strings STR1 0 nil STR2 0 nil (not CASE-SENS)) t)
)

(defun array-pos-text (str str-array &optional case-sens)
  "Seeks for string STR in STR-ARRAY (list of strings).
If CASE-SENS is nil it ignores-case.
Returns zero-based index of first matching string
in STR-ARRAY or nil is STR is not found."
  (block func-block
    (if (not case-sens) (setq str (upcase str)) )

    (let (i str-from-array)
      (dotimes (i (safe-length str-array))
        (setq str-from-array (nth i str-array))
        (if (not case-sens) (setq str-from-array (upcase str-from-array)))
        (if (equal str str-from-array) (return-from func-block i))
      )
      nil ; ruturn nil if no string matched
    )
  )
)

(defun string-unquoted (str quotechar)
  "Jezeli pierwszy i ostatni znak str sa = quotechar i jezeli str ma dlugosc >= 2
to zwraca str obciete, tzn. bez tego pierwszego i ostatniego znaku.
Wpp. zwraca niezmodyfikowane str."
  ;; pierwszy check na length str > 1 zabezpiecza nas jednoczesnie przed
  ;; zmodyfikowaniem stringu dlugosci 1 zawierajacym samo quotechar
  ;; jak i przed blednym pobraniem aref() ze stringu o zerowej dlugosci.
  (if (and (> (length str) 1)
           (= (aref str (1- (length str))) quotechar)
           (= (aref str 0) quotechar))
      (substring str 1 (1- (length str)))
    str)
)

(defun string-is-prefix (prefix s &optional ignore-case)
  "Checks is PREFIX the beginnig of S."
  (eq (compare-strings prefix 0 nil s 0 (length prefix) ignore-case) t)
)

;; testy:
;; (string-is-prefix "aa" "aabb")
;; (string-is-prefix "ab" "aabb")

(defun string-is-suffix (ending s)
  "Return non-nil if string S ends with ENDING.
From http://emacswiki.org/emacs/ElispCookbook#toc4 ."
    (cond ((>= (length s) (length ending))
      (let ((elength (length ending)))
        (string= (substring s (- 0 elength)) ending)))
          (t nil)))

(defun kam-number-to-string-zero-pad (num minlength)
  "Converts NUM to string, adding zeros at the beginning if needed,
to make result at least MINLENGTH long."
  (let* ( (result (number-to-string num))
          (resultlen (length result)))
    (when (< resultlen minlength)
      (setq result (concat (make-string (- minlength resultlen) ?0) result)))
    result
  )
)

;; ------------------------------------------------------------
;; define kam-is-* consts (requires string-pos to be defined)

(defconst kam-is-windows
  (or (string-pos "windows" system-configuration)
      (string-pos "msvc-nt" system-configuration)
      (string-pos "mingw" system-configuration) )
  "Non-nil if we're under Windows, else nil.")

(defconst kam-is-freebsd
  (string-pos "freebsd" system-configuration)
  "Non-nil if we're under FreeBSD, else nil.")

(defconst kam-is-linux
  (and (not kam-is-windows) (not kam-is-freebsd))
  "Non-nil if we're under Linux, else nil.")

(defconst kam-is-darwin
  (string-pos "apple-darwin" system-configuration)
  "Non-nil if we're under Darwin (Mac OS X), else nil.")

(defconst kam-is-unix
  (or kam-is-freebsd kam-is-linux kam-is-darwin)
  "Non-nil if we're under UNIX (Linux or FreeBSD or others, precise
definition is \"whatever FPC considers as UNIX\"), else nil.")

(defconst kam-os-exe-extension
  (if kam-is-windows ".exe" "")
  "Extension of executable files. \".exe\" under Windows, \"\" under UNIXes.")

;; filenames operations -------------------------------------------------

(defun extract-file-path (fname)
  "Return the directory part, including final slash or backslash,
from given FNAME. Returns empty string (as opposed to nil, like
file-name-directory) if there's no directory part."
  (let ((result (file-name-directory fname)))
    (if result
        result
      "")
  )
)

;; tests:
;; (extract-file-path "/")
;; (extract-file-path "/blah")
;; (extract-file-path "/blah/")
;; (extract-file-path "blah")

(defun extract-file-name (fname)
  (file-name-nondirectory fname))

;; tests:
;; (extract-file-name "/")
;; (extract-file-name "/blah")
;; (extract-file-name "/blah/")

(defun basename (fname)
  (file-name-sans-extension (extract-file-name fname)))

(defun extract-file-ext (filename)
  "Find the extension in FILENAME, which is part of the basename after the last
dot. Examples:
  \"foo\" => \"\" (extensionless files give empty extension)
  \"foo.\" => \".\" (under UNIX-like filesystems,
    \"foo.\" and \"foo\" are different filenames)
  \"foo.bar\" => \".bar\"
  \"foo.bar.xyz\" => \".xyz\" (return only the *last* extension)
Summarizing, the definition is the same as in ObjectPascal's RTL."
  (file-name-extension filename t))

;; testy:
;; (extract-file-ext "foo")
;; (extract-file-ext "foo.")
;; (extract-file-ext "foo.bar")
;; (extract-file-ext "foo.bar.xyz")

(defun change-file-ext (filename new-ext)
  "Find the extension in FILENAME (for details what an extension of
filename is, look at `extract-file-ext') and replace it with NEW-EXT."
  (concat (file-name-sans-extension filename) new-ext))

(defun kam-file-name-in-directory (dir file-name)
  "DIR is a directory name, not necessary ending with a path delimiter.
Works even in case dir is nil or empty (interpreted as current dir).
This returns DIR and FILE-NAME concatenated making sure that between
DIR and FILE-NAME there is a path delimiter (slash or backslash)."
  (let ((dir-only (directory-file-name dir)))
    (if (and (stringp dir-only) (not (equal dir-only "")))
        (concat dir-only "/" file-name)
      file-name)
  )
)

;; tests:
;; (kam-file-name-in-directory "bla" "foo")
;; (kam-file-name-in-directory "bla/" "foo")
;; (kam-file-name-in-directory "/" "foo")
;; (kam-file-name-in-directory "" "foo") ;; should return "foo", not "/foo"

(defun special-directory-name-p (dir)
  "Returns non-nil if DIR is some special directory name that is a
subdirectory in all directories and that is not a real new subdirectory.
Stating it simply, returns non-nil when DIR is \"..\" or \".\".
Else returns nil."
  (or (equal dir "..") (equal dir "."))
)

;; tests:
;; (special-directory-name-p ".")
;; (special-directory-name-p "..")
;; (special-directory-name-p "blabla")
;; (special-directory-name-p "/blabla")

;; files operations -------------------------------------------------

(defun kam-search-for-file (dir file-name &optional dirs-names-to-omit
  nil-on-error)
  "Searches for file named FILE-NAME. FILE-NAME is a simple file
name with no directory components. Note that found file may be actually
himself a directory or a symlink, i.e. this searches for *any* file,
not only *regular* file.

Starts searching in directory DIR, descends recursively
 (i.e. descends into all subdirectories that can be examined,
avoids descending into symlinks to not fall in loop,
TODO: avoiding loops should be implemented better, of course).
Does not descend into directories with names in DIRS-NAMES-TO-OMIT
list.

Returns nil (if not found) or a string giving full filename.
If DIR was relative, than this string will be relative filename too,
else (if dir was absolute) than this string will be absolute filename
too.

When directory DIR will not exist (or will not be a directory,
or a symlink to directory) then value of NIL-ON-ERROR decides what happens:
if NIL-ON-ERROR is nil then we will exit with error (raised by `directory-files').
if NIL-ON-ERROR is non-nil then we will return nil.

Summing it up, this is something like implementation of program find
as ELisp function. I know, there already existed some ELisp functions
to achieve this, but I felt that those were just dirty hacks.
This function is clean."

  ;; TODO: I should avoid here signalling an error when trying to enter
  ;; some non-readable directory, i.e. errors raised by directory-files
  ;; should be silently ignored. Directories that we can't descend into
  ;; should be treated like empty directories, not like an error.

  ;; TODO: I can't use here nil-named block and simple (return ...)
  ;; instead of (return-from func-block ...). Why ?

  (if (and nil-on-error (not (file-directory-p dir)))
      nil

    (block func-block
      (let ((dirlist (directory-files dir nil nil t)))
        (dolist (item dirlist)
          (unless (special-directory-name-p item)
            (let ((file-name-full (kam-file-name-in-directory dir item)))
              (if (equal file-name item)
                  (return-from func-block file-name-full)
                (progn
                  (when (and (file-directory-p file-name-full)
                             (not (file-symlink-p file-name-full))
                             (not (member item dirs-names-to-omit))
                        )
                    (let ((recursive-result (kam-search-for-file file-name-full file-name)))
                      (when recursive-result (return-from func-block recursive-result))
                    ))))))))
      nil ;; if nothing called "(return ...)" then return nil
    ))
)


;; tests:
;; (kam-search-for-file "/home/michalis/elisp/" "kambi-pascal.el")
;; (kam-search-for-file "/home/michalis/elisp" "tuareg")
;; (kam-search-for-file "/home/michalis/sources/castle-engine/castle-engine/" "castleimages.pas" nil)
;; (kam-search-for-file "/home/michalis/sources/castle-engine/castle-engine/" "castleimages.pas" '("images"))
;; (kam-search-for-file "/home/michalis/sources/castle-engine/castle-engine/" "x3dnodes.pas" '("images"))

(defun kam-search-for-file-regexp-in-dirs (dir-list file-name-regexp)
  "This searches for file with name (only name, without any directory parts)
matching to regexp FILE-NAME-REGEXP. It searches in every existing
directory in DIR-LIST.
It's not important does directory names in DIR-LIST end with PathDelim.

If found, returns full filename (i.e. it's one of items from DIR-LIST
concatenated with filename, so if given item in DIR-LIST
is absolute filename -- result will be absolute filename,
else (if this item in DIR-LIST is relative filename) --
then result will be relative filename).

Else (if not found) returns nil.

Note: If more than one filename in some directory will match
to FILE-NAME-REGEXP, it's unspecified which one will be returned by
this function. If more than one directory in DIR-LIST will contain
files matching to FILE-NAME-REGEXP, this function will return
result in the 1st directory from DIR-LIST."
  (block func-block
    (dolist (dir dir-list)
      (when (file-exists-p dir)
        (let ((dir-files (directory-files dir nil file-name-regexp t)))
          (when dir-files
            (return-from func-block
              (kam-file-name-in-directory dir (car dir-files))))))))
)

;; tests:
;; (kam-search-for-file-regexp-in-dirs exec-path "^emacs$")
;; => "/usr/bin/emacs"
;; (kam-search-for-file-regexp-in-dirs exec-path "emacs")
;; => "/home/michal/bin/emacs_batch~"
;; (kam-search-for-file-regexp-in-dirs exec-path "not_existing")
;; => nil
;; (kam-search-for-file-regexp-in-dirs exec-path "fo.*o")
;; (kam-search-for-file-regexp-in-dirs exec-path "ba.*r")
;; (kam-search-for-file-regexp-in-dirs exec-path "aa")
;; (kam-search-for-file-regexp-in-dirs exec-path "ray")

(defun kam-search-for-program (program-file-name)
  "This searches directories `exec-path' for file
named PROGRAM-FILE-NAME (or PROGRAM-FILE-NAME.exe under Windows).

Returns filename with path if found (note that this filename
may stil be relative if one of entries on `exec-path' was a relative path;
e.g. some people put (dangerous) ./ in their $PATH, then `exec-path'
will contain relative path).

Returns nil if not found."
  (kam-search-for-file-regexp-in-dirs exec-path
    (concat "^" (regexp-quote program-file-name) kam-os-exe-extension "$"))
)

;; tests:
;;(kam-search-for-program "emacs")
;;(kam-search-for-program "ispell")
;;(kam-search-for-program "foo")
;;(kam-search-for-program "emacsnwclient")

;; operacje zamiany na stringach ----------------------------------------

(defun string-re-replace-all (string from-regexp to-string
  &optional fixedcase literal)
  "Change all occurences of from-regexp in string to to-string.

Case sensitivity of searching is determined by the value case-fold-search
in the current buffer (this behaviour is inherited from `string-match' function,
see there for more comments).

If fixedcase is non-nil, do not alter case of replacement text
 (this is inherited from `replace-match' function).

If literal is non-nil, insert to-string literally, else treat some things
in to-string specially, like \\D stands for Dth match in from-regexp
- look at help for `replace-match' function for details."
  (save-match-data
    (let ((string-to-do string)
          (result "")
	  (string-to-replace-and-append ""))
      (while (string-match from-regexp string-to-do)
        (setq string-to-replace-and-append (substring string-to-do 0 (match-end 0)))
        (setq string-to-do (substring string-to-do (match-end 0)))
	(setq result (concat result
	  (replace-match to-string fixedcase literal string-to-replace-and-append)
	))
      )
      (setq result (concat result string-to-do))
      result
    )
  )
)

;; re-function-replace ----------------------------------------

(defun re-function-replace (from-string to-string-function begin-pos end-pos)
  "Zamien tekst w aktualnym buforze pomiedzy pozycjami BEGIN-POS END-POS
z FROM-STRING na to co zwroci TO-STRING-FUNCTION.
TO-STRING-FUNCTION to funkcja bezparametrowa ktora zwraca co wstawic
zamiast znalezionego wystapienia FROM-STRING,
tzn. po kazdym znalezieniu FROM-STRING TO-STRING-FUNCTION bedzie wywolywane.
Wiec TO-STRING-FUNCTION moze uzywac np. takich funkcji jak `match-string'
zeby podjac decyzje o tym co wstawic na podstawic znalezionego
FROM-STRING."
  (save-excursion
    (goto-char begin-pos)
    (while (re-search-forward from-string end-pos t)
      (replace-match (funcall to-string-function) t t)
    )
  )
)

(defun re-function-replace-region (from-string to-string-function)
  (re-function-replace from-string to-string-function
    (region-beginning) (region-end))
)

(defun re-function-replace-buffer (from-string to-string-function)
  (re-function-replace from-string to-string-function
    (point-min) (point-max))
)

(defun kam-upcase-last (m)
  "Return M with last latter uppercased."
  (concat (substring m 0 (1- (length m)))
    (upcase (substring m (1- (length m))))))

(defun kam-match-upcase-last ()
  "Return `match-string' with last latter uppercased.
Example function to use as TO-STRING-FUNCTION with `re-function-replace'
and friends."
  (kam-upcase-last (match-string 0)))

;; --------------------------------------------------------------
;; operacje zamiany tekstu w buforze bez pytania usera :
;; kam-simple-replace podstawowe funkcje. Zamieniaja tekst FROM-STRING
;; na TO-STRING, zaczynaja od FROM-POS a koncza na END-POS. Jezeli
;; END-POS = nil to znaczy ze ma zamieniac do konca bufora.
;; Aktualna pozycja kursora (a takze mark, chyba ze Transient mark mode)
;; pozostaje nienaruszona, czyli cale nieniejsze proc sa w
;; bloku save-excursion.
;;
;; kam-simple-re-replace operuje na regexp.
;;
;; TODO: In simple-[re-]replace we should use markes, not just positions,
;; In some edge cases, the current approach may replace too much / too little.

(defun kam-simple-replace (from-string to-string from-pos
  &optional end-pos fixedcase)
  "Zamien tekst w aktualnym buforze z FROM-STRING na TO-STRING,
zacznij od FROM-POS, skoncz na END-POS (END-POS = nil oznacza ze ma zamieniac
do konca bufora). Aktualna pozycja kursora pozostaje nienaruszona.
Parametr FIXEDCASE - patrz replace-match.

Returns nil if no match was found, else returns non-nil."
  (let ((return-value))
    (save-excursion
      (goto-char from-pos)
      (while (search-forward from-string end-pos t)
        (replace-match to-string fixedcase t)
        (setq return-value t)))
    return-value)
)

(defun kam-simple-re-replace (from-string to-string from-pos
  &optional end-pos fixedcase)
  "Replace text in current buffer from FROM-STRING to TO-STRING,
starting from position FROM-POS to END-POS (END-POS = nil means
to the end of buffer).

Current position of cursor is not changed by this function.
Uses regexps. FIXEDCASE has the same meaning as in `replace-match'.

Returns nil if no match was found, else returns non-nil."
  (let ((return-value))
    (save-excursion
      (goto-char from-pos)
      (while (re-search-forward from-string end-pos t)
        (replace-match to-string fixedcase nil)
        (setq return-value t)))
    return-value)
)

;; tests: (message-ok (concat "replacing '" (match-string 0) "' with '" TO-STRING "'."))

;; --------------------------------------------------------------
;; operacje zamiany tekstu w buforze bez pytania usera,
;; funkcje interaktywne. inregion, frompos, frombeg

(defun kam-simple-replace-region (from-string to-string)
  "Simple replace in current region. Without asking user."
  (interactive)
  (kam-simple-replace from-string to-string (region-beginning) (region-end))
)

(defun kam-simple-re-replace-region (from-string to-string)
  "Simple regexp replace in current region. Without asking user."
  (interactive)
  (kam-simple-re-replace from-string to-string (region-beginning) (region-end))
)

(defun kam-simple-replace-frompos (from-string to-string)
  "Simple replace from cursor position. Without asking user."
  (interactive)
  (kam-simple-replace from-string to-string (point))
)

(defun kam-simple-re-replace-frompos (from-string to-string)
  "Simple regexp replace from cursor position. Without asking user."
  (interactive)
  (kam-simple-re-replace from-string to-string (point))
)

(defun kam-simple-replace-buffer (from-string to-string)
  "Simple replace in whole buffer. Without asking user."
  (interactive)
  (kam-simple-replace from-string to-string (point-min))
)

(defun kam-simple-re-replace-buffer (from-string to-string)
  "Simple regexp replace in whole buffer. Without asking user."
  (interactive)
  (kam-simple-re-replace from-string to-string (point-min))
)

;; ------------------------------------------------------------------------
;; funkcje realizujace jakies male komendy edytora ("male" w sensie
;; "zmieniaja tekst tylko lokalnie, zamiast operowac jakos na calym buforze")

(defun kam-indent-block-space ()
  "Indents current region by 1 space."
  (interactive)
  (increase-left-margin (region-beginning) (region-end) 1)
)

(defun kam-unindent-block-space ()
  "Unindents current region by 1 space."
  (interactive)
  (decrease-left-margin (region-beginning) (region-end) 1)
)

(defun kam-insert-dashes ()
  "Insert dashes until 78th column."
  (interactive)
  (while (< (current-column) 78) (insert "-")))

(defun kam-insert-20-dashes ()
  "Simply inserts 20 '-' chars. Kambi likes to use long lines of dashes
in comments in source files, to separate larger chunks of code.
If mode-name is PHP or HTML then it inserts 20 '=' chars."
  (interactive)

  (let (s)
    (if (or (string= mode-name "PHP") (string= mode-name "HTML"))
      (setq s "=")
      (setq s "-")
    )

    (insert (string-repeat s 20))
  )
)

(defun ins-char-code ()
  "Pobiera integer z minibuffera i wstawia into current buffer at
the cursor position char o takim numerze."
  (interactive)
  (insert (char-to-string (string-to-number (read-string "CharCode: "))))
)

(defun insert-newline-indented-as-prev ()
  "Wstaw w bufor tam gdzie stoimy NewLine (czyli to co powinnismy zrobic
gdy uzytkownik wpisze enter) i dodatkowo zacznij nowa linie wcieta
tak samo jak linia na ktorej stalismy (tzn. zbadaj poczatkowe whitespace'y
z aktualnej linii, pisz NewLine i pisz te whitespace'y w nowej linii).

To jest moje (Kambiego) ulubione indentation do ktorego jestem przyzwyczajony
jeszcze z edytorow Borlanda."
  (interactive)

  (let ((needs-indenting))
    (save-excursion
      (beginning-of-line)
      (setq needs-indenting (looking-at "[ \t]"))
    )
    (newline)
    ;; Robimy indent-relative tylko jesli linia powyzej zaczyna sie
    ;;   na jakies whitespace; jezeli linia powyzej nie zaczyna sie od whitespace
    ;;   to indent-relative nie przeniesie nas jak chcemy.
    ;; Zwracam uwage ze jesli linia powyzej miala 0 znakow (a wiec stojac w niej
    ;;   mielismy looking-at na \n) to needs-indenting = nil (false).
    (if needs-indenting (indent-relative-maybe))
  )
)

;; including gpl ----------------------------------------

(defun kam-gpl-licensed (program-name copyright-years copyright-holder)
  "Returns English text stating that this file is part of PROGRAM-NAME
and that it is distributed on GNU GPL (without any warranty).
At the beginning we also add appropriate Copyright line,
with given COPYRIGHT-YEARS and COPYRIGHT-HOLDER names.
See http://www.gnu.org/licenses/gpl-howto.html.

Text is indented by 2 spaces, there is newline at the end.
PROGRAM-NAME is not automatically surrounded in double-quotes -
add them yourself if you want.

Usage example:
 (insert
  (kam-gpl-licensed \"\\\"Bad Blaster\\\"\" \"2004\" \"Michalis Kamburelis\")
)"
  (concat
  "  Copyright " copyright-years " " copyright-holder ".

  This file is part of " program-name ".

  " program-name " is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  " program-name " is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with " program-name "; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
"
  )
)

(defun kam-gpl-licensed-comment (program-name copyright-years copyright-holder)
  "Returns typical GPL license and copyright header,
surrounded by comment markers. Uses comment-begin and comment-end,
so these cannot be one-line comment markers. There is a newline at the end."
  (concat comment-start "\n"
    (kam-gpl-licensed program-name copyright-years copyright-holder)
    comment-end "\n")
)

(defun kam-gpl-licensed-pascal (program-name)
  (concat
    (kam-gpl-licensed-comment program-name "2007" "Michalis Kamburelis")
    "\n"
  )
)

(defconst kam-gpl-header-regexp
  (concat
    "  Copyright \\([^ ]+\\) \\(.+\\)\\.

  This file is part of "
        "\\(.*\\)"
        (regexp-quote ".

  ")
        "\\(\\3\\)"
        (regexp-quote " is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  ")
        "\\(\\3\\)"
        (regexp-quote " is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with ")
        "\\(\\3\\)"
        (regexp-quote "; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
")))

(defun kam-check-is-gpl-licensed ()
  "Checks whether you inserted `gpl-licensed' text in current buffer.
Returns list of 3 items (string you used as PROGRAM-NAME,
COPYRIGHT-YEARS and COPYRIGHT-HOLDER) if yes. Returns nil if not."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward kam-gpl-header-regexp nil t)
        (list (match-string 3) (match-string 1) (match-string 2))
      nil
    )
  )
)

(defun kam-gpl-licensed-add-year (year)
  "Checks whether you inserted `gpl-licensed' text in current buffer.
If yes, then to the year of the copyright we add \",\"YEAR (and return non-nil).
Otherwise, just return nil."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward kam-gpl-header-regexp nil t)
      (replace-match (concat (match-string 1) "," year) t t nil 1)
      t)
  )
)

;; (defun kam-gpl-licensed-change-program-name (program-name)
;;   "Checks whether you inserted `gpl-licensed' text in current buffer.
;; If yes, then to the year of the copyright we add \",\"YEAR (and return non-nil).
;; Otherwise, just return nil."
;;   (save-excursion
;;     (goto-char (point-min))
;;     (when (re-search-forward kam-gpl-header-regexp nil t)
;;       (replace-match program-name t t nil 3)
;;       ;; TODO: awww. This doesn't work like it should.
;;       ;; First replace-match moves the text, and so next replace-match
;;       ;; will not work as you would expect --- the replacement text
;;       ;; will be misplaced.
;;       (replace-match program-name t t nil 4)
;;       (replace-match program-name t t nil 5)
;;       (replace-match program-name t t nil 6)
;;       t)
;;   )
;; )

;; inne funkcje ------------------------------------------------------------

(defun eval-list (list)
  "Dla kazdego elementu list wywolaj eval zwroc wynik ostatniego wywolania
 (lub nil jesli lista pusta). To NIE jest to samo co (eval list) - wywolujac
 (eval list) stwierdzasz ze pierwszy element list to nazwa funkcji a pozostale
to jej parametry."
  (eval `(progn ,@list))
)

(defun simple-case (expr &rest clauses)
  "Proste case, w przeciwienstwie do oryginalnego case Emacsa uzywa equal
do porownywania elementow zamiast eql. Nie jest przy tym makrem wiec
nie jest tak wygodne.

CLAUSES to ciag list. Pierwszy element kazdej listy bedzie porownywany
po kolei z expr i dla pierwszej listy dla ktorej elementy beda
rowne (w sensie funkcji equal) zostanie wywolane (eval-list (cdr LISTA))
gdzie LISTA to ta pasujaca lista (tzn. dla niej (equal (car LISTA) expr)).

simple-case wykonuje wiec instrukcje dla pierwszej listy ktorej pierwszy
element pasuje i potem konczy dzialanie (zwracajac wynik wykonania
instrukcji tej pasujacej listy).

Ostatnia lista moze miec VALUE rowne 't lub 'otherwise i wtedy jezeli
zadna poprzednia lista nie miala (equal VALUE BODY) to ostatnia lista
zostanie zostanie potraktowana jako pasujaca i zostanie zwrocone
jej (eval-list (cdr LISTA)). Jezeli zadna lista nie miala (equal VALUE BODY)
i ostatnia lista nie ma symbolu 't lub 'otherwise jako VALUE to
zwracane jest nil.

Jak widac, w przeciwienstwie do oryginalnego case (VALUE BODY ...) bedzie
musialo byc zazwyczaj pisane z apostrofem gdy bedziemy wywolywac ta funkcje
 (zeby to obejsc musialbym zrobic makro; nie robie makra bo mi sie nie
chce meczyc). Ponadto VALUE jest zawsze porownywane jako calosc;
nie robimy tu specjalnych sprawdzen gdy VALUE jest lista ale byc moze
kiedys zrobimy wiec tymczasowo NIGDY NIE UZYWAJ JAKO VALUE JAKIEJS
LISTY !! "

  (let (result done)
    (while (and clauses (not done))
      (let
        ((c (car clauses))) ; c = aktualna klauzula (VALUE BODY ..)

        ;; jezeli klauzula c pasuje do expr lub jezeli klauzula c jest
        ;; klauzula "else" do case'a to result := (eval-list (cdr c))
        ;; i ustaw done na 't (nie mozemy uzywac result do sprawdzenia
        ;; czy juz done bo przeciez ktoras funkcja case'a tez moze
        ;; zwrocic nil i wtedy simple-case ma zwracac nil mimo ze znalazlo
        ;; pasujaca klauzule.

	(if (or (equal (car c) expr)
                (and (memq (car c) '(t otherwise)) (null (cdr clauses))) )
            (progn
              (setq result (eval-list (cdr c)))
              (setq done t)
            )
        )
      )
      (setq clauses (cdr clauses))
    )
    result
  )
)

(defun add-to-list-new-items (list new-list &optional append)
  "Every element from NEW-LIST is added to the LIST if and only if it
does not already exist in the list LIST (test for presence of the element
is done using `equal'). All new items are added at the beginning of the
list unless append is non-nil , in which case new items are added at the
end.

At the end new list is returned (this function is not destructive, i.e.
the value of list is not modified).

Don't count on that, maybe I will change this if I'll need it later:
Order of adding new items to the list is reversed when adding items
at the beginning, e.g.
  (add-to-list-new-items '(1 2) '(3 4) t) => '(1 2 3 4)
  (add-to-list-new-items '(1 2) '(3 4)  ) => '(4 3 1 2)
"
  (let (i (result list))
    (dotimes (i (safe-length new-list))
      (add-to-list 'result (nth i new-list) append)
    )
    result
  )
)

(defun silently-save-current-buffer ()
  "Wywolujac (save-buffer) zapamietujesz aktualny bufor ale jest wyswietlane
message \"(No changes need to be saved)\" jezeli bufor nie byl modified.
To jest dobre w trybie interactive ale (czasami) niedobre w trybie batch.
Wiec niniejsza funkcja wywoluje (save-buffer) tylko jezeli (buffer-modified-p)
i tym samym dziala jak save-buffer (przynajmniej w typowym przypadku dla
zwuklych buforow (nie indirect) i dopoki nie chcesz przekazac jakiegos speszyl
parametru do save-buffer) tyle ze siedzi cicho jesli pliku nie trzeba
zapamietac."
  (interactive)
  (if (buffer-modified-p)
      (save-buffer)
  )
)

(defun message-ok (prompt)
  "Show PROMPT in minibuffer and waits until user presses Enter. USUALLY you
want to use standard Emacs (message ...) instead, but sometimes it
is useful to wait for user."
  (let (answer)
    (setq answer (read-event (concat prompt " [Enter]")))
    (while (not (eq answer 'return))
      (beep)
      (setq answer (read-event (concat prompt " [Enter]")))
    )
  )
)

(defun kam-beg-of-buf ()
  "Like `beginning-of-buffer' but don't set mark."
  (interactive)
  ;; hack to make c-home work fast with minimap set to "free" mode.
  ;; Resigned, because minimap was generally buggy with mode <> relative,
  ;; update function sometimes crashed?
;;  (minimap-mode nil)
  (goto-char (point-min))
;;  (minimap-mode t) ;; hack to make c-home work fast with minimap set to "free" mode
)

(defun kam-end-of-buf ()
  "Like `end-of-buffer' but don't set mark."
  (interactive)
  (goto-char (point-max))
)

(defun define-keys-to-nil (map key-list)
  "For each element of key-list calls (define-key map key nil).
Useful when you want to \"turn off\" some keybindings in keymap copied
from keymap of another mode (e.g. copied by define-derived-mode).

Returns something unspecified (currently nil but don't depend on this -
maybe I'll invent some use for this)."
  (dolist (elt key-list) (define-key map elt nil))
)

(defun untabify-buf ()
  (interactive)
  (untabify (point-min) (point-max))
)

(defun set-local-compile-command (VALUE)
  "Just a shortcut for
  (make-local-variable 'compile-command)
  (setq compile-command VALUE)"
  (make-local-variable 'compile-command)
  (setq compile-command VALUE)
)

(defun kam-compile-prompt (command)
  "It works like setting `compile-command' and then calling `compile'
interactively: it always asks the user to confirm the compilation
 (and optionally adjust proposed COMMAND).

So you can use this function as a drop-in replacement for
`compile' function in cases when you want to call it
from Elisp code (i.e. not interactively)
and when you *always* want to ask user for confirmation."

  ;; Below is based on implementation of `compile' command
  ;; (setq command
  ;;   (read-from-minibuffer "Compile command: "
  ;;     command nil nil '(compile-history . 1)))
  ;; (save-some-buffers (not compilation-ask-about-save) nil)
  ;; (compile command)

  (setq compile-command command)
  (let ((saved-compilation-read-command compilation-read-command))
    (setq compilation-read-command t)
    (call-interactively 'compile)
    (setq compilation-read-command saved-compilation-read-command))
)

(defun kam-string-delete-ctrl-m (text)
  "Returns TEXT with all occurences of Ctrl-M (displayed as ^M) deleted."
  (string-re-replace-all text (string 13) "" t t))

(defun kam-cvs-occur-problems ()
  "Extract lines from `cvs update' output that indicate some possible
problems: lines that indicate that merge or conflict occured."
  (interactive)
  (occur "^[CM] ")
)

(defun kam-current-file-name ()
  "Returns file-name visited in current buffer.
If current buffer is not visiting any file, returns name of the file
visited in the last buffer where you been (thanks to that you can use
this function e.g. in minibuffer)."
  (if (buffer-file-name)
      buffer-file-name
    (buffer-file-name (other-buffer nil t)))
)

(defun kam-diff-with-current-version ()
  "Displays unified diff of changes of current buffer contents versus
contents of the file saved on disk.

Works by saving current buffer to a temporary file (but doesn't change
buffer's file name or modified flag etc.) and running `diff -u' to make
the patch and then displaying this patch in a new buffer."
  (interactive)

  ;; TODO -- using two temporary files, and shell-command (that parses
  ;; string into args itself) makes impl of this very unclean.

  (write-region (point-min) (point-max) "/tmp/kam-diff-with-current-version")
  (shell-command (concat "diff -u \"" (buffer-file-name)
    "\" /tmp/kam-diff-with-current-version "
    " > /tmp/kam-diff-with-current-version.patch"))
  (find-file "/tmp/kam-diff-with-current-version.patch")
)

(defun kam-line-at-point ()
  "Returns text (only the string, no properties)
from the point to the nearest end of line.
Uses `looking-at', so it changes current match information."
  (interactive)
  (when (looking-at "\\(.*\\)$")
    (match-string-no-properties 0))
)

(defconst kam-home-directory (expand-file-name "~"))

(defun kam-fix-nbsp-once (text)
  (query-replace (concat " " text " ") (concat " " text "&nbsp;"))
  (query-replace (concat " " text "\n") (concat "\n" text "&nbsp;")) ;; move newline after "text"
  (query-replace (concat "\n" text " ") (concat "\n" text "&nbsp;")))

(defun kam-fix-nbsp ()
  "Fix all common place in Polish texts where you should usually insert &nbsp;."
  (interactive)
  (setq case-fold-search nil)
  (goto-char (point-min)) (kam-fix-nbsp-once "m.in.")
  (goto-char (point-min)) (kam-fix-nbsp-once "np.")
  (goto-char (point-min)) (kam-fix-nbsp-once "w")
  (goto-char (point-min)) (kam-fix-nbsp-once "do")
  (goto-char (point-min)) (kam-fix-nbsp-once "dla")
  (goto-char (point-min)) (kam-fix-nbsp-once "o")
  (goto-char (point-min)) (kam-fix-nbsp-once "z")
  (goto-char (point-min)) (kam-fix-nbsp-once "za")
  (goto-char (point-min)) (kam-fix-nbsp-once "na")
  (goto-char (point-min)) (kam-fix-nbsp-once "lub")
  (goto-char (point-min)) (kam-fix-nbsp-once "i")

  (goto-char (point-min)) (kam-fix-nbsp-once "M.in.")
  (goto-char (point-min)) (kam-fix-nbsp-once "Np.")
  (goto-char (point-min)) (kam-fix-nbsp-once "W")
  (goto-char (point-min)) (kam-fix-nbsp-once "Do")
  (goto-char (point-min)) (kam-fix-nbsp-once "Dla")
  (goto-char (point-min)) (kam-fix-nbsp-once "O")
  (goto-char (point-min)) (kam-fix-nbsp-once "Z")
  (goto-char (point-min)) (kam-fix-nbsp-once "Za")
  (goto-char (point-min)) (kam-fix-nbsp-once "Na")
  (goto-char (point-min)) (kam-fix-nbsp-once "Lub")
  (goto-char (point-min)) (kam-fix-nbsp-once "I")
)

;; todo: merge kam-fix-textilde and kam-fix-nbsp

(defun kam-fix-textilde-once (text)
  (query-replace (concat " " text " ") (concat " " text "~"))
  (query-replace (concat " " text "\n") (concat "\n" text "~")) ;; move newline after "text"
  (query-replace (concat "\n" text " ") (concat "\n" text "~")))

(defun kam-fix-textilde ()
  "Fix all common place in Polish texts where you should usually insert ~."
  (interactive)
  (setq case-fold-search nil)
  (goto-char (point-min)) (kam-fix-textilde-once "m.in.")
  (goto-char (point-min)) (kam-fix-textilde-once "np.")
  (goto-char (point-min)) (kam-fix-textilde-once "w")
  (goto-char (point-min)) (kam-fix-textilde-once "do")
  (goto-char (point-min)) (kam-fix-textilde-once "dla")
  (goto-char (point-min)) (kam-fix-textilde-once "o")
  (goto-char (point-min)) (kam-fix-textilde-once "z")
  (goto-char (point-min)) (kam-fix-textilde-once "za")
  (goto-char (point-min)) (kam-fix-textilde-once "na")
  (goto-char (point-min)) (kam-fix-textilde-once "lub")
  (goto-char (point-min)) (kam-fix-textilde-once "i")

  (goto-char (point-min)) (kam-fix-textilde-once "M.in.")
  (goto-char (point-min)) (kam-fix-textilde-once "Np.")
  (goto-char (point-min)) (kam-fix-textilde-once "W")
  (goto-char (point-min)) (kam-fix-textilde-once "Do")
  (goto-char (point-min)) (kam-fix-textilde-once "Dla")
  (goto-char (point-min)) (kam-fix-textilde-once "O")
  (goto-char (point-min)) (kam-fix-textilde-once "Z")
  (goto-char (point-min)) (kam-fix-textilde-once "Za")
  (goto-char (point-min)) (kam-fix-textilde-once "Na")
  (goto-char (point-min)) (kam-fix-textilde-once "Lub")
  (goto-char (point-min)) (kam-fix-textilde-once "I")
)

(defun kam-fix-textilde-en ()
  "Fix all common place in English texts where you should usually insert ~."
  (interactive)
  (setq case-fold-search nil)
  (goto-char (point-min)) (kam-fix-textilde-once "a")
  (goto-char (point-min)) (kam-fix-textilde-once "the")
  (goto-char (point-min)) (kam-fix-textilde-once "and")
  (goto-char (point-min)) (kam-fix-textilde-once "or")
  (goto-char (point-min)) (kam-fix-textilde-once "e.g.")

  (goto-char (point-min)) (kam-fix-textilde-once "A")
  (goto-char (point-min)) (kam-fix-textilde-once "The")
  (goto-char (point-min)) (kam-fix-textilde-once "And")
  (goto-char (point-min)) (kam-fix-textilde-once "Or")
  (goto-char (point-min)) (kam-fix-textilde-once "E.g.")
)

(defun kam-tex-to-html ()
  "A series of (interactive) regexp replacements,
that do (very very basic) convertion from TeX to HTML."
  (interactive)
  (query-replace-regexp "---" "&mdash;")
  (query-replace-regexp "--" "&ndash;")
  (query-replace-regexp "\\\\texttt{\\([^}]+\\)}" "<tt>\\1</tt>")
  (query-replace-regexp "\\\\_" "_")
  (query-replace-regexp "\\\\emph{\\([^}]+\\)}" "<em>\\1</em>")
  (query-replace-regexp "\\$\\([^$\n]+\\)\\$" "<i>\\1</i>")
)

(defun kam-insert-bom ()
  "Insert BOM into utf8-8 file.

From http://andrewcoxtech.blogspot.com/2009/11/inserting-bom-into-file.html"
  (interactive)
  (goto-char (point-min))
  (ucs-insert (string-to-number "FEFF" 16))
)

(defun kam-non-empty-stringp (s)
  "Return non-nil if S is a non-empty (not \"\") string."
  (and (stringp buffer-file-name) (not (equal s ""))))

(defun kam-open-sudo ()
  "Open current buffer through sudo."
  (interactive)
  (if (fboundp 'crux-reopen-as-root)
      ;; crux-reopen-as-root-mode is a better replacement, from https://github.com/bbatsov/crux
      ;; or no, it doesn't reopen the file *now*
      (crux-reopen-as-root)
    (if (kam-non-empty-stringp buffer-file-name)
        (let ((file-name buffer-file-name))
          ;; adding sudo to another sudo prefix doesn't work
          (when (string-is-prefix "/sudo:" file-name)
            (error "Already viewing the file through \"sudo\"."))
          (kill-buffer)
          (find-file (concat "/sudo::" file-name)))
      (let ((dir-name default-directory))
        ;; adding sudo to another sudo prefix doesn't work
        (when (string-is-prefix "/sudo:" dir-name)
          (error "Already viewing the file through \"sudo\"."))
        (kill-buffer)
        (dired (concat "/sudo::" dir-name)))
    )
  )
)

(defun kam-count-lines-buffer ()
  "Return the total number of lines in this buffer."
  (count-lines (point-min) (point-max)))

(defun kam-delete-matching-lines-buffer (regexp)
  "Delete all matching lines, always in the whole buffer.

Exactly like `delete-matching-lines', but always uses the whole buffer,
regardless of current position and regardless of mark position
 (and mark position existence, and Transient mode being active)."
  (interactive "sRegexp to remove: ")
  (save-excursion
    (let ((lines-before (kam-count-lines-buffer)))
      (delete-matching-lines regexp (point-min-marker) (point-max-marker))
      (let ((lines-after (kam-count-lines-buffer)))
        (message "Removed %d lines, remaining %d." (- lines-before lines-after) lines-after)))))

(defun kam-clean-here ()
  "Clear this project."
  (interactive)
  ;; use default-directory instead of (buffer-file-name),
  ;; so it also works in non-file buffers, like compilation or "asynch shell command"
  (if (kam-is-castle-engine-project-p default-directory)
      (async-shell-command "castle-engine clean")
    (async-shell-command "dircleaner . clean")
  ))

(defun kam-run-here ()
  "Run this project."
  (interactive)
  (if (kam-is-castle-engine-project-p default-directory)
      (async-shell-command "castle-engine run")
    (if (stringp buffer-file-name)
        (async-shell-command (concat "./"
          (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
      (error "Cannot run this project, because we don't know how. Not inside castle-engine project, and not visiting a file."))
  )
)

(defun kam-open-dir-external ()
  "Open current directory in an external program (file manager)."
  (interactive)
  ;; (condition-case nil
    (async-start-process (concat "kam-open-dir-external " default-directory)
      "xdg-open" nil (expand-file-name default-directory))
    ;; (error
    ;;   ;; fallback from xdg-open to pantheon-files (Elementary file manager),
    ;;   ;; possibly not needed anymore
    ;;   (async-start-process (concat "kam-open-dir-external " default-directory)
    ;;     "pantheon-files" nil
    ;;     (expand-file-name default-directory)))
  ;; )
)

(defun kam-open-terminal-here ()
  "Open terminal in the current directory."
  (interactive)
  (async-start-process (concat "kam-open-terminal-here " default-directory)
    ;; config on Debian with
    ;; sudo update-alternatives --config x-terminal-emulator
    "x-terminal-emulator" nil
    (concat "--working-directory=" (expand-file-name default-directory)))
)

(defun kam-project-dir (file-name)
  "Is the file FILE-NAME inside a project.
A \"project\" is just a directory
with .git or .svn subdirectory, which is a simple and working way
inspired by https://github.com/bbatsov/projectile .
FILE-NAME may be relative or even empty, will be resolved with respect
to the ``default-directory''.

Note: we don't use the existence of CastleEngineManifest.xml to
indicate project's root dir, because some projects have several
subdirs, each with their own CastleEngineManifest.xml,
and with code using each other. IOW, .git or .svn subdirectory
is just a wider and more comfortable definition of a \"project\" right now
for us."
  (let ((path (extract-file-path (expand-file-name file-name))))
    (if (or (file-exists-p (concat path "/.git"))
            (file-exists-p (concat path "/.svn")))
        path
      (when (and (not (member path '("/" "~/")))
                 ;; avoid infinite recursion
                 (not (equal file-name (file-name-directory (directory-file-name path)))))
        (kam-project-dir (file-name-directory (directory-file-name path))))
    )
  )
)

(defun kam-top-most-svn-dir (dir)
  "Find the top-most SVN dir from DIR.

Also stops at DIR with .projectile file, does not go to higher SVN dir.
Reasoning:

- .projectile files sometimes delimit subprojects within larger SVN repos.
- Limiting to the projectile project means that projectile-find-file cache
  is up-to-date (otherwise you land in higher projectile project,
  that has never updated cache because it's usually shadowed by subproject
  when opening particular file).
- kam-run-here also works here better, as proper CastleEngineManifest.xml
  is more likely found here.

Assumes that DIR is for sure an SVN dir."
  (if (or (member dir '("/" nil))
          (file-exists-p (kam-file-name-in-directory dir ".projectile")))
      dir ;; don't go upward
    (let ((parent-dir (file-name-directory (directory-file-name dir))))
      (if (svn-version-controlled-dir-p parent-dir)
          (kam-top-most-svn-dir parent-dir)
        dir))))

(defun kam-version-control ()
  "Call magit-status in GIT, svn-status in SVN."
  (interactive)

  ;; Load magit, for kam-version-control.
  ;; Simply ignore the error in case magit not available (version control
  ;; stuff will not work but the rest of my Emacs config will).
  ;;
  ;; Why it doesn't work (it seems like require 'magit-git...
  ;; is ignored) when this is outside of this function, and executes when Emacs
  ;; loads? Possibly because package-initialize is done after loading kambi-utils?
  ;; Not really a problem for now.
  (require 'magit nil 'noerror)
  (require 'magit-git nil 'noerror)

  (unless (fboundp 'magit-toplevel)
    (defun magit-toplevel ()
      "Is the current ``default-directory'' within a GIT repository.
  This is compatibility hack in case of older magit version."
      (interactive)
      (magit-get-top-dir default-directory))
  )

  (if (and (require 'magit nil 'noerror)
           (magit-toplevel))
      (call-interactively 'magit-status)
    ;; we expand dir, because svn-version-controlled-dir-p calls "svn info ..."
    ;; on it, so it needs ~ expanded.
    (let ((expanded-dir (expand-file-name default-directory)))
      (if (svn-version-controlled-dir-p expanded-dir)
          (svn-status-1 (kam-top-most-svn-dir expanded-dir))
        (error "Neither in GIT or SVN repository.")))))

(defun kam-nondir-file-readable-p (fname)
  "Return non-nil if the file exists, is readable and is not a directory."
  (and (file-readable-p fname) (not (eq (car (file-attributes fname)) t)))
)

(defun kam-refresh-colors-in-buffer ()
  (interactive)
  (font-lock-fontify-buffer)
  (when mmm-available
    (save-excursion
      ;; mmm-parse-buffer is harmless if current buffer does not use mmm-mode,
      ;; so I just do it for every buffer.
      ;; Also note that mmm-parse-buffer always moves the cursor to
      ;; the beginning of buffer (I don't know why), that's why I'm doing
      ;; it all inside save-excursion.
      (mmm-parse-buffer))))

(defun kam-recenter-if-needed ()
  "Recenter, but only if needd."
  ;; TODO
  ;; (recenter)
)

(defconst kam-sensible-scroll-margin 3)

(defun kam-nonincremental-repeat-search-forward ()
  "Repeat last nonincremental search, foward, and recenter intelligently."
  (interactive)
  ;; In newer Emacs version, there's no
  ;; function nonincremental-repeat-re-search-forward.
  ;; Instead, nonincremental-repeat-search-forward repeats last search
  ;; -- either literal or for regexp.
  (if (>= emacs-major-version 22)
      (call-interactively 'nonincremental-repeat-search-forward)
    (call-interactively 'nonincremental-repeat-re-search-forward))
  (kam-recenter-if-needed)
)

(defun kam-nonincremental-repeat-search-backward ()
  "Repeat last nonincremental search, foward, and recenter intelligently."
  (interactive)
  (if (>= emacs-major-version 22)
      (call-interactively 'nonincremental-repeat-search-backward)
    (call-interactively 'nonincremental-repeat-re-search-backward))
  (kam-recenter-if-needed)
)

(defun kam-noindent () 'noindent)

(defun kam-find-file-at-point ()
  (interactive)
  "Find file at point."
  ;; Do not try to use helm anymore (since we have ivy),
  ;; this is causing more trouble (as I cannot copy filename in helm
  ;; like in ivy).
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
    (call-interactively 'find-file-at-point)
  ;; )
)

(defun kam-strip-final-slash (x)
  "If X ends with slash, strip it. Otherwise, returns X unmodified."
  (if (string-is-suffix "/" x)
      (substring x 0 -1)
    x
  )
)

;; tests:
;; (kam-strip-final-slash "foo/bar")
;; (kam-strip-final-slash "foo/bar/")
;; (kam-strip-final-slash "")
;; (kam-strip-final-slash "/")

;; ------------------------------------------------------------

(provide 'kambi-utils)

;;; kambi-utils.el ends here
