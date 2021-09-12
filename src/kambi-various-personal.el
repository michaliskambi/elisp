;;;; kambi-utils --- This file is a random mix of many Michalis Emacs adjustments.

;;; Commentary:

(require 'kambi-utils)

;; package -------------------------------------------------------------------

;; set kam-package-force-http to nil, if not defined.
(unless (boundp 'kam-package-force-http)
  (setq kam-package-force-http nil))

(defconst kam-package-protocol
  ;; Note: On Windows, use http instead of https, standard Emacs cannot handle https:
  ;; https://stackoverflow.com/questions/35345045/emacs-install-a-package-with-melpa-on-windows
  ;; https://emacs.stackexchange.com/questions/22468/how-do-i-get-melpa-working-on-my-windows-8-laptop
  ;; Same is necessary on Raspberry Pi... Just honor kam-package-force-http.
  (if (or kam-is-windows kam-package-force-http) "http" "https")
)

(when (require 'package nil 'noerror)
  ;; Add melpa.
  ;; Do this early, as it may be used by auto-complete and others,
  ;; lower in this file.
  ;; See https://www.gnu.org/software/emacs/manual/html_node/elisp/Backquote.html#Backquote about backqoute.
  (add-to-list 'package-archives `("melpa" . ,(concat kam-package-protocol "://melpa.org/packages/")))

  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives `("gnu" . ,(concat kam-package-protocol "://elpa.gnu.org/packages/"))))

  ;; if kam-package-force-http, remove ELPA through https, and add through http
  (when (string= kam-package-protocol "http")
    (delete '("gnu" . "https://elpa.gnu.org/packages/") package-archives)
    (add-to-list 'package-archives `("gnu" . ,(concat kam-package-protocol "://elpa.gnu.org/packages/")))
  )

  ;; In case of problems "wrong type argument arrayp" on the line below:
  ;; they may indicate temporary connectivity problems.
  ;; You still have to manually delete ~/.emacs.d/elpa/archives/melpa/
  ;; to get rid of the error, next list-packages or such will reinitialize it.
  (package-initialize)

  ;; TODO: change below to just use
  ;; package-install-selected-packages, once I have emacs 25 everywhere.
  (defun kam-install-if-needed (package-list)
    "Install the listed packages, if not installed yet.
PACKAGE-LIST is expected to be a list of symbols (packages)."
    (let ((refresh-already-done nil))
      (dolist (package package-list)
        ;; inspired by https://github.com/bbatsov/crux
        (unless (package-installed-p package)
          (unless refresh-already-done
            (package-refresh-contents)
            (setq refresh-already-done t)
          )
          (package-install package)
        )
      )
      (unless refresh-already-done
        (message "No need to install anything, everything was already installed"))
    )
  )
  (kam-install-if-needed (list
;;    'ag ;; no longer used, ripgrep is even better
    'ripgrep
    'projectile-ripgrep
    'projectile
    'counsel
    'ivy-hydra
;;    'ivy-pass ;; not used now
    'auto-complete
    'adoc-mode
;;    'dired-collapse
;;    'dired-du
;;    'dired-subtree ;; not used after all
    'paradox
    'crux
    'smartscan
    'magit
;;    'wgrep-ag
    'iedit
    'php-mode
    'csharp-mode
    'groovy-mode
    'lua-mode
    'wgrep ;; makes also ripgrep results editable, press "r" to edit, C-x C-s to save
    'browse-kill-ring
    'dsvn
  ))
)

;; ---------------------------------------------------------------------------

;; w tej chwili przydatne mi zeby skonfigurowac php-mode (nie uzywam
;; c-mode i c++-mode, mam wlasne wersje tych modes ktore nie robia indentation).
;; Ale moze kiedys znajda sie tez jakies inne modes ktore beda tego uzywac.
(setq c-syntactic-indentation nil)

;; language to polish (potrzebne zeby dobrze wstawial/wyswietlal pl znaczki)
(when (fboundp 'set-language-environment)
  (set-language-environment "Polish")
  ;; special configurarion needed to copy&paste text with pl ogonki
  ;; between Emacs and other Windows apps.
  (when kam-is-windows
    (when (fboundp 'codepage-setup) (codepage-setup 1250))
    (set-selection-coding-system 'cp1250))
)

;; prefer utf-8 (for newly opened files, buffers (like svn diff, svn log etc.))
(prefer-coding-system 'utf-8)

;; set window size on Windows
;; (under X window sizes are better adjusted by ~/.Xresources)
(when kam-is-windows
  (defun kam-set-default-frame-size ()
    "Sets width and height of `selected-frame' to Kambi's default values.
This function is useful to sometimes call explicitly, because NTEmacs frame size
may be screwed up after running some program that changed screen size
(using WinAPI's ChangeDisplaySettings)."
    (interactive)
    (set-frame-height (selected-frame) 33)
    (set-frame-width (selected-frame) 96))
  (kam-set-default-frame-size)
)
(when kam-is-darwin
  (defun kam-set-default-frame-size ()
    "Sets width and height of `selected-frame' to Kambi's default values."
    (interactive)
    (set-frame-height (selected-frame) 33)
    (set-frame-width (selected-frame) 96))
  (kam-set-default-frame-size)
)

;; zeby ekran przesuwal sie o 1 linie, nie o pol ekranu
(setq scroll-step 1)

;; pokazuj numer linii i kolumny na modeline
;; (although line-number-mode is "on" by default, so "(line-number-mode 1)" should not be needed)
(column-number-mode 1)
(line-number-mode 1)

;; pokazuj region wizualnie gdy mark jest active
;; (jednak wylaczone - mark za szybko staje sie not active !)
;; (transient-mark-mode t)

;; ustal format tytulu okienka
(setq frame-title-format
  (concat "%b - "
    (if (and (= (user-uid) 0) (not kam-is-windows)) "ROOT " "")
    "emacs"
  )
)

;; zeby nie zawijal linii (hscroll-mode is obsolete in emacs 2.1,
;; using truncate-lines variable instead)
(if (< emacs-major-version 21)
    (hscroll-global-mode)
  ;; truncate-lines is per-buffer variable, so we need to set it using setq-default
  (setq-default truncate-lines t)
  (setq truncate-partial-width-windows t)
)

;; ponizej dodaje maly hook aby w buffer-menu-mode (Alt-0 o ile moje keybings
;; sa) przycisniecie Enter zamykalo bufor buffer-menu-mode i otwieralo
;; zaznaczony plik. To jest nieco lepsze niz standardowe komenda przypisana
;; do Entera ktora nie zamyka bufora buffer-menu.
(add-hook 'buffer-menu-mode-hook
  (lambda ()
    (local-set-key "\n" (quote Buffer-menu-1-window))
  )
  t)

;; Emacs >= 22 has ibuffer, and I think that I prefer it over standard
;; buffer-menu.
;;
;; Under Windows and Darwin ibuffer has some dumb standard configuration
;; and I'm unable to revert it (opens in other window even though it's
;; configured to not do this, shows some [ Default ] like).
;; No time to investigate it now.
;; So there I switch back to default buffer-menu for now.
;;
;; Later: oh, forget it. Newer Debian also has some strange ibuffer config,
;; I'm too lazy to adjust it. Besides, I don't see so much improvement over
;; buffer-menu --- I guess that I simply got accustomed and like buffer-menu
;; now, no need to change to ibuffer.
(defconst kam-use-ibuffer nil
  "non-nil if I should use ibuffer")

(add-hook 'ibuffer-mode-hook
  (lambda ()
    ;; redefine it to Kambi standard shortcut
    (local-set-key (kbd "M-o") 'other-window)
  )
  t)

;; server --------------------------------------------------------------------

;; Emacs server, to have emacsclient working
(if kam-is-windows
    (when (kam-search-for-program "gnuserv")
      ;; Uwaga: gnuserv musi byc gdzies na sciezce.
      ;; Zeby sie polaczyc, potrzebujesz tez gnuclient[w].
      ;;
      ;; Uwaga: gnuclient z Cygwina (instaluje sie razem z jakims pakietem
      ;; xemacsa pod Cygwina) NIE umie sie polaczyc do NTEmacsa z gnuserv
      ;; z contrib/gnuserv-win32/unpacked/Release/. Innymi slowy,
      ;; nigdy nie uzywaj gnuclient z Cygwina, a najlepiej w ogole upewnij
      ;; ze Cygwin nie zainstalowal zadnego gnuclient, zeby uniknac ew. pomylek.
      ;;
      ;; gnuclient[w] dziala tak samo jak emacsclient (tylko byl najwyrazniej pisany
      ;; z idea bardziej ogolnego narzedzia, nie tylko do Emacsa).
      ;; Parametr --no-wait emacsclienta odpowiada parametrowi -q gnuclient[w]a.
      ;;
      ;; gnuclient[w] automatycznie uruchamia Emacsa jesli Emacs nie jest
      ;; aktualnie uruchomiony (patrz gnuserv-win32/unpacked/README.NT,
      ;; punkt 8. Innymi slowy jest tam kawalek kodu specyficzny dla Emacsa,
      ;; pewna sciezka do registry Emacsa jest hardcoded).
      ;; Wiec nigdy nie ma potrzeby robienia czegos jak
      ;; "emacsclient -a emacs" z gnuclient[w]em.
      ;;
      ;; Roznica miedzy gnuclient a gnuclientw : patrz
      ;; gnuserv-win32/unpacked/README.NT pkt 5).

      (add-to-list 'load-path
        (concat kambi-elisp-path "contrib/gnuserv-win32/unpacked/"))
      (require 'gnuserv))
  (require 'server))

(server-start)

(when kam-is-windows
  (setq gnuserv-frame (selected-frame)))

;; compilation ---------------------------------------------------------------

(setq compilation-scroll-output t)
(add-hook 'compilation-mode-hook
  (lambda () (setq truncate-lines nil)) t)

;; Prolog --------------------------------------------------------------------

;; On .pl extension run prolog mode, not perl (later commented out, back to Perl:).
;; (add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))
;; Color Prolog source.
(add-hook 'prolog-mode-hook 'font-lock-mode)

;; Perl ----------------------------------------------------------------------

(add-hook 'perl-mode-hook
  (lambda ()
    (set-local-compile-command (concat "perl " (buffer-file-name)))
  ) t)

;; ---------------------------------------------------------------------------

(add-to-list 'auto-mode-alist '("\\.mrconfig\\'" . conf-mode))

;; load shell-script-mode for VRML/classic X3D and PO.
;; Works good enough: syntax of strings (double quotes, backslash)
;; and comments (hash) is the same, so you get helpful syntax coloring.
(add-to-list 'auto-mode-alist '("\\.wrl\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.x3dv\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.po[tx]?\\'\\|\\.po\\." . shell-script-mode))

;; objective C
(add-to-list 'auto-mode-alist '("\\.m$" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

;; automatycznie dekompresuj / kompresuj edytowane pliki
(auto-compression-mode 1)

;; save recent files
(recentf-mode 1)
;; remove from the "recent files" list the non-existing files
(recentf-cleanup)

;; wylacz pinging w ffap (bo pinging moze zawiesic na kilkanascie sekund Emacsa)
(setq ffap-machine-p-known 'accept)

;; backspace key handling (normal-erase-is-backspace-mode is not available
;; (and not needed) under xemacs)
(when (and window-system (fboundp 'normal-erase-is-backspace-mode))
  (normal-erase-is-backspace-mode 1)
)

;; zeby pisanie "(" na poczatku linii (w kolumnie nr 0) w srodku stringa nie
;; rozwalalo font-lock (patrz PROBLEMS, C-h P, po wyjasnienie)
(defun font-lock-mode-make-good ()
  ;; we initialize here font-lock-mode to make sure that we adjust
  ;; font-lock-beginning-of-syntax-function AFTER initializing font-lock
  ;; (not BEFORE, which would be useless)
  (font-lock-mode)
  (setq font-lock-beginning-of-syntax-function nil))
(add-hook 'emacs-lisp-mode-hook 'font-lock-mode-make-good t)
(add-hook 'c-mode-common-hook 'font-lock-mode-make-good t)
(add-hook 'makefile-mode-hook 'font-lock-mode-make-good t)
(add-hook 'texinfo-mode-hook 'font-lock-mode-make-good t)

;; adjust completion-ignored-extensions
(setq completion-ignored-extensions
  (delete ".log" completion-ignored-extensions))
(setq completion-ignored-extensions
  (add-to-list-new-items completion-ignored-extensions
    '(".dcu"               ; created by Delphi
      ".obj"               ; created by C++Builder, TurboC
      ".tpu"               ; created by TurboPascal
      ".ppu"               ; created by fpc (1.0.10 on linux and 1.9.x)
      ".ow" ".ppw"         ; created by fpc 1.0.10 on windows
      ".~pas"              ; created by Delphi/win32
      ".exe" ".dll" ".so"  ; executable files
      ".cmo" ".cmi" ".cmx" ; ocamlc/opt
      ".blend1"            ; blender
      ".bak"               ; various editors (e.g. Lazarus by default)
      ; Do not add lpi here, as then helm find files ignores it too,
      ; and it's difficult to open such files, and sometimes I need to...
      ;; ".lpi"
      ".compiled" ".lps" ; Lazarus
     )
))

;; info viewing under Emacs
;; We (require 'info) to get info-initialize defined as function and
;; Info-directory-list defined as variable (and initialized to nil).
;; Then we want to initialize Info-directory-list so that it gets at least
;; info files installed with Emacs (it's obvious that we have to add
;; cygwin and other info files manually)
(require 'info)
(info-initialize)
;; (setq Info-directory-list (add-to-list-new-items Info-directory-list
;;   '("/example/path/" )))

;; Ustaw dla niektorych modes zeby nie uzywal znakow Tab.
;; Mimo ze dla wielu modes to ustawiam to nie chce ustawic tego defaultowo przez
;;   (setq-default indent-tabs-mode t)
;; bo nigdy nie wiadomo jakich modes przyjdzie mi uzywac. (tzn. jesli ustawie
;; defaultowo indent-tabs-mode na t i zapomne  dodac do jakiegos mode
;; "ustaw mu indent-tabs-mode na nil" to moze byc katastrofa;; a jesli
;; robie tak jak teraz to co najwyzej w jakims mode bedzie zapisywal z tabami,
;; co katastrofa nie jest).
(defun set-buffer-space-or-tabs ()
  (if (and (buffer-file-name)
        (or
          ;; LH JS and PHP and .shader files
          (and (string-is-prefix "/srv/webroot/huntdev-" (buffer-file-name))
               (string-is-suffix ".js" (buffer-file-name)))
          (and (string-is-prefix "/srv/webroot/huntdev-" (buffer-file-name))
               (string-is-suffix ".php" (buffer-file-name)))
          (string-is-suffix ".shader" (buffer-file-name))
          (string-is-suffix ".cginc" (buffer-file-name))
        )
      )
      (progn
        (message "USE TABS (detected as LH file with tabs)")
        (setq indent-tabs-mode t)
      )
    (progn
      ;; (message "DO NOT USE TABS")
      (setq indent-tabs-mode nil)
    )
  )
)

(define-derived-mode kambi-no-tab-mode fundamental-mode
  "K-No-Tabs"
  "Fundamental mode with tabs auto-converted to spaces."
  (set-buffer-space-or-tabs))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . kambi-no-tab-mode))
(add-to-list 'auto-mode-alist '("\\.lock\\'" . kambi-no-tab-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . kambi-no-tab-mode))

(add-hook 'kambi-pascal-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'kambi-php-mode 'set-buffer-space-or-tabs t)
(add-hook 'kambi-c-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'kambi-c++-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'java-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'tuareg-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'emacs-lisp-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'sh-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'html-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'octave-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'sql-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'latex-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'text-mode-hook 'set-buffer-space-or-tabs t)
;; (add-hook 'ada-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'sml-mode-hook 'set-buffer-space-or-tabs t)
;; (add-hook 'nxml-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'scheme-mode-hook 'set-buffer-space-or-tabs t)
;; (add-hook 'python-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'js-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'css-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'c-mode-common-hook 'set-buffer-space-or-tabs t)
(add-hook 'groovy-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'lua-mode-hook 'set-buffer-space-or-tabs t)
(add-hook 'csharp-mode-hook 'set-buffer-space-or-tabs t)

;; Usually I don't want tabs, but sometimes I have to work on php projects
;; where tabs must be used.
(defconst kam-tabs-in-php nil)
(unless kam-tabs-in-php
  (add-hook 'c-mode-hook 'set-buffer-space-or-tabs t)
  (add-hook 'php-mode-user-hook 'set-buffer-space-or-tabs t))

;; disable todoo mode
(setq auto-mode-alist
  (delete '("TODO$" . todoo-mode) auto-mode-alist))

;; disable change-log-mode
(setq auto-mode-alist
  (delete '("changelog\\'" . change-log-mode)
  (delete '("ChangeLog\\'" . change-log-mode)
    auto-mode-alist)))

;; disable po-mode
(setq auto-mode-alist
  (delete '("\\.po[tx]?\\'\\|\\.po\\." . po-mode) auto-mode-alist))

;; text-mode
(setq auto-mode-alist (add-to-list-new-items auto-mode-alist
  '(("\\TODO\\'" . text-mode)
    ("\\DONE\\'" . text-mode)
    ("ChangeLog\\'" . text-mode)
   )
))

;; js-mode
(add-to-list 'auto-mode-alist '("\\.ts\\'" . js-mode))

;; mouse wheel
(when (functionp 'mouse-wheel-mode)
  (mouse-wheel-mode t)
  ;; mouse-wheel-follow-mouse non-nil oznacza ze mouse-wheel przewijaja ta
  ;; frame nad ktora jest aktualnie pointer myszy (zamiast przewijac zawsze
  ;; frame w ktorej jest kursor)
  ;; TODO: check is it needed under Win
  (setq mouse-wheel-follow-mouse t)
)

;; xrdb mode
(defun xrdb-load-buffer ()
  "Save current buffer and then run 'xrdb < buffer-file-name'"
  (interactive)
  (save-buffer)
  (shell-command (concat "xrdb < " (buffer-file-name)))
)
(add-hook 'xrdb-mode-hook
  (lambda () (local-set-key (kbd "C-c C-l") 'xrdb-load-buffer) )
  t)

;; occur mode
(defun occur-mode-show-occurrence ()
  "Like occur-mode-goto-occurence, but does not change the current buffer,
i.e. point remains in the occur buffer."
  ;; TODO: to nie dziala tak jak trzeba, try i zobacz
  (interactive)
  (let ((last-occur-buffer (current-buffer)))
    (occur-mode-goto-occurrence)
    (switch-to-buffer last-occur-buffer)
  )
)
(define-key occur-mode-map (kbd "SPC") 'occur-mode-show-occurrence)

;; special-mode keymap (used e.g. by diff-mode, used by git-status) breaks our M-digits,
;; hijacking it again to mean digit-argument. That is because special-mode-map
;; calls suppress-keymap, which is a really shitty way to disable editing a buffer,
;; since it just blindly remaps the keys, even when they would not cause editing.
(add-hook 'special-mode-hook
  (lambda ()
    (local-set-key (kbd "M-0") 'kam-buffer-menu)
    (local-set-key (kbd "M-1") 'delete-other-windows)
    (local-set-key (kbd "M-2") 'split-window-vertically)
    (local-set-key (kbd "M-3") 'split-window-horizontally)
  ) t)

;; diff-mode
(add-hook 'diff-mode-hook
  (lambda ()
    ;; redefine to use my standard shortcut, since diff-mode overrides
    ;; the global definitions for these keys.
    ;; Note that special-mode-hook above is not enough,
    ;; we need to repeat this, since diff-mode doesn't inherit from special-mode,
    ;; it only uses it's keymap as a starting point.
    (local-set-key (kbd "M-o") 'other-window)
    (local-set-key (kbd "M-1") 'delete-other-windows)       ; like C-x 1
    (local-set-key (kbd "M-2") 'split-window-vertically)    ; like C-x 2
    (local-set-key (kbd "M-3") 'split-window-horizontally)  ; like C-x 3
    (local-set-key (kbd "M-0") 'kam-buffer-menu)
  ) t)

;; fill-column
(setq default-fill-column 70)

;; tex -----------------------------------------------------------------------

(setq tex-open-quote ",,") ; set polish quote
;; below is cut&pasted from help for variable tex-dvi-view-command
(setq tex-dvi-view-command
  (if (eq window-system 'x) "xdvi" "dvi2tty * | cat -s"))
(add-hook 'tex-mode-hook
  (lambda ()
    ;; TeX and LaTeX modes redefine C-return key to some operation
    ;; that I don't use, so here I'm resettig C-return binding
    ;; to my global binding.
    (local-set-key (kbd "<C-return>") 'kam-find-file-at-point)) t)
(setq auto-mode-alist (add-to-list-new-items auto-mode-alist
  '(("\\.pretex\\'" . latex-mode)
  )))

;; grep ----------------------------------------------------------------------

(require 'compile)
;; add "-i" to be case insensitive, for both simple grep, and lgrep, and rgrep
(setq grep-command "grep -n -i -e ")
;; call (grep-compute-defaults) to recompute grep-find-command based on
;; new value of grep-command. xemacs does not have grep-compute-defaults,
;; I'm simply hardcoding grep-find-command there.
(if (fboundp 'grep-compute-defaults)
    (progn
      (setq grep-find-command nil)
      (grep-compute-defaults)
    )
  (setq grep-find-command "find . -type f -print0 | xargs -0 -e grep -n -i -e ")
)
(defun kam-no-wrap-lines ()
  (interactive)
  "Disable line wrapping."
  ;; Of course, Emacs must have a weird and obsolete name for "line wrapping":
  ;; "do not truncate lines".
  (setq truncate-lines t)
;;  (redraw-frame (selected-frame))
)
(add-hook 'grep-mode-hook 'kam-no-wrap-lines)
(add-hook 'ag-mode-hook 'kam-no-wrap-lines)

;; ispell --------------------------------------------------------------------

;; Change global ispell default dictionary to "american"
(setq ispell-program-name "aspell")
(when (kam-search-for-program ispell-program-name)
  (ispell-change-dictionary "american" t))

;; only for testing
(defun kambi-simple-send (proc string)
  (message (concat "Input is \"" (prin1-to-string (string-to-list string)) "\" now."))

  ;; This is equivalent to (comint-simple-send proc string)
  (comint-send-string proc string)
  (comint-send-string proc "\n")
)

(when kam-is-unix
  ;; Under Unix yes-no dialog box uses some very stupid toolkit,
  ;; you can't control it with keys. This is very pissing off for me,
  ;; because when you close Emacs using Alt + F4 then Emacs treats
  ;; it as "using a mouse to close Emacs" (because it's a message from
  ;; window manager, noone knows if it was caused by mouse or key)
  ;; and if any processes are active, asks you for confirmation
  ;; *using that stupid dialog box*.
  ;; This means that you have to use mouse to close Emacs, if you
  ;; accidentaly used Alt + F4 to close Emacs
  ;; while e.g. some shell buffer was active.
  (setq use-dialog-box nil))

(auto-image-file-mode 1)

;; chess
(setq chess-images-separate-frame nil)

;; This way kill-ring-save, kill-region, yank watch out for X clipboard.
;; I want this, this is good for me, since using clipboard is easier
;; to comminicate with other programs (no need to use mouse,
;; as (almost) every program assigns Ctrl+X/C/V to operate on X clipboard,
;; while many programs (Emacs being the only exception ?) does not allow
;; me to operate on X primary selection with keyboard.)
;;
;; As pointed out by jwz
;; [http://www.jwz.org/doc/x-cut-and-paste.html]
;; this has one disadvantage : it's easy in Emacs to kill some large amounts
;; of text (e.g. whole buffer). This creates a problem if you use
;; program like xclipboard running, that constantly monitors your
;; clipboard. Such program would immediately "catch" your killing.
;;
;; This is somehow unavoidable since all Emacs commands do not
;; differentiate between "cutting text to delete it *and* make it a primary
;; selection (and clipboad, if x-select-enable-clipboard is non-nil)"
;; and "cutting text just to delete it".
(setq x-select-enable-clipboard t)

(when kam-is-linux
  ;; browse-url adjust for Debian
  (setq browse-url-generic-program "sensible-browser")
  (setq browse-url-browser-function 'browse-url-generic))

;; kanim files
(setq auto-mode-alist (add-to-list-new-items auto-mode-alist
  '(("\\.kanim\\'" . nxml-mode)
  )))

(define-derived-mode kambi-text-eof-mode text-mode
  "K-Text-EOF"
  "A trivial extension of text mode to automatically jump to
the end of the file when opening."
  (kam-end-of-buf))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; various packages ----------------------------------------------------------

(when (require 'browse-kill-ring nil 'noerror)
  (global-set-key (kbd "C-x k") 'browse-kill-ring))

;; I never got used to non-cua-mode shortcuts for operating clipboard.
;; Mostly because they are inconsistent with all other software.
;; Activate cua-mode wherever possible.
(when (fboundp 'cua-mode) ;; cua-mode is only in emacs >= 22
  (cua-mode 1)
  (add-hook 'comint-mode-hook
    (lambda ()
      ;; below is needed to have delete work as cua-delete-region in shell-mode,
      ;; otherwise comint overrides delete key and it doesn't work like usual
      ;; with cua.
      ;;
      ;; I could do it only once, globally (instead of from each hook),
      ;; but at the beginning comint-mode-map symbol is not loaded.
      (define-key comint-mode-map "\C-d" nil))
    t)
)

;; Interpret shell commands input/output as coded in utf-8
;; Bound to comint-exec-hook, not comint-mode-hook, because when
;; comint-mode-hook runs, process is not yet available.
(add-hook 'comint-exec-hook
  (lambda ()
    (set-buffer-process-coding-system 'utf-8 'utf-8))
  t)

(when (fboundp 'savehist-mode) ;; savehist-mode is only in emacs >= 22
  (savehist-mode 1))

;; key bindings helper funcs ----------------------------------------------------

(defun switch-buf ()
  "Zmien bufor na inny."
  (interactive)
  (switch-to-buffer (other-buffer)) )

(defun kam-insert-current-file-name ()
  "Insert into current buffer `kam-current-file-name'."
  (interactive)
  (insert (kam-current-file-name))
)

(defun kam-insert-current-file-name-nondirectory ()
  "Just like `kam-insert-current-file-name' but without directory
part of file-name."
  (interactive)
  (insert (file-name-nondirectory (kam-current-file-name)))
)

(defun ins-eval-expression ()
  "Wykonaj Lisp expression taken from minibuffer and insert result
into current buffer at the cursor position."
  (interactive)
  (insert (prin1-to-string (eval-minibuffer "LispExpr: ")))
)

(defun lambda-jump-to-register (reg-number)
  "Return (lambda ...) value that contains a parameterless function
that does (jump-to-register reg-number)."
  `(lambda ()
     (interactive)
     (jump-to-register ,reg-number)
   )
)

(defun lambda-point-to-register (reg-number)
  "Return (lambda ...) value that contains a parameterless function
that does (point-to-register reg-number) (and some message ...)."
  `(lambda ()
     (interactive)
     (point-to-register ,reg-number)
     (message "Register %d set." ,reg-number)
   )
)

(defun after-find-file-i ()
  "Simply calls `after-find-file'. The only difference is that *this*
function can be called interactively, I (Kambi) find it useful because it
parses local variables written in buffer."
  (interactive)
  (after-find-file))

(defun invert-truncate-lines ()
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; Jak rozumiem, truncate-lines jest zwykla zmienna i dlatego po jego zmianie
  ;; musimy "recznie" powiedziec Emacsowi ze aktualne frame musi byc przemalowane.
  (redraw-frame (selected-frame))
)

(defun kam-buffer-menu ()
  "Call my preferred buffer-menu'-like function."
  (interactive)
  (if (require 'counsel nil 'noerror)
      (ivy-switch-buffer)
    (if (require 'helm-config nil 'noerror)
        (helm-mini)
      (if kam-use-ibuffer
          ;; Kambi really likes to call ibuffer with update=t
          (ibuffer nil nil nil t)
        (buffer-menu))))
)

;; key bindings ------------------------------------------------------------
;;
;; Jak widac preferuje podawanie klawiszy przy pomocy funkcji (kbd "...").
;; W ten sposob jest czytelnie zapisane jaki to jest klawisz
;;   i wszystko dziala (co nie mialo miejsca gdy uzywalem konstrukcji '[...]
;;   gdzie czasem trzeba bylo pisac jakies cyferki ktore trzeba
;;   bylo znajdywac przegladajac "C-x <ESC> <ESC>").
;; Jest tez latwo zobaczyc jaki string odpowiada jakiemus klawiszowi
;;   pytajac o ten klawisz "C-h c".
;;
;; Ze wzgledu na to ze Emacs niezbyt dobrze sobie radzi z rozpoznawaniem
;; roznych klawiszy (np. zupelnie nie umie rozroznic Ctrl+C, Shift+Ctrl+C,
;; Shift+Ctrl+Alt+C, itd., w kazdym z tych przypadkow powie (prawdopodobnie)
;; "Ctrl+C") pod X-ami, a jeszcze gorzej (i inaczej) pod konsola to niektore
;; key-bindings musza byc zrobione inaczej pod X-ami (i NTEmacsem pod Windows)
;; niz pod konsola Linuxowa.

(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

(global-set-key (kbd "<C-home>") 'kam-beg-of-buf)
(global-set-key (kbd "<C-end>") 'kam-end-of-buf)
(global-set-key (kbd "<C-kp-home>") 'kam-beg-of-buf)
(global-set-key (kbd "<C-kp-end>") 'kam-end-of-buf)
;; easier to press than C-home/end on laptop keyboards that put home/end them under function
(global-set-key (kbd "M-<up>") 'kam-beg-of-buf)
(global-set-key (kbd "M-<down>") 'kam-end-of-buf)

(global-set-key (kbd "C-w") 'kill-this-buffer)
;; The meaning is "up directory".
;; For simple file buffers, it's sensible to just open dired here.
;; Some modes, like dired, will override this to go "up dir".
(global-set-key (kbd "C-l") 'dired-jump)

(global-set-key (kbd "C-<f2>") 'kill-compilation)
(global-set-key (kbd "<f8>") 'kam-run-here)
(global-set-key (kbd "<f9>") 'kam-compile-immediate)
(global-set-key (kbd "<C-f9>") 'kam-compile)
(global-set-key (kbd "<S-f9>") 'recompile)
;; (global-set-key (kbd "<S-f9>") 'kam-clean-here)
;; (global-set-key (kbd "<S-f10>") 'kam-clean-cge)

(global-set-key (kbd "<C-tab>") 'switch-buf)
;; (global-set-key (kbd "<C-SPC>") 'dabbrev-completion) ; default is uncomfortable C-M-\

(global-set-key (kbd "M-0") 'kam-buffer-menu)
(global-set-key (kbd "M-o") 'other-window)               ; jak C-x o
(global-set-key (kbd "M-1") 'delete-other-windows)       ; jak C-x 1
(global-set-key (kbd "M-2") 'split-window-vertically)    ; jak C-x 2
(global-set-key (kbd "M-3") 'split-window-horizontally)  ; jak C-x 3

(global-set-key (kbd "<C-return>") 'kam-find-file-at-point)
(global-set-key (kbd  "C-x <C-return>") 'kam-insert-current-file-name-nondirectory)
(global-set-key (kbd "C-f") 'nonincremental-re-search-forward)
(global-set-key (kbd "<f3>") 'kam-nonincremental-repeat-search-forward)
(global-set-key (kbd "<S-f3>") 'kam-nonincremental-repeat-search-backward)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-/") 'comment-line) ;; similar to "//", to remember

;; zapamietaj jako "jak f5 (= reload w niektorych programach)"
(global-set-key (kbd "<C-f5>") 'kam-refresh-colors-in-buffer)

(global-set-key (kbd "C-t") 'invert-truncate-lines)

(global-set-key (kbd "M-a") 'after-find-file-i)
(global-set-key (kbd "M-c") 'set-buffer-file-coding-system)
;;(global-set-key (michalis-prefix-2-kbd "t") 'kam-open-terminal-here)
(global-set-key (kbd "M-t") 'delete-trailing-whitespace)
(global-set-key (kbd "M-<f5>") 'revert-buffer)
 ;; (global-set-key (michalis-prefix-kbd "u") 'rename-uniquely)

(global-set-key (kbd "M-l") 'goto-line)

;; shell that can be easily killed
(global-set-key (kbd "M-h") 'kam-shell-killable)
;; shell that cannot be easily killed
;; (global-set-key (michalis-prefix-2-kbd "s") 'shell)

(define-key cua--cua-keys-keymap (kbd "M-v") nil)
(global-set-key (kbd "M-v") 'kam-version-control)
;; (global-set-key (kbd "M-d") 'dired-jump) ;; just like C-l in practice
(global-set-key (kbd "M-e") 'kam-open-dir-external)
;; (global-set-key (kbd "<f12> o") 'sort-lines)

;; Also cua-rectangle-mark-key is set to [(control f12)]

(when window-system

  ;; komendy jump-to-register1-9 i point-to-register1-9 tworza
  ;; mechanizm podobny do zakladek w edytorach Borlanda (Delphi itp.).
  ;; (tak naprawde, jest to duzo silniejszy mechanizm od zakladek Delphi !
  ;; Rejestry Emacsa sa globalne, a proba skoku do rejestru w nieotwartym pliku
  ;; moze nawet spowodowac otwarcie tego pliku - to mi sie bardzo podoba !)
  ;; Tak jak w Delphi sa one przypisane do klawiszy
  ;; Ctrl+1..9 i Shift+Ctrl+1..9.

  (global-set-key (kbd "C-1") (lambda-jump-to-register 1))
  (global-set-key (kbd "C-2") (lambda-jump-to-register 2))
  (global-set-key (kbd "C-3") (lambda-jump-to-register 3))
  (global-set-key (kbd "C-4") (lambda-jump-to-register 4))
  (global-set-key (kbd "C-5") (lambda-jump-to-register 5))
  (global-set-key (kbd "C-6") (lambda-jump-to-register 6))
  (global-set-key (kbd "C-7") (lambda-jump-to-register 7))
  (global-set-key (kbd "C-8") (lambda-jump-to-register 8))
  (global-set-key (kbd "C-9") (lambda-jump-to-register 9))

  (global-set-key (kbd "C-!") (lambda-point-to-register 1)) ; C-Shift-1 ...
  (global-set-key (kbd "C-@") (lambda-point-to-register 2))
  (global-set-key (kbd "C-#") (lambda-point-to-register 3))
  (global-set-key (kbd "C-$") (lambda-point-to-register 4))
  (global-set-key (kbd "C-%") (lambda-point-to-register 5))
  (global-set-key (kbd "C-^") (lambda-point-to-register 6))
  (global-set-key (kbd "C-&") (lambda-point-to-register 7))
  (global-set-key (kbd "C-*") (lambda-point-to-register 8))
  (global-set-key (kbd "C-(") (lambda-point-to-register 9)) ; ... C-Shift-9
)

(global-set-key (kbd "M-r") 'query-replace)
(global-set-key (kbd "C-M-r") 'query-replace-regexp)

;; (global-set-key (kbd "M-e") 'ins-eval-expression) ;; unused
(global-set-key (kbd "C--") 'kam-insert-dashes)

(global-set-key (kbd "M-i") 'kam-indent-block-space)
(global-set-key (kbd "M-u") 'kam-unindent-block-space)

(global-set-key (kbd "C-x C-r") 'recentf-open-files)

(global-set-key (kbd "RET") 'insert-newline-indented-as-prev)
;;(global-set-key (kbd "RET") 'newline)

(global-set-key (kbd "C-M-c") 'comment-region)
(global-set-key (kbd "C-M-u") 'uncomment-region)

;; default in to open dir in brief mode, which I find rather useless
(global-set-key (kbd "C-x C-d") 'dired)

;; http://stackoverflow.com/questions/648817/how-to-bind-esc-to-keyboard-escape-quit-in-emacs
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
;; also this, otherwise "C-x <escape>" is just a prefix to other unused commands
(global-set-key (kbd "C-x <escape>") 'keyboard-escape-quit)

;; colors --------------------------------------------------------------------

;; zeby kolorowal skladnie zawsze kiedy moze
;;
;; Note: sthg like this:
;;   (add-hook 'php-mode-user-hook 'turn-on-font-lock)
;; would turn on font-lock only for some specific mode, php-mode in this case
;;
(setq font-lock-maximum-decoration t)
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode 1)
)

;; (set-foreground/background-color not avail under xemacs;
;; TODO: but is it really useful ? is it really useful for me under emacs ?)
(when (fboundp 'set-foreground-color) (set-foreground-color "white"))
(when (fboundp 'set-background-color) (set-background-color "black"))

(defun set-face-colors (face fg-color bg-color)
  "set-face-foreground to FG-COLOR (or leave as is if FG-COLOR is nil) and
set-face-background to BG-COLOR (or leave as is if BG-COLOR is nil)."
  ;; some time ago I thought that maybe I will use here
  ;; (if (facep face) ... do set-face-... ), that would be nice since
  ;; XEmacs and GNUEmacs have some distinct faces. But I found out
  ;; that facep in XEmacs does not work correctly (probably I'm using
  ;; it wrong way ?), e.g. (facep 'default) returns nil
  ;; (but you can see M-x list-faces-display that such face exists)
  (when fg-color (set-face-foreground face fg-color))
  (when bg-color (set-face-background face bg-color))
)

;; Trying to keep this working under xemacs, where faces names
;; are different, is just too much work for me.
(unless (featurep' xemacs)
  ;; remember : use M-x list-faces-display to see all available faces
  ;;                    list-colors-display to see all available color names
  (set-face-colors 'bold-italic "white" "black")
  (set-face-colors 'default "white" "black")
  (set-face-colors 'italic "white" "black")
  (set-face-colors 'underline "white" "black")
  (set-face-colors 'font-lock-builtin-face "limegreen" "black")
  (set-face-colors 'region "black" "khaki")
  (set-face-colors 'font-lock-comment-face "gray" "black")
  (set-face-colors 'font-lock-constant-face "plum" "black")
  (set-face-colors 'font-lock-function-name-face "white" "black")
  (set-face-colors 'font-lock-keyword-face "cyan" "black")
  (set-face-colors 'font-lock-string-face "yellow" "black")
  (set-face-colors 'font-lock-type-face "cyan" "black")
  (set-face-colors 'font-lock-variable-name-face "white" "black")
  (set-face-colors 'font-lock-warning-face "red" "black")
  ;; Uzywane w tuareg mode aby pokolorowac komentarze zaczynajace sie
  ;; od (**, czyli dla ocamldoc
  (set-face-colors 'font-lock-doc-face "medium aquamarine" "black")
  (if (display-graphic-p)
      (set-face-colors 'highlight nil "gray10")
    (set-face-colors 'highlight nil "cyan")
  )
  (set-face-colors 'mode-line "black" "Aquamarine")
  (set-face-colors 'secondary-selection "black" "white")
  (set-face-colors 'cursor nil "white")
)

;; auto-complete -------------------------------------------------------------

(when (require 'auto-complete-config nil 'noerror)
  (ac-config-default)

  (global-auto-complete-mode t)
  (add-to-list 'ac-modes 'kambi-pascal-mode)
  (add-to-list 'ac-modes 'kambi-c-mode)
  (add-to-list 'ac-modes 'kambi-c++-mode)
  (add-to-list 'ac-modes 'kambi-java-mode)
  (add-to-list 'ac-modes 'kambi-objc-mode)
  (add-to-list 'ac-modes 'js-mode)
  (add-to-list 'ac-modes 'kambi-php-mode)
  (add-to-list 'ac-modes 'kambi-css-mode)
  (add-to-list 'ac-modes 'kambi-csharp-mode)
  (add-to-list 'ac-modes 'html-mode)
  (add-to-list 'ac-modes 'nxml-mode)
  ;; complete when writing commit log, very useful
  ;; (add-to-list 'ac-modes 'git-commit-mode) ;; TODO: doesn't work

  ;; By default you have to press 3 escapes (or C-g) to make keyboard-escape-quit
  ;; to make completion menu disappear. Not comfortable enough for me,
  ;; so I redefine one escape stroke (and I do no have to set it to
  ;; keyboard-escape-quit, ac-stop works as well).
  (define-key ac-completing-map (kbd "<escape>") 'ac-stop)

  ;; For "Disapear automatically when you complete a candidate." feature.
  ;; Hmm, doesn't seem to work. Even tried with (setq ac-dwim-enable t).
  ;; Later: hmm, works, but only when kam-ac-source-pascal is not used.
  (setq ac-dwim t)
)

;; gud -----------------------------------------------------------------------

;; Prev/next prompt, like in shell-mode (actually both gud and shell-mode
;; are derived from comint), otherwise gud changes these keys to other commands.
(add-hook 'gdb-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-p") 'comint-previous-prompt)
    (local-set-key (kbd "C-c C-n") 'comint-next-prompt)
  )
  t)

;; tramp ---------------------------------------------------------------------

;; Turn off backup files for tramp (ssh, su etc.) files.
;; Useful, as on some servers dir is not writeable (like /var/www/wpi/) for security.
;; See http://www.gnu.org/software/emacs/manual/html_node/tramp/Auto_002dsave-and-Backup.html

(setq backup-enable-predicate
  (lambda (name)
    (and (normal-backup-enable-predicate name)
         (not
          (let ((method (file-remote-p name 'method)))
            (when (stringp method)
              (member method '("su" "sudo" "scp"))))))))

(defconst kam-inc-extension-default-php nil
  "Should the .inc extension by default indicate PHP (non-nil) or Pascal (nil).")

;; org -----------------------------------------------------------------------

(add-hook 'org-mode-hook
  (lambda ()
    ;; org-mode overrides some keys, I prefer to keep my preferences
    (local-set-key (kbd "<C-tab>") 'switch-buf)
    (local-set-key (kbd "<C-return>") 'kam-find-file-at-point)
    (local-set-key (kbd "RET") 'insert-newline-indented-as-prev)
    (local-set-key (kbd "C-e") 'move-end-of-line)
    (local-set-key (kbd "<end>") 'move-end-of-line)
  )
  t)

(add-to-list 'auto-mode-alist '("\\.org.txt\\'" . org-mode))

;; ido -----------------------------------------------------------------------

;; disabled now, because I discovered helm:)

;; ;; from https://www.emacswiki.org/emacs/InteractivelyDoThings
;; ;; also see https://www.masteringemacs.org/article/introduction-to-ido-mode
;; (require 'ido)
;; (setq ido-enable-flex-matching t)
;; ;; (setq ido-everywhere t)
;; (ido-mode t)
;; ;; (setq ido-use-filename-at-point 'guess)
;; ;; (setq ido-ignore-extensions t)
;; ;; (setq ido-create-new-buffer 'always)

;; ;; following https://github.com/lewang/flx
;; (when (require 'flx-ido nil 'noerror)
;;   (flx-ido-mode 1)
;;   (setq flx-ido-use-faces nil))

;; asciidoc ------------------------------------------------------------------

(when (require 'adoc-mode nil 'noerror)
  (add-to-list 'auto-mode-alist '("\\.adoc\\'" . adoc-mode)))

;; bell ----------------------------------------------------------------------

;; See https://www.emacswiki.org/emacs/AlarmBell ,
;; I find both sound and visual bell annoying
;;(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; colorize compilation ------------------------------------------------------

;; See https://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; crux ----------------------------------------------------------------------

;; (unless (package-installed-p 'crux)
;;   (package-refresh-contents)
;;   (package-install 'crux))

(when (require 'crux nil 'noerror)
  ;; (global-set-key (kbd "C-k") 'crux-smart-kill-line)
  (global-set-key (kbd "C-c D") 'crux-delete-file-and-buffer)
  (global-set-key (kbd "C-c r") 'crux-rename-file-and-buffer)

  ;; crux-recentf-find-file is a little better than counsel-recentf,
  ;; (shortens ~) but also a little worse (not correct sorting).
  ;; (global-set-key (kbd "C-x C-r") 'crux-recentf-find-file)

  ;; for unknown reason, crux-reopen-as-root is not interactive
  (defun kam-crux-reopen-as-root ()
    "Like crux-reopen-as-root, but interactive."
    (interactive)
    (crux-reopen-as-root)
  )
  ;;(global-set-key (kbd "<f12> C-r") 'kam-crux-reopen-as-root) ;; unused, long time ago forgotten

  ;; This makes all non-writeable files to be opened using sudo,
  ;; which is troublesome if you don't want to provide sudo password yet,
  ;; and want to only open read-only.
  ;; (crux-reopen-as-root-mode)
)

;; smartscan -----------------------------------------------------------------

(when (require 'smartscan nil 'noerror)
  ;; (global-smartscan-mode 1)

  ;; Do not use global-smartscan-mode, which rebinds M-p etc. even in shell,
  ;; and I cannot seem to override it with my own (it's probably a matter of
  ;; hook order).
  ;;
  ;; Instead define my own shortcuts, with Ctrl, not with Alt,
  ;; as M-p and M-n have already comfortable meaning for me in shell mode.
  (global-set-key (kbd "C-p") 'smartscan-symbol-go-backward)
  (global-set-key (kbd "C-n") 'smartscan-symbol-go-forward)
  ;; iedit is better at this
  ;; (global-set-key (kbd "C-'") 'smartscan-symbol-replace)
)

;; undohist ------------------------------------------------------------------

(when (require 'undohist nil 'noerror)
  (undohist-initialize)
)

;; scratch -------------------------------------------------------------------

;; put fortune in scratch buffer (disabled -- too distracting)
;; (when (kam-search-for-program "fortune")
;;   ;; From https://www.reddit.com/r/emacs/comments/4agorq/got_bored_of_the_initial_scratch_message_so/
;;   (setq initial-scratch-message
;;         (format
;;          ";; %s\n\n"
;;          (replace-regexp-in-string
;;           "\n" "\n;; " ; comment each line
;;           (replace-regexp-in-string
;;            "\n$" ""    ; remove trailing linebreak
;;            (shell-command-to-string "fortune")))))
;; )

;; anzu ----------------------------------------------------------------------

(when (require 'anzu nil 'noerror)
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  ;; iedit is better at this
  ;; (global-set-key (kbd "C-'") 'anzu-replace-at-cursor-thing)
)

;; git-gutter ----------------------------------------------------------------

(when (require 'git-gutter+ nil 'noerror)
  ;; having it always on is too distracting
  ;; (global-git-gutter+-mode)
  (global-set-key (kbd "C-c g") 'global-git-gutter+-mode)
)

;; hl-line-mode --------------------------------------------------------------

;; on terminals, hl-mode often looks unreadable (background and foreground colors look the same)
(when (display-graphic-p)
  (global-hl-line-mode))

;; wgrep ---------------------------------------------------------------------

;; see https://github.com/mhayashi1120/Emacs-wgrep/
;; http://irreal.org/blog/?p=3638

(if (require 'wgrep-ag+ nil 'noerror)
    (setq wgrep-enable-key "r"))
(if (require 'wgrep nil 'noerror)
    (setq wgrep-enable-key "r"))

;; iedit ---------------------------------------------------------------------

(when (require 'iedit nil 'noerror)
  (global-set-key (kbd "<f2> <f2>") 'iedit-mode)
)

;; ag ------------------------------------------------------------------------

;; use my wrapper on ag, if available
(when (kam-search-for-program "ag-michalis")
  (setq ag-executable "ag-michalis"))

;; compat --------------------------------------------------------------------

(defalias 'set-default-font 'set-frame-font)

;; provides (keep at the end) ------------------------------------------------

(provide 'kambi-various-personal)

;;; kambi-various-personal.el ends here
