(add-hook 'shell-mode-hook
  (lambda ()
    ;; "M-x shell" will create new shell every time
    (rename-uniquely)
    ;; shell can be easily killed
    ;; trick from http://stackoverflow.com/questions/2706527/make-emacs-stop-asking-active-processes-exist-kill-them-and-exit-anyway
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
  ) t)

(add-hook 'eshell-mode-hook
  (lambda ()
    ;; domyslnie <down> i <up> sa przywiazane do specjalnych polecen eshella,
    ;; w ten sposob eshell dziala bardziej jak normalny bash (up i down sluza do poruszania
    ;; sie po historii) ale ja wole jak eshell zachowuje sie bardziej jak w Emacsie :
    ;; mam bufor, moge po nim chodzic strzalkami itp.
    (local-set-key (kbd "<down>") 'next-line)
    (local-set-key (kbd "<up>") 'previous-line)
  )
  t)

;; Customize shell mode, so that my alias "mkdir_and_cd" is recognized and
;; understood to change current dir.
;; REMOVED because does not seem to work. Probably because when
;; "mkdir_and_cd xxx" is run, "xxx" directory does not exist yet.
;; (setq shell-cd-regexp "\\(mkdir_and_\\)?cd")

;; Some special things to cooperate with Cygwin under Windows
(when kam-is-windows
  (require 'cygwin-mount)
  (cygwin-mount-activate)

  ;; cygwin's bash ----------------------------------------
  ;; Note: useful interactive function to call is w32-check-shell-configuration.
  ;; TODO: correct this, seems broken with emacs23.
  ;; Look at http://www.khngai.com/emacs/cygwin.php and
  ;; and http://www.cygwin.com/faq/faq-nochunks.html#faq.using.ntemacs

  (defun kambi-shell-mode-setup-hook ()
    ;; based on NTEmacs FAQ
    (setq comint-scroll-show-maximum-output 'this)
    (setq comint-eol-on-send t)

    ;; Some not needed but possible things to put/try here:

    ;; (from NTEmacs FAQ about comint-process-echoes below:
    ;; "reported that this is no longer needed")
    ;; (setq comint-process-echoes t)

    ;; (setq comint-completion-addsuffix t) ; <- not needed, this is the default
    ;; (make-variable-buffer-local 'comint-completion-addsuffix)
    ;; (setq shell-command-switch "-c") ; <- not needed, this is the default

    ;; (setq explicit-bash-args '("-login" "-i"))

    ;; This is useless, because final ^M is included by Emacs
    ;; *after* processing input with comint-input-filter-functions.
    ;; So comint-input-filter-functions cannot fix thiis.
    ;; (setq comint-input-filter-functions '(kam-string-delete-ctrl-m))

    ;; Needed based on [http://www.khngai.com/emacs/cygwin.php],
    ;; as far as I know this is not needed because process output coding system
    ;; (see default-process-coding-system below in this file)
    ;; is already set to DOS.
    ;; (setq comint-output-filter-functions '(shell-strip-ctrl-m))

    ;; Kambi 2009: makes it working with emacs23.
    ;; Possibly needed because my default-process-coding-system gets somehow ignored?
    ;; Following http://osdir.com/ml/emacs.windows/2007-12/msg00004.html
    (set-buffer-process-coding-system 'undecided-unix 'undecided-unix)
  )

  (add-hook 'shell-mode-hook 'kambi-shell-mode-setup-hook)

  ;; According to w32-check-shell-configuration, w32-quote-process-args
  ;; should not be left nil when using shell other than "standard Windows"
  ;; Later emacs23 docs suggest it's not needed, default is Ok.
  ;; (setq w32-quote-process-args ?\")

  (setq explicit-shell-file-name "bash.exe")

  ;; Kambi notes: I fought with getting Cygwin bash to work under NTEmacs
  ;; M-x shell for a while. After some time I realised that I was fooled
  ;; by various comint-output-filter-functions that were processing
  ;; output and hiding from my eyes what text was really passed between
  ;; Emacs and a shell. The solution was to temporarily put
  ;;   (setq comint-output-filter-functions nil)
  ;; inside kambi-shell-mode-setup-hook. Then I saw my problem:
  ;;   /win/elisp/kambi-lib $ testparams.exe
  ;;   Params Count : 1
  ;;   0 : 'c:\bin\testparams.exe'
  ;;   1 : '^M'

  ;; I.e. Emacs is adding some excessive ctrl-m at the end of the line !
  ;;
  ;; Then I fought for a couple of hours reading docs and debugging comint-*
  ;; functions. At the end I figured out what was the problem:
  ;; default-process-coding-system is set to '(iso-latin-2-dos . iso-latin-2-dos)
  ;; by (set-language-environment "Latin-2") higher in this file !
  ;;
  ;; That's why sending something to a process (this is related to
  ;; process-* functions, not comint-*, comint-* just uses process-*)
  ;; replaces all (string 10) to (string 13 10). So that was the place
  ;; where excessive ^M were added.
  ;;
  ;; Solution is of course to correct here 2nd item of
  ;; default-process-coding-system, so that it ends with "-unix", not "-dos".
  ;; Note that
  ;; 1) I have to preserve "iso-latin-2-*" prefix, otherwise polish characters
  ;;    could be screwed up.
  ;; 2) I want to leave output coding (1st item of
  ;;    default-process-coding-system) to "-dos". Otherwise, output
  ;;    of programs that print Windows line-endings under Windows
  ;;    (like my own programs or FPC) will contain those ^M characters.
  ;;    In any case, setting process output coding to dos does not do any harm
  ;;    (because if process outputs unix line-endings, they will be correctly
  ;;    recognized anyway)
  (setq default-process-coding-system '(iso-latin-2-dos . iso-latin-2-unix))

  ;; standard Windows shell ----------------------------------------
  ;; You can use this to use standard Windows shell.
  ;; Note that Emacs will handle things like command-name and parameter
  ;; completion (tab key) and commands history (M-p, M-n) internally,
  ;; so this "standard Windows shell" will not suck *so* much.

  ;; Old version:
  ;; (setq explicit-shell-file-name "cmd.exe")
  ;; (setq shell-command-switch "/c")

  ;; Better to use cmdproxy from NTEmacs:
  ;; (setq explicit-shell-file-name "c:/progs/ntemacs/emacs-21.2/bin/cmdproxy.exe")

  ;; ----------------------------------------

  (setq shell-file-name explicit-shell-file-name)
)

(provide 'kambi-shell)