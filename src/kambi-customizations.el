(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method 'pushy)
 '(ac-auto-start 3)
 '(ac-ignore-case t)
 '(ac-trigger-key "TAB")
 '(ada-auto-case nil)
 '(ada-clean-buffer-before-saving nil)
 '(ada-indent 2)
 '(ada-indent-after-return nil)
 '(ada-indent-align-comments nil)
 '(ada-indent-comment-as-code nil)
 '(ag-arguments
   '("--line-number" "--ignore-case" "--nogroup" "--column" "--stats" "--"))
 '(ag-group-matches nil)
 '(ag-highlight-search t)
 '(ag-ignore-list '("*~"))
 '(anzu-cons-mode-line-p t)
 '(bibtex-comment-start "%%")
 '(blink-matching-paren-distance nil)
 '(c-tab-always-indent nil)
 '(comint-password-prompt-regexp
   "Kambi put here regexp impossible to match to disable comint-watch-for-password-prompt")
 '(completion-ignored-extensions
   '(".lps" ".compiled" ".bak" ".blend1" ".cmx" ".cmi" ".cmo" ".dll" ".exe" ".~pas" ".ppw" ".ow" ".ppu" ".tpu" ".dcu" ".o" "~" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ".lof" ".glo" ".idx" ".lot" ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/" ".fmt" ".tfm" ".class" ".fas" ".lib" ".mem" ".x86f" ".sparcf" ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl" ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl" ".sx32fsl" ".wx64fsl" ".wx32fsl" ".fasl" ".ufsl" ".fsl" ".dxl" ".lo" ".la" ".gmo" ".mo" ".toc" ".aux" ".cp" ".fn" ".ky" ".pg" ".tp" ".vr" ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs" ".pyc" ".pyo"))
 '(copyright-names-regexp "Michalis Kamburelis")
 '(copyright-query nil)
 '(copyright-year-ranges t)
 '(counsel-find-file-ignore-regexp "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)")
 '(css-electric-keys nil)
 '(cua-rectangle-mark-key [C-f12])
 '(dabbrev-case-replace nil)
 '(delphi-tab-always-indents nil)
 '(dired-auto-revert-buffer t)
 '(dired-backup-overwrite t)
 '(dired-clean-up-buffers-too nil)
 '(dired-dnd-protocol-alist nil)
 '(dired-du-size-format t)
 '(dired-du-update-headers nil)
 '(dired-hide-details-hide-information-lines t)
 '(dired-hide-details-hide-symlink-targets nil)
 '(dired-listing-switches "-alh --group-directories-first")
 '(dired-omit-files "^\\.?#\\|^\\.\\.$\\|^\\.$\\|^\\.[^.]")
 '(dired-omit-verbose nil)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(electric-indent-mode nil)
 '(focus-follows-mouse nil)
 '(grep-command "grep -i -n -e ")
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.json" "*.compiled" "*.lps" "*.atlas"))
 '(grep-find-template
   "find . <X> -type f <F> -print0 | xargs -0 -e grep <C> -i -n -e <R>")
 '(grep-template "grep <C> -i -n -e <R> <F>")
 '(helm-ag-insert-at-point 'word)
 '(helm-ag-use-grep-ignore-list t)
 '(helm-buffer-max-length 30)
 '(helm-buffers-truncate-lines t)
 '(helm-ff-guess-ffap-filenames t)
 '(helm-ff-newfile-prompt-p nil)
 '(helm-ff-skip-boring-files t)
 '(helm-full-frame nil)
 '(helm-grep-truncate-lines t)
 '(helm-projectile-fuzzy-match nil)
 '(ibuffer-expert t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(ispell-highlight-face 'highlight)
 '(ivy-do-completion-in-region nil)
 '(ivy-magic-tilde nil)
 '(jka-compr-load-suffixes '(".gz" ".x3dz"))
 '(js-auto-indent-flag nil)
 '(log-edit-hook '(log-edit-show-files))
 '(ls-lisp-dirs-first t)
 '(ls-lisp-ignore-case t)
 '(ls-lisp-verbosity nil)
 '(magit-diff-section-arguments '("--no-ext-diff" "-M" "-C"))
 '(magit-pull-arguments nil)
 '(minimap-major-modes
   '(prog-mode delphi-mode Custom-mode kambi-pascal-mode fundamental-mode text-mode comint-mode))
 '(minimap-mode t)
 '(minimap-recenter-type 'relative)
 '(minimap-update-delay 0.1)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location 'right)
 '(mmm-submode-decoration-level 1)
 '(mode-require-final-newline nil)
 '(ns-right-alternate-modifier 'none)
 '(opascal-tab-always-indents nil)
 '(org-cycle-separator-lines 1)
 '(org-link-descriptive nil)
 '(org-replace-disputed-keys t)
 '(org-support-shift-select 'always)
 '(package-selected-packages
   '(sass-mode w3m ethan-wspace json-mode dsvn browse-kill-ring wgrep lua-mode csharp-mode php-mode groovy-mode projectile-ripgrep ripgrep iedit git-gutter+ json-reformat adoc-mode crux esup gh-md hydra ivy paradox projectile dired-collapse dired-du dired-quick-sort smartscan counsel-projectile counsel ivy-hydra auto-complete magit))
 '(paradox-automatically-star t)
 '(paradox-github-token t)
 '(password-store-password-length 33)
 '(projectile-generic-command
   "if which /home/michalis/common/scripts/projectile_ls > /dev/null; then /home/michalis/common/scripts/projectile_ls; else find . -type f -print0; fi")
 '(projectile-globally-ignored-directories
   '(".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work"))
 '(projectile-globally-ignored-file-suffixes
   '(".ppu" ".blend" ".blend1" ".blend2" ".pdf" ".png" ".xcf" ".jpg" ".jpeg" "castletexturefont_*.pas" ".ttf" ".otf" ".wav" ".dll" ".atlas" ".spine" ".json" ".dds" ".ai" "~"))
 '(projectile-ignored-project-function 'file-remote-p)
 '(projectile-ignored-projects
   '("/mnt/linux-32/home/michalis/sources/cat-astrophe-games/dragon_squash/" "/mnt/linux-32/home/michalis/sources/cat-astrophe-games/escape_universe/" "/home/michalis/sources/cat-astrophe-games/escape_universe/" "/home/michalis/sources/cat-astrophe-games/dragon_squash/" "/mnt/elementary-os/home/michalis/sources/cat-astrophe-games/escape_universe/" "/mnt/elementary-os/home/michalis/sources/cat-astrophe-games/dragon_squash/"))
 '(projectile-svn-command
   "if which /home/michalis/common/scripts/projectile_ls > /dev/null; then /home/michalis/common/scripts/projectile_ls; else find . -type f -print0; fi # use generic method, since svn list requires autenticating")
 '(ps-header-lines 1)
 '(ps-left-header '(ps-get-buffer-name))
 '(ps-right-header '("/pagenumberstring load"))
 '(recentf-auto-cleanup 'never)
 '(recentf-exclude nil)
 '(recentf-max-saved-items 100)
 '(ripgrep-arguments
   '("--max-columns" "2000" "--fixed-strings" "--glob=!*~" "--glob=!#*#" "--ignore-case"))
 '(safe-local-variable-values
   '((kam-force-compilation-not-in-project . t)
     (encoding . utf-8)
     (encoding . cp852)))
 '(scroll-conservatively 0)
 '(scroll-margin 3)
 '(svn-status-hide-unmodified t)
 '(svn-status-sort-status-buffer nil)
 '(tab-always-indent nil)
 '(tool-bar-mode nil)
 '(truncate-lines t)
 '(vc-handled-backends '(RCS SCCS))
 '(visible-bell t)
 '(w3m-home-page "https://duckduckgo.com/")
 '(w3m-key-binding 'info)
 '(warning-suppress-types '((undo discard-info))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:inherit font-lock-function-name-face :foreground "medium spring green"))))
 '(dired-flagged ((t (:inherit error :strike-through t))))
 '(dired-header ((t (:inherit font-lock-type-face :foreground "green yellow"))))
 '(dired-ignored ((t (:inherit shadow :foreground "dim gray"))))
 '(dired-marked ((t (:inherit warning :foreground "yellow"))))
 '(dired-symlink ((t (:inherit font-lock-keyword-face :foreground "cyan"))))
 '(ethan-wspace-face ((t (:background "dim gray"))))
 '(ffap ((t (:inherit highlight :background "royal blue"))))
 '(font-lock-keyword-face ((t (:foreground "cyan"))))
 '(iedit-occurrence ((t (:background "gold" :foreground "black"))))
 '(ivy-current-match ((t (:background "green" :foreground "black"))))
 '(markup-complex-replacement-face ((t (:inherit markup-meta-face :foreground "plum1" :box (:line-width 2 :style released-button)))))
 '(markup-list-face ((t (:inherit markup-meta-face :foreground "plum1"))))
 '(markup-meta-face ((t (:stipple nil :foreground "gray30" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 1.0 :width normal :foundry "unknown" :family "Monospace"))))
 '(markup-secondary-text-face ((t (:inherit markup-gen-face :foreground "firebrick"))))
 '(markup-small-face ((t (:inherit markup-gen-face))))
 '(markup-table-cell-face ((t (:inherit markup-table-face))))
 '(markup-table-face ((t (:inherit markup-meta-face :foreground "dodger blue"))))
 '(markup-title-0-face ((t (:inherit markup-gen-face :height 2.0))))
 '(markup-title-1-face ((t (:inherit markup-gen-face :height 1.8))))
 '(markup-title-2-face ((t (:inherit markup-gen-face :height 1.6))))
 '(markup-verbatim-face ((t (:box (:line-width 1 :color "grey25")))))
 '(mmm-default-submode-face ((t (:background "gray10"))))
 '(nxml-comment-content-face ((t (:foreground "gray"))))
 '(t2t-bold-face ((t (:background unspecified :foreground unspecified :weight bold :width extra-expanded))))
 '(t2t-bold-italic-face ((t (:background unspecified :foreground unspecified :underline nil :slant italic :weight extra-bold :width extra-expanded))))
 '(t2t-comments-face ((t (:background "black" :foreground "gray"))))
 '(t2t-config-face ((t (:background "black" :foreground "light green"))))
 '(t2t-images-face ((t (:background unspecified :foreground "yellow"))))
 '(t2t-internet-face ((t (:background unspecified :foreground "LightBlue"))))
 '(t2t-italic-face ((t (:background unspecified :foreground "light yellow" :slant italic))))
 '(t2t-lists-face ((t (:background "black" :foreground "yellow"))))
 '(t2t-underline-face ((t (:background unspecified :foreground unspecified :underline t)))))
