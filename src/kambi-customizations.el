(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Man-notify-method (quote pushy))
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
   (quote
    ("--line-number" "--ignore-case" "--nogroup" "--column" "--stats" "--")))
 '(ag-highlight-search t)
 '(bibtex-comment-start "%%")
 '(blink-matching-paren-distance nil)
 '(comint-password-prompt-regexp
   "Kambi put here regexp impossible to match to disable comint-watch-for-password-prompt")
 '(cua-rectangle-mark-key [C-f12])
 '(dabbrev-case-replace nil)
 '(dired-backup-overwrite t)
 '(dired-listing-switches "-al --group-directories-first")
 '(focus-follows-mouse nil)
 '(grep-command "grep -i -n -e ")
 '(grep-find-ignored-files
   (quote
    (".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "*.json" "*.compiled" "*.lps" "*.atlas")))
 '(grep-find-template
   "find . <X> -type f <F> -print0 | xargs -0 -e grep <C> -i -n -e <R>")
 '(grep-template "grep <C> -i -n -e <R> <F>")
 '(helm-ag-insert-at-point (quote word))
 '(helm-ag-use-grep-ignore-list t)
 '(helm-buffer-max-length 30)
 '(helm-buffers-truncate-lines t)
 '(helm-ff-skip-boring-files t)
 '(helm-full-frame nil)
 '(helm-grep-truncate-lines t)
 '(helm-projectile-fuzzy-match nil)
 '(ibuffer-expert t)
 '(inhibit-startup-screen t)
 '(initial-buffer-choice nil)
 '(ispell-highlight-face (quote highlight))
 '(jka-compr-load-suffixes (quote (".gz" ".x3dz")))
 '(js-auto-indent-flag nil)
 '(magit-pull-arguments (quote ("--rebase")))
 '(mmm-submode-decoration-level 0)
 '(org-replace-disputed-keys t)
 '(org-support-shift-select (quote always))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".eunit" ".git" ".hg" ".fslckout" ".bzr" "_darcs" ".tox" ".svn" ".stack-work")))
 '(projectile-globally-ignored-file-suffixes
   (quote
    (".ppu" ".blend" ".blend1" ".blend2" ".pdf" ".png" ".xcf" ".jpg" ".jpeg" "castletexturefont_*.pas" ".ttf" ".otf" ".wav" ".dll" ".atlas" ".spine" ".json" ".dds" ".ai")))
 '(ps-header-lines 1)
 '(ps-left-header (quote (ps-get-buffer-name)))
 '(ps-right-header (quote ("/pagenumberstring load")))
 '(recentf-auto-cleanup (quote never))
 '(recentf-exclude nil)
 '(recentf-max-saved-items 100)
 '(safe-local-variable-values (quote ((encoding . cp852))))
 '(scroll-conservatively 100000)
 '(truncate-lines t)
 '(vc-handled-backends (quote (RCS SCCS)))
 '(w3m-home-page "http://www.google.pl/")
 '(w3m-key-binding (quote info))
 '(warning-suppress-types (quote ((undo discard-info)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(mmm-code-submode-face ((t nil)))
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
