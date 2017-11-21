;;;; Configuration for ripgrep.
;;
;; See about ripgrep:
;; https://github.com/BurntSushi/ripgrep
;; http://oremacs.com/2017/08/04/ripgrep/
;;
;; If ripgrep fails with "illegal instruction":
;;   https://github.com/BurntSushi/ripgrep/issues/283
;;   https://github.com/BurntSushi/ripgrep/issues/135
;; apt-get install cargo
;; cargo install ripgrep


;; include kambi-projectile 1st
(require 'kambi-projectile)

;; use our own binaries, to make it easy for me under Linux ------------------

;; from https://emacs.stackexchange.com/questions/11052/how-to-determine-operating-system-bits-32-vs-64-bit-in-elisp
(defconst kam-cpu-i386
  (or
    (not (null (string-match "^i386-.*" system-configuration)))
    (not (null (string-match "^i686-.*" system-configuration)))
  )
)
(defconst kam-cpu-x86_64
  (not (null (string-match "^x86_64-.*" system-configuration))))

(when (require 'ripgrep nil 'noerror)
  (unless (file-exists-p ripgrep-executable)
    (when (string-equal system-type "gnu/linux")

      ;; try the one installed by cargo
      (when (file-exists-p (concat kam-home-directory "/.cargo/bin/rg"))
        (setq ripgrep-executable (concat kam-home-directory "/.cargo/bin/rg")))

      ;; if not found, use the one in contrib/
      (unless (file-exists-p ripgrep-executable)
        (if kam-cpu-i386
            (setq ripgrep-executable (concat kambi-elisp-path "contrib/ripgrep-bin/linux-i686/rg"))
          (when kam-cpu-x86_64
            (setq ripgrep-executable (concat kambi-elisp-path "contrib/ripgrep-bin/linux-x86_64/rg"))
          )
        )
      )
    )
    (when (string-equal system-type "windows-nt")
      ;; use the one in contrib/
      (setq ripgrep-executable (concat kambi-elisp-path "contrib/ripgrep-bin/win64/rg.exe"))
    )
  )
)

;; outside projectile ---------------------------------------------------

(defun kam-ripgrep-regexp (search-term &optional arg default-search-term)
  (interactive
   ;; Note: do not use default-search-term of read-from-minibuffer,
   ;; because it has brain-dead specification.
   ;; Others rant about it too: http://xahlee.info/comp/lisp_read-from-minibuffer_propels_deep_questions.html
   (list (read-from-minibuffer
          (format "Ripgrep dir %ssearch (default \"%s\"): " (if current-prefix-arg "regexp " "") (projectile-symbol-or-selection-at-point)))
         current-prefix-arg
         (projectile-symbol-or-selection-at-point)))
  "Like `ripgrep-regexp', but uses \"thing at point\" if you just press enter."
  (if (equal search-term "")
      (ripgrep-regexp default-search-term default-directory)
    (ripgrep-regexp search-term default-directory)))

(global-set-key (kbd "C-g") 'kam-ripgrep-regexp)

;; inside projectile ----------------------------------------------------

(when (require 'projectile nil 'noerror)
  (when (require 'ripgrep nil 'noerror)

    (defun kam-projectile-ripgrep (search-term &optional arg default-search-term)
      (interactive
       ;; Note: do not use default-search-term of read-from-minibuffer,
       ;; because it has brain-dead specification.
       ;; Others rant about it too: http://xahlee.info/comp/lisp_read-from-minibuffer_propels_deep_questions.html
       (list (read-from-minibuffer
              (projectile-prepend-project-name (format "Ripgrep project %ssearch (default \"%s\"): " (if current-prefix-arg "regexp " "") (projectile-symbol-or-selection-at-point))))
             current-prefix-arg
             (projectile-symbol-or-selection-at-point)))
      "Like `projectile-ripgrep', but uses \"thing at point\" if you just press enter."
      (if (equal search-term "")
          (projectile-ripgrep default-search-term)
        (projectile-ripgrep search-term)))

    (defun kam-optional-projectile-ripgrep ()
      (interactive)
      "Search using Ripgrep in projectile project, or just in current dir if outside project."
      (if (projectile-project-p)
          (call-interactively 'kam-projectile-ripgrep)
        (call-interactively 'ripgrep-regexp)))

    (define-key projectile-mode-map (kbd "M-g") 'kam-optional-projectile-ripgrep))
)

;; ---------------------------------------------------------------------------

(provide 'kambi-ripgrep)

;;; kambi-ripgrep.el ends here
