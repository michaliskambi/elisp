;;;; Mac OS X customizations.

(when (string-equal system-type "darwin")

  ;; Extend exec-path ----------------------------------------------------------
  ;;
  ;; useful when Emacs is not running with environment set by ~/.bashrc etc.,
  ;; which is e.g. not easy on Mac OS X.
  (setq exec-path (append
    (list
      "/usr/local/bin/"
      (concat kam-home-directory "/bin")
      (concat kam-home-directory "/common/scripts")
    )
    exec-path)
  )
  ;; for shell processes, set $PATH synchronized
  ;; http://ergoemacs.org/emacs/emacs_env_var_paths.html
  (setenv "PATH" (mapconcat 'identity (butlast exec-path 1) path-separator))
  (message (concat "Path customized to: " (getenv "PATH")))

  (setenv "CASTLE_ENGINE_PATH"
    (concat kam-home-directory "/sources/castle-engine/castle-engine"))

  ;; Use tools from MacPorts ---------------------------------------------------

  (when (file-exists-p "/opt/local/bin/git")
    (setq magit-git-executable "/opt/local/bin/git")
    (setq projectile-git-command "/opt/local/bin/git ls-files -zco --exclude-standard")
    (setq projectile-git-ignored-command "/opt/local/bin/git ls-files -zcoi --exclude-standard")
    (setq projectile-git-submodule-command  "/opt/local/bin/git submodule --quiet foreach 'echo $path' | tr '\\n' '\\0'")
  )

  (when (file-exists-p "/opt/local/bin/find")
    (setq projectile-generic-command "/opt/local/bin/find . -type f -print0")
  )

  (when (file-exists-p "/opt/local/bin/gls")
    ;; Better dired on Mac OS X.
    ;; http://hocuspokus.net/2008/01/a-better-ls-for-mac-os-x/
    ;; https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Dired-ls.html
    (setq ls-lisp-use-insert-directory-program t)      ;; use external ls
    (setq insert-directory-program "/opt/local/bin/gls") ;; ls program name
  )

  ;; Use tools from HomeBrew ---------------------------------------------------

  (when (file-exists-p "/usr/local/bin/svn")
    (setq svn-status-svn-executable "/usr/local/bin/svn")
    (setq projectile-svn-command "/usr/local/bin/svn list -R . | grep -v '$/' | tr '\\n' '\\0'")
  )

  ;; use ag (silver_searcher) from HomeBrew
  (when (file-exists-p "/usr/local/bin/ag")
    (setq ag-executable "/usr/local/bin/ag")
    (setq helm-grep-ag-command
      "/usr/local/bin/ag --line-numbers -S --hidden --color --nogroup %s %s %s")
  )

)

;; ------------------------------------------------------------

(provide 'kambi-macosx)

;; eof ------------------------------------------------------------
