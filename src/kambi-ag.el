;;;; Configuration for ag (the silver searcher), also related to projectile.
;;

;; include kambi-projectile 1st
(require 'kambi-projectile)

;; outside projectile ---------------------------------------------------

(global-set-key (kbd "C-g") 'ag)

;; inside projectile ----------------------------------------------------

(when (require 'projectile nil 'noerror)
  (when (require 'ag nil 'noerror)

    (when kam-is-windows
      ;; highlighting on Windows always causes weird bug: the first component
      ;; (filename)is eaten. Happens even if you run 'ag' from shell in Emacs.
      ;; See https://github.com/Wilfred/ag.el/issues/101
      ;; https://github.com/Wilfred/ag.el/issues/97
      (setq ag-highlight-search nil))

    (defun kam-projectile-ag (search-term &optional arg default-search-term)
      (interactive
       ;; Note: do not use default-search-term of read-from-minibuffer,
       ;; because it has brain-dead specification.
       ;; Others rant about it too: http://xahlee.info/comp/lisp_read-from-minibuffer_propels_deep_questions.html
       (list (read-from-minibuffer
              (projectile-prepend-project-name (format "Ag %ssearch for (default \"%s\"): " (if current-prefix-arg "regexp " "") (projectile-symbol-or-selection-at-point))))
             current-prefix-arg
             (projectile-symbol-or-selection-at-point)))
      "Like `projectile-ag', but without uncomfortable \"insert thing at point\" behavior.
It only uses \"thing at point\" if you just press enter.

Longer explanation:

projectile-ag by default suggests (projectile-symbol-or-selection-at-point) value.

While this is often useful (often you want to search for this),
it's uncomfortable when you need to search for something else,
because then you have to remember to presss C-a C-k
before you start typing your phrase in the minibuffer.

Other GUIs often preselect the default input box contents, to allow you
to start typing to automatically delete the default contents.

In Emacs, the solution to this is that many commands have a default value,
like \"Search [WordUnderPoint]: \" and if you input nothing (just press enter)
-- then the default \"WordUnderPoint\" is used.

(This is also not a perfect solution, as you cannot modify the default value,
e.g. if you want to search for \"WordUnderPointBlah\", then you have to type
it whole. But it's better than the current I think, because I seldom want to
extend the thing under point, and if I want -- I can eventually copy it before
entering projectile-ag, and paste then. But at least the case when
\"my cursor was over something irrelevant, I want to type what to search\"
is fast.
"
      (if (equal search-term "")
          (projectile-ag default-search-term arg)
        (projectile-ag search-term arg)))

    (defun kam-optional-projectile-ag ()
      (interactive)
      "Search using AG in projectile project, or just in current dir if outside project."
      (if (projectile-project-p)
          (call-interactively 'kam-projectile-ag)
        (call-interactively 'ag)))

    (define-key projectile-mode-map (kbd "M-g") 'kam-optional-projectile-ag))
)

;; ---------------------------------------------------------------------------

(provide 'kambi-ag)

;;; kambi-ag.el ends here
