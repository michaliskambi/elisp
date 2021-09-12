;; define makefile-fpc-mode to use with Makefile.fpc files
(define-derived-mode makefile-fpc-mode makefile-mode "Makefile.fpc"
  "Kambi mode for editing Makefile.fpc files."
  (set-local-compile-command "fpcmake")
)

(setq auto-mode-alist (add-to-list-new-items auto-mode-alist
  '(("Makefile\\.fpc\\'" . makefile-fpc-mode)
   )
))

;; This allows to open Pascal unit files using ffap from makefile-fpc-mode,
;; e.g. you're visiting 'unitname' and ffap opens 'unitname.pas'.
(add-to-list 'ffap-alist '(makefile-fpc-mode . ffap-kambi-pascal-mode))


(defun compile-make-word-at-point ()
  (interactive)
  (kam-compile-prompt (concat "make " (thing-at-point 'word)))
)

(add-hook 'makefile-mode-hook
  (lambda ()
    ;; (local-set-key (kbd "<C-f10>") 'compile-make-word-at-point) ;; unused, long time ago forgotten
  ) t)

(provide 'kambi-makefile)
