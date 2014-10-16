;;;; Some Kambi adjustments for editing Matlab *.m files.

;; Now I'm using matlab-mode
(add-to-list 'load-path (concat kambi-elisp-path "contrib/matlab/"))

;; Init matlab-mode, code copied from matlab.el comments
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)

;; Note: setting matlab-return-function to something NOT listed as
;; allowed functions for matlab-return-function is a HACK.
;; But it works, and I checked matlab.el sources: it must work.
(setq matlab-return-function 'insert-newline-indented-as-prev)

;; my customizations to Matlab mode
(add-hook 'matlab-mode-hook
  (lambda ()
    (setq indent-line-function nil)
    (setq comment-indent-function nil)
    ;; auto-fill-mode turned off because it makes very unpleasant effects
    ;; when indenting a block of code (e.g. with kam-indent-block-space or
    ;; kam-unindent-block-space)
    (auto-fill-mode 0)
  ) t)

(provide 'kambi-matlab)

;; Old things: ----------------------------------------
;;
;; Adjust octave-mode (no octave-electric-space, octave-electric-semi
;; because even when octave-auto-indent = nil, octave-electric-space 
;; reindents line and I don't want it)
;; (add-hook 'octave-mode-hook
;;   (lambda () 
;;     (local-set-key (kbd ";") 'self-insert-command)
;;     (local-set-key (kbd "SPC") 'self-insert-command)
;;   t))
;; ;; Default octave-comment-char is ?#, it's valid for octave but not for matlab
;; (setq octave-comment-char ?%)

;; Grrr. Hell with octave-mode, it tries to be too damn SMART.
;; I'm going to use modified shell-script-mode for editing Matlab code.
;; (define-derived-mode kambi-matlab-mode shell-script-mode "Kambi-Matlab"
;;   "Kambi Matlab mode based on shell-script-mode."
;;   (setq comment-start "% "))
;; (add-to-list 'auto-mode-alist '("\\.m\\'" . kambi-matlab-mode))
