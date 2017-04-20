;;;; Michalis configuration for ivy, counsel (and projectile and ag, when related to ivy).
;;
;; See https://github.com/abo-abo/swiper
;; http://oremacs.com/swiper/
;; https://www.reddit.com/r/emacs/comments/51lqn9/helm_or_ivy/
;; https://oremacs.com/2015/04/16/ivy-mode/

(require 'kambi-utils)

(when (require 'counsel nil 'noerror)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  ;; (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (setq projectile-completion-system 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
)

(provide 'kambi-ivy)

;;; kambi-ivy.el ends here
