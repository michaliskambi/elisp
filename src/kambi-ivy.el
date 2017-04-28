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

  ;; counsel-yank-pop is not very comfortable IMHO,
  ;; and cannot be used to paste to minibuffer.
  ;; Better use standard browse-kill-ring, it's actually OK for me.
  ;; (global-set-key (kbd "C-x k") 'counsel-yank-pop)
  ;; (global-set-key (kbd "C-x k") 'browse-kill-ring)

  ;; Similar to http://oremacs.com/ example, but use file-name-nondirectory
  (defun kam-kill-new-nondirectory (x)
    (kill-new (file-name-nondirectory
     (if (stringp x)
         x
       (car x)))))
  (defun kam-kill-new (x)
    (kill-new
     (if (stringp x)
         x
       (car x))))

  (ivy-set-actions
   t
   '(("w" kam-kill-new              "copy whole")
     ("k" kam-kill-new-nondirectory "copy basename")))

  ;; Not stable:
  ;; - do not exit ivy
  ;; - C-w for some reason does the same thing as C-k, so (ivy-state-current ivy-last)
  ;;   is not a complete filename
  ;;
  ;; (defun kam-ivy-wrap-kill-new-nondirectory ()
  ;;   (interactive)
  ;;   (kam-kill-new-nondirectory (ivy-state-current ivy-last))
  ;;   (ivy-state-unwind ivy-last)
  ;; )
  ;; (defun kam-ivy-wrap-kill-new ()
  ;;   (interactive)
  ;;   (kam-kill-new (ivy-state-current ivy-last))
  ;;   (ivy-state-unwind ivy-last)
  ;; )

  ;; ;; also enable consistent C-k and C-w shortcuts for above commands
  ;; (define-key ivy-minibuffer-map (kbd "C-k") 'kam-ivy-wrap-kill-new-nondirectory)
  ;; (define-key ivy-minibuffer-map (kbd "C-w") 'kam-ivy-wrap-kill-new)
)

(provide 'kambi-ivy)

;;; kambi-ivy.el ends here
