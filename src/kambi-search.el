;; previous/next-error that wraps --------------------------------------------
;; From http://stackoverflow.com/a/21161239

(defvar kam-modeline-flash-color "#af00d7")

(defun kam-indicate-error-nav-wrapped (direction)
  "Display a message in minibuffer indicating that we wrapped
also flash the mode-line"
  (let ((mode-line-color (face-background 'mode-line)))
    (message "Wrapped %s error" (symbol-name direction))
    (set-face-background 'mode-line kam-modeline-flash-color)
    (sit-for 0.3)
    (set-face-background 'mode-line mode-line-color)))

(defun kam-next-error-wrapped (&optional arg reset)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move forwards (or
backwards, if negative). With just C-u as prefix moves to first error"
  (interactive "P")
  (condition-case nil
      (call-interactively 'next-error)
    ('user-error (progn (next-error 1 t)
                        (kam-indicate-error-nav-wrapped 'next)))))

(defun kam-jump-to-last-error (buffer)
  "Jump to last error in the BUFFER, this assumes that
the error is at last but third line"
  (save-selected-window
    (select-window (get-buffer-window buffer))
    (goto-char (point-max))
    (forward-line -3)
    (call-interactively 'compile-goto-error)))

(defun kam-previous-error-wrapped (&optional arg)
  "Jumps to previous error if at first error jump to last error instead.
Prefix argument ARG says how many error messages to move backwards (or
forwards, if negative)."
  (interactive "P")
  (condition-case nil
      (if (compilation-buffer-p (current-buffer))
          (compilation-previous-error 1)
        (call-interactively 'previous-error))
    ('user-error (progn
                   (let ((error-buffer (next-error-find-buffer)))
                     ;; If the buffer has an associated error buffer use it to
                     ;; to move to last error
                     (if (and (not (eq (current-buffer) error-buffer))
                              (compilation-buffer-p error-buffer))
                         (kam-jump-to-last-error error-buffer)
                       ;; Otherwise move to last point and invoke previous error
                       (goto-char (point-max))
                       (call-interactively 'previous-error))
                     (kam-indicate-error-nav-wrapped 'previous))))))

;; previous/next-error keys --------------------------------------------------

;; (global-set-key (kbd "<S-f4>") 'previous-error)
;; (global-set-key (kbd "<f4>") 'next-error)
(global-set-key (kbd "<S-f4>") 'kam-previous-error-wrapped)
(global-set-key (kbd "<f4>") 'kam-next-error-wrapped)

;; occur ---------------------------------------------------------------------

(defun kam-symbol-or-selection-at-point ()
  "Get the symbol or selected text at point.
Just like `projectile-symbol-or-selection-at-point', to work even
when projectile not available."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    ;; Get the symbol at point and strip its properties.
    ;; Copied from `projectile-symbol-at-point', to work even
    ;; when projectile not available.
    (substring-no-properties (or (thing-at-point 'symbol) ""))
  )
)

(defun kam-occur (search-term &optional default-search-term)
  (interactive
   ;; Note: do not use default-search-term of read-from-minibuffer,
   ;; because it has brain-dead specification.
   ;; Others rant about it too: http://xahlee.info/comp/lisp_read-from-minibuffer_propels_deep_questions.html
   (list (read-from-minibuffer
          (format "Occurences of (default \"%s\"): " (kam-symbol-or-selection-at-point)))
         (kam-symbol-or-selection-at-point)))
  "Like `occur', but the default text to search (if you just press enter)
is the \"thing at point\"."
  (if (equal search-term "")
      (occur default-search-term)
    (occur search-term)))

;; occur is very useful, place under comfortable key
(global-set-key (kbd "C-o") 'kam-occur)
(define-key dired-mode-map (kbd "C-o") 'kam-occur)
;; one of the comint or shell redefines C-o, change it back to kam-occur
(define-key comint-mode-map (kbd "C-o") 'kam-occur)
(define-key shell-mode-map (kbd "C-o") 'kam-occur)

;; provides (keep at the end) ------------------------------------------------

(provide 'kambi-search)

;;; kambi-search.el ends here
