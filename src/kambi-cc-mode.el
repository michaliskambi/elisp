;;;; Wersje kambi-c/c++/java/objc-mode by Kambi.

(require 'cc-mode)
(require 'ffap)
(require 'kambi-utils)

;; define-derived-mode : zrob kambi-c-mode i kambi-c++-mode --------------------

(define-derived-mode 
  kambi-c-mode c-mode "Kambi-C"
  "Wersja c-mode by Kambi - wyciete indentation (w keymap zaden klawisz 
nie jest bound do jakiejs \"electric\" function).

Ten mode nie probuje byc madrzejszy od Ciebie i nie wkurwia Cie zgadujac 
 (zawsze ZLE) jakie chcesz gdziestam zrobic wciecie - wszystko to robisz sam."
)

(define-derived-mode 
  kambi-c++-mode c++-mode "Kambi-C++"
  "Wersja C++-mode by Kambi - wyciete indentation (w keymap zaden klawisz 
nie jest bound do jakiejs \"electric\" function)."
)

(define-derived-mode 
  kambi-java-mode java-mode "Kambi-Java"
  "Wersja Java-mode by Kambi - wyciete indentation (w keymap zaden klawisz 
nie jest bound do jakiejs \"electric\" function)."
)

(define-derived-mode 
  kambi-objc-mode objc-mode "Kambi-ObjC"
  "Wersja ObjC-mode by Kambi - wyciete indentation (w keymap zaden klawisz 
nie jest bound do jakiejs \"electric\" function).
Uwaga : kompletnie untested no nigdy nie pisalem nic w 'objective C' !"
)

;; popraw kambi-c/c++/java/objc-mode-map --------------------------------

(defun remove-indentation-from-cc-mode-map (map)
  (define-keys-to-nil map 
    '("{" "}" ";" "#" ":" "(" ")" "\t" "\C-d" "\177" "," "*" "/"))
  (when (boundp 'delete-key-deletes-forward)
    (define-keys-to-nil map '([delete] [backspace]))
  )
)

(remove-indentation-from-cc-mode-map kambi-c-mode-map)
(remove-indentation-from-cc-mode-map kambi-c++-mode-map)
(define-keys-to-nil kambi-c++-mode-map '("<" ">"))
(remove-indentation-from-cc-mode-map kambi-java-mode-map)
(remove-indentation-from-cc-mode-map kambi-objc-mode-map)

;; -----------------------------------------------------------------------------
;; musimy zrobic "sztuczki" zeby ustawic prawidlowy font-lock mode, 
;; takie same sztuczki uzywane sa w font-lock.el przy inicjalizacji
;; font-lock-defaults. Gdyby c/c++/java/objc-mode byly napisane
;; porzadnie to font-lock bylby zalatwiany w funkcjach inicjujacych ten mode
;; i wszystko byloby OK !
;;
;; Ponizszy kod wycialem i przerobilem z inicjalizacji 
;; font-lock-defaults w font-lock.el

(let 
  (
    (c-mode-defaults
     '((c-font-lock-keywords c-font-lock-keywords-1
        c-font-lock-keywords-2 c-font-lock-keywords-3)
       nil nil ((?_ . "w")) beginning-of-defun
       (font-lock-mark-block-function . mark-defun)))
    (c++-mode-defaults
     '((c++-font-lock-keywords c++-font-lock-keywords-1
        c++-font-lock-keywords-2 c++-font-lock-keywords-3)
       nil nil ((?_ . "w")) beginning-of-defun
    (font-lock-mark-block-function . mark-defun)))
      (objc-mode-defaults
      '((objc-font-lock-keywords objc-font-lock-keywords-1
         objc-font-lock-keywords-2 objc-font-lock-keywords-3)
       nil nil ((?_ . "w") (?$ . "w")) nil
       (font-lock-mark-block-function . mark-defun)))
    (java-mode-defaults
     '((java-font-lock-keywords java-font-lock-keywords-1
        java-font-lock-keywords-2 java-font-lock-keywords-3)
       nil nil ((?_ . "w") (?$ . "w")) nil
       (font-lock-mark-block-function . mark-defun)))
  )
         
  (setq font-lock-defaults (append
    (list
      (cons 'kambi-c-mode    c-mode-defaults)
      (cons 'kambi-c++-mode  c++-mode-defaults)
      (cons 'kambi-objc-mode objc-mode-defaults)
      (cons 'kambi-java-mode java-mode-defaults)
    )
    font-lock-defaults)
  )
)

;; zdefiniuj rozszerzenia dla kambi-c/c++/java-mode --------------------------

(setq auto-mode-alist (add-to-list-new-items auto-mode-alist 
  '(("\\.c\\'" . kambi-c-mode)
    ("\\.cu\\'" . kambi-c-mode)
    ("\\.h\\'" . kambi-c++-mode)
    ("\\.hpp\\'" . kambi-c++-mode)
    ("\\.cpp\\'" . kambi-c++-mode)
    ("\\.java\\'" . kambi-java-mode)
    ;; on NVidia Cg programs
    ("\\.cg\\'" . kambi-c-mode)
    ;; vertex, geometry and fragment shaders
    ("\\.vs\\'" . kambi-c-mode)
    ("\\.gs\\'" . kambi-c-mode)
    ("\\.fs\\'" . kambi-c-mode)
    ("\\.glsl\\'" . kambi-c-mode)
    ("\\.shader\\'" . kambi-c-mode) ;; unity shaders
    ("\\.cginc\\'" . kambi-c-mode) ;; unity shaders
   )
))

;; ffap adjustments ----------------------------------------

;; (setq ffap-c-path (add-to-list-new-items ffap-c-path (list
;;   "/home/michalis/c/kambilib/"
;; ) t) )

;; ------------------------------------------------------------

(provide 'kambi-cc-mode)

;; eof ------------------------------------------------------------
