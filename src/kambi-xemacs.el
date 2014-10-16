;;;; Functions needed by Kambi elisp files under XEmacs 

(unless (featurep 'xemacs)
  (error "Include kambi-xemacs ONLY under XEmacs"))

(defun compare-strings (str1 start1 end1 str2 start2 end2 &optional ignore-case)
  "Simple version of compare-strings from FSF Emacs. 
  
Differences: 
1) I don't know can it handle localised/multibyte characters 
   correctly if ignore-case<>nil. 
2) The value is 
     < 0 if left string is lesser,
     > 0 if right string is lesser,
     t if if strings are equal.
     
Unlike in FSF Emacs we do not guarantee what exactly will be that
'< 0' or '> 0' value."
  (let ((s1 (substring str1 start1 end1))
        (s2 (substring str2 start2 end2)))
    (if ignore-case  
        (progn
          (setq s1 (upcase s1))
          (setq s2 (upcase s2))
        )
    )
    (if (string= s1 s2) 
        t
      (if (string< s1 s2) -1 +1)    
    )
  )
)

;; testy :
;; (my-compare-strings "ala" 0 nil "balac" 0 nil) ; => not t
;; (my-compare-strings "ala" 0 nil "balac" 1 nil) ; => not t
;; (my-compare-strings "ala" 0 nil "balac" 1 4) ; => t
;; (my-compare-strings "ala" 0 nil "bALc" 1 4) ; => not t
;; (my-compare-strings "ala" 0 nil "bALAc" 1 4) ; => not t
;; (my-compare-strings "ala" 0 nil "bALAc" 1 4 t) ; => t
;; (substring "balac" 1 4)

;; redefine add-to-list under XEmacs to make 3rd argument available;
;; code below is taken from subr.el from FSF Emacs
(defun add-to-list (list-var element &optional append)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))

;; ------------------------------------------------------------

(provide 'kambi-xemacs)

;; eof ------------------------------------------------------------