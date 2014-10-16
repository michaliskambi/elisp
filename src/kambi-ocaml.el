;;;; OCaml and Tuareg (Emacs support for OCaml) customizations.

(require 'kambi-utils)

(defconst kam-tuareg-loaded (load "append-tuareg" t)
  "Was Tuareg (Emacs support for OCaml) successfully loaded.")

(when kam-tuareg-loaded
  (setq tuareg-font-lock-governing '("darkorange3" "cyan" nil t nil t))
  (setq tuareg-use-abbrev-mode nil)
)

;; ffap ------------------------------------------------------------

(defconst ocaml-mli-paths
  '("/usr/lib/ocaml/3.07/camlimages/"
    "/usr/lib/ocaml/3.07/sdl/"
    "/usr/lib/ocaml/3.07/lablgl/"
    "/usr/lib/ocaml/3.07/"
  )
  "These are paths to be searched by kambi-ffap-ocaml. They should contain
mli files of OCaml modules that I (Kambi) use.")

(defun kambi-ffap-ocaml (str)
  (let ((result-basename str)
        maybe-result)

    ;; make first letter of result-basename downcase
    (aset result-basename 0 (downcase (aref result-basename 0)))

    ;; change file extension to ".mli" (do it even it the file already
    ;; has extension - e.g. if ffap-string-at-point found something with
    ;; an extension, like "Filename.dirname", we want to strip here
    ;; ".dirname" and replace this with ".mli". We could change
    ;; here the way ffap-string-at-point works, e.g. by
    ;;   (add-to-list 'ffap-string-at-point-mode-alist '(tuareg-mode "A-Za-z_"))
    ;; but we do not want to do this. Actually, I think it is almost
    ;; never a good solution to change the way ffap-string-at-point works.
    ;; If I would change the way ffap-string-at-point works then I could
    ;; use ffap in tuareg-mode *only* to find me ".mli" files and I do not
    ;; want that - ffap *may* find some ".mli" for me, that's the purpose
    ;; of this function, but I still want to be able to use ffap
    ;; to find files in usual way - e.g. having (* ocaml comment: readme.txt *)
    ;; I should be able to visit readme.txt with ffap.
    (setq result-basename (change-file-ext result-basename ".mli"))

    ;; iteruj szukajac pliku result-basename na sciezkach w ocaml-mli-paths.
    ;; (oraz na aktualnej sciezce, tzn. na "")
    ;; Zwroc pelna nazwe pliku jesli znalazles, nil wpp.
    (block func-block
      (dolist (path (cons "" ocaml-mli-paths))

        (setq maybe-result (concat path result-basename))
        (when (nondir-file-readable-p maybe-result)
          (return-from func-block maybe-result))
      )
    )
  )
)
(when kam-tuareg-loaded
  (add-to-list 'ffap-alist '(tuareg-mode . kambi-ffap-ocaml)))

;; insert gpl ----------------------------------------

(defun ocaml-insert-gpl-licensed (program-name copyright)
  "Inserts (gpl-licensed program-name copyright) surrounded
by proper OCaml comments and whitespaces."
  (insert "(*\n" (gpl-licensed program-name copyright) "*)\n\n")
)

;; ------------------------------------------------------------

(provide 'kambi-ocaml)

;; eof ------------------------------------------------------------
