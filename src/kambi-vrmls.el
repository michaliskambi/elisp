(require 'kambi-utils)

;; ------------------------------------------------------------------------
;; funkcje do przerabiania opisu node'ow na Pascala

(defconst vrmlnode-black-item "\\([^ \t\n]+\\)")
(defconst vrmlnode-white "[\t ]+")
(defconst vrmlnode-line
  (concat "^"
    vrmlnode-white "exposedField"
    vrmlnode-white vrmlnode-black-item ; MFFloat (type) = \1
    vrmlnode-white vrmlnode-black-item ; groundAngle (name) = \2
    ;; whitespaces are allowed in default value. It must only begin with
    ;; non-whitespace. Comments (after #) are expected to be stripped at
    ;; this point.
    vrmlnode-white "\\([^ \t\n].*\\)?$" ; [] (default value) = \3
  )
  "Regexp to match line like
  exposedField MFFloat  groundAngle  []"
)

(defun vrmlnode-to-pascal-interface ()
  "Przerabia zapis node'a ze specyfikacji VRMLa 97 na deklaracje interfejsu do mojego
VRMLNodes. Operuje na zaznaczonym regionie (TODO: aktualnie na calym buforze !)
w aktualnym buforze. Np.
  exposedField MFFloat  groundAngle  []
  exposedField MFColor  groundColor  []
zamienia na
  property FdGroundAngle: TMFFloat index ? read GetFieldAsMFFloat;
  property FdGroundColor: TMFColor index ? read GetFieldAsMFColor;
 (jak widac zamiast numeru indeksu zapisuje ? bo jest nastawione raczej na to zeby
generowac czesc opisu node'a, tzn. jest przygotowane na to ze zaznaczony region
to tylko czesc opisu node'a)"
  (interactive)
  (kam-simple-re-replace-buffer vrmlnode-line
    "  property Fd\\2: T\\1 index ? read GetFieldAs\\1;"
  )
)

(defun vrmlnode-to-pascal-implementation ()
  "Podobnie jak vrmlnode-to-pascal-interface tylko zamienia na cos innego.
  exposedField MFFloat  groundAngle  []
zamienia na
  Fields.Add(TMFFloat.Create('groundAngle', []));
Default value specifications should ma manually corrected to proper Pascal syntax."
  (interactive)
  (kam-simple-re-replace-buffer vrmlnode-line
    "  Fields.Add(T\\1.Create('\\2', \\3)); Fields.Last.Exposed := true;"
  )
  (kam-simple-replace-buffer " {  }" "") ; delete empty comments
)

;; ---------------------------------------------------------------------------
;; ponizej sa ogolne funkcje do automatycznego przetwarzania VRMLi zapisanych
;; Blenderem

(defconst vrml-white-spaces-regexp "[\t\n ]+")
(defconst vrml-float-regexp "\\(\\|-\\)[0-9]+\\.[0-9]+")
(defconst vrml-starting-sep-name "StartingSeparator")
(defconst vrml-our-comment
  "# Modified by some Kambi's Emacs' macros to transform VRML files from Blender\n"
)

(defun vrml-floats-regexp (COUNT)
  (string-repeat (concat vrml-white-spaces-regexp vrml-float-regexp) COUNT)
)

(defun vrml-remove-stupid-blender-start-transform ()
  (interactive)

  (kam-simple-re-replace-buffer
    (concat "# Visible Objects\n\nSeparator {\n"
        vrml-white-spaces-regexp "MatrixTransform {"
	vrml-white-spaces-regexp "matrix"
        (vrml-floats-regexp 16)
        vrml-white-spaces-regexp "}"
	vrml-white-spaces-regexp "PerspectiveCamera {"
	vrml-white-spaces-regexp "focalDistance"
        (vrml-floats-regexp 1)
        vrml-white-spaces-regexp "}"
    )
    (concat "# Visible Objects\n\nSeparator {\n" )
  )
)

(defun vrml-add-starting-sep-name ()
  (interactive)
  (kam-simple-replace-buffer
    "# Visible Objects\n\nSeparator {\n"
    (concat "# Visible Objects\n\nDEF " vrml-starting-sep-name " Separator {\n" )
  )
)

(defun vrml-add-text-at-the-beginning-of-separator
  (SEP-NODE-NAME-REGEXP TEXT-TO-ADD)
  (interactive)

  (kam-simple-re-replace-buffer
    (concat "\\(DEF"
      vrml-white-spaces-regexp SEP-NODE-NAME-REGEXP
      vrml-white-spaces-regexp "Separator"
      vrml-white-spaces-regexp "{\\)")
    (concat "\\1\n" TEXT-TO-ADD "\n")
  )
)

(defun vrml-add-text-at-start (TEXT-TO-ADD)
  (interactive)
  (vrml-add-text-at-the-beginning-of-separator vrml-starting-sep-name TEXT-TO-ADD)
)

(defun vrml-add-our-comment ()
   "Adds to buffer some special comment marking that this buffer was modified
by macros defined in this file."
  (interactive)

  (kam-simple-re-replace-buffer
    "\\(# Blender .*\n\\)"
    (concat "\\1" vrml-our-comment)
  )
)

(defun vrml-check-there-is-no-our-comment ()
  "Checks that there is NO our-comment in this buffer - use this with add-our-comment
to make sure you don't modify the same buffer twice."
  (interactive)

  (save-excursion
    (goto-char (point-min))
    (if (search-forward vrml-our-comment nil t)
      (error (concat (buffer-name) ": buffer is already modified by Kambi's VRML macros."))
    )
  )
)

(defun vrml-process-common (remove-stupid-blender-start-transform
  make-texture-filenames-relative)
  "Some common things to do at the beginning of vrml-process-* function :
Namely:

  1. vrml-check-there-is-no-our-comment,
     vrml-add-our-comment
     (this makes sure that we will not call vrml-process-common
     twice on the same file, as vrml-process-common may not be able
     to handle such cases; I would consider such use non-elegant anyway,
     vrml-process-common is intended to be called only on a file
     generated straight by Blender)

  2. make Texture2.filename fields relative to current buffer filename
       (Blender tends to use absolute paths)
       if MAKE-TEXTURE-FILENAMES-RELATIVE is non-nil,

  3. vrml-remove-stupid-blender-start-transform
       if REMOVE-STUPID-BLENDER-START-TRANSFORM is not-nil
     vrml-add-starting-sep-name."
  (interactive)

  (vrml-check-there-is-no-our-comment)
  (vrml-add-our-comment)

  (re-function-replace-buffer "\\(filename \\)\\(.*\\)$"
    (lambda ()
      (concat
        (match-string 1)
          (if make-texture-filenames-relative
              (file-relative-name (match-string 2))
            (match-string 2)
          )
      )
    ))

  (if remove-stupid-blender-start-transform
    (vrml-remove-stupid-blender-start-transform)
  )
  (vrml-add-starting-sep-name)
)

;; ------------------------------------------------------------------------------
;; ponizej sa funkcje vrml-process-* do konkretnych VRMLi

(defun vrml-process-mobius ()
  (interactive)
  (vrml-process-common t t)
  (vrml-add-text-at-the-beginning-of-separator
    "MeshKulkaInside"
    "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID creaseAngle 3 }")
  (vrml-add-text-at-the-beginning-of-separator
    "MeshKulkaPipeEnd"
    "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID creaseAngle 3 }" )
  (vrml-add-text-at-the-beginning-of-separator
    "MeshPipe"
    "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID creaseAngle 3 }")
  (vrml-add-text-at-the-beginning-of-separator
    "MeshEnterprCorpse"
    "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }")
  (vrml-add-text-at-the-beginning-of-separator
    "MeshEnterprCorMid"
    "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }" )
  (vrml-add-text-at-the-beginning-of-separator
    "MeshEnterprPlate"
    "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }")
)

(defun vrml-process-wawoz ()
  (interactive)
  (vrml-process-common t t)
)

(defun vrml-process-lake ()
  (interactive)
  (vrml-process-common t t)
  (vrml-add-text-at-the-beginning-of-separator
    "MeshWater"
    "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }" )
  (vrml-add-text-at-the-beginning-of-separator
    "MeshGround"
    "ShapeHints {
       vertexOrdering COUNTERCLOCKWISE
       shapeType SOLID
       creaseAngle 1.4
     }")
)

(defun vrml-process-black-hedgehog ()
  (interactive)
  (vrml-process-common t t)
  (vrml-add-text-at-start
    "Scale { scaleFactor 30 30 30 }
     ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }" )
)

(defun vrml-process-rocket ()
  (interactive)
  (vrml-process-common t t)
  (vrml-add-text-at-start "ShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }" )
)

(defun vrml-process-tie-fighter ()
  (interactive)
  (vrml-process-common t t)
  (vrml-add-text-at-start "Scale { scaleFactor 2.5 2.5 2.5 }" )
)

(defun vrml-process-watchtower ()
  (interactive)
  (vrml-process-common t t)
  (vrml-add-text-at-the-beginning-of-separator "MeshTowerLower" "\t\tShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }")
  (vrml-add-text-at-the-beginning-of-separator "MeshTowerHigher" "\t\tShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }")
  (vrml-add-text-at-the-beginning-of-separator "MeshGlobe" "\t\tShapeHints { vertexOrdering COUNTERCLOCKWISE shapeType SOLID }")
)

(defun vrml-process-prop-edit ()
  (interactive)
  (vrml-process-common t t)
  (vrml-add-text-at-the-beginning-of-separator "MeshGround"
    "\t\tShapeHints { creaseAngle 1.4 }")
)

(defun vrml-process-amulet ()
  (interactive)
  (vrml-process-common t t)
;; no longer needed :
;;  (vrml-add-text-at-the-beginning-of-separator "MeshInside"
;;    "\t\tTexture2 { filename \"inside_texture.jpg\" }")
)

(defun vrml-process-castle ()
  (interactive)
  (vrml-process-common t t)
;;  (vrml-add-text-at-the-beginning-of-separator "MeshKulkaNaWiezy"
;;    "\t\tShapeHints { creaseAngle 1.4 }")
)

;; -----------------------------------------------------------------------------
;; ponizej jest funkcja ktora automatycznie wywoluje odpowiednie vrml-process-*

(defun vrml-process (&optional on-no-case)
  "Wywolaj odpowiednia funkcje vrml-process-* na podstawie
 (extract-file-name buffer-file-name) lub niekiedy na podstawie
 (buffer-file-name). (nie uzywam w tym celu buffer-name bo to nieeleganckie -
przeciez nic mi nie gwarantuje jaka postac bedzie mialo buffer-name w
porownaniu do buffer-file-name).

Wartosc on-no-case kontroluje co sie stanie jezeli dla danego pliku nie zapisano
tutaj zadnej funkcji vrml-process-*.
- nil (a wiec domyslnie) : wyrzuci error()
- use-common : wywola (vrml-process-common t t)
- ignore : nic nie zrobi (zwroci wtedy nil)
- nie dawaj innych wartosci dla on-no-case, rezerwuje sobie mozilwosc zwiekszenia
  dopuszczalnych tutaj wartosci jesli znajde kiedys po temu powod."
  (interactive)
  (let ((fname (extract-file-name buffer-file-name)))
    (simple-case fname
      ;; for malfunction
      '("lake_bl.wrl" (vrml-process-lake))
      '("mobius_bl.wrl" (vrml-process-mobius))
      '("wawoz_bl.wrl" (vrml-process-wawoz))
      '("black_hedgehog.wrl" (vrml-process-black-hedgehog))
      '("tie_fighter.wrl" (vrml-process-tie-fighter))
      '("rocket.wrl" (vrml-process-rocket))
      '("destroyer.wrl" (vrml-process-common t t))

      ;; other
      '("watchtower.wrl" (vrml-process-watchtower))
      '("prop_edit.wrl" (vrml-process-prop-edit))
      '("amulet.wrl" (vrml-process-amulet))
      '("uv_coords.wrl" (vrml-process-common t t))
      '("castle.wrl" (vrml-process-castle))

      '(otherwise
         (case on-no-case
           ('nil (error (concat fname ": file doesn't have any vrml-process-* function defined.")))
           ('use-common (vrml-process-common t t))
           ('ignore nil)
           (otherwise (error "Invalid on-no-case parameter in vrml-process"))
         )
       )
    )
  )
)

;; other vrml related funcs ---------------------------------------

(defun show-all-lines-with-texture-or-def ()
  (interactive)
  (occur "^.*\\(DEF\\|Texture\\).*$" 1)
)

;; ----------------------------------------------------------------

(provide 'kambi-vrmls)

;; eof ------------------------------------------------------------
