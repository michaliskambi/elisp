;; Various temp functions. Probably for 1-time usage, but preserving here
;; just in case.
;; ---------------------------------------------------------------------------

;; temp:
;; (global-set-key (kbd "<f12> q")
;;   (lambda () (interactive) (query-replace "\"en\"" "LANG_EN")))

;; (defun myutils-to-kambi ()
;;   (interactive)
;;   (kam-simple-replace-buffer "myutils" "KambiUtils")
;;   (save-buffer)
;; )
;; (global-set-key (kbd "<f5>") 'myutils-to-kambi)

;; (defun myglutils-to-kambi ()
;;   (interactive)
;;   (kam-simple-replace-buffer "myglutils" "KambiGLUtils")
;;   (save-buffer)
;; )
;; (global-set-key (kbd "<f5>") 'myglutils-to-kambi)

;; (global-set-key (kbd "`")
;;    (lambda () (interactive) (query-replace "keypressing" "KeyDown")))

;;     ;; TESTS:
;;     (setq comint-input-filter-functions '(kam-message-1))
;; (defun kam-message-1 (text)
;;   (message (concat "Input is \"" (prin1-to-string (string-to-list text)) "\" now.")))

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf)
;;   (query-replace "MatrixNavigation" "Navigation")
;;   (query-replace "TMatrixNavigator" "TNavigator")
;;   (query-replace "TMatrixExaminer" "TExamineNavigator")
;;   (query-replace "TMatrixWalker" "TWalkNavigator")
;;   (query-replace "NavWalker" "WalkNav")
;;   (query-replace "NavExaminer" "ExamineNav")
;; )

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf)
;;   (query-replace "maxleafitemscount" "LeafCapacity")
;;   (query-replace "max-leaf-items-count" "leaf-capacity")
;;   (query-replace "max_leaf_items_count" "leaf_capacity")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "vrmloctreeitems" "VRMLTriangle")
;;   (kam-beg-of-buf) (query-replace "toctreeitem" "TVRMLTriangle")
;;   (kam-beg-of-buf) (query-replace "poctreeitem" "PVRMLTriangle")
;;   (kam-beg-of-buf) (query-replace "tvrmlitemsoctreenode" "TVRMLBaseTrianglesOctreeNode")
;;   (kam-beg-of-buf) (query-replace "tvrmlitemsoctree" "TVRMLBaseTrianglesOctree")
;;   (kam-beg-of-buf) (query-replace "tdynoctreeitemsarray" "TDynVRMLTriangleArray")
;;   (kam-beg-of-buf) (query-replace "OctreeItem" "Triangle")
;;   (kam-beg-of-buf) (query-replace "OctreeItemToIgnore" "TriangleToIgnore")
;;   (kam-beg-of-buf) (query-replace "ItemsToIgnoreFunc" "TrianglesToIgnoreFunc")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "vrmlnonfatalerror_warningwrite" "VRMLWarning_Write")
;;   (kam-beg-of-buf) (query-replace "vrmlnonfatalerror" "VRMLWarning")
;;   (kam-beg-of-buf) (query-replace "datanonfatalerror" "DataWarning")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "tglwindowsmanager" "TGLApplication")
;;   (kam-beg-of-buf) (query-replace "glwm" "Application")
;;   (kam-beg-of-buf) (query-replace "application.loop" "Application.Run")
;;   (kam-beg-of-buf) (query-replace "initloop" "InitAndRun")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "tbase3d" "T3D")
;; ;;  (kam-beg-of-buf) (query-replace "tbase3dlist" "T3DList")
;;   (kam-beg-of-buf) (query-replace "tcustomtranslated3d" "T3DCustomTranslated")
;;   (kam-beg-of-buf) (query-replace "ttranslated3d" "T3DTranslated")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "tbaseshadowvolumes" "TBaseShadowVolumeRenderer")
;;   (kam-beg-of-buf) (query-replace "tshadowvolumes" "TGLShadowVolumeRenderer")
;;   (kam-beg-of-buf) (query-replace "shadowvolumes: tbaseshadowvolumes" "ShadowVolumeRenderer: TBaseShadowVolumeRenderer")
;;   (kam-beg-of-buf) (query-replace "shadowvolumes" "GLShadowVolumeRenderer")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)


;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace ".data" "")
;;   (kam-beg-of-buf) (query-replace "vector_normalize" "NormalizeTo1st")
;;   (kam-beg-of-buf) (query-replace "vector_get_normalized" "Normalized")
;;   (kam-beg-of-buf) (query-replace "_single" "Single")
;;   (kam-beg-of-buf) (query-replace "_double" "Double")
;;   (kam-beg-of-buf) (query-replace ".Init_Zero" " := ZeroVector3Single")
;;   (kam-beg-of-buf) (query-replace ".Init(" " := Vector3Single(") ; you will have to remove closing paren by hand
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; Tests:(kam-upcase-last "aasd")

;; (defun a ()
;;   (interactive)
;;   (re-function-replace-buffer "property Fd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "private FFd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "read FFd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer ":= Fd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer ":= FFd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "property Event." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "private FEvent." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "read FEvent." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "^   Fd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "^  FFd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "^   Event." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "^  FEvent." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "Fields\\.Add(FFd." 'kam-match-upcase-last)
;;   (re-function-replace-buffer "Events\\.Add(FEvent." 'kam-match-upcase-last)
;; )

;; temp:
;; (global-set-key (kbd "<f12> q")
;;   (lambda () (interactive) (query-replace "\"en\"" "LANG_EN")))

;; (defun myutils-to-kambi ()
;;   (interactive)
;;   (kam-simple-replace-buffer "myutils" "KambiUtils")
;;   (save-buffer)
;; )
;; (global-set-key (kbd "<f5>") 'myutils-to-kambi)

;; (defun myglutils-to-kambi ()
;;   (interactive)
;;   (kam-simple-replace-buffer "myglutils" "KambiGLUtils")
;;   (save-buffer)
;; )
;; (global-set-key (kbd "<f5>") 'myglutils-to-kambi)

;; (global-set-key (kbd "`")
;;    (lambda () (interactive) (query-replace "keypressing" "KeyDown")))

;;     ;; TESTS:
;;     (setq comint-input-filter-functions '(kam-message-1))
;; (defun kam-message-1 (text)
;;   (message (concat "Input is \"" (prin1-to-string (string-to-list text)) "\" now.")))

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf)
;;   (query-replace "MatrixNavigation" "Navigation")
;;   (query-replace "TMatrixNavigator" "TNavigator")
;;   (query-replace "TMatrixExaminer" "TExamineNavigator")
;;   (query-replace "TMatrixWalker" "TWalkNavigator")
;;   (query-replace "NavWalker" "WalkNav")
;;   (query-replace "NavExaminer" "ExamineNav")
;; )

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf)
;;   (query-replace "maxleafitemscount" "LeafCapacity")
;;   (query-replace "max-leaf-items-count" "leaf-capacity")
;;   (query-replace "max_leaf_items_count" "leaf_capacity")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "vrmloctreeitems" "VRMLTriangle")
;;   (kam-beg-of-buf) (query-replace "toctreeitem" "TVRMLTriangle")
;;   (kam-beg-of-buf) (query-replace "poctreeitem" "PVRMLTriangle")
;;   (kam-beg-of-buf) (query-replace "tvrmlitemsoctreenode" "TVRMLBaseTrianglesOctreeNode")
;;   (kam-beg-of-buf) (query-replace "tvrmlitemsoctree" "TVRMLBaseTrianglesOctree")
;;   (kam-beg-of-buf) (query-replace "tdynoctreeitemsarray" "TDynVRMLTriangleArray")
;;   (kam-beg-of-buf) (query-replace "OctreeItem" "Triangle")
;;   (kam-beg-of-buf) (query-replace "OctreeItemToIgnore" "TriangleToIgnore")
;;   (kam-beg-of-buf) (query-replace "ItemsToIgnoreFunc" "TrianglesToIgnoreFunc")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "vrmlnonfatalerror_warningwrite" "VRMLWarning_Write")
;;   (kam-beg-of-buf) (query-replace "vrmlnonfatalerror" "VRMLWarning")
;;   (kam-beg-of-buf) (query-replace "datanonfatalerror" "DataWarning")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "tglwindowsmanager" "TGLApplication")
;;   (kam-beg-of-buf) (query-replace "glwm" "Application")
;;   (kam-beg-of-buf) (query-replace "application.loop" "Application.Run")
;;   (kam-beg-of-buf) (query-replace "initloop" "InitAndRun")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "tbase3d" "T3D")
;; ;;  (kam-beg-of-buf) (query-replace "tbase3dlist" "T3DList")
;;   (kam-beg-of-buf) (query-replace "tcustomtranslated3d" "T3DCustomTranslated")
;;   (kam-beg-of-buf) (query-replace "ttranslated3d" "T3DTranslated")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)

;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace "tbaseshadowvolumes" "TBaseShadowVolumeRenderer")
;;   (kam-beg-of-buf) (query-replace "tshadowvolumes" "TGLShadowVolumeRenderer")
;;   (kam-beg-of-buf) (query-replace "shadowvolumes: tbaseshadowvolumes" "ShadowVolumeRenderer: TBaseShadowVolumeRenderer")
;;   (kam-beg-of-buf) (query-replace "shadowvolumes" "GLShadowVolumeRenderer")
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)


;; (defun kam-aaa ()
;;   (interactive)
;;   (kam-beg-of-buf) (query-replace ".data" "")
;;   (kam-beg-of-buf) (query-replace "vector_normalize" "NormalizeTo1st")
;;   (kam-beg-of-buf) (query-replace "vector_get_normalized" "Normalized")
;;   (kam-beg-of-buf) (query-replace "_single" "Single")
;;   (kam-beg-of-buf) (query-replace "_double" "Double")
;;   (kam-beg-of-buf) (query-replace ".Init_Zero" " := ZeroVector3Single")
;;   (kam-beg-of-buf) (query-replace ".Init(" " := Vector3Single(") ; you will have to remove closing paren by hand
;; )
;; (global-set-key (kbd "<f5>") 'kam-aaa)


(defun kam-aaa ()
  (interactive)
  (kam-beg-of-buf) (query-replace "Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 3, 2, 2000);"
 "Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, X3DVersion, 2000);")

  (kam-beg-of-buf) (query-replace "Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);"
 "Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);")

  (kam-beg-of-buf) (query-replace "Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 100);"
    "Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 100);")

  (kam-beg-of-buf) (query-replace "Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Node is since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);"
  "Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Node is since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);")

  (kam-beg-of-buf) (query-replace "Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);"
  "Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML2Version, 1000);")

  (kam-beg-of-buf) (query-replace "ForVRMLVersion(const VerMajor, VerMinor: Integer)"
  "ForVRMLVersion(const Version: TVRMLVersion)")

  (kam-beg-of-buf) (query-replace "Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 1, 0, 1000);"
  "Result := inherited SuggestedVRMLVersion(Version, SuggestionPriority);
  AndSuggestedVRMLVersion(Result, Version, SuggestionPriority,
    true, VRML1Version, 1000);")

  (kam-beg-of-buf) (query-replace "VerMajor >= 2" "Version.Major >= 2")
  (kam-beg-of-buf) (query-replace "VerMajor >= 3" "Version.Major >= 3")
  (kam-beg-of-buf) (query-replace "VerMajor <= 1" "Version.Major <= 1")
  (kam-beg-of-buf) (query-replace "VerMajor = 2" "Version.Major = 2")
  (kam-beg-of-buf) (query-replace "VerMajor, VerMinor" "Version")
)

(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (kam-beg-of-buf) (query-replace "box[" "Box.Data[")
  (kam-beg-of-buf) (query-replace "box1[" "Box1.Data[")
  (kam-beg-of-buf) (query-replace "box2[" "Box2.Data[")
  (kam-beg-of-buf) (query-replace-regexp "isemptybox3d(\\([^)]+\\))" "\\1.IsEmpty")
  (kam-beg-of-buf) (query-replace-regexp "box3dmiddle(\\([^)]+\\))" "\\1.Middle")
  (kam-beg-of-buf) (query-replace "Box3DMaxSize(Box, " "Box.MaxSize(")
  (kam-beg-of-buf) (query-replace "Box3DMinSize(Box, " "Box.MinSize(")
  (kam-beg-of-buf) (query-replace "Box3DAvgSize(Box, " "Box.AverageSize(")
  (kam-beg-of-buf) (query-replace "Box3DTransform(Box, " "Box.Transform(")
  (kam-beg-of-buf) (query-replace-regexp "box3dtonicestr(\\([^)]+\\))" "\\1.ToNiceStr")
  (kam-beg-of-buf) (query-replace-regexp "box3davgsize(\\([^)]+\\))" "\\1.AverageSize")
  (kam-beg-of-buf) (query-replace-regexp "box3dradius(\\([^)]+\\))" "\\1.Radius")
  (kam-beg-of-buf) (query-replace-regexp "box3dxyradius(\\([^)]+\\))" "\\1.XYRadius")
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-update-pascal-style ()
  "My old style was to declare variables like

  function Name(VarName1,VarName2:SomeType):SomeType;
  var VarName:SomeType;

and assignments like

  VarName:=Value;

and calls like

  Name(1,2,3);

and comparisons like

  A=B, A<>B, A>B

My new style (changed somewhere around the 2004-07, when starting to work
on uwoj_mandaty, but I was tempting to do that since some time) seems to me
more \"related\", identifiers have space to \"breathe\" :

  function Name(VarName1, VarName2: SomeType): SomeType;
  var VarName: SomeType;

  VarName := Value;

  Name(1, 2, 3);

  A = B, A <> B, A > B

I.e. space before type specification,
and spaces around operators :=, =, <, >, <>, >=, <=,
and space after every comma.
This new style is also conforming to Borland coding styles,
that is also widely used in many ObjectPascal code, for Delphi and FPC.

This function tries to do interactive replacements to automatically convert
such things."
  (interactive)
  (let
    ( (first-ident-char "\\([a-zA-Z_]\\)")
      ( last-ident-char "\\([a-zA-Z0-9)_]\\)")
      (first-value-char "\\([-@'$#a-zA-Z0-9]\\)")
      ( last-value-char "\\([][a-zA-Z0-9)_]\\)")
    )
    (save-excursion
      ;; This handles all cases of
      ;;   last-value-char -- something -- first-value-char
      ;; where you want to insert spaces at both "--".
      (kam-beg-of-buf)
      (query-replace-regexp (concat last-value-char
        "\\(=\\|>\\|<\\|>=\\|<=\\|<>\\|:=\\)" first-value-char)
        "\\1 \\2 \\3")
      (kam-beg-of-buf)
      (query-replace-regexp
        (concat last-ident-char ":" first-ident-char) "\\1: \\2")
      (kam-beg-of-buf)
      (query-replace-regexp
        (concat last-value-char "," first-value-char) "\\1, \\2")
      (kam-beg-of-buf)
      (query-replace-regexp
        (concat last-value-char ":= " first-value-char) "\\1 := \\2")
      (kam-beg-of-buf)
      (query-replace-regexp
        (concat last-value-char " :=" first-value-char) "\\1 := \\2")
    )
  )
)

(defun kam-update-images-interface ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace-regexp "sizex" "Width")
    (kam-beg-of-buf) (query-replace-regexp "sizey" "Height")
    (kam-beg-of-buf) (query-replace-regexp "imagerowptr(image, "   "Image.RowPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagerowptr(image,"    "Image.RowPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagepixelptr(image, " "Image.PixelPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagepixelptr(image,"  "Image.PixelPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagerowptr(result, "   "Result.RowPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagerowptr(result,"    "Result.RowPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagepixelptr(result, " "Result.PixelPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagepixelptr(result,"  "Result.PixelPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagerowptr(img, "   "Img.RowPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagerowptr(img,"    "Img.RowPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagepixelptr(img, " "Img.PixelPtr(")
    (kam-beg-of-buf) (query-replace-regexp "imagepixelptr(img,"  "Img.PixelPtr(")
    (kam-beg-of-buf) (query-replace-regexp "timagerec" "TImage")
    (kam-beg-of-buf) (query-replace-regexp "trgbimagerec" "TRGBImage")
    (kam-beg-of-buf) (query-replace-regexp "talphaimagerec" "TAlphaImage")
    (kam-beg-of-buf) (query-replace-regexp "imagealloc(\\(.*\\), \\(.*\\), ikalpha)" "TAlphaImage.Create(\\1, \\2)")
    (kam-beg-of-buf) (query-replace-regexp "imagealloc(\\(.*\\), \\(.*\\), ikrgb)"     "TRGBImage.Create(\\1, \\2)")
    (kam-beg-of-buf) (query-replace-regexp "imagealloc(\\(.*\\), \\(.*\\), ikrgbe)"   "TRGBEImage.Create(\\1, \\2)")
    (kam-beg-of-buf) (query-replace-regexp "imagealloc(\\([^,\n]*\\), \\([^,\n]*\\))"  "TRGBImage.Create(\\1, \\2)")
    (kam-beg-of-buf) (query-replace-regexp "imagefree(result)" "Result.Free")
    (kam-beg-of-buf) (query-replace-regexp "imagefree" "FreeAndNil")
    (kam-beg-of-buf) (query-replace-regexp "colorsize" "PixelSize")
    (kam-beg-of-buf) (query-replace-regexp "rawdata" "RawPixels")
    (kam-beg-of-buf) (query-replace-regexp "rgbdata" "RGBPixels")
    (kam-beg-of-buf) (query-replace-regexp "alphadata" "AlphaPixels")
    ;; first replace ikrgbe, then ikrgb, otherwise ikrgb is substring of every
    ;; ikrgbe.
    (kam-beg-of-buf) (query-replace-regexp "ikrgbe" "TRGBEImage")
    (kam-beg-of-buf) (query-replace-regexp "ikrgb" "TRGBImage")
    (kam-beg-of-buf) (query-replace-regexp "ikalpha" "TAlphaImage")
  )
)

(defun kam-update-parameters-interface ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace-regexp "parstr(\\([^)]+\\))" "Parameters[\\1]")
    (kam-beg-of-buf) (query-replace "parcount" "Parameters.High")
  )
)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "vrmlengine.sourceforge.net" "castle-engine.sourceforge.net")
    (kam-beg-of-buf) (query-replace "vrmlengine.sf.net" "castle-engine.sf.net")
    (kam-beg-of-buf) (query-replace "https://vrmlengine.svn.sourceforge.net/svnroot/vrmlengine" "http://svn.code.sf.net/p/castle-engine/code")
    (kam-beg-of-buf) (query-replace "VRMLENGINE_HTDOCS_LOCAL_PATH" "CASTLE_ENGINE_HTDOCS_LOCAL_PATH")
    (kam-beg-of-buf) (query-replace "sources/vrmlengine/trunk" "sources/castle-engine/trunk")
    (kam-beg-of-buf) (query-replace "VRMLENGINE_PATH" "CASTLE_ENGINE_PATH")
    (kam-beg-of-buf) (query-replace "http://michalis.ii.uni.wroc.pl/vrmlengine-snapshots/" "http://michalis.ii.uni.wroc.pl/castle-engine-snapshots/")
    (kam-beg-of-buf) (query-replace "svrmlengineprogramhelpsuffix" "SCastleEngineProgramHelpSuffix")
    (kam-beg-of-buf) (query-replace "strings_addvrmlengineprogramhelpsuffix" "Strings_AddCastleEngineProgramHelpSuffix")
    (kam-beg-of-buf) (query-replace "VRMLENGINE_TRUNK_AVAILABLE" "CASTLE_ENGINE_TRUNK_AVAILABLE")
    (kam-beg-of-buf) (query-replace "kambi_base" "castle_base")
    (kam-beg-of-buf) (query-replace "kambi_glwindow" "castle_glwindow")
    (kam-beg-of-buf) (query-replace "kambi_components" "castle_components")
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "vrmlengine_sidebar" "castle_sidebar")
    (kam-beg-of-buf) (query-replace "vrmlengine_sitemap" "castle_sitemap")
    (kam-beg-of-buf) (query-replace "vrmlengine_thumbs" "castle_thumbs")
    (kam-beg-of-buf) (query-replace "vrmlengine_news_date" "castle_news_date")
    (kam-beg-of-buf) (query-replace "vrmlengine_breadcrumbs" "castle_breadcrumbs")
    (kam-beg-of-buf) (query-replace "vrmlengine_force_absolute" "castle_force_absolute")
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "castle_engine_header" "castle_header")
    (kam-beg-of-buf) (query-replace "castle_engine_footer" "castle_footer")
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "tcoordinate3node"            "TCoordinate3Node_1")
    (kam-beg-of-buf) (query-replace "tinfonode"                   "TInfoNode_1")
    (kam-beg-of-buf) (query-replace "tmaterialbindingnode"        "TMaterialBindingNode_1")
    (kam-beg-of-buf) (query-replace "tnormalbindingnode"          "TNormalBindingNode_1")
    (kam-beg-of-buf) (query-replace "ttexture2node"               "TTexture2Node_1")
    (kam-beg-of-buf) (query-replace "ttexture2transformnode"      "TTexture2TransformNode_1")
    (kam-beg-of-buf) (query-replace "ttexturecoordinate2node"     "TTextureCoordinate2Node_1")
    (kam-beg-of-buf) (query-replace "tshapehintsnode"             "TShapeHintsNode_1")
    (kam-beg-of-buf) (query-replace "trotationnode"               "TRotationNode_1")
    (kam-beg-of-buf) (query-replace "tscalenode"                  "TScaleNode_1")
    (kam-beg-of-buf) (query-replace "ttranslationnode"            "TTranslationNode_1")
    (kam-beg-of-buf) (query-replace "torthographiccameranode"     "TOrthographicCameraNode_1")
    (kam-beg-of-buf) (query-replace "tperspectivecameranode"      "TPerspectiveCameraNode_1")
    (kam-beg-of-buf) (query-replace "tabstractseparatornode"      "TAbstractSeparatorNode_1")
    (kam-beg-of-buf) (query-replace "tseparatornode"              "TSeparatorNode_1")
    (kam-beg-of-buf) (query-replace "ttransformseparatornode"     "TTransformSeparatorNode_1")
    (kam-beg-of-buf) (query-replace "twwwanchornode"              "TWWWAnchorNode_1")
    (kam-beg-of-buf) (query-replace "twwwinlinenode"              "TWWWInlineNode_1")
    (kam-beg-of-buf) (query-replace "tabstracttransformationnode" "TAbstractTransformationNode_1")
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace  "tkamopenglcontrolcore"  "TCastleControlBase"            )
    (kam-beg-of-buf) (query-replace  "tkamvrmlbrowser"        "TCastleControl"                )
    (kam-beg-of-buf) (query-replace  "tkamopenglcontrol"      "TCastleControlCustom"          )

    (kam-beg-of-buf) (query-replace  "tglwindowstate" "TWindowState")
    (kam-beg-of-buf) (query-replace  "read_tglwindowmanager"  "read_application")
    (kam-beg-of-buf) (query-replace  "read_tglwindow"  "read_window")
    (kam-beg-of-buf) (query-replace  "tglwindowparseoption"     "TWindowParseOption")
    (kam-beg-of-buf) (query-replace  "tglwindowparseoptions"    "TWindowParseOptions")
    (kam-beg-of-buf) (query-replace  "pglwindowparseoptions"    "PWindowParseOptions")
    (kam-beg-of-buf) (query-replace  "tglwindowmessagetype"     "TWindowMessageType")
    (kam-beg-of-buf) (query-replace  "tglwindowfunc"            "TWindowFunc")
    (kam-beg-of-buf) (query-replace  "glwindowmenu"             "castlewindowmenu")
    (kam-beg-of-buf) (query-replace  "tglwindowcallbacks"       "TWindowCallbacks")
    (kam-beg-of-buf) (query-replace  "tglwindowlist"            "TWindowList")

    (kam-beg-of-buf) (query-replace  "tkamrecentfiles" "TLazRecentFiles")
    (kam-beg-of-buf) (query-replace  "tglrecentfiles" "TCastleRecentFiles")
    (kam-beg-of-buf) (query-replace  "tkamglbutton"           "TCastleButton"                 )
    (kam-beg-of-buf) (query-replace  "tkamglimage"            "TCastleImage"                  )
    (kam-beg-of-buf) (query-replace  "tglnotifications"       "TCastleNotifications"          )
    (kam-beg-of-buf) (query-replace  "tglmenu"                "TCastleMenu"                   )
    (kam-beg-of-buf) (query-replace  "tkamxmlconfig"          "TCastleConfig"                 )
    (kam-beg-of-buf) (query-replace  "tkamscenemanager"       "TCastleSceneManager"           )
    (kam-beg-of-buf) (query-replace  "tvrmlanimation"         "T3DPrecalculatedAnimationCore" )
    (kam-beg-of-buf) (query-replace  "tvrmlglanimation"       "T3DPrecalculatedAnimation"     )

    (kam-beg-of-buf) (query-replace  "tvrmlscenespatialstructure"  "TSceneSpatialStructure")
    (kam-beg-of-buf) (query-replace  "tvrmlscenegeometrychanged"   "TSceneGeometryChanged")
    (kam-beg-of-buf) (query-replace  "tvrmlscenenotification"      "TSceneNotification")
    (kam-beg-of-buf) (query-replace  "tvrmlscenefreeresource"      "TSceneFreeResource")
    (kam-beg-of-buf) (query-replace  "tvrmlscenevalidit"           "TSceneValidit")
    (kam-beg-of-buf) (query-replace  "tvrmlscenerenderingattributes" "TSceneRenderingAttributes")
    (kam-beg-of-buf) (query-replace  "TVRMLSceneFileName" "TSceneFileName")

    (kam-beg-of-buf) (query-replace  "tvrmlscene"             "T3DSceneCore"                  )
    (kam-beg-of-buf) (query-replace  "tvrmlglscene"           "T3DScene"                      )

    (kam-beg-of-buf) (query-replace  "tvrmlshape"             "TShape"                      )
    (kam-beg-of-buf) (query-replace  "tvrmlglshape"             "TGLShape"                      )
    (kam-beg-of-buf) (query-replace  "tvrmltriangle"          "TTriangle")
    (kam-beg-of-buf) (query-replace  "pvrmltriangle"          "PTriangle")
    (kam-beg-of-buf) (query-replace  "tvrmlbasetrianglesoctree"            "TBaseTrianglesOctree")
    (kam-beg-of-buf) (query-replace  "tvrmlshapeoctree"                    "TShapeOctree")
    (kam-beg-of-buf) (query-replace  "tvrmltriangleoctree"                 "TTriangleOctree")
    (kam-beg-of-buf) (query-replace  "tvrmlbasetrianglesoctreenode"        "TBaseTrianglesOctreeNode")
    (kam-beg-of-buf) (query-replace  "tvrmlshapeoctreenode"                "TShapeOctreeNode")

    (kam-beg-of-buf) (query-replace  "tvrmlwireframeeffect" "TWireframeEffect")

    (kam-beg-of-buf) (query-replace  "TCastleMenuItemAccessory"      "TMenuAccessory")
    (kam-beg-of-buf) (query-replace  "TCastleMenuItemArgument"       "TMenuArgument")
    (kam-beg-of-buf) (query-replace  "TCastleMenuBooleanArgument"    "TMenuBooleanArgument")
    (kam-beg-of-buf) (query-replace  "TCastleMenuSlider"             "TMenuSlider")
    (kam-beg-of-buf) (query-replace  "TCastleMenuFloatSlider"        "TMenuFloatSlider")
    (kam-beg-of-buf) (query-replace  "TCastleMenuIntegerSlider"      "TMenuIntegerSlider")

;    (kam-beg-of-buf) (query-replace  "kambiscript:"              "castlescript:"       )

    (kam-beg-of-buf) (query-replace  "vrmlkambiscript"           "X3DCastleScript" )
    (kam-beg-of-buf) (query-replace  "kambiogg"                  "CastleOgg"                 )
    (kam-beg-of-buf) (query-replace  "kambiopenal"               "CastleOpenAL"              )
    (kam-beg-of-buf) (query-replace  "kambiurlutils"             "CastleURLUtils"            )
    (kam-beg-of-buf) (query-replace  "kambipng"                  "CastlePng"                 )
    (kam-beg-of-buf) (query-replace  "kambipropedits"            "CastlePropEdits"           )
    (kam-beg-of-buf) (query-replace  "kambiglcontrol"            "CastleGLControl"           )
    (kam-beg-of-buf) (query-replace  "kambilclutils"             "CastleLCLUtils"            )
    (kam-beg-of-buf) (query-replace  "kambiglutils"              "CastleGLUtils"             )
    (kam-beg-of-buf) (query-replace  "kambiglut"                 "CastleGlut"                )
    (kam-beg-of-buf) (query-replace  "kambixf86vmode"            "CastleXF86VMode"           )
    (kam-beg-of-buf) (query-replace  "kambiglx"                  "CastleGlx"                 )
    (kam-beg-of-buf) (query-replace  "kambitimeutils"            "CastleTimeUtils"           )
    (kam-beg-of-buf) (query-replace  "kambistringutils"          "CastleStringUtils"         )
    (kam-beg-of-buf) (query-replace  "kambiwarnings"             "CastleWarnings"            )
    (kam-beg-of-buf) (query-replace  "kambidynlib"               "CastleDynLib"              )
    (kam-beg-of-buf) (query-replace  "kambiclassutils"           "CastleClassUtils"          )
    (kam-beg-of-buf) (query-replace  "kambiinterfaces"           "CastleInterfaces"          )
    (kam-beg-of-buf) (query-replace  "kambixmlconfig"            "CastleXMLConfig"           )
    (kam-beg-of-buf) (query-replace  "kambixmlutils"             "CastleXMLUtils"            )
    (kam-beg-of-buf) (query-replace  "kambiparameters"           "CastleParameters"          )
    (kam-beg-of-buf) (query-replace  "kambizstream"              "CastleZStream"             )
    (kam-beg-of-buf) (query-replace  "kambilog"                  "CastleLog"                 )
    (kam-beg-of-buf) (query-replace  "kambiutils"                "CastleUtils"               )
    (kam-beg-of-buf) (query-replace  "kambifilesutils"           "CastleFilesUtils"          )
    (kam-beg-of-buf) (query-replace  "kambizlib"                 "CastleZLib"                )
    (kam-beg-of-buf) (query-replace  "kambiscenemanager"         "CastleSceneManager"        )
    (kam-beg-of-buf) (query-replace  "kambiscriptcorefunctions"  "CastleScriptCoreFunctions" )
    (kam-beg-of-buf) (query-replace  "kambiscriptimages"         "CastleScriptImages"        )
;    (kam-beg-of-buf) (query-replace  "kambiscript"               "CastleScript"              )
    (kam-beg-of-buf) (query-replace  "kambiscriptparser"         "CastleScriptParser"        )
    (kam-beg-of-buf) (query-replace  "kambiscriptarrays"         "CastleScriptArrays"        )
    (kam-beg-of-buf) (query-replace  "kambiscriptvectors"        "CastleScriptVectors"       )
    (kam-beg-of-buf) (query-replace  "kambiscriptlexer"          "CastleScriptLexer"         )
    (kam-beg-of-buf) (query-replace  "testkambistringutils"      "TestCastleStringUtils"     )
    (kam-beg-of-buf) (query-replace  "testkambiscriptvectors"    "TestCastleScriptVectors"   )
    (kam-beg-of-buf) (query-replace  "testkambiclassutils"       "TestCastleClassUtils"      )
    (kam-beg-of-buf) (query-replace  "testkambiscript"           "TestCastleScript"          )
;    (kam-beg-of-buf) (query-replace  "kambioctree" "CastleOctree")

    (kam-beg-of-buf) (query-replace  "castle_glwindow" "castle_window")
    (kam-beg-of-buf) (query-replace  "clean_glwindow" "clean_castle_window")
    (kam-beg-of-buf) (query-replace  "testglwindow" "TestCastleWindow")
    (kam-beg-of-buf) (query-replace  "GLWinMessagesTheme" "MessagesTheme")
    (kam-beg-of-buf) (query-replace  "{$I glwindow_" "{$I castlewindow_")
    (kam-beg-of-buf) (query-replace  "glwindow_winsystem" "castlewindow_winsystem")
    (kam-beg-of-buf) (query-replace  "glwindow_"                 "CASTLE_WINDOW_")
    (kam-beg-of-buf) (query-replace  "glwindowopen"                 "WindowOpen")
    (kam-beg-of-buf) (query-replace  "glwindowclose"                "WindowClose")
    (kam-beg-of-buf) (query-replace  "glwinmodes"                 "CastleWindowModes")
    (kam-beg-of-buf) (query-replace  "glwindowrecentfiles"        "CastleRecentFiles")
    (kam-beg-of-buf) (query-replace  "glwindow"                   "CastleWindow")
    (kam-beg-of-buf) (query-replace  "glwinmessages"              "CastleMessages")
    (kam-beg-of-buf) (query-replace  "glsoundmenu"                "CastleSoundMenu")
    (kam-beg-of-buf) (query-replace  "tglprogress"        "TWindowProgress")
    (kam-beg-of-buf) (query-replace  "glprogressinterface"        "WindowProgressInterface")
    (kam-beg-of-buf) (query-replace  "glprogress"                 "CastleProgress")
    (kam-beg-of-buf) (query-replace  "glwininputs"                "CastleInputs")
    (kam-beg-of-buf) (query-replace  "TCastleMenuVolumeSlider" "TMenuVolumeSlider")
    (kam-beg-of-buf) (query-replace  "TGLSoundInfoMenuItem"   "TSoundInfoMenuItem")
    (kam-beg-of-buf) (query-replace  "TGLSoundVolumeMenuItem" "TSoundVolumeMenuItem")
    (kam-beg-of-buf) (query-replace  "TGLMusicVolumeMenuItem" "TMusicVolumeMenuItem")

    (kam-beg-of-buf) (query-replace  "tglwindowdemo"          "TCastleWindowDemo"       )
    (kam-beg-of-buf) (query-replace  "tgluiwindow"            "TCastleWindowCustom"           )
    (kam-beg-of-buf) (query-replace  "tglwindowvrmlbrowser"   "TCastleWindow"                 )
    (kam-beg-of-buf) (query-replace  "tglwindow"              "TCastleWindowBase"       )

    (kam-beg-of-buf) (query-replace  "KamTimerResult"    "TimerResult")
    (kam-beg-of-buf) (query-replace  "KamTimerFrequency" "TimerFrequency")
    (kam-beg-of-buf) (query-replace  "KamTimer"          "Timer")
    (kam-beg-of-buf) (query-replace  "tkamtime"              "TFloatTime"       )
    (kam-beg-of-buf) (query-replace  "oldestvrmltime"              "OldestX3DTime"       )
    (kam-beg-of-buf) (query-replace  "tvrmltime"              "TX3DTime"       )
    (kam-beg-of-buf) (query-replace  "pvrmltime"              "PX3DTime"       )
    (kam-beg-of-buf) (query-replace  "kambi.cfg"  "castle-fpc.cfg")
    (kam-beg-of-buf) (query-replace  "KAMBI_FPC_OPTIONS"  "CASTLE_FPC_OPTIONS")


    (kam-beg-of-buf) (query-replace  "vrmlglscene"  "CastleScene")
    (kam-beg-of-buf) (query-replace  "vrmlshape"  "Shape")
    (kam-beg-of-buf) (query-replace  "vrmltriangle"  "Triangle")

    (kam-beg-of-buf) (query-replace  "TVRMLShaderProgram" "TX3DShaderProgram")
    (kam-beg-of-buf) (query-replace  "TVRMLShader" "TShader")
    (kam-beg-of-buf) (query-replace  "vrmlnodes_"                  "x3dnodes_")
    (kam-beg-of-buf) (query-replace  "vrmlglrenderer_" "glrenderer_")
    (kam-beg-of-buf) (query-replace  "vrmltime"                  "X3DTime")
    (kam-beg-of-buf) (query-replace  "vrmltriangleoctree"        "TriangleOctree")
    (kam-beg-of-buf) (query-replace  "vrmlfields"                "X3DFields")
    (kam-beg-of-buf) (query-replace  "vrmlscenewaypoints"        "SceneWaypoints")
    (kam-beg-of-buf) (query-replace  "vrmlnodesdetailoptions"    "X3DNodesDetailOptions")
    (kam-beg-of-buf) (query-replace  "vrmllexer"                 "X3DLexer")
    (kam-beg-of-buf) (query-replace  "vrmlglbackground"          "Background")
    (kam-beg-of-buf) (query-replace  "vrmlglrendererlights"      "GLRendererLights")
    (kam-beg-of-buf) (query-replace  "vrmlglrenderer"            "GLRenderer")
    (kam-beg-of-buf) (query-replace  "vrmlglanimation"           "PrecalculatedAnimation")
    (kam-beg-of-buf) (query-replace  "vrmlglrenderertextureenv"  "GLRendererTextureEnv")
    (kam-beg-of-buf) (query-replace  "vrmlshader"                "GLRendererShader")
    (kam-beg-of-buf) (query-replace  "vrmlanimation"             "PrecalculatedAnimationCore")
    (kam-beg-of-buf) (query-replace  "vrmlshadowmaps"            "X3DShadowMaps")
    (kam-beg-of-buf) (query-replace  "vrmlarraysgenerator"       "ArraysGenerator")
    (kam-beg-of-buf) (query-replace  "vrmlcamerautils"           "X3DCameraUtils")
    (kam-beg-of-buf) (query-replace  "vrmlraytracer"             "RayTracer")
    (kam-beg-of-buf) (query-replace  "vrmlnodes"                 "X3DNodes")
    (kam-beg-of-buf) (query-replace  "vrmllighting"              "Lighting")
    (kam-beg-of-buf) (query-replace  "vrmlshapeoctree"           "ShapeOctree")
    (kam-beg-of-buf) (query-replace  "vrmlscene"                 "CastleSceneCore")

    (kam-beg-of-buf) (query-replace  "tkamglfontcontrol" "TUIControlFont")
    (kam-beg-of-buf) (query-replace  "tkambuttonimagelayout" "TCastleButtonImageLayout")
    (kam-beg-of-buf) (query-replace  "tkampanel" "TCastlePanel")
    (kam-beg-of-buf) (query-replace  "tcastleimage" "TCastleImageControl")
    (kam-beg-of-buf) (query-replace  "tcastlemenu" "TCastleOnScreenMenu")

    (kam-beg-of-buf) (query-replace  "glnotifications" "CastleNotifications")
    (kam-beg-of-buf) (query-replace  "glcontrols" "CastleControls")
    (kam-beg-of-buf) (query-replace  "glmenu" "OnScreenMenu")
    (kam-beg-of-buf) (query-replace  "KamScript" "CasScript")

    (kam-beg-of-buf) (query-replace  "tvrmlaccesstype"                             "TX3DAccessType"                          )
    (kam-beg-of-buf) (query-replace  "tvrmlaccesstypes"                            "TX3DAccessTypes"                         )
    (kam-beg-of-buf) (query-replace  "tvrmlbindablestack"                          "TX3DBindableStack"                       )
    (kam-beg-of-buf) (query-replace  "tvrmlbindablestackbasic"                     "TX3DBindableStackBasic"                  )
    (kam-beg-of-buf) (query-replace  "tvrmlcameraversion"                          "TX3DCameraVersion"                       )
    (kam-beg-of-buf) (query-replace  "tvrmlchange"                                 "TX3DChange"                              )
    (kam-beg-of-buf) (query-replace  "tvrmlchanges"                                "TX3DChanges"                             )
    (kam-beg-of-buf) (query-replace  "tvrmlevent"                                  "TX3DEvent"                               )
    (kam-beg-of-buf) (query-replace  "tvrmleventlist"                              "TX3DEventList"                           )
    (kam-beg-of-buf) (query-replace  "tvrmleventreceive"                           "TX3DEventReceive"                        )
    (kam-beg-of-buf) (query-replace  "tvrmleventreceivelist"                       "TX3DEventReceiveList"                    )
    (kam-beg-of-buf) (query-replace  "tvrmleventsengine"                           "TX3DEventsEngine"                        )
    (kam-beg-of-buf) (query-replace  "tvrmleventsenginelist"                       "TX3DEventsEngineList"                    )
    (kam-beg-of-buf) (query-replace  "tvrmlexport"                                 "TX3DExport"                              )
    (kam-beg-of-buf) (query-replace  "tvrmlexternalprototype"                      "TX3DExternalPrototype"                   )
    (kam-beg-of-buf) (query-replace  "tvrmlfield"                                  "TX3DField"                               )
    (kam-beg-of-buf) (query-replace  "tvrmlfieldclass"                             "TX3DFieldClass"                          )
    (kam-beg-of-buf) (query-replace  "tvrmlfieldlist"                              "TX3DFieldList"                           )
    (kam-beg-of-buf) (query-replace  "tvrmlfieldorevent"                           "TX3DFieldOrEvent"                        )
    (kam-beg-of-buf) (query-replace  "tvrmlfieldoreventlist"                       "TX3DFieldOrEventList"                    )
    (kam-beg-of-buf) (query-replace  "tvrmlfileitem"                               "TX3DFileItem"                            )
    (kam-beg-of-buf) (query-replace  "tvrmlfileitemlist"                           "TX3DFileItemList"                        )
    (kam-beg-of-buf) (query-replace  "tvrmlfontfamily"                             "TX3DFontFamily"                          )
    (kam-beg-of-buf) (query-replace  "tvrmlfontjustify"                            "TX3DFontJustify"                         )
    (kam-beg-of-buf) (query-replace  "tvrmlglslprogram"                            "TX3DGLSLProgram"                         )
    (kam-beg-of-buf) (query-replace  "tvrmlgraphtraversestate"                     "TX3DGraphTraverseState"                  )
    (kam-beg-of-buf) (query-replace  "tvrmlgraphtraversestatestack"                "TX3DGraphTraverseStateStack"             )
    (kam-beg-of-buf) (query-replace  "tvrmlimport"                                 "TX3DImport"                              )
    (kam-beg-of-buf) (query-replace  "tvrmlimportablenames"                        "TX3DImportableNames"                     )
    (kam-beg-of-buf) (query-replace  "tvrmlinterfacedeclaration"                   "TX3DInterfaceDeclaration"                )
    (kam-beg-of-buf) (query-replace  "tvrmlinterfacedeclarationlist"               "TX3DInterfaceDeclarationList"            )
    (kam-beg-of-buf) (query-replace  "tvrmlkeyword"                                "TX3DKeyword"                             )
    (kam-beg-of-buf) (query-replace  "tvrmlkeywords"                               "TX3DKeywords"                            )
    (kam-beg-of-buf) (query-replace  "tvrmlmaterialinfo"                           "TX3DMaterialInfo"                        )
    (kam-beg-of-buf) (query-replace  "tvrmlmaterialinfoabstract"                   "TX3DMaterialInfoAbstract"                )
    (kam-beg-of-buf) (query-replace  "tvrmlmaterialinfo_1"                         "TX3DMaterialInfo_1"                      )
    (kam-beg-of-buf) (query-replace  "tvrmlmultfield"                              "TX3DMultField"                           )
    (kam-beg-of-buf) (query-replace  "tvrmlnames"                                  "TX3DNames"                               )
    (kam-beg-of-buf) (query-replace  "tvrmloctreeignoreforshadowraysandoneitem"    "TOctreeIgnoreForShadowRaysAndOneItem"    )
    (kam-beg-of-buf) (query-replace  "tvrmlprototype"                              "TX3DPrototype"                           )
    (kam-beg-of-buf) (query-replace  "tvrmlprototypebase"                          "TX3DPrototypeBase"                       )
    (kam-beg-of-buf) (query-replace  "tvrmlprototypebaselist"                      "TX3DPrototypeBaseList"                   )
    (kam-beg-of-buf) (query-replace  "tvrmlprototypenames"                         "TX3DPrototypeNames"                      )
    (kam-beg-of-buf) (query-replace  "tvrmlrenderershape"                          "TX3DRendererShape"                       )
    (kam-beg-of-buf) (query-replace  "tvrmlrenderingattributes"                    "TX3DRenderingAttributes"                 )
    (kam-beg-of-buf) (query-replace  "tvrmlrenderingattributesclass"               "TX3DRenderingAttributesClass"            )
    (kam-beg-of-buf) (query-replace  "tvrmlroute"                                  "TX3DRoute"                               )
    (kam-beg-of-buf) (query-replace  "tvrmlroutelist"                              "TX3DRouteList"                           )
    (kam-beg-of-buf) (query-replace  "tvrmlsimplemultfield"                        "TX3DSimpleMultField"                     )
    (kam-beg-of-buf) (query-replace  "tvrmlsinglefield"                            "TX3DSingleField"                         )
    (kam-beg-of-buf) (query-replace  "tvrmlsinglefieldclass"                       "TX3DSingleFieldClass"                    )
    (kam-beg-of-buf) (query-replace  "tvrmlsinglefieldlist"                        "TX3DSingleFieldList"                     )
    (kam-beg-of-buf) (query-replace  "tvrmltoken"                                  "TX3DToken"                               )
    (kam-beg-of-buf) (query-replace  "tvrmltokens"                                 "TX3DTokens"                              )
    (kam-beg-of-buf) (query-replace  "tvrmlviewpointclassnode"                     "TX3DViewpointClassNode"                  )
    (kam-beg-of-buf) (query-replace  "tkamabstractviewport"                        "TCastleAbstractViewport"                 )
    (kam-beg-of-buf) (query-replace  "tkamabstractviewportlist"                    "TCastleAbstractViewportList"             )
    (kam-beg-of-buf) (query-replace  "tkamobjectlist"                              "TCastleObjectList"                       )
    (kam-beg-of-buf) (query-replace  "tkamobjectqueue"                             "TCastleObjectQueue"                      )
    (kam-beg-of-buf) (query-replace  "tkamobjectstack"                             "TCastleObjectStack"                      )
    (kam-beg-of-buf) (query-replace  "tkamstringlist"                              "TCastleStringList"                       )
    (kam-beg-of-buf) (query-replace  "tkamviewport"                                "TCastleViewport"                         )
    (kam-beg-of-buf) (query-replace  "loadfromvrmlevents"                                "LoadFromEvents"                         )

    (kam-beg-of-buf) (query-replace  "KamCoTan"                 "CastleCoTan")
    (kam-beg-of-buf) (query-replace  "KamDivMod"                "CastleDivMod")
    (kam-beg-of-buf) (query-replace  "KamGLPolygonStipple"      "CastleGLPolygonStipple")
    (kam-beg-of-buf) (query-replace  "KamGluSphere"             "CastleGluSphere")
    (kam-beg-of-buf) (query-replace  "KamGL_CLAMP_TO_EDGE"      "CastleGL_CLAMP_TO_EDGE")
    (kam-beg-of-buf) (query-replace  "KamReadLink"              "CastleReadLink")

    (kam-beg-of-buf) (query-replace  "t3dscene"              "TCastleScene")
    (kam-beg-of-buf) (query-replace  "t3dprecalculatedanimation"              "TCastlePrecalculatedAnimation")

    (kam-beg-of-buf) (query-replace  "savevrml"              "Save3D")
    (kam-beg-of-buf) (query-replace  "loadvrmlclassic"       "LoadX3DClassic")
    (kam-beg-of-buf) (query-replace  "loadvrml"              "Load3D")
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "openglwin" "WindowOpen")
    (kam-beg-of-buf) (query-replace "openwindow" "WindowOpen")
    (kam-beg-of-buf) (query-replace "closeglwin" "WindowClose")
    (kam-beg-of-buf) (query-replace "closewindow" "WindowClose")

    (kam-beg-of-buf) (query-replace "Window.OnOpenList" "OnGLContextOpen")
    (kam-beg-of-buf) (query-replace "WindowOpen(Window: TCastleWindowBase)" "WindowOpen(const Container: IUIContainer)")

    (kam-beg-of-buf) (query-replace "Window.OnCloseList" "OnGLContextClose")
    (kam-beg-of-buf) (query-replace "WindowClose(Window: TCastleWindowBase)" "WindowClose(const Container: IUIContainer)")
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "TRUE" "true")
    (kam-beg-of-buf) (query-replace "FALSE" "false")
    (kam-beg-of-buf) (query-replace "NULL" "null")
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

;; hacky way to make comint-output-filter-functions *not* contain comint-watch-for-password-prompt
;; (which breaks using history for passwords, which is sometimes uncomfortable)
(setq comint-output-filter-functions '(ansi-color-process-output comint-postoutput-scroll-to-bottom))

;; doesn't work, comint-output-filter-functions is special locally?
(setq comint-output-filter-functions
  (delete 'comint-watch-for-password-prompt comint-output-filter-functions))

(defun kam-aaa ()
  (interactive)
  (save-excursion

    (kam-beg-of-buf) (query-replace  "Shaders"                           "CastleShaders"                            )
    (kam-beg-of-buf) (query-replace  "ALUtils"				 "CastleALUtils"                            )
    (kam-beg-of-buf) (query-replace  "ArraysGenerator"			 "CastleArraysGenerator"                    )
    (kam-beg-of-buf) (query-replace  "Background"			 "CastleBackground"                         )
    (kam-beg-of-buf) (query-replace  "Cameras"				 "CastleCameras"                            )
    (kam-beg-of-buf) (query-replace  "CubeMap"				 "CastleCubeMaps"                           )
    (kam-beg-of-buf) (query-replace  "Curve"				 "CastleCurves"                             )
    (kam-beg-of-buf) (query-replace  "DataURI"				 "CastleDataURI"                            )
    (kam-beg-of-buf) (query-replace  "DDS"				 "CastleDDS"                                )
    (kam-beg-of-buf) (query-replace  "EFX"				 "CastleEFX"                                )
    (kam-beg-of-buf) (query-replace  "EnumerateFiles"			 "CastleEnumerateFiles"                     )
    (kam-beg-of-buf) (query-replace  "FileFilters"			 "CastleFileFilters"                        )
    (kam-beg-of-buf) (query-replace  "GenericStructList"		 "CastleGenericLists"                       )
    (kam-beg-of-buf) (query-replace  "GeometryArrays"			 "CastleGeometryArrays"                     )
    (kam-beg-of-buf) (query-replace  "NURBS"				 "CastleNURBS"                              )
    (kam-beg-of-buf) (query-replace  "OnScreenMenu"			 "CastleOnScreenMenu"                       )
    (kam-beg-of-buf) (query-replace  "OnScreenMenuImages"		 "CastleOnScreenMenuImages"                 )
    (kam-beg-of-buf) (query-replace  "PrecalculatedAnimation"		 "CastlePrecalculatedAnimation"             )
    (kam-beg-of-buf) (query-replace  "PrecalculatedAnimationCore"	 "CastlePrecalculatedAnimationCore"         )
    (kam-beg-of-buf) (query-replace  "ProgressConsole"			 "CastleProgressConsole"                    )
    (kam-beg-of-buf) (query-replace  "Quaternions"			 "CastleQuaternions"                        )
    (kam-beg-of-buf) (query-replace  "RaysWindow"			 "CastleRays"                               )
    (kam-beg-of-buf) (query-replace  "RayTracer"			 "CastleRayTracer"                          )
    (kam-beg-of-buf) (query-replace  "SectorsWaypoints"			 "CastleSectors"                            )
    (kam-beg-of-buf) (query-replace  "ShapeOctree"			 "CastleShapeOctree"                        )
    (kam-beg-of-buf) (query-replace  "SoundFile"			 "CastleSoundFile"                          )
    (kam-beg-of-buf) (query-replace  "SpaceFillingCurves"		 "CastleSpaceFillingCurves"                 )
    (kam-beg-of-buf) (query-replace  "SphereSampling"			 "CastleSphereSampling"                     )
    (kam-beg-of-buf) (query-replace  "SphericalHarmonics"		 "CastleSphericalHarmonics"                 )
    (kam-beg-of-buf) (query-replace  "TextureImages"			 "CastleTextureImages"                      )
    (kam-beg-of-buf) (query-replace  "TriangleOctree"			 "CastleTriangleOctree"                     )
    (kam-beg-of-buf) (query-replace  "UIControls"			 "CastleUIControls"                         )
    (kam-beg-of-buf) (query-replace  "Videos"				 "CastleVideos"                             )
    (kam-beg-of-buf) (query-replace  "VorbisCodec"			 "CastleVorbisCodec"                        )
    (kam-beg-of-buf) (query-replace  "VorbisDecoder"			 "CastleVorbisDecoder"                      )
    (kam-beg-of-buf) (query-replace  "VorbisFile"			 "CastleVorbisFile"                         )
    (kam-beg-of-buf) (query-replace  "WindowModes"			 "CastleWindowModes"                        )
    (kam-beg-of-buf) (query-replace  "WindowsFonts"			 "CastleWindowsFonts"                       )
    (kam-beg-of-buf) (query-replace  "WinFontConvert"			 "CastleWinFontConvert"                     )
    (kam-beg-of-buf) (query-replace  "XlibUtils"			 "CastleXlib"                               )
    (kam-beg-of-buf) (query-replace  "GLCubeMap"			 "CastleGLCubeMap"                          )
    (kam-beg-of-buf) (query-replace  "GLImages"				 "CastleGLImages"                           )
    (kam-beg-of-buf) (query-replace  "GLRenderer"			 "CastleRenderer"                         )
    (kam-beg-of-buf) (query-replace  "GLRendererLights"			 "CastleRendererLights"                   )
    (kam-beg-of-buf) (query-replace  "GLRendererShader"			 "CastleRendererShader"                   )
    (kam-beg-of-buf) (query-replace  "GLRendererTextureEnv"		 "CastleRendererTextureEnv"               )
    (kam-beg-of-buf) (query-replace  "GLShaders"			 "CastleGLShaders"                          )
    (kam-beg-of-buf) (query-replace  "GLShadowVolumeRenderer"		 "CastleGLShadowVolumes"                    )
    (kam-beg-of-buf) (query-replace  "GLVersionUnit"			 "CastleGLVersion"                          )
  )
)
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-aaa ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace  "idle"       "Update"        )
    (kam-beg-of-buf) (query-replace  "CompSpeed"  "SecondsPassed" )
    (kam-beg-of-buf) (query-replace  "updatespeed"  "UpdateSecondsPassed" )
  ))
(global-set-key (kbd "<f5>") 'kam-aaa)

(defun kam-cge-vector-api-upgrade ()
  (interactive)
  (save-excursion
    ;; this needs to be early, otherwise the type replacement below with handle "Vector3Single" replacement
    (kam-beg-of-buf) (query-replace-regexp "Vector3SingleCut(\\([^-+*,()]+\\))" "\\1.XYZ")
    (kam-beg-of-buf) (query-replace-regexp "Vector3SingleCut(\\([^()]+\\))" "(\\1).XYZ")

    ;; types and constructor names

    (kam-beg-of-buf) (query-replace  "ZeroVector2Single"       "TVector2.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector3Single"       "TVector3.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector4Single"       "TVector4.Zero")

    (kam-beg-of-buf) (query-replace  "ZeroVector2Double"       "TVector2Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector3Double"       "TVector3Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroVector4Double"       "TVector4Double.Zero")

    (kam-beg-of-buf) (query-replace  "ZeroMatrix2Single"       "TMatrix2.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix3Single"       "TMatrix3.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix4Single"       "TMatrix4.Zero")

    (kam-beg-of-buf) (query-replace  "ZeroMatrix2Double"       "TMatrix2Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix3Double"       "TMatrix3Double.Zero")
    (kam-beg-of-buf) (query-replace  "ZeroMatrix4Double"       "TMatrix4Double.Zero")

    (kam-beg-of-buf) (query-replace  "IdentityMatrix2Single"       "TMatrix2.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix3Single"       "TMatrix3.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix4Single"       "TMatrix4.Identity")

    (kam-beg-of-buf) (query-replace  "IdentityMatrix2Double"       "TMatrix2Double.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix3Double"       "TMatrix3Double.Identity")
    (kam-beg-of-buf) (query-replace  "IdentityMatrix4Double"       "TMatrix4Double.Identity")

    (kam-beg-of-buf) (query-replace  "Vector2Single"       "Vector2")
    (kam-beg-of-buf) (query-replace  "Vector3Single"       "Vector3")
    (kam-beg-of-buf) (query-replace  "Vector4Single"       "Vector4")

    (kam-beg-of-buf) (query-replace  "Matrix2Single"       "Matrix2")
    (kam-beg-of-buf) (query-replace  "Matrix3Single"       "Matrix3")
    (kam-beg-of-buf) (query-replace  "Matrix4Single"       "Matrix4")

    (kam-beg-of-buf) (query-replace  "Triangle2Single"       "Triangle2")
    (kam-beg-of-buf) (query-replace  "Triangle3Single"       "Triangle3")
    (kam-beg-of-buf) (query-replace  "Triangle4Single"       "Triangle4")

    (kam-beg-of-buf) (query-replace  "UnitVector2Single" "TVector3.One")
    (kam-beg-of-buf) (query-replace  "UnitVector2" "TVector3.One")

    (kam-beg-of-buf) (query-replace  "UnitVector3Single" "TVector3.One")
    (kam-beg-of-buf) (query-replace  "UnitVector3" "TVector3.One")

    (kam-beg-of-buf) (query-replace  "UnitVector4Single" "TVector3.One")
    (kam-beg-of-buf) (query-replace  "UnitVector4" "TVector3.One")

    (kam-beg-of-buf) (query-replace  "EmptyBox3D" "TBox3D.Empty")

    (kam-beg-of-buf) (query-replace  "VectorProduct" "TVector3.CrossProduct")
    (kam-beg-of-buf) (query-replace  "VectorDotProduct" "TVector3.DotProduct")
    (kam-beg-of-buf) (query-replace  "VectorsPerfectlyEqual" "TVector3.PerfectlyEquals")
    (kam-beg-of-buf) (query-replace  "MatricesPerfectlyEqual" "TMatrix4.PerfectlyEquals")
    (kam-beg-of-buf) (query-replace  "FloatsEqual" "SameValue")
    (kam-beg-of-buf) (query-replace  "VectorsEqual" "TVector3.Equals")
    (kam-beg-of-buf) (query-replace  "TriangleDir(" "TriangleDirection(")

    ;; this is actually a change in CGE 6.2
    (kam-beg-of-buf) (query-replace  "PointInside" "Contains")
    (kam-beg-of-buf) (query-replace  "GLFadeRectangle(" "GLFadeRectangleDark(")

    ;; 0-argument vector/matrix methods
    (kam-beg-of-buf) (query-replace-regexp "VectorLenSqr(\\([^-+*,()]+\\))" "\\1.LengthSqr")
    (kam-beg-of-buf) (query-replace-regexp "VectorLenSqr(\\([^()]+\\))" "(\\1).LengthSqr")

    (kam-beg-of-buf) (query-replace-regexp "VectorLen(\\([^-+*,()]+\\))" "\\1.Length")
    (kam-beg-of-buf) (query-replace-regexp "VectorLen(\\([^()]+\\))" "(\\1).Length")

    (kam-beg-of-buf) (query-replace-regexp "Normalized(\\([^-+*,()]+\\))" "\\1.Normalize")
    (kam-beg-of-buf) (query-replace-regexp "Normalized(\\([^()]+\\))" "(\\1).Normalize")

    (kam-beg-of-buf) (query-replace-regexp "VectorToNiceStr(\\([^-+*,()]+\\))" "\\1.ToString")
    (kam-beg-of-buf) (query-replace-regexp "VectorToNiceStr(\\([^()]+\\))" "(\\1).ToString")

    (kam-beg-of-buf) (query-replace-regexp "VectorToRawStr(\\([^-+*,()]+\\))" "\\1.ToRawString")
    (kam-beg-of-buf) (query-replace-regexp "VectorToRawStr(\\([^()]+\\))" "(\\1).ToRawString")

    (kam-beg-of-buf) (query-replace-regexp "VectorAverage(\\([^-+*,()]+\\))" "\\1.Average")
    (kam-beg-of-buf) (query-replace-regexp "VectorAverage(\\([^()]+\\))" "(\\1).Average")

    (kam-beg-of-buf) (query-replace-regexp "PerfectlyZeroVector(\\([^-+*,()]+\\))" "\\1.IsPerfectlyZero")
    (kam-beg-of-buf) (query-replace-regexp "PerfectlyZeroVector(\\([^()]+\\))" "(\\1).IsPerfectlyZero")

    ;; after PerfectlyZeroVector
    (kam-beg-of-buf) (query-replace-regexp "ZeroVector(\\([^-+*,()]+\\))" "\\1.IsZero")
    (kam-beg-of-buf) (query-replace-regexp "ZeroVector(\\([^()]+\\))" "(\\1).IsZero")

    (kam-beg-of-buf) (query-replace-regexp "TriangleNormPlane(\\([^-+*,()]+\\))" "\\1.NormalizedPlane")
    (kam-beg-of-buf) (query-replace-regexp "TriangleNormPlane(\\([^()]+\\))" "(\\1).NormalizedPlane")

    (kam-beg-of-buf) (query-replace-regexp "FloatToNiceStr(\\([^-+*,()]+\\))" "Format('%f', [\\1])")
    (kam-beg-of-buf) (query-replace-regexp "FloatToNiceStr(\\([^()]+\\))" "Format('%f', [(\\1)])")

    ;; 2-argument vector/matrix methods
    (kam-beg-of-buf) (query-replace-regexp "VectorAdjustToLength(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.AdjustToLength(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixMultPoint(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.MultPoint(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixMultDirection(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.MultDirection(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "TryMatrixInverse(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.TryInverse(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixToNiceStr(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.ToString(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixToRawStr(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.ToRawString(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "MatrixRow(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.Row(\\2)")
    (kam-beg-of-buf) (query-replace-regexp "TriangleTransform(\\([^-+*,()]+\\), *\\([^-+*,()]+\\))" "\\1.Transform(\\2)")

    ;; misc changes
    (kam-beg-of-buf) (query-replace-regexp "Box.Data\\[\\(.\\), \\(.\\)\\]" "Box.Data[\\1].Data[\\2]")
  ))
(global-set-key (kbd "<f5>") 'kam-cge-vector-api-upgrade)

(global-set-key (kbd "<f5>") 'delete-matching-lines)
------------------------------------------------------------------------------
(declare-function 'diff-hunk-next "diff-mode")

(defun my-diff-refine-all ()
  "Refine all diffs."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (condition-case nil
        (diff-beginning-of-hunk t)
      (error (diff-hunk-next)))
    (condition-case nil
        (while (not (eobp))
          (diff-refine-hunk)
          (diff-hunk-next))
      (user-error) ;; catches "No next hunk." from `diff-hunk-next' if there is garbage at the end of the file.
      )))

(defun my-diff-hunks-highlight-all ()
  "Highlight all hunks in diff-mode."
  (add-hook 'font-lock-mode-hook #'my-diff-refine-all t t))
------------------------------------------------------------------------------
(defun kam-cge-delphi-upgrade ()
  (interactive)
  (save-excursion
    (kam-beg-of-buf) (query-replace "(specialize " "({$ifdef CASTLE_OBJFPC}specialize{$endif} ")
    (kam-beg-of-buf) (query-replace "= specialize " "= {$ifdef CASTLE_OBJFPC}specialize{$endif} ")
    (kam-beg-of-buf) (query-replace-regexp "\\([-. (]\\)L\\[" "\\1List^[")
    (kam-beg-of-buf) (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\([-+*/]\\)= \\([^-+*/;]+\\);" " \\1 := \\1 \\3 \\4;")
    ;; if the expression on right-hand side has some +/*/etc then
    ;; for safety it needs parenthesis (for accuracy, in case of +=,
    ;; and for correctness in case of others)
    (kam-beg-of-buf) (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\([-+*/]\\)= \\([^;]+?\\)\\(;\\| else\\)"     " \\1 := \\1 \\3 (\\4)\\5")
    (kam-beg-of-buf) (query-replace-regexp "\\(T\\|P\\)Array_\\([a-zA-Z0-9_]+\\)" "\\1\\2Array")

    (kam-beg-of-buf) (query-replace "%00000001" "$01 { binary 00000001 }")
    (kam-beg-of-buf) (query-replace "%11111110" "$FE { binary 11111110 }")
    (kam-beg-of-buf) (query-replace "%00000011" "$03 { binary 00000011 }")
    (kam-beg-of-buf) (query-replace "%11111100" "$FC { binary 11111100 }")
    (kam-beg-of-buf) (query-replace "%00000111" "$07 { binary 00000111 }")
    (kam-beg-of-buf) (query-replace "%11111000" "$F8 { binary 11111000 }")
    (kam-beg-of-buf) (query-replace "%00001111" "$0F { binary 00001111 }")
    (kam-beg-of-buf) (query-replace "%11110000" "$F0 { binary 11110000 }")
    (kam-beg-of-buf) (query-replace "%00011111" "$1F { binary 00011111 }")
    (kam-beg-of-buf) (query-replace "%11100000" "$E0 { binary 11100000 }")
    (kam-beg-of-buf) (query-replace "%00111111" "$3F { binary 00111111 }")
    (kam-beg-of-buf) (query-replace "%11000000" "$C0 { binary 11000000 }")
    (kam-beg-of-buf) (query-replace "%01111111" "$7F { binary 01111111 }")
    (kam-beg-of-buf) (query-replace "%10000000" "$80 { binary 10000000 }")
    (kam-beg-of-buf) (query-replace "%11111111" "$FF { binary 11111111 }")
    (kam-beg-of-buf) (query-replace "%00000000" "$00 { binary 00000000 }")

    (kam-beg-of-buf) (query-replace-regexp " DepthRange" " RenderContext.DepthRange")
    (kam-beg-of-buf) (query-replace-regexp " ProjectionMatrix" " RenderContext.ProjectionMatrix")
  ))
(global-set-key (kbd "<f5>") 'kam-cge-delphi-upgrade)
(defun kam-cge-delphi-upgrade-dangerous ()
  (interactive)
  ;; replacements to negate sign for += and 0-, only for 1-argument right hand side
  (save-excursion
    (query-replace-regexp " \\+= -" " -= ")
  )
  (save-excursion
    (query-replace-regexp " -= -" " += ")
  )
  ;; replacements for += and -= to Inc and Dec, only for integers
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\+= \\([^-+*/;]+\\);" " Inc(\\1, \\3);")
  )
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) -= \\([^-+*/;]+\\);" " Dec(\\1, \\3);")
  )
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) \\+= \\([^;]+?\\)\\(;\\| else\\)"     " Inc(\\1, \\3)\\4")
  )
  (save-excursion
    (query-replace-regexp " \\([^ =]+\\(\\[[^]=]+\\][^ =]*\\)*\\) -= \\([^;]+?\\)\\(;\\| else\\)"     " Dec(\\1, \\3)\\4")
  )
  ;; replacement of @, only when used to callbacks
  (save-excursion
    (query-replace "@" "{$ifdef CASTLE_OBJFPC}@{$endif} ")
  )
)
(global-set-key (kbd "<f6>") 'kam-cge-delphi-upgrade-dangerous)
