;;; Derived mode for C editing.
(define-derived-mode starc-mode c-mode "StarC" "Major mode for editing C"

;;; Initial string for new files.
  (make-local-variable 'edstar-initial-string)
  (setq edstar-initial-string "<[module]>\n")

;;; Placeholder delimiters.
  (make-local-variable 'edstar-placeholder-begin)
  (setq edstar-placeholder-begin "<{")
  (make-local-variable 'edstar-placeholder-end)
  (setq edstar-placeholder-end "}>")
  (make-local-variable 'edstar-placeholder-begin-opt)
  (setq edstar-placeholder-begin-opt "<[")
  (make-local-variable 'edstar-placeholder-end-opt)
  (setq edstar-placeholder-end-opt "]>")

  (modify-syntax-entry ?_ "w" starc-mode-syntax-table)
  (modify-syntax-entry ?# "w" starc-mode-syntax-table)

;;; Indent using spaces only.
  (make-local-variable 'indent-tabs-mode)
  (setq indent-tabs-mode nil)

;;; Indentation levels.
  (make-local-variable 'c-basic-offset)
  (setq c-basic-offset 3)
  (make-local-variable 'c-comment-only-line-offset)
  (setq c-comment-only-line-offset -1000)
  (make-local-variable 'c-label-minimum-indentation)
  (setq c-label-minimum-indentation 0)
  (make-local-variable 'c-comment-continuation-stars)
  (setq c-comment-continuation-stars "* ")

;  (make-local-variable 'c-indent-level)
;  (setq c-indent-level 3)
;  (make-local-variable 'c-continued-statement-offset)
;  (setq c-continued-statement-offset 0)
;  (make-local-variable 'c-argdecl-index)
;  (setq c-argdecl-index 3)
;  (make-local-variable 'c-label-offset)
;  (setq c-label-offset 0)

;;; Key definitions.
  (define-key starc-mode-map "\t" 'indent-according-to-mode)
  (define-key starc-mode-map "#"  'edstar-self-insert)

;;;EXPERIMENTAL BIT
(let (new-ast-routine)

;;; Define a local function to create a token for an AST routine with
;;; associated argument placeholders and a helpkey.
  (fset 'new-ast-routine
        (lambda (name desc args &optional help)
          (if (not help) (setq help name))         ; use routine name to find
                                                   ; help by default
          (new-routine name desc args
                       (list (cons 'helpkey name)) ; routine name is helpkey so
                                                   ; put this into plist
                       "starc")
          (new-helpkey name (list "sun211" help)   ; this is where the external
                                                   ; help can be found
                       nil "starc")))

  (new-ast-routine "astAddFrame"
                   "Add a Frame to a FrameSet to define a new coordinate system"
                   '("this" "iframe"))
  (new-ast-routine "astAnnul"
                   "Annul a pointer to an Object"
                   '("this"))
  (new-ast-routine "astBegin"
                   "Begin a new AST context"
                   nil)
  (new-ast-routine "astBorder"
                   "Draw a border around valid regions of a Plot"
                   '("this"))
  (new-ast-routine "astChannel"
                   "Create a Channel"
                   '("source" "sink" "options" "..."))
  (new-ast-routine "astClear"
                   "Clear attribute values for an Object"
                   '("this" "attrib"))
  (new-ast-routine "astClearStatus"
                   "Clear the AST error status"
                   nil)
  (new-ast-routine "astClip"
                   "Set up or remove clipping for a Plot"
                   '("this" "iframe" "lbnd" "ubnd"))
  (new-ast-routine "astClone"
                   "Clone (duplicate) an Object pointer"
                   '("this"))
  (new-ast-routine "astCmpFrame"
                   "Create a CmpFrame"
                   '("frame1" "frame2" "options" "..."))
  (new-ast-routine "astCmpMap"
                   "Create a CmpMap"
                   '("map1" "map2" "series" "options" "..."))
  (new-ast-routine "astConvert"
                   "Determine how to convert between two coordinate systems"
                   '("from" "to" "domainlist"))
  (new-ast-routine "astCopy"
                   "Copy an Object"
                   '("this"))
  (new-ast-routine "astCurve"
                   "Draw a geodesic curve"
                   '("this" "start" "finish"))
  (new-ast-routine "astDelFits"
                   "Delete the current FITS card in a FitsChan"
                   '("this"))
  (new-ast-routine "astDelete"
                   "Delete an Object"
                   '("this"))
  (new-ast-routine "astDistance"
                   "Calculate the distance between two points in a Frame"
                   '("this" "point1" "point2"))
  (new-ast-routine "astEnd"
                   "End an AST context"
                   nil)
  (new-ast-routine "astExempt"
                   "Exempt an Object pointer from AST context handling"
                   '("this"))
  (new-ast-routine "astExport"
                   "Export an Object pointer to an outer context"
                   '("this"))
  (new-ast-routine "astFindFits"
                   "Find a FITS card in a FitsChan by keyword"
                   '("this" "card" "inc"))
  (new-ast-routine "astFindFrame"
                   "Find a coordinate system with specified characteristics"
                   '("target" "template" "domainlist"))
  (new-ast-routine "astFitsChan"
                   "Create a FitsChan"
                   '("source" "sink" "options" "..."))
  (new-ast-routine "astFormat"
                   "Format a coordinate value for a Frame axis"
                   '("this" "axis" "value"))
  (new-ast-routine "astFrame"
                   "Create a Frame"
                   '("naxes" "options" "..."))
  (new-ast-routine "astFrameSet"
                   "Create a FrameSet"
                   '("frame" "options" "..."))
  (new-ast-routine "astGetX"
                   "Get an attribute value for an Object"
                   '("this" "attrib"))
  (new-ast-routine "astGetFrame"
                   "Obtain a pointer to a specified Frame in a FrameSet"
                   '("this" "iframe"))
  (new-ast-routine "astGetMapping"
                   "Obtain a Mapping that converts between two Frames in a FrameSet"
                   '("this" "iframe1" "iframe2"))
  (new-ast-routine "astGrid"
                   "Draw a set of labelled coordinate axes"
                   '("this"))
  (new-ast-routine "astGridLine"
                   "Draw a grid line (or axis) for a Plot"
                   '("this" "axis" "start" "length"))
  (new-ast-routine "astIntraMap"
                   "Create an IntraMap"
                   '("name" "nin" "nout" "options" "..."))
  (new-ast-routine "astIntraReg"
                   "Register a transformation function for use by an IntraMap"
                   '("name" "nin" "nout" "tran" "flags" "purpose"
                   "author" "contact"))
  (new-ast-routine "astInvert"
                   "Invert a Mapping"
                   '("this"))
  (new-ast-routine "astIsAClass"
                   "Test membership of a class by an Object"
                   '("this"))
  (new-ast-routine "astLutMap"
                   "Create a LutMap"
                   '("nlut" "lut" "start" "inc" "options" "..."))
  (new-ast-routine "astMapBox"
                   "Find a bounding box for a Mapping"
                   '("this" "lbnd_in" "ubnd_in" "forward" "coord_out"
                   "lbnd_out" "ubnd_out" "xl" "xu"))
  (new-ast-routine "astMark"
                   "Draw a set of markers for a Plot"
                   '("this" "nmark" "ncoord" "indim" "in" "type"))
  (new-ast-routine "astMathMap"
                   "Create a MathMap"
                   '("nin" "nout" "nfwd" "fwd" "ninv" "inv" "options" "..."))
  (new-ast-routine "astMatrixMap"
                   "Create a MatrixMap"
                   '("nin" "nout" "form" "matrix" "options" "..."))
  (new-ast-routine "astNorm"
                   "Normalise a set of Frame coordinates"
                   '("this" "value"))
  (new-ast-routine "astOK"
                   "Test whether AST functions have been successful"
                   nil)
  (new-ast-routine "astOffset"
                   "Calculate an offset along a geodesic curve"
                   '("this" "point1" "point2" "offset" "point3"))
  (new-ast-routine "astPcdMap"
                   "Create a PcdMap"
                   '("disco" "pcdcen" "options" "..."))
  (new-ast-routine "astPermAxes"
                   "Permute the axis order in a Frame"
                   '("this" "perm"))
  (new-ast-routine "astPermMap"
                   "Create a PermMap"
                   '("nin" "inperm" "nout" "outperm" "constant"
                   "options" "..."))
  (new-ast-routine "astPickAxes"
                   "Create a new Frame by picking axes from an existing one"
                   '("this" "naxes" "axes" "map"))
  (new-ast-routine "astPlot"
                   "Create a Plot"
                   '("frame" "graphbox" "basebox" "options" "..."))
  (new-ast-routine "astPolyCurve"
                   "Draw a series of connected geodesic curves"
                   '("this" "npoint" "ncoord" "indim" "in"))
  (new-ast-routine "astPutFits"
                   "Store a FITS header card in a FitsChan"
                   '("this" "card" "overwrite"))
  (new-ast-routine "astRead"
                   "Read an Object from a Channel"
                   '("this"))
  (new-ast-routine "astRemapFrame"
                   "Modify a Frame's relationship to other Frames in a FrameSet"
                   '("this" "iframe" "map"))
  (new-ast-routine "astRemoveFrame"
                   "Remove a Frame from a FrameSet"
                   '("this" "iframe"))
  (new-ast-routine "astResampleX$"
                   "Resample a region of a data grid"
                   '("this" "ndim_in" "lbnd_in" "ubnd_in" "in"
                   "in_var" "interp" "finterp" "params" "flags" "tol"
                   "maxpix" "badval" "ndim_out" "lbnd_out" "ubnd_out"
                   "lbnd" "ubnd" "out" "out_var"))
  (new-ast-routine "astSet"
                   "Set attribute values for an Object"
                   '("this" "settings" "..."))
  (new-ast-routine "astSetX"
                   "Set an attribute value for an Object"
                   '("this" "attrib" "value"))
  (new-ast-routine "astSetStatus"
                   "Set the AST error status to an explicit value"
                   '("status"))
  (new-ast-routine "astShow"
                   "Display a textual representation of an Object on standard output"
                   '("this"))
  (new-ast-routine "astSimplify"
                   "Simplify a Mapping"
                   '("this"))
  (new-ast-routine "astSkyFrame"
                   "Create a SkyFrame"
                   '("options" "..."))
  (new-ast-routine "astSlaAdd"
                   "Add a celestial coordinate conversion to an SlaMap"
                   '("this" "cvt" "args"))
  (new-ast-routine "astSlaMap"
                   "Create an SlaMap"
                   '("flags" "options" "..."))
  (new-ast-routine "astSphMap"
                   "Create a SphMap"
                   '("options" "..."))
  (new-ast-routine "astStatus"
                   "Obtain the current AST error status value"
                   nil)
  (new-ast-routine "astTest"
                   "Test if an Object attribute value is set"
                   '("this" "attrib"))
  (new-ast-routine "astText"
                   "Draw a text string for a Plot"
                   '("this" "text" "pos" "up" "just"))
  (new-ast-routine "astTran1"
                   "Transform 1-dimensional coordinates"
                   '("this" "npoint" "xin" "forward" "xout"))
  (new-ast-routine "astTran2"
                   "Transform 2-dimensional coordinates"
                   '("this" "npoint" "xin" "yin" "forward" "xout" "yout"))
  (new-ast-routine "astTranN"
                   "Transform N-dimensional coordinates"
                   '("this" "npoint" "ncoord_in" "indim" "in"
                   "forward" "ncoord_out" "outdim" "out"))
  (new-ast-routine "astTranP"
                   "Transform N-dimensional coordinates held in separate arrays"
                   '("this" "npoint" "ncoord_in" "ptr_in" "forward"
                   "ncoord_out" "ptr_out"))
  (new-ast-routine "astUinterp"
                   "Perform sub-pixel interpolation on a grid of data"
                   '("ndim_in" "lbnd_in" "ubnd_in" "in" "in_var"
                   "npoint" "offset" "coords" "params" "flags"
                   "badval" "out" "out_var" "nbad"))
  (new-ast-routine "astUkern1"
                   "1-dimensional sub-pixel interpolation kernel"
                   '("offset" "params" "flags" "value"))
  (new-ast-routine "astUnformat"
                   "Read a formatted coordinate value for a Frame axis"
                   '("this" "axis" "string" "value"))
  (new-ast-routine "astUnitMap"
                   "Create a UnitMap"
                   '("ncoord" "options" "..."))
  (new-ast-routine "astWatch"
                   "Identify a new error status variable for the AST library"
                   '("status_address"))
  (new-ast-routine "astWcsMap"
                   "Create a WcsMap"
                   '("ncoord" "type" "lonax" "latax" "options" "..."))
  (new-ast-routine "astWinMap"
                   "Create a WinMap"
                   '("ncoord" "ina" "inb" "outa" "outb" "options" "..."))
  (new-ast-routine "astWrite"
                   "Write an Object to a Channel"
                   '("this" "object"))
  (new-ast-routine "astZoomMap"
                   "Create a ZoomMap"
                   '("ncoord" "zoom" "options" "...")))
; END OF EXPERIMENT

  (new-token "#DEFINE"
             "\\#define <{macro_name}>(<[macro_par]>...) <[macro_expansion]> /* <[comment]> */"
             '((desc . "Define a macro")))

  (new-place "#ELIF"
             "\\#elif <{constant_expression}>
\\<{preprocessor_statement}>..."
             '((desc . "#elif ...")
               (vert . t)))

  (new-place "#ELSE"
             "\\#else
\\<{preprocessor_statement}>..."
             '((desc . "#else")))

  (new-token "#IF_ELSE"
             "\\#if <{constant_expression}>
\\<{preprocessor_statement}>...
\\<[#elif]>...
\\<[#else]>
\\#endif"
             '((desc . "#if...#endif block")))

  (new-token "#IFDEF"
             "\\#ifdef <{macro_name}>
\\<{preprocessor_statement}>...
\\<[#elif]>...
\\<[#else]>
\\#endif"
             '((desc . "#ifdef...#endif block")))

  (new-token "#IFNDEF"
             "\\#ifndef <{macro_name}>
\\<{preprocessor_statement}>...
\\<[#elif]>...
\\<[#else]>
\\#endif"
             '((desc . "#ifndef...#endif block")))

  (new-token "#INCLUDE"
             "\\#include \"<{header_file}>.h\" /* <[comment]> */"
             '((desc . "Include \".h\" header file")))

  (new-token "#SYSINCLUDE"
             "\\#include <<{header_file}>.h> /* <[comment]> */"
             '((desc . "Include system header file")))

  (new-token "#UNDEF"
             "\\#undef <{macro_name}>"
             '((desc . "Undefine a macro")))

  (new-place "AFFILIATION"
             '(lambda ()
                (or (getenv "EDSTAR_PERSONAL_AFFILIATION") "STARLINK")))

  (new-place "AUTHOR_IDENTIFIER"
             '(lambda ()
                (or (getenv "EDSTAR_PERSONAL_USERID")
                    (upcase (user-login-name)))))

  (new-place "AUTHORS_NAME"
             '(lambda ()
                (or (getenv "EDSTAR_PERSONAL_NAME") (user-full-name))))

  (new-place "BREAK"
             "break"
             '((desc . "Break statement.")
	       (tail . ";")))

  (new-place "CASE_CONDITION"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "case <{constant_expression}>:")
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
;	     '(lambda ()
;		(insert "case <{constant_expression}>:\n")
;		(indent-according-to-mode)
;		(backward-delete-char-untabify edstar-indent)
;		t)
             '((desc . "Case condition.")
               (head . "\n")
	       (vert . t)))

  (new-place "CASE"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "<{case_condition}>...\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{break}>;")
		  (if (looking-at ";") (delete-char 1))
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
             '((desc . "Case statement.")
	       (vert . t)))

  (new-place "CHANGES"
             nil
             '((help . "Describe any changes made.")))

  (new-place "COMMENT"
             nil
             '((help . "A comment.")
               (head . "/*")
               (tail . "*/")))

  (new-place "CONSTANT_EXPRESSION"
             nil
             '((desc . "Constant expression.")))

  (new-place "COPYRIGHT"
             "Copyright (C) <{year}> Central Laboratory of the Research Coincils."
             '((desc . "Copyright message.")
               (head . "*  Copyright:\n*")
               (tail . "\n")))

  (new-place "DATE"
             '(lambda ()
                (let (date (time (upcase (current-time-string))))
                  (setq date (concat (substring time 8 10)
                                     "-"
                                     (substring time 4 7)
                                     "-"
                                     (substring time 20 24)))
                  (if (= (aref date 0) ? ) (setq date (substring date 1)))
                  date)))

  (new-place "DECLARATION_SPECIFIER"
             nil
             '((desc . "A declaration specifier.")))

  (new-place "DEFAULT"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "default:\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{break}>;")
		  (if (looking-at ";") (delete-char 1))
		  t))
             '((desc . "Default statement.")
               (head . "\n")
	       (tail . ";")))

  (new-token "DO"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "do {\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;\n")
		  (insert-char ?  indent)
		  (insert "} while ( <{expression}> )")
		  (if (looking-at ";") (delete-char 1))
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
             '((desc . "do {...} while (...)")))

  (new-place "ELSE"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "} else {\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;")
		  t))
;	     '(lambda ()
;		(insert "} else {")
;		(indent-according-to-mode)
;		(insert "\n}")
;		(indent-according-to-mode)
;		(backward-delete-char-untabify 1)
;		(insert-char ?  edstar-indent)
;		(insert "<{statement}>...;")
;		(if (looking-at ";")
;		    (delete-char 1))
;		t)
             '((desc . "} else {")
	       (tail . ";")))

  (new-place "ELSE_IF"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "} else if ( <{expression}> ) {\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;")
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
             '((desc . "} else if (...) {")
               (vert . t)
	       (tail . ";")))

  (new-place "ENTER_CHANGES_HERE"
             "<{date}> (<{author_identifier}>):
*        <{changes}>
*     <{enter_further_changes_here}>"
             '((desc . "Note of when later changes were made.")
               (head . "*")))

  (new-place "ENTER_FURTHER_CHANGES_HERE"
             "<{date}> (<{author_identifier}>):
*        <{changes}>
*     <{enter_further_changes_here}>"
             '((desc . "Note of when further changes were made.")
               (head . "*")))

  (new-place "ENTER_NEW_AUTHORS_HERE"
             "<{author_identifier}>: <{authors_name}> (<{affiliation}>)
*     <{enter_new_authors_here}>"
             '((desc . "Note of new author.")
               (head . "*")))

  (new-place "EXPRESSION"
             nil
             '((help . "A C expression.")
               (head . "")
               (tail . "")))

  (new-token "EXPRESSION_STATEMENT"
             "<[expression]>"
             '((help . "An expression statement.")))

;;; NB special code is used for expanding bracketed expressions so that
;;; any following ";" statement terminator may be removed.
  (new-token "FOR"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "for ( <[expression]>; <[expression]>; <[expression]> ) {\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;\n")
		  (insert-char ?  indent)
		  (insert "}")
		  (if (looking-at ";") (delete-char 1))
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
	     '((desc . "for ( ... ; ... ; ...) {...}")))

  (new-place "FUNCTION_DEFINITION"
             "<[declaration_specifier]> <{function_name}>( <[param]>... ) {
\\/*
\\*<{function_prologue}>
\\*/

<[local_variables]>
<[statement]>...;
<[return]>;
\b}
"
             '((desc . "Function definition.")
               (vert . t)
               (head . "\n")))

  (new-place "FUNCTION_DEFINITIONS"
             "/* Function definitions. */
/* ===================== */
<{function_definition}>..."
             '((desc . "Function definitions")
               (vert . t)
               (head . "\n")))

  (new-place "FUNCTION_NAME"
             nil
             '((desc . "Function name.")))

  (new-place "FUNCTION_PROLOGUE"
             "+
*  Name:
*     <{function_name}>

*  Purpose:
*     <{function_purpose}>

*  Description:
*     <{function_description}>

*-"
             '((desc . "Module prologue.")
               (head . "/*\n*")
               (tail . "\n*/\n")))

  (new-place "FUNCTION_PROTOTYPE"
             "<[declaration_specifier]> <{function_name}>( <[param]>... );"
             '((desc . "Function prototype.")
               (vert . t)
               (head . "\n")))

  (new-place "FUNCTION_PROTOTYPES"
             "/* Function prototypes. */
/* ==================== */
<{function_prototype}>...
"
             '((desc . "Declare module-spacific function prototypes")
               (vert . t)))

  (new-place "HEADER_FILE"
             nil
             '((help . "The name of a \".h\" header file.")))

  (new-place "HEADER_FILES"
             "\\/* Header files. */
/* ============= */
<[RTL_header_files]>
<[system_header_files]>
<[user_header_files]>"
             '((desc . "C header files")))

  (new-token "IF"
             "if ( <{expression}> ) <{statement}>"
             '((desc . "if (...) statement")))

  (new-token "IF_ELSE"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "if ( <{expression}> ) {\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;\n")
		  (insert-char ?  indent)
		  (insert "<[else_if]>...\n")
		  (insert-char ?  indent)
		  (insert "<[else]>\n")
		  (insert-char ?  indent)
		  (insert "}")
		  (if (looking-at ";") (delete-char 1))
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
             '((desc . "if (...) {...} else if (...) {...} else {...}")))

  (new-place "INIT"
             nil
             '((help . "Initial value for the variable.")
               (head . "=")))

  (new-place "LOCAL_VARIABLES"
             "\\/* Local Variables: */
\t<{variable_declaration}>...
"
             '((desc . "Declarations for local variables.")))

  (new-place "MACRO_EXPANSION"
             nil
             '((help . "The value to which the macro should expand.")))

  (new-place "MACRO_NAME"
             nil
             '((help . "The name of a preprocessor macro.")))

  (new-place "MACRO_PAR"
             nil
             '((help . "The name of a macro parameter.")
               (sep . ",")
               (head . "(")
               (tail . ")")
               (posttail . t)))  ; Preserve space following tail

  (new-place "MODULE"
             "/*
\\*<[module_prologue]>
*/

<[module_macros]>
<[header_files]>
<[module_static_variables]>
<[function_prototypes]>
<[function_definitions]>")

(new-place "MODULE_DESCRIPTION"
	   nil
	   '((help .
"Give a full description (in English) of what the module does and how
it does it (if relevant).  This description should be comprehensible
to the user and may be extracted for use in documentation.")
             (head . "*  Description:\n*")
             (tail . "\n")))

  (new-place "MODULE_NAME"
             '(lambda ()
                (let (name i dot)
                  (if (setq name (buffer-file-name))
                      (progn
                        (setq name (file-name-nondirectory
                                    (file-name-sans-versions name)))
                        (setq i (length name))
                        (while (and (> i 0) (not dot))
                          (setq dot (= ?. (elt name (setq i (- i 1))))))
                        (if dot (setq name (substring name 0 i)))
                        name)
                    nil)))
             '((help .
"The name of the C module. Normally, this should match the name of the
file which contains it (with the \".c\" file extension removed).")
               (auto . t)
               (head . "*  Name:\n*")
               (tail . "\n")))

  (new-place "MODULE_MACROS"
             "/* Macro definitions for this module. */
/* ================================== */
<{preprocessor_statement}>...
"
             '((help . "Macro definitions for the module.")))

  (new-place "MODULE_PROLOGUE"
             "+
*  Name:
*     <{module_name}>

*  Purpose:
*     <{module_purpose}>

*  Description:
*     <{module_description}>

*  Copyright:
*     <[copyright]>

*  Authors:
*     <{original_author_entry}>

*  History:
*     <{original_version_entry}>
*-"
             '((desc . "Module prologue.")
               (head . "/*\n*")
               (tail . "\n*/\n")))

  (new-place "MODULE_PURPOSE"
             nil
             '((help .
"A very brief (about one line) description of the service the module
provides.")
               (head . "*  Purpose:\n*")
               (tail . "\n")))

  (new-place "MODULE_STATIC_VARIABLES"
             "/* Static variables for this module. */
/* ================================= */
<{static_variable_declaration}>...
"
             '((desc . "Declare module-spacific static variables")))

  (new-place "ORIGINAL_AUTHOR_ENTRY"
             "<{author_identifier}>: <{authors_name}> (<{affiliation}>)
*     <{enter_new_authors_here}>"
             '((desc . "Note of original author.")
               (head . "*  Authors:\n*")
               (tail . "\n")))

(new-place "ORIGINAL_VERSION_ENTRY"
	   "<{date}> (<{author_identifier}>):
*        Original version.
*     <{enter_changes_here}>"
           '((desc . "Note of original creation date.")
             (head . "\n*  History:\n*")))

  (new-place "PARAM"
             nil
             '((desc . "Function parameter declaration.")
               (sep . ", ")
               (head . "")))

  (new-place "PREPROCESSOR_STATEMENT"
             '(("#define" nil token)
               ("#if_else" nil token)
               ("#ifdef" nil token)
               ("#ifndef" nil token)
               ("#undef" nil token)
               ("#include" nil token)
               ("#sysinclude" nil token)
               ("statement" "Ordinary C statement" "<{statement}>;"))
             '((desc . "C preprocessor statements")
               (vert . t)))

  (new-token "PREPROCESSOR_STATEMENT"
             "<{preprocessor_statement}>..."
             '((desc . "A preprocessor statement.")))

  (new-place "RTL_HEADER"
             '(("stdio.h" "Standard input and output"
                "#include <stdio.h> /* Standard input/output */")
               ("errno.h" "Declare errno"
                "#include <errno.h> /* Declare errno */")
               ("ctype.h" "Character class tests"
                "#include <errno.h> /* Character class tests */")
               ("string.h" "String handling"
                "#include <string.h> /* String handling */")
               ("math.h" "Mathematical functions"
                "#include <math.h> /* Mathematical functions */")
               ("stdlib.h" "Utility functions"
                "#include <stdlib.h> /* Utility functions */")
               ("assert.h" "Diagnostics"
                "#include <assert.h> /* Diagnostics */")
               ("stdarg.h" "Variable argument lists"
                "#include <stdarg.h> /* Variable argument lists */")
               ("setjmp.h" "Non-local jumps"
                "#include <setjmp.h> /* Non-local jumps */")
               ("signal.h" "Signals"
                "#include <signal.h> /* Signals */")
               ("time.h" "Date and time functions"
                "#include <time.h> /* Date and time functions */")
               ("limits.h" "Implementation-defined limits"
                "#include <limits.h> /* Implementation-defined limits */")
               ("float.h" "Floating point values"
                "#include <float.h> /* Floating point values */"))
             '((vert . t)))

  (new-token "RTL_HEADER"
             "<{RTL_header}>..."
             '((desc . "Include RTL header file")))

  (new-place "RETURN"
	     '(lambda ()
		(insert "return <[expression]>;")
		(if (looking-at ";")
		    (delete-char 1))
		t)
             '((desc . "Return statement")
	       (tail . ";")))

  (new-place "RTL_HEADER_FILES"
             "/* C run-time library header files. */
<{RTL_header}>...
"
             '((desc .
                     "Include \".h\" header files for the C run-time library")))

  (new-place "STATEMENT"
             '(("for" nil token)
               ("while" nil token)
               ("switch" nil token)
               ("do" nil token)
               ("if" nil token)
               ("if_else" nil token)
               ("expression_statement" nil token))
             '((desc . "C statements")
               (vert . t)
	       (tail . ";")
	       (sep . ";")))

  (new-token "STATEMENT"
             "<{statement}>...;"
             '((desc . "A statement.")))

  (new-place "STATIC_VARIABLE_DECLARATION"
             "static <[declaration_specifier]> <{variable_name}> = <[init]>; /* <[comment]> */"
             '((desc . "Declare module-specific static variable")
               (vert . t )
               (head . "\n")))

  (new-token "SWITCH"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "switch ( <{expression}> ) {\n")
		  (insert-char ?  indent)
		  (insert "<{case}>...\n")
		  (insert-char ?  indent)
		  (insert "<[default]>;\n")
		  (insert-char ?  indent)
		  (insert "}")
		  (if (looking-at ";") (delete-char 1))
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
             '((desc . "switch (...) {...}")))

  (new-token "SYSTEM_HEADER"
             "<{system_header}>..."
             '((desc . "Include system header file")))

  (new-place "SYSTEM_HEADER"
             '(("#SYSINCLUDE" "" token))
             '((desc . "Include system header file")
               (vert . t)))

  (new-place "SYSTEM_HEADER_FILES"
             "/* System header files. */
<{system_header}>...
"
             '((desc . "Include system \".h\" header files")))

  (new-token "USER_HEADER"
             "<{user_header}>..."
             '((desc . "Include user header file")))

  (new-place "USER_HEADER"
             '(("#INCLUDE" "" token))
             '((desc . "Include user header file")
               (vert . t)))

  (new-place "USER_HEADER_FILES"
             "/* User header files. */
<{user_header}>...
"
             '((desc . "Include user \".h\" header files")))

  (new-place "VARIABLE_DECLARATION"
             "<[declaration_specifier]> <{variable_name}> = <[init]>; /* <[comment]> */"
             '((desc . "Declaration for local variable.")
               (vert . t )
               (head . "\n")))

  (new-place "VARIABLE_NAME"
             nil
             '((help . "The name of a C variable.")))

  (new-token "WHILE"
	     '(lambda ()
		(let ((indent (current-column)))
		  (insert "while ( <{expression}> ) {\n")
		  (insert-char ?  (+ indent edstar-indent))
		  (insert "<{statement}>...;\n")
		  (insert-char ?  indent)
		  (insert "}")
		  (if (looking-at ";") (delete-char 1))
		  (forward-line 1)
		  (let ((opoint (point)))
		    (skip-chars-forward " \t")
		    (delete-region opoint (point)))
		  (insert-char ?  indent)
		  t))
             '((desc . "while (...) {...}")))

  (new-place "YEAR"
             '(lambda () (substring (current-time-string) 20 24)))

  (make-local-variable 'edstar-placeholder-table)
  (setq edstar-placeholder-table 'starc-mode-placeholder-table)
  (make-local-variable 'edstar-token-table)
  (setq edstar-token-table 'starc-mode-token-table)
  (make-local-variable 'edstar-helpkey-table)
  (setq edstar-helpkey-table 'starc-mode-helpkey-table))
