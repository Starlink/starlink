<![ ignore [

Title:
  Starlink General DTD -- LaTeX stylesheet miscellaneous elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<misccode>
<description>
This file contains constructors which don't really fit in anywhere
else.  They're not necessarily unimportant, just simple enough not
to need explanation or elaboration.

This file forms much of the effective body of sl.dsl, 
the main DSSSL stylesheet for the LaTeX stylesheet.

<codebody>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Figures and tables
;;;
(element figure
  (make environment name: "figure"
	(process-matching-children 'figurecontent)
	(process-matching-children 'caption)))

(element caption
  (let ((caption-details (get-caption-details (parent (current-node)))))
    (make command name: "caption"
	  parameters: '("?")	;empty [] to suppress LOF entry
	  (literal (string-append (car caption-details) ": "))
	  (process-matching-children 'caption))))

(element figurecontent
  (let* ((image-ents (attribute-string (normalize "image")
				       (current-node)))
	 (best-ent (and image-ents
			(get-sysid-by-notation image-ents
					       '("EPS")))))
    (if image-ents
	(if best-ent
	    (make empty-command name: "includegraphics"
		  parameters: (list best-ent))
	    (error "No suitable entity in figurecontent"))
	(process-children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The rest of this file consists of the constructors for miscellaneous 
;;; elements which don't need much in the way of fancy processing.

;;; Phrase markup

(element code
  (make command name: "texttt"
	(process-children)))

(element em
  (make command name: "emph"
    (process-children)))

(element kbd
  (make command name: "texttt"
	(process-children)))

(element quote
  (make sequence
    (literal "`")
    (process-children)
    (literal "'")))

(element strong
  (make command name: "emph"
    (process-children)))

;;; Lists

(element dl
  (make environment name: "description"
    (process-children)))

(element dt
  (make empty-command name: "item"
	parameters: (string-append "?" (data (current-node)))))

(element dd
  (process-children))

(element ol
  (make environment name: "enumerate"
    (process-children)))

(element ul
  (make environment name: "itemize"
    (process-children)))

(element li
  (make sequence
    (make empty-command name: "item")
    (process-children)))

;;; Paragraphing

(element p
  (make paragraph
    (process-children)))

(element px
  (make paragraph
    (process-children-trim)))

(element cite
  (make command name: "textit"
    (process-children-trim)))

(element blockquote
  (make environment name: "quotation"
    (process-children-trim)))

(element verbatim
  (make environment
    name: "verbatim"
    recontrol: "/-/"
    escape-tex?: #f
    (process-children)))

(element attribution
  (make command name: "textit"
    (make sequence
      (literal "[ ")
      (process-children)
      (literal " ]"))))

(element draftnote
  (make command name: "emph"
	(literal "Draft Note:")
	(process-children)))
