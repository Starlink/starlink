<![ ignore [
Title:
  Starlink stylesheet - miscellaneous functions

Author:
  Norman Gray, Glasgow (NG)

Revision history
  February 1999 (original version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<misccode>
<description>
Miscellaneous constructors

This file contains constructors which don't really fit in anywhere
else.  They're not necessarily unimportant, just simple enough not
to need explanation or elaboration.

This file forms the effective body of sl.dsl, the main DSSSL stylesheet.

<codebody>

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Figures and tables
;;;
(element figure
  (let* ((caption-details (get-caption-details (current-node)))
	 (caption-id (cadr caption-details)))
    (make element gi: "table"
	  attributes: '(("align" "center") ("border" "1"))
      (make element gi: "tr"
	    (make element gi: "td"
		  (process-matching-children 'figurecontent)))
      (make element gi: "tr"
	    attributes: '(("align" "left"))
	    (make element gi: "td"
		  (make sequence
		    (if caption-id
			(make element gi: "a"
			      attributes: (list (list "name" caption-id))
			      (literal (car caption-details)))
			(literal (car caption-details)))
		    (literal ": ")
		    (process-matching-children 'caption)))))))
; (element figure
;   (let* ((caption-details (get-caption-details (current-node)))
; 	 (caption-id (cadr caption-details)))
;     (make sequence
;       (make element gi: "div"
; 	    attributes: '(("align" "center"))
; 	    (process-matching-children 'figurecontent))
;       (make element gi: "blockquote"
; 	    (make element gi: "p"
; 		  (if caption-id
; 		      (make element gi: "a"
; 			    attributes: (list (list "name" caption-id))
; 			    (literal (car caption-details)))
; 		      (literal (car caption-details))))
; 	    (process-matching-children 'caption)))))

(element caption
  (process-children))

(element figurecontent
  (let* ((alt-text (attribute-string (normalize "alt")
				     (current-node)))
	 (image-ents (attribute-string (normalize "image")
				       (current-node)))
	 (best-ent (and image-ents
			(get-sysid-by-notation image-ents
					       '("JPEG" "GIF87A")))))
    (if image-ents
	(if best-ent
	    (make empty-element gi: "img"
		  attributes: (list (list "src" best-ent)
				    (list "alt" alt-text)))
	    (error "No suitable entity in figurecontent"))
	(process-children))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The rest of this file consists of the constructors for miscellaneous 
;;; elements which don't need much in the way of fancy processing.

;;; Phrase markup

(element code
  (make element
    (process-children)))

(element em
  (make element
    (process-children)))

(element kbd
  (make element gi: "code"
	(process-children)))

(element verbatim
  (make element gi: "pre"
	(process-children)))

(element quote
  (make sequence
    (literal "`")
    (process-children)
    (literal "'")))

(element strong
  (make element
    (process-children)))

;;; Lists

(element dl
  (make element
    (process-children-trim)))

(element dt
  (make element
    (process-children-trim)))

(element dd
  (make element
    (process-children-trim)))

(element ol
  (make element
    (process-children-trim)))

(element ul
  (make element
    (process-children-trim)))

(element li
  (make element
    (process-children-trim)))

;;; Paragraphing

(element p
  (make element
    (process-children-trim)))

(element px
  (make element gi: "p"
    (process-children-trim)))

(element cite
  (make element
    (process-children-trim)))

(element blockquote
  (make element
    (process-children-trim)))

(element attribution
  (make element gi: "em"
    (make sequence
      (literal "[ ")
      (process-children)
      (literal " ]"))))

(element draftnote
  (make element
    gi: "strong"
    (literal "Draft Note:")
    (process-children)))

