<!--
Title:
  Starlink stylesheet - miscellaneous functions

Author:
  Norman Gray, Glasgow (NG)

Revision history
  February 1999 (original version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
-->

<routine>
<description>
Support figures and figurecontent.
Note that changes here might need corresponding changes to manifest-mode
within sl.dsl.
<codebody>
(element figure
  (let* ((caption-details (get-caption-details (current-node)))
	 (caption-id (caddr caption-details))
	 (kids (children (current-node)))
	 (content (get-best-figurecontent
		   (node-list (select-elements kids (normalize "figurecontent"))
			      (select-elements kids (normalize "px")))
		   '("GIF89A" "JPEG"))))
    (make element gi: "table"
	  attributes: '(("align" "center") ("border" "1"))
      (make element gi: "tr"
	    (make element gi: "td"
		  (if content
		      (process-node-list content)
		      (literal "No processable content"))
		  ))
      (make element gi: "tr"
	    attributes: '(("align" "left"))
	    (make element gi: "td"
		  (make sequence
		    (if caption-id
			(make element gi: "a"
			      attributes:
			      (list (list "name"
					  (string-append "xref_" caption-id)))
			      (literal (car caption-details)))
			(literal (car caption-details)))
		    (if show-element-ids
			(literal (display-element-ids (current-node)))
			(empty-sosofo))
		    (literal ": ")
		    (process-matching-children 'caption)))))))

(element caption
  (process-children))

;; If the `image' attribute is not present, then the figure content is
;; given as the element content.
;; Changes here might need matching changes in 
;; mode make-manifest-mode in sl.dsl.
;;
;; NOTE that the ent-sysid which is used is _stripped_ of its path.
;; It is the responsibility of this stylesheet's harness to make sure
;; that the positions of entities like this are resolved post-hoc.
(element figurecontent
  (let* ((alt-text (attribute-string (normalize "alt")
				     (parent (current-node))))
	 (ent (attribute-string (normalize "image")
				(current-node)))
	 (ent-sysid (and ent
			 (car (reverse
			       (tokenise-string
				(entity-system-id ent)
				boundary-char?: (lambda (c)
						  (char=? c #\/)))))))
	 (ent-notation (and ent
			    (entity-notation ent))))
    (if ent-notation
	(if (member ent-notation '("GIF89A" "JPEG"))
	    (make empty-element gi: "img"
		  attributes: (list (list "src" ent-sysid)
				    (list "alt"
					  (if alt-text
					      alt-text
					      (string-append ent-notation
							     " image")))))
	    (error (string-append "Can't process entities of type "
				  ent-notation)))
	(error "Can't extract entity"))))

(element coverimage
  (let* ((kids (children (current-node)))
	 (content (get-best-figurecontent
		   (node-list (select-elements kids
					       (normalize "figurecontent"))
			      (select-elements kids
					       (normalize "px")))
		  '("GIF89A" "JPEG"))))
    (if content
	(process-node-list content)
	(error "Can't process coverimage"))))

; ;; If the `image' attribute is not present, then the figure content is
; ;; given as the element content.
; ;; Changes here might need matching changes in 
; ;; mode make-manifest-mode in sl.dsl.
; (element figurecontent
;   (let* ((alt-text (attribute-string (normalize "alt")
; 				     (current-node)))
; 	 (image-ents (attribute-string (normalize "image")
; 				       (current-node)))
; 	 (best-ent (and image-ents
; 			(get-sysid-by-notation image-ents
; 					       '("JPEG" "GIF89A")))))
;     (if image-ents
; 	(if best-ent
; 	    (make empty-element gi: "img"
; 		  attributes: (list (list "src" best-ent)
; 				    (list "alt" alt-text)))
; 	    (error "No suitable entity in figurecontent"))
; 	(process-children))))


<routine>
<description>
Miscellaneous constructors.  These are constructors which don't really fit
in anywhere else.  They're not necessarily unimportant, just simple enough not
to need explanation or elaboration.

<codebody>
;;; Phrase markup

(element code
  (make element
    (process-children)))

(element em
  (make element
    (process-children)))

(element kbd
  (make element gi: "code"
	(make element gi: "u"
	      (process-children))))

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
    attributes: (if (attribute-string (normalize "compact") (current-node))
		    '(("compact" "compact"))
		    '())
    (process-children-trim)))

(element dt
  (make element
    (process-children-trim)))

(element dd
  (make element
    (process-children-trim)))

(element ol
  (make element
    attributes: (if (attribute-string (normalize "compact") (current-node))
		    '(("compact" "compact"))
		    '())
    (process-children-trim)))

(element ul
  (make element
    attributes: (if (attribute-string (normalize "compact") (current-node))
		    '(("compact" "compact"))
		    '())
    (process-children-trim)))

(element li
  (make element
    (process-children-trim)))

;;; Paragraphing

(element p
  (let ((id (attribute-string (normalize "id")
			      (current-node))))
    (if id
	(let ((kids (children (current-node))))
	  (make sequence
	    (make element gi: "a"
		  attributes: (list (list "name" (string-append "xref_" id)))
		  (process-node-list (node-list-first kids)))
	    (process-node-list (node-list-rest kids))))
	(make element gi: "p"
	      (process-children-trim)))))

(element px
  (make element gi: "p"
    (process-children-trim)))

(element cite
  (make element
    (process-children-trim)))

(element blockquote
  (make element
    (process-children-trim)))

(element linespecific
  (make element gi: "blockquote"	; a reasonable choice?
	(process-children-trim)))
(element line
  (make sequence
    (process-children-trim)
    (make empty-element gi: "br")))

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

