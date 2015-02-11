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
		   '("gif89a" "jpeg"))))
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
	(if (member ent-notation '("gif89a" "jpeg"))
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
		  '("gif89a" "jpeg"))))
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
; 					       '("jpeg" "gif89a")))))
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

(element foreign
  (make element gi: "i"
	(process-children)))

(element kbd
  (make element gi: "code"
	(make element gi: "u"
	      (process-children))))

;; Put an extra bit of space around verbatim elements
(element verbatim
  (make element gi: "blockquote"
	(make element gi: "pre"
	      (process-children))))

(element quote
  (make sequence
    (literal "`")
    (process-children)
    (literal "'")))

(element strong
  (make element
    (process-children)))

;; Examine the span element's `media' attribute.  If there is no such
;; attribute, or if there is and its value is either
;; %passthrough-mediatype% or `all', then process the children,
;; otherwise do nothing.
(element span
  (let ((mediatypes (get-mediatypes (current-node))))
    (if (or (not mediatypes)
	    (assoc %passthrough-mediatype% mediatypes)
	    (assoc "all" mediatypes))
	(process-children)
	(empty-sosofo))))

;;; Lists

(element dl
  (make element
    attributes: (if (attribute-string (normalize "compact") (current-node))
		    '(("compact" "compact"))
		    '())
    (process-children-trim)))

;; embolden dt contents (suggested by Mark)
(element dt
  (make element gi: "dt"
    (make element gi: "strong"
	  (if (attribute-string (normalize "id"))
	      (make element gi: "a"
		    attributes: `(("name" ,(href-to (current-node)
						    frag-only: #t)))
		    (process-children-trim))
	      (process-children-trim)))))

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
  (make element gi: "li"
	(let ((myid (attribute-string (normalize "id"))))
	  (if myid
	    (let ((kids (children (current-node))))
	      (make sequence
		(make element gi: "a"
		      attributes: `(("name" , (href-to (current-node)
						       frag-only: #t)))
		      (process-node-list (node-list-first kids)))
		(process-node-list (node-list-rest kids))))
	    (process-children-trim)))))

(mode section-reference
  (element dt
    (make sequence
      (literal "item `")
      ;; this dt had better be short...
      (process-children)
      (literal "'")))
  (element li
    (literal (string-append "item "
			    (number->string (child-number (current-node)))))))


(element (li p)
  (if (equal? (child-number (current-node)) 1)
      (process-children-trim)
      (make element gi: "p" (process-children-trim))))

(element (dd p)
  (if (equal? (child-number (current-node)) 1)
      (process-children-trim)
      (make element gi: "p" (process-children-trim))))

;;; Paragraphing

;(element p
;  (let ((id (attribute-string (normalize "id")
;			      (current-node))))
;    (if id
;	(let ((kids (children (current-node))))
;	  (make sequence
;	    (make element gi: "a"
;		  attributes: (list (list "name" (string-append "xref_" id)))
;		  (process-node-list (node-list-first kids)))
;	    (process-node-list (node-list-rest kids))))
;	(make element gi: "p"
;	      (process-children-trim)))))

;; In the HTML DTDs, the list elements and blockquote are (amongst the)
;; %block elements, though these are para-content elements in the
;; Starlink General's paracontent DTD.  To produce conformant output,
;; therefore, we must move these elements `up' a level on output
;; (include linespecific because it's mapped to blockquote in this 
;; stylesheet).
;;
;; This is complicated by our desire to generate a link target for 
;; any ID attribute on the paragraph
;(element p
;  (let* ((shift-els (list (normalize "ul") (normalize "ol") (normalize "dl")
;			  (normalize "blockquote") (normalize "linespecific")))
;	 (kids-nl (node-list-split-by-gi (children (current-node))
;					 shift-els)))
;      (let loop ((l kids-nl) 
;		 (id (attribute-string (normalize "id") (current-node))))
;	(if (null? l)
;	    (empty-sosofo)
;	    (let* ((nl (car l))
;		   (g (gi (node-list-first nl))))
;	      (make sequence
;		(if (and g (member g shift-els))
;		    (make sequence
;		      (if id
;			  ;; make a paragraph containing only a space. Yuk!
;			  (make element gi: "p"
;				(make element gi: "a"
;				      attributes:
;				      (list (list "name"
;						  (string-append "xref_" id)))
;				      " "))
;			  (empty-sosofo))
;		      (process-node-list nl))
;		    (if id
;			(make sequence
;			  (make element gi: "a"
;				attributes:
;				(list (list "name" (string-append "xref_" id)))
;				(process-node-list (node-list-first nl)))
;			  (process-node-list (node-list-rest nl)))
;			(make element gi: "p"
;			      (process-node-list nl))))
;		(loop (cdr l) #f)))))))

;(element px				;identical to `p'
;  (let* ((shift-els (list (normalize "ul") (normalize "ol") (normalize "dl")
;			  (normalize "blockquote") (normalize "linespecific")))
;	 (kids-nl (node-list-split-by-gi (children (current-node))
;					 shift-els)))
;      (let loop ((l kids-nl) 
;		 (id (attribute-string (normalize "id") (current-node))))
;	(if (null? l)
;	    (empty-sosofo)
;	    (let* ((nl (car l))
;		   (g (gi (node-list-first nl))))
;	      (make sequence
;		(if (and g (member g shift-els))
;		    (make sequence
;		      (if id
;			  ;; make a paragraph containing only a space. Yuk!
;			  (make element gi: "p"
;				(make element gi: "a"
;				      attributes:
;				      (list (list "name"
;						  (string-append "xref_" id)))
;				      " "))
;			  (empty-sosofo))
;		      (process-node-list nl))
;		    (if id
;			(make sequence
;			  (make element gi: "a"
;				attributes:
;				(list (list "name" (string-append "xref_" id)))
;				(process-node-list (node-list-first nl)))
;			  (process-node-list (node-list-rest nl)))
;			(make element gi: "p"
;			      (process-node-list nl))))
;		(loop (cdr l) #f)))))))


(element px
  (make element gi: "p"
    (process-children-trim)))
(element p
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

(element index
  ;; Index generation is handled in slback.dsl
  (empty-sosofo))


(element angle
  (let ((ishours (and (attribute-string (normalize "unit"))
		      (string=? (attribute-string (normalize "unit"))
				"hours")))
	(angle (attribute-string (normalize "angle")))
	(minutes (attribute-string (normalize "minutes")))
	(seconds (attribute-string (normalize "seconds")))
	(fraction (attribute-string (normalize "fraction"))))
      (if ishours
	  (make sequence
	    (if angle
		(make formatting-instruction
		  data: (string-append angle "&lt;sup>h&lt;/sup>"))
		(empty-sosofo))
	    (if minutes
		(make formatting-instruction
		  data: (string-append " " minutes "&lt;sup>m&lt;/sup>"))
		(empty-sosofo))
	    (if seconds
		(make formatting-instruction
		  data: (string-append " " seconds "&lt;sup>s&lt;/sup>"))
		(empty-sosofo))
	    (if fraction
		(make formatting-instruction
		  data: (string-append "." fraction))
		(empty-sosofo)))
	  (make sequence
	    (if angle
		(make formatting-instruction
		  data: (string-append angle "&lt;sup>o&lt;/sup>"))
		(empty-sosofo))
	    (if minutes
		(make formatting-instruction
		  data: (string-append " " minutes "'"))
		(empty-sosofo))
	    (if seconds
		(make formatting-instruction
		  data: (string-append " "seconds "''"))
		(empty-sosofo))
	    (if fraction
		(make formatting-instruction
		  data: (string-append "." fraction))
		(empty-sosofo))))))
