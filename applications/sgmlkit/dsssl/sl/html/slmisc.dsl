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
;;; Document Head

(element title
  (literal (normalise-string (data (current-node)))))

(element authorlist
  (process-children))

(mode make-html-author-links
  (element author
    (let ((email (attribute-string "email")))
      (if email
	  (make empty-element
	    gi: "link"
	    attributes: (list (list "rev" "made")
			      (list "href" email)
			      (list "title" (data (current-node)))))
	  (empty-sosofo)))))

;; In the default mode, make author a link to the author details,
;; if either of the webpage or email attributes is present
(element author
  (let* ((webpage (attribute-string (normalize "webpage") (current-node)))
	 (link (cond (webpage)
		     ((attribute-string (normalize "email")
					 (current-node))
		       (string-append "mailto:"
				      (attribute-string (normalize "email")
							(current-node))))
		     (else #f))))
      (if link
	  (make element gi: "A"
		attributes: (list (list "HREF" link))
		(literal (normalise-string (data (current-node)))))
	  (literal (normalise-string (data (current-node)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Document Body starts here
;;;
;;; Process the docbody element by creating a `title page', using
;;; information from the docinfo element, then doing (process-children)
;;; to format the document content.
;;;
(element docbody
  (let* ((tsosofo (process-node-list (getdocinfo 'title)))
	 (authors (children (getdocinfo 'authorlist)))
	 (rel (document-release-info))
	 (vers (car (cdr (cdr rel))))
	 (date (format-date (car rel)))
	 (docref (getdocnumber))
	 )
    (make sequence
      (make element gi: "TABLE"
	    attributes: '(("WIDTH" "100%"))
	    (make sequence
	      (make element gi: "TR"
		    (make element gi: "TD"
			  attributes: (list (list "COLSPAN" "2")
					    (list "ALIGN" "CENTER"))
			  (make element gi: "H1"
				tsosofo)))
	      (if docref
		  (make element gi: "TR"
			(make sequence
			  (make element gi: "TD"
				attributes: (list (list "ALIGN"	"RIGHT")
						  (list "WIDTH" "50%"))
				(make element gi: "EM"
				      (literal "Document")))
			  ;; (make element gi: "TD"
			  ;; (literal docref))
			  (make element gi: "TD"
				(make sequence
				  (literal docref)
				  (literal (if vers
				      (string-append "." vers)
				      ""))))
			  ))
		  (empty-sosofo))
	      (make element gi: "TR"
		    (make sequence
		      (make element gi: "TD"
			    attributes: '(("ALIGN" "RIGHT"))
			    (make element gi: "EM"
				      (literal "Author")))
		      (make element gi: "TD"
			    (node-list-reduce
			     authors
			     (lambda (result a)
			       (sosofo-append
				result
				(make sequence
				  (process-node-list a)
				  (make empty-element gi: "BR"))))
			     (empty-sosofo)))))
	      (make element gi: "TR"
		    (make sequence
		      (make element gi: "TD"
			    attributes: '(("ALIGN" "RIGHT"))
			    (make element gi: "EM"
				      (literal "Release date")))
		      (make element gi: "TD"
			      (literal date))))
	      (if (and %starlink-banner% (not suppress-banner))
		  (make element gi: "TR"
			(make element gi: "TD"
			      attributes: (list (list "ALIGN" "CENTER")
						(list "COLSPAN" "2"))
			      (make element gi: "SMALL"
				    %starlink-banner%)))
		  (empty-sosofo))))
      (process-children))))

(element abstract
  (make sequence
    (make empty-element gi: "hr")
    (make element
      gi: "P"
      (make element
	gi: "STRONG"
	(literal "Abstract: ")))
    (make element
      gi: "BLOCKQUOTE"
      (process-children))
    (make empty-element gi: "hr")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Figures and tables
;;;
(element figure
  (let* ((caption-details (get-caption-details (current-node)))
	 (caption-id (cadr caption-details)))
    (make sequence
      (make element gi: "div"
	    attributes: '(("align" "center"))
	    (process-matching-children 'figurecontent))
      (make element gi: "blockquote"
	    (make element gi: "p"
		  (if caption-id
		      (make element gi: "a"
			    attributes: (list (list "name" caption-id))
			    (literal (car caption-details)))
		      (literal (car caption-details))))
	    (process-matching-children 'caption)))))

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
