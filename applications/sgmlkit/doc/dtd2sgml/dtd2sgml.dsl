<!DOCTYPE style-sheet PUBLIC "-//James Clark//DTD DSSSL Style Sheet//EN">
;;; $Id$


(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")
(declare-flow-object-class empty-element
  "UNREGISTERED::James Clark//Flow Object Class::empty-element")
;(define debug
;  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

(define %sect-gi% "subsubsect")
(define %subsect-gi% "subsubsubsect")
;(define %xref-prefix% "element.")
;(define (xref-prefix)
;  (string-append "el." (attribute-string "prefix"
;					 (document-element (current-node)))))
(define (xref-prefix)
  "el.")

(root
    (process-children))

;; The obvious thing to do below is to (process-node-list ellist), and
;; define an element-construction-rule to handle dtdelement.  That
;; doesn't work, however, because the dtdelement construction rule is
;; invoked within the grove constructed by the auxiliary parse of the
;; entity referred to by the ELEMENTLIST attribute (ie, the grove
;; which describes the DTD structure).  That means that when you try
;; to find the element descriptions using (element-with-gi gi ...),
;; you can't, because you can't directly refer to the grove which
;; contains the grove you're in when you invoke that rule (clear?).
;; Instead, we do the sosofo construction `by hand', looping through
;; ellist, and calling (proc-dtdelement) for each dtdelement node,
;; explicitly passing a node within the main grove -- the grove which
;; contains the ELEMENT elements, which provide additional
;; descriptions of the elements.
(element dtddescription
  (let* ((elentity (attribute-string "ELEMENTLIST" (current-node)))
	 (eldocfile (entity-generated-system-id elentity))
	 (eldoc (and eldocfile
		     (document-element (sgml-parse eldocfile))))
	 (ellist (and eldoc
		      (children eldoc))))
    (if eldoc
	;(process-node-list ellist)
	(let loop ((res (empty-sosofo))
		   (nl ellist))
	  (if (node-list-empty? nl)
	      res
	      (loop (sosofo-append res (proc-dtdelement (node-list-first nl)
							(current-node)))
		    (node-list-rest nl))))
	(error (string-append "Can't find element list in entity "
			      elentity)))))

(define (proc-dtdelement struct-node descrip-root)
  (let* ((gi (attribute-string "GI" struct-node))
	 (descrip (element-with-id gi descrip-root))
	 (kids (children struct-node))
	 (dtdatts (select-elements kids "DTDATTRIBUTE")))
    (make element gi: %sect-gi%
	  attributes: (list (list "id" (string-append (xref-prefix)
						      gi)))
	  (make element gi: "title"
		(literal "Element " gi))
	  (if (node-list-empty? descrip)
	      (empty-sosofo)
	      (make element gi: %subsect-gi%
		    (make element gi: "title"
			  (literal "Description"))
		    (with-mode discard-attribute
		      (process-node-list descrip))))
	  (process-node-list kids)
	  (let loop ((att-nl dtdatts)
		     (res (empty-sosofo)))
	    (if (node-list-empty? att-nl)
		res
		(loop (node-list-rest att-nl)
		      (sosofo-append
		       res
		       (proc-dtdattribute gi
					  (node-list-first att-nl)
					  descrip))))))))

(element dtdattribute
  (empty-sosofo))

(define (proc-dtdattribute gi attr-node element-descrip)
  (let* ((name (attribute-string "NAME"    attr-node))
	 (def  (attribute-string "DEFAULT" attr-node))
	 (type (attribute-string "TYPE"    attr-node))
	 (cont (data attr-node))
	 (descrip (select-elements (children element-descrip)
				   (list "ATTRIBUTE" (list "NAME" name)))))
    (make element gi: %subsect-gi%
	  (make element gi: "title"
		(literal "Attribute " gi "/" name))
	  (if (node-list-empty? descrip)
	      (empty-sosofo)
	      (process-node-list descrip))
	  (make element gi: "p"
		(make element gi: "ul"
		      (make element gi: "li"
			    (literal "Default: " def))
		      (if type
			  (make element gi: "li"
				(literal "Type: " type))
			  (empty-sosofo))
		      (if (> (string-length cont) 0)
			  (make element gi: "li"
				(literal "Allowed values: " cont))
			  (empty-sosofo)))))))

; (element dtdelement
;   (let* ((gi (attribute-string "GI" (current-node)))
; 	 (descrip (element-with-id gi (current-node))))
;   (make element gi: %sect-gi%
; 	attributes: (list (list "id" (string-append (xref-prefix) gi)))
; 	(make element gi: "title"
; 	      (literal "Element " gi))
; 	(if (node-list-empty? descrip)
; 	    (empty-sosofo)
; 	    (make element gi: %subsect-gi%
; 		  (make element gi: "title"
; 			(literal "Description"))
; 		  (process-node-list descrip)))
; 	(process-children))))

; (element dtdattribute
;   (let ((name (attribute-string "NAME"    (current-node)))
; 	(def  (attribute-string "DEFAULT" (current-node)))
; 	(type (attribute-string "TYPE"    (current-node)))
; 	(cont (data (current-node))))
;     (make element gi: %subsect-gi%
; 	  (make element gi: "title"
; 		(literal "Attribute " name))
; 	  (make element gi: "p"
; 		(make element gi: "ul"
; 		      (make element gi: "li"
; 			    (literal "Default: " def))
; 		      (if type
; 			  (make element gi: "li"
; 				(literal "Type: " type))
; 			  (empty-sosofo))
; 		      (if (> (string-length cont) 0)
; 			  (make element gi: "li"
; 				(literal "Allowed values: " cont))
; 			  (empty-sosofo)))))))

(element dtdparents
    (make element gi: %subsect-gi%
	  (make element gi: "title"
		(literal "Parents"))
	  (make element gi: "p"
		(with-mode dtdelemref-list
		  (process-children)))))

(element dtdcontent
    (make element gi: %subsect-gi%
	  (make element gi: "title"
		(literal "Content"))
	  (make element gi: "p"
		(process-children))))

(element dtdtree
    (make element gi: %subsect-gi%
	  (make element gi: "title"
		(literal "Content tree"))
	  (make element gi: "p"
		(make element gi: "verbatim"
		      (process-children)))))

(element dtdelemref
  (let ((gi (attribute-string "GI" (current-node))))
    (make empty-element gi: "ref"
	  attributes: (list (list "id" (string-append (xref-prefix) gi))
			    (list "text" gi)))))
(mode dtdelemref-list
  (element dtdelemref
    (let ((gi (attribute-string "GI" (current-node))))
      (make sequence
	(make empty-element gi: "ref"
	      attributes: (list (list "id" (string-append (xref-prefix) gi))
				(list "text" gi)))
	(if (last-sibling? (current-node))
	    (empty-sosofo)
	    (literal ", "))))))

;; Following are elements in the dtddescription DTD
(element element
  (process-children))
(element attribute
  (process-children))
(mode discard-attribute
  (element attribute
    (empty-sosofo)))
(element elemref
  (let ((gi (attribute-string "GI" (current-node))))
    (make empty-element gi: "ref"
	  attributes: (list (list "id" (string-append (xref-prefix) gi))
			    (list "text" gi)))))
(element example
  (make sequence
    (literal "Example:")
    (make element gi: "verbatim"
	  (process-children))))
(element p
  (make element
    (process-children)))
(element em
  (make element
    (process-children)))
(element code
  (make element
    (process-children)))

;; Utility function
(define (document-element #!optional (node (current-node)))
  (node-property 'document-element
		 (node-property 'grove-root node)
		 default: #f ;(empty-node-list)
		 ))
