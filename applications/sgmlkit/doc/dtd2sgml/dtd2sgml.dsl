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

;; The xref-id function is slightly problematic.  The elemref FO
;; rule (within the dtddescription DTD) is within a grove constructed
;; by a (sgml-parse), so that it cannot reach into the main grove, to
;; find out what sort of document-element the main grove is for.
;; Solve this problem by defining a DOCELEM attribute in the
;; dtddescription element, which must match the TOP attribute for the
;; dtdelementlist element.  As a bonus, this allows an additional
;; check, within the FO rule for dtdelementlist, that the element list
;; and description match.
(define (xref-id giname)
  (let ((docelem (attribute-string "DOCELEM" (document-element)))
	(top (attribute-string "TOP" (document-element))))
    (cond (docelem (string-append "el." docelem "." giname))
	  (top (string-append "el." top "." giname))
	  (else (error "Can't find DOCELEM or TOP")))))

(root
    (process-children))

;; Return the grove root for the dtddescription document.
(define (get-descrip-grove)
  (let ((desc-sysid (attribute-string "DESCRIPTION"
				       (document-element (current-node)))))
    (sgml-parse desc-sysid)))

(element dtdelementlist
  (let ((my-docelem
	 (attribute-string "TOP"     (document-element (current-node))))
	(descrip-docelem
	 (attribute-string "DOCELEM" (document-element (get-descrip-grove)))))
    (if (string=? my-docelem descrip-docelem) ;check match
	(process-children)
	(error (string-append "Element list for " my-docelem
			      " does not match description for "
			      descrip-docelem)))))

(element dtdelement
  (let* ((gi (attribute-string "GI" (current-node)))
	 (descrip (element-with-id gi (get-descrip-grove))))
    (make element gi: %sect-gi%
	  attributes: (list (list "id" (xref-id gi)))
	  (make element gi: "title"
		(literal "Element " gi))
	  (if (node-list-empty? descrip)
	      (empty-sosofo)
	      (make element gi: %subsect-gi%
		    (make element gi: "title"
			  (literal "Description"))
		    (with-mode discard-attribute
		      (process-node-list descrip))))
	  (process-children))))

(element dtdattribute
  (let* ((name (attribute-string "NAME"    (current-node)))
	 (def  (attribute-string "DEFAULT" (current-node)))
	 (type (attribute-string "TYPE"    (current-node)))
	 (cont (data (current-node)))
	 (gi (attribute-string "GI" (parent (current-node))))
	 (el-descrip (element-with-id gi (get-descrip-grove)))
	 (att-descrip (select-elements (children el-descrip)
				       (list "ATTRIBUTE" (list "NAME" name)))))
    (make element gi: %subsect-gi%
 	  (make element gi: "title"
 		(literal "Attribute " gi "/" name))
	  (if (node-list-empty? att-descrip)
	      (empty-sosofo)
	      (process-node-list att-descrip))
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
	(process-children)))

(element dtdcontentmodel
  (make element gi: "p"
	(process-children)))

(element dtdcontenttree
  (make element gi: "p"
	(make element gi: "verbatim"
	      (process-children))))

(element dtdelemref
  (let ((gi (attribute-string "GI" (current-node))))
    (make empty-element gi: "ref"
	  attributes: (list (list "id" (xref-id gi))
			    (list "text" gi)))))
(mode dtdelemref-list
  (element dtdelemref
    (let ((gi (attribute-string "GI" (current-node))))
      (make sequence
	(make empty-element gi: "ref"
	      attributes: (list (list "id" (xref-id gi))
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
  (let ((gi (attribute-string "GI" (current-node)))
	(self (attribute-string "SELF" (current-node))))
    (if self
	(literal gi)
	(make empty-element gi: "ref"
	      attributes: (list (list "id" (xref-id gi))
				(list "text" gi))))))
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
