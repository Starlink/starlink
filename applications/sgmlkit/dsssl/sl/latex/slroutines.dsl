<![ ignore [
Title:
  Starlink stylesheet - routine list

Author:
  Norman Gray, Glasgow (NG)

Revision history
  February 1999 (original version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<misccode>
<description>Constructors for the ROUTINELIST element (LaTeX)
<codebody>
(mode section-reference
  (element routinelist
    (make-section-reference title: (literal "Routine list")))
  (element codecollection
    (make-section-reference
     title: (with-mode routine-ref-get-reference
	      (process-codecollection (attribute-string (normalize "doc")))))))


;; Routinelist is simple
(element routinelist
  ($latex-section$ "section"))

;; Supporting the codecollection chunking/sectioning isn't as easy as with
;; the other such elements, because it doesn't have any children in this
;; document.  We have to do it rather more by hand, therefore.
;; Don't yet support the IDS attribute.
(element codecollection
  (make sequence
    ($latex-section$ "section" children: #f) ;don't process children
    (with-mode routine-ref
      (process-codecollection (attribute-string (normalize "doc"))))))

(define (process-codecollection docent)
  (let ((docelem (document-element-from-entity docent)))
      (process-node-list docelem)))

(mode routine-ref
  (element codegroup
    (make sequence
      (make command name: "subsection"
	    (literal "Code group: ")
	    (with-mode routine-ref-get-reference
	      (process-children)))
      (process-children)))
  (element codereference
    (let ((ref-docelem (document-element-from-entity
			(attribute-string (normalize "doc")))))
      ;; possibly make this a link, in future
      (make sequence
	(make command name: "subsection"
	      (literal "Code reference"))
	(literal "Refers to ")
	(make command name: "textit"
	      (with-mode routine-ref-get-reference
		(process-node-list ref-docelem)))
	(process-children))))
  (element docblock
    (process-children))
  (element title
    (make command name: "section"
	  (process-children)))
  (element (codereference docblock title) ; discard, in this mode
    (empty-sosofo))			; (see mode routine-ref-get-reference)
  (element (codegroup docblock title)	; discard, in this mode
    (empty-sosofo))			; (see mode routine-ref-get-reference)
  (element authorlist
    (make sequence
      (make command name: "subsubsection"
	    (literal "Authors"))
      (make environment name: "itemize"
	    (process-children))))
  (element author
    (let ((attrib (attribute-string (normalize "attribution")))
	  (id (attribute-string (normalize "id"))))
      (make sequence
	(make empty-command name: "item")
	(process-children)
	(if attrib
	    (literal (string-append " (" attrib ")"))
	    (empty-sosofo)))))
  (element authorref
    (let* ((aut-id (attribute-string (normalize "id")))
	   (aut-el (and aut-id
			(element-with-id aut-id)))
	   (note (attribute-string (normalize "note"))))
      (if (and (not (node-list-empty? aut-el))
	       (string=? (gi aut-el) (normalize "author")))
	  (make sequence
	    (with-mode routine-ref-get-reference
	      (process-node-list aut-el))
	    (if note
		(literal (string-append " (" note ")"))
		(empty-sosofo)))
	  (error (string-append "ID " aut-id " is not an AUTHOR element")))))
  (element authornote
    (make sequence
      (literal " --- ")
      (process-children)))
  (element otherauthors
    (make sequence
      (make empty-command name: "item")
      (make paragraph (literal "Other contributors"))
      (make environment name: "itemize"
	    (process-children))))
  (element copyright
    (make sequence
      (make command name: "subsection"
	    (literal "Copyright"))
      (process-children)))
  (element history
    (make sequence
      (make command name: "subsection"
	    (literal "Change history"))
      (make environment name: "description"
	    (process-children))))
  (element change
    (let ((auth-id (attribute-string (normalize "author")))
	  (date-str (attribute-string (normalize "date"))))
      (make sequence
	(make empty-command name: "item"
	      parameters: (list
			   (string-append "?" ; optional arg
					  (trim-data (element-with-id auth-id))
					  ", "
					  (format-date date-str))))
	(process-children))))
  ;; Process the codeprologue elements.  Code here relies on the
  ;; elements being in the `correct' order, and would have to be
  ;; changed if we adopt an unordered content model (ampersand connectors)
  (element codeprologue
      (process-children))
  (element routinename
    (make sequence
      (make command name: "subsection"
	    (process-children))))
  (element name
    (make command name: "textbf"
	  (process-children)))
  (element othernames
    (let* ((names (children (current-node)))
	   (namelist (node-list-reduce
		      names
		      (lambda (res i)
			(if (string=? res "")
			    (data i)
			    (string-append res ", " (data i))))
		      "")))
      (literal (string-append " (also: " namelist ")"))))
  (element purpose
    (make sequence
      (make command name: "textbf"
	    (literal "Purpose: "))
      (process-children)))
  (element description
    (process-children))
  (element returnvalue
    (let ((none-att (attribute-string (normalize "none")))
	  (type-att (attribute-string (normalize "type"))))
      (if none-att
	  (make command name: "subsubsection"
		(literal "No return value")) ;...and discard any data
	  (make sequence
	    (make command name: "subsubsection"
		  (literal "Return value"))
	    (if type-att
		(literal (string-append "Type: " type-att))
		(empty-sosofo))
	    (process-children)))))
  (element argumentlist
    (let ((none-att (attribute-string (normalize "none"))))
      (if none-att
	  (make command name: "subsubsection"
		(literal "No arguments"))
	  (make sequence
	    (make command name: "subsubsection"
		  (literal "Argument list"))
	    (process-children)))))
  (element parameter
    (let* ((kids (children (current-node)))
	   (name (select-elements kids (normalize "name")))
	   (type (select-elements kids (normalize "type")))
	   (desc (select-elements kids (normalize "description")))
	   (opt-att (attribute-string (normalize "optional")))
	   (dir-att (attribute-string (normalize "direction"))))
      (make sequence
	(process-node-list name)
	(literal (string-append " (" (data type) ") " dir-att
				(if opt-att
				    (string-append ", " opt-att)
				    "")))
	(process-node-list desc))))
  ;; The funcname element could be made more sophisticated, so that
  ;; it includes a link (possibly using the source-code browser) to
  ;; the function definition/documentation.
  (element funcname
    (make command name: "texttt"
          (literal (string-append "(" (data (current-node)) ")"))))
  ;; discard the following elements, at present
  (element codebody
    (empty-sosofo))
  (element misccode
    (empty-sosofo)))

;; Mode which includes assorted variants of the handlers above, designed
;; to extract information to which other handlers have made cross-reference.
(mode routine-ref-get-reference
  (default (empty-sosofo))
  (element programcode
    (process-matching-children 'docblock))
  (element docblock
    (process-matching-children 'title))
  (element title
    (process-children))
  (element author
    (let ((attrib (attribute-string (normalize "attribution"))))
      (make sequence
	(make empty-command name: "item")
	(process-matching-children 'name)
	(if attrib
	    (literal (string-append " (" attrib ")"))
	    (empty-sosofo)))))
  (element name
    (process-children)))

