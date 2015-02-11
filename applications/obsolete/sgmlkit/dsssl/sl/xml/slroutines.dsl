<!--
Title:
  Starlink stylesheet - routine list

Author:
  Norman Gray, Glasgow (NG)

Revision history
  February 1999 (original version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
-->

(element routinelist			; discard at present
  (empty-sosofo))

<![ IGNORE [
<routine>
<description>Constructors for the ROUTINELIST element (LaTeX)
<codebody>
(mode section-reference
  ;;(element routinelist
  ;;  (make-section-reference title: (literal "Routine list")))
  (element routinelist
    (literal "Routine list"))
  (element codecollection
    (make-section-reference
     set-prefix: (literal (number->string (child-number)) " ")
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
    (let ((affil (attribute-string (normalize "affiliation")))
	  (id (attribute-string (normalize "id"))))
      (make sequence
	(make empty-command name: "item")
	(process-children)
	(if affil
	    (literal (string-append " (" affil ")"))
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
  (element routineopener
    (empty-sosofo))
  (element description
    (process-children))
  
  ;; The funcname element could be made more sophisticated, so that
  ;; it includes a link (possibly using the source-code browser) to
  ;; the function definition/documentation.
  (element funcname
    (make command name: "texttt"
          (literal (string-append "(" (data (current-node)) ")"))))
  ;; discard the following elements, at present
  (element codebody
    (empty-sosofo))
  ;;(element misccode
  ;;  (empty-sosofo))
  )

(mode routine-ref-sst
  (element routineprologue
    (let ((kids (nl-to-pairs (children (current-node)))))
      (make sequence
	(make command name: "sstroutine"
	      (let ((name (assoc (normalize "routinename") kids)))
		(if name
		    (process-node-list (cdr name))
		    (literal "Unknown name!"))))
	(make environment brackets: '("{" "}")
	      (let ((purp (assoc (normalize "purpose") kids)))
		(if purp
		    (process-node-list (cdr purp))
		    (empty-sosofo))))
	(make environment brackets: '("{" "}")
	      (make sequence
		(apply sosofo-append
		       (map (lambda (gi)
			      (let ((gi-and-nd (assoc (normalize gi) kids)))
				(if gi-and-nd
				    (process-node-list (cdr gi-and-nd))
				    (empty-sosofo))))
			    '(;;"routinename"
			      ;;"purpose"
			      "description"
			      "userkeywords"
			      "softwarekeywords"
			      "returnvalue"
			      "argumentlist"
			      "parameterlist"
			      ;;"authorlist"
			      ;;"history"
			      "usage"
			      "invocation"
			      "examplelist"
			      "implementationstatus"
			      "bugs")))
		; now collect together the diytopics
		(apply sosofo-append
		       (map (lambda (gi-and-nd)
			      (if (string=? (normalize (car gi-and-nd))
					    (normalize "diytopic"))
				  (process-node-list (cdr gi-and-nd))
				  (empty-sosofo)))
			    kids)))))))
  (element routinename
    (process-children))
  (element name
    (make command name: "Code"
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
  ;;  (element purpose
  ;;    (make sequence
  ;;      (make command name: "textbf"
  ;;	    (literal "Purpose: "))
  ;;      (process-children)))
  (element description
    (make command name: "sstdescription"
	  (process-children)))
  (element userkeywords
    (make command name: "sstdiytopic"
	  parameters: '("Keywords")
	  (process-children)))
  (element softwarekeywords
    (make command name: "sstdiytopic"
	  parameters: '("Code group")
	  (process-children)))
  (element returnvalue
    (let ((none-att (attribute-string (normalize "none")))
	  (type-att (attribute-string (normalize "type"))))
      (make command name: "sstreturnedvalue"
	    (if none-att
		(make command name: "emph"
		      (literal "No return value")) ;...and discard any data
		(process-children)))))
  (element parameterlist
    (let ((none-att (attribute-string (normalize "none"))))
      (make command name: "sstparameters"
	    (if none-att
		(make command name: "sstsubsection"
		      (literal "No parameters"))
		(process-children)))))
  (element argumentlist
    (let ((none-att (attribute-string (normalize "none"))))
      (make command name: "sstarguments"
	    (if none-att
		(literal "No arguments")
		(process-children)))))
  (element parameter
    (let* ((kids (children (current-node)))
	   (name (select-elements kids (normalize "name")))
	   (type (select-elements kids (normalize "type")))
	   (desc (select-elements kids (normalize "description")))
	   ;;(opt-att (attribute-string (normalize "optional")))
	   (given-att (attribute-string (normalize "given")))
	   (returned-att (attribute-string (normalize "returned"))))
      (make sequence
	(make command name: "sstsubsection"
	      (make sequence
		(process-node-list name)
		(literal "=")
		(process-node-list type)
		(literal "("
			 (cond
			  ((and given-att returned-att)
			   "given and returned")
			  (given-att "given")
			  (returned-att "returned")
			  (else		;default is given
			   "given"))
			 ")")))
	(make environment brackets: '("{" "}")
	      (process-node-list desc)))))
  (element examplelist
    (make command name: "sstexamples"
	  (process-children)))
  (element example
    (make sequence
      (make command name: "sstexamplesubsection"
	    (process-children))))
  (element usage
    (make command name: "sstusage"
	  (process-children)))
  (element invocation
    (make command name: "sstinvocation"
	  (process-children)))
  (element implementationstatus
    (make command name: "sstimplementationstatus"
	  (process-children)))
  (element bugs
    (make command name: "sstbugs"
	  (process-children)))
  (element diytopic
    (let ((kids (children (current-node))))
      (make sequence
	(make command name: "sstdiytopic"
	      parameters: (list (data (node-list-first kids)))
	      (process-node-list (node-list-rest kids))))))
  )


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

]]>
