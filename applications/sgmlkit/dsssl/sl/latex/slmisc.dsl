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
Support figures and figurecontent.
Changes here might need matching changes in
mode make-manifest-mode in sl.dsl.
<codebody>
(element figure
  (let* ((kids (children (current-node)))
	 (content (get-best-figurecontent
		   (node-list (select-elements kids
					       (normalize "figurecontent"))
			      (select-elements kids (normalize "px")))
		   '("EPS" "LATEXGRAPHICS"))))
  (make environment name: "figure"
	(if content
	    (process-node-list content)
	    (literal "No processable content"))
	(process-matching-children 'caption))))

(element caption
  (let ((caption-details (get-caption-details (parent (current-node))))
	;; optional argument of caption command mustn't be more than
	;; a para.
	(firstpara (node-list-first (children (current-node)))))
    (make command name: "Caption"
	  parameters: (list (string-append (car caption-details)
					   (if show-element-ids
					       (display-element-ids)
					       ""))
			    (cadr caption-details)
			    (string-append "?" (data firstpara)))
	  (process-children))))

(element figurecontent
  (let* ((ent (attribute-string (normalize "image")
				(current-node)))
	 (ent-sysid (and ent
			 (entity-system-id ent)))
	 (ent-notation (and ent-sysid
			    (entity-notation ent))))
    (if ent-notation
	(case ent-notation
	  (("EPS")
	   (make empty-command name: "includegraphics"
		 escape-tex?: #f
		 parameters: (list ent-sysid)))
	  (("LATEXGRAPHICS")
	   (let ((package (entity-attribute-string ent
						   (normalize "package")
						   (current-node))))
	     (if package
		 (error "Entity attribute PACKAGE not supported")
		 (make fi data: (read-entity ent-sysid)))))
	  (else
	   (error (string-append "Can't process entities of type "
				 ent-notation))))
	(let ((cont-notation (attribute-string (normalize "notation")
					       (current-node))))
	  (if cont-notation
	      (if (string=? cont-notation "LATEXGRAPHICS")
		  (make fi data: (data (current-node)))
		  (error (string-append
			  "Can't process inline graphics of type "
			  cont-notation)))
	      (error "Can't extract entity"))))))

(element coverimage
  (let* ((kids (children (current-node)))
	 (content (get-best-figurecontent
		   (node-list (select-elements kids
					       (normalize "figurecontent"))
			      (select-elements kids
					       (normalize "px")))
		  '("EPS" "LATEXGRAPHICS"))))
    (if content
	(process-node-list content)
	(error "Can't process coverimage"))))

<misccode>
<description>
Miscellaneous constructors.  These are constructors which don't really fit
in anywhere else.  They're not necessarily unimportant, just simple enough not
to need explanation or elaboration.

<codebody>
;;; Phrase markup

(element code
  (let ((codetype (attribute-string (normalize "type"))))
    (if codetype
	(make command name: "Url"	; type=fspath is only one at present
	      (process-children))	; present as URL to get good hyphen'n
	(make command name: "Code"
	      (process-children)))))

(element em
  (make command name: "emph"
    (process-children)))

(element kbd
  (make command name: "Kbd"
	(process-children)))

(element quote
  (make command name: "Quote"
	(process-children)))

(element strong
  (make command name: "Strong"
    (process-children)))

;;; Lists

(element dl
  (make environment name: "description"
    (process-children)))

(element dt
  (let ((is-compact? (attribute-string (normalize "compact")
				       (parent (current-node)))))
    (make sequence
      (make command name: "DTitem"
	    (process-children))
      (if is-compact?
	  (empty-sosofo)
	  (make fi data: "\\hfill\\\\")))))

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
  (make command name: "Cite"
    (process-children-trim)))

(element blockquote
  (make environment name: "quote"
    (process-children-trim)))

(element linespecific
  (make environment name: "verse"
    (process-children-trim)))
(element line
  (make sequence
    (process-children-trim)
    (make fi data: "\\\\")))

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
  (make command name: "textbf"
	(literal "Draft Note:")
	(process-children)))

(element update				; ignore in default mode
  (empty-sosofo))
