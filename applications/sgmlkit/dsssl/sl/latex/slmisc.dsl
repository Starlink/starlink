<!--

Title:
  Starlink General DTD: LaTeX stylesheet miscellaneous elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
-->

<routine>
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
		   '("eps" "latexgraphics")))
	 (float (attribute-string (normalize "float"))))
  (if (and float
	   (string=? float "float"))
      (make environment name: "figure"
	    parameters: `(,%latex-float-spec%)
	    (if content
		(process-node-list content)
		(literal "No processable content"))
	    (process-matching-children 'caption))
      (make environment brackets: '("{" "}")
	    (make empty-command name: "SetCapType"
		  parameters: '("figure"))
	    (process-node-list content)
	    (process-matching-children 'caption)))))

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

;; Changes here might need matching changes in 
;; mode make-manifest-mode in sl.dsl.
;;
;; NOTE that the ent-sysid which is used is _stripped_ of its path.
;; It is the responsibility of this stylesheet's harness to make sure
;; that the positions of entities like this are resolved post-hoc.
(element figurecontent
  (let* ((ent (attribute-string (normalize "image")
				(current-node)))
	 ;;(ent-sysid (and ent
	 ;;		 (entity-system-id ent)))
	 (ent-sysid (and ent
			 (car (reverse
			       (tokenise-string
				(entity-system-id ent)
				boundary-char?: (lambda (c)
						  (char=? c #\/)))))))
	 (ent-notation (and ent-sysid
			    (entity-notation ent))))
    (if ent-notation
	(case ent-notation
	  (("eps")
	   (make empty-command name: "includegraphics"
		 escape-tex?: #f
		 parameters: (list ent-sysid)))
	  (("latexgraphics")
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
	      (if (string=? cont-notation "latexgraphics")
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
		  '("eps" "latexgraphics"))))
    (if content
	(process-node-list content)
	(error "Can't process coverimage"))))

<routine>
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

(element foreign
  (make command name: "textit"
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
  (make environment name: "quote"
	(make environment name: "small"
	      (make environment
		name: "verbatim"
		recontrol: "/-/"
		escape-tex?: #f
		(process-children)))))

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

;;; Angle element
; There's no error-checking here, so if the user includes an angle
; element with _no_ angles within it, or includes, say, just an angle
; and fractions of a second, this won't pick it up.
(element angle
  (let ((ishours (and (attribute-string (normalize "unit"))
		      (string=? (attribute-string (normalize "unit"))
				"hours"))))
    (make environment brackets: '("$" "$")
	  (apply sosofo-append
		 (map (lambda (el)
			(let ((val (attribute-string
				    (normalize (symbol->string el)))))
			  (if val
			      (make empty-command
				name: (string-append (if ishours "hms" "dms")
						     (symbol->string el))
				parameters: `(,val))
			      (empty-sosofo))))
		      '(angle minutes seconds fraction))))))
