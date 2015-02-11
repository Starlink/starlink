<!--

Title:
  Starlink General DTD: TeXML stylesheet miscellaneous elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, 2004, Council for the Central Laboratory of the Research Councils

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
         (content (figurecontent-to-notation-map
                   (node-list (select-elements kids
                                               (normalize "figurecontent"))
                              (select-elements kids (normalize "px")))))
	 (float (attribute-string (normalize "float")))
         (figurecontent-sosofo (*make-figurecontent-sosofo* content)))
  (if (and float
	   (string=? float "float"))
      (make-latex-environment name: "figure"
	    parameters: `(,%latex-float-spec%)
	    (if content
		figurecontent-sosofo
		(literal "No processable content"))
	    (process-matching-children 'caption))
      (make element gi: "group"
	    (make-latex-empty-command name: "SetCapType"
		  parameters: '("figure"))
	    figurecontent-sosofo
	    (process-matching-children 'caption)))))

;; Choose which of the content elements to put into the output.  CONTENT
;; is a list of (notation . node-list) pairs.  Returns a sosofo.
(define (*make-figurecontent-sosofo* content)
  (let ((eps-content (assoc "eps" content))
        (pdf-content (assoc "pdf" content))
        (latexgraphics-content (assoc "latexgraphics" content))
        (text-content (assoc "XML" content)))
    (cond
     ;; Following are in preference order -- first EPS/PDF, then latexgraphics
     ;; finally text
     ((or eps-content pdf-content)
      (make sequence
        (make-latex-empty-command name: "ifpdf")
        (if pdf-content
            (process-node-list (cdr pdf-content))
            (empty-sosofo))
        (if eps-content
            (make sequence
              (make-latex-empty-command name: "else")
              (process-node-list (cdr eps-content)))
            (empty-sosofo))
        (make-latex-empty-command name: "fi")))
     (latexgraphics-content
      (process-node-list (cdr latexgraphics-content)))
     (text-content
      ;; CONTENT may have more than one node with notation "XML" (ie,
      ;; more than one paragraph).  Form a list of _all_ the nodes
      ;; with this notation, apply node-list to it, and process it.
      (process-node-list
       (apply node-list (map (lambda (p)
                               (if (string=? (car p) "XML")
                                   (cdr p)
                                   (empty-node-list)))
                             content)))))))

(element caption
  (let ((caption-details (get-caption-details (parent (current-node))))
	;; optional argument of caption command mustn't be more than
	;; a para.
	(firstpara (node-list-first (children (current-node)))))
    (make-latex-command name: "Caption"
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
	   (make-latex-empty-command name: "includegraphics"
		 escape-tex?: #f
		 parameters: (list ent-sysid)))
	  (("pdf")
	   (make-latex-empty-command name: "includegraphics"
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
         (content (figurecontent-to-notation-map
                   (node-list (select-elements kids
                                               (normalize "figurecontent"))
                              (select-elements kids (normalize "px"))))))
    (if content
        (*make-figurecontent-sosofo* content)
        (error "Can't process coverimage"))))
;(element coverimage
;  (let* ((kids (children (current-node)))
;	 (content (get-best-figurecontent
;		   (node-list (select-elements kids
;					       (normalize "figurecontent"))
;			      (select-elements kids
;					       (normalize "px")))
;		  '("eps" "latexgraphics"))))
;    (if content
;	(process-node-list content)
;	(error "Can't process coverimage"))))

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
        (make-latex-command name: "Url"	; type=fspath is only one at present
                            (process-children)) ; present as URL to get good hyphen'n
        (make-latex-command name: "Code"
                            (process-children)))))

(element em
  (make-latex-command name: "emph"
	(process-children)))

(element foreign
  (make-latex-command name: "textit"
	(process-children)))

(element kbd
  (make-latex-command name: "Kbd"
	(process-children)))

(element quote
  (make-latex-command name: "Quote"
	(process-children)))

(element strong
  (make-latex-command name: "Strong"
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
  (make-latex-environment name: "description"
    (process-children)))

(element dt
  (let ((is-compact? (attribute-string (normalize "compact")
				       (parent (current-node)))))
    (make sequence
      (make-latex-command name: "DTitem"
	    (make sequence
	      (if (attribute-string (normalize "id"))
		  (make-latex-command name: "label"
			(literal (gen-label)))
		  (empty-sosofo))
	      (process-children)))
      (if is-compact?
	  (empty-sosofo)
          (make sequence
            (make-latex-empty-command name: "hfill")
            (latex-newline))))))

(element dd
  (process-children))

(element ol
  (make-latex-environment name: "enumerate"
    (process-children)))

(element ul
  (make-latex-environment name: "itemize"
    (process-children)))

(element li
  (make sequence
    (make-latex-empty-command name: "item")
    (if (attribute-string (normalize "id"))
	(make-latex-command name: "label"
	      (literal (gen-label)))
	(empty-sosofo))
    (process-children)))

(mode section-reference
  (element dt
    (make sequence
      (literal "item `")
      ;; this dt had better be short...
      (process-children)
      (literal "'")))
  ;; Have references to li elements just produce the item number.
  ;; This isn't going to be much use by itself for a reference far
  ;; from the target, but in that case one assumes the reference would
  ;; be supplemented by a reference to the section.
  (element li
    (literal (string-append "item "
			    (number->string (child-number (current-node))))))
;  (element dt
;    (make sequence
;      (literal "in section ")
;      (make-section-reference
;       target: (ancestor-member (current-node)
;				(section-element-list)))))
;   (element li
;     (if (string=? (gi (parent (current-node)))
; 		  (normalize "ol"))
; 	(make command name: "ref"
; 	      (literal (gen-label)))
; 	(make sequence
; 	  (literal "in section ")
; 	  (make-section-reference
; 	   target: (ancestor-member (current-node)
; 				    (section-element-list))))))
  )

;;; Paragraphing

(element p
  (make-latex-paragraph
    (process-children)))

(element px
  (make-latex-paragraph
    (process-children-trim)))

(element cite
  (make-latex-command name: "Cite"
    (process-children-trim)))

(element blockquote
  (make-latex-environment name: "quote"
    (process-children-trim)))

(element linespecific
  (make-latex-environment name: "verse"
    (process-children-trim)))
(element line
  (make sequence
    (process-children-trim)
    (make fi data: "\\\\")))

(element verbatim
  (make element gi: "TeXML" attributes: '(("emptylines" "1"))
        (make-latex-environment
         name: "Verbatimlines"
         recontrol: "/-/"
         (process-children-trim))))

(element attribution
  (make-latex-command name: "textit"
    (make sequence
      (literal "[ ")
      (process-children)
      (literal " ]"))))

(element draftnote
  (make-latex-command name: "textbf"
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
    (make-latex-environment brackets: '("$" "$")
	  (apply sosofo-append
		 (map (lambda (el)
			(let ((val (attribute-string
				    (normalize (symbol->string el)))))
			  (if val
			      (make-latex-empty-command
				name: (string-append (if ishours "hms" "dms")
						     (symbol->string el))
				parameters: `(,val))
			      (empty-sosofo))))
		      '(angle minutes seconds fraction))))))

;; Indexing is handled in slback.dsl
