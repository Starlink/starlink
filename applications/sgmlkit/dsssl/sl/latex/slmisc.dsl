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
  (make environment name: "figure"
	(process-matching-children 'figurecontent)
	(process-matching-children 'caption)))

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
  (let* ((image-ents (attribute-string (normalize "image")
				       (current-node)))
	 (best-ent (and image-ents
			(get-sysid-by-notation image-ents
					       '("EPS")))))
    (if image-ents
	(if best-ent
	    (make empty-command name: "includegraphics"
		  parameters: (list best-ent))
	    (error "No suitable entity in figurecontent"))
	(process-children))))

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
  (make empty-command name: "item"
	parameters: (list (string-append "?" (data (current-node))))))

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
  (make environment name: "quotation"
    (process-children-trim)))

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
