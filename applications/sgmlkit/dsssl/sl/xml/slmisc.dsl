<![ ignore [

Title:
  Starlink General DTD -- XML stylesheet miscellaneous elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<misccode>
<description>
Support figures and figurecontent.  Omitted at present!
Changes here might need matching changes in
mode make-manifest-mode in sl.dsl.
<codebody>
(element figure
  (empty-sosofo))

<misccode>
<description>
Miscellaneous constructors.  These are constructors which don't really fit
in anywhere else.  They're not necessarily unimportant, just simple enough not
to need explanation or elaboration.

<codebody>
;;; Phrase markup

(define (span-type class)
  (make element gi: "span"
	attributes: (list (list "class" class))
	(process-children)))

(element code
  (span-type "code"))

(element em
  (span-type "emph"))

(element kbd
  (span-type "code"))

(element quote
  (span-type "quote"))

(element strong
  (span-type "strong"))

;;; Lists

(element dl
  (make element gi: "list"
	attributes: '(("class" "description"))
	(process-children)))

(element dt
  (make element gi: "listlabel"
	(process-children)))

(element dd
  (make element gi: "listitem"
	(process-children)))

(element ol
  (make element gi: "list"
	attributes: '(("class" "ordered"))
	(process-children)))

(element ul
  (make element gi: "list"
	attributes: '(("class" "unordered"))
	(process-children)))

(element li
  (make element gi: "listitem"
	(process-children)))

;;; Paragraphing

(element p
  (make element gi: "para"
	(process-children)))

(element px
  (make element gi: "para"
	(process-children)))

(element cite
  (span-type "citation"))

(element blockquote
  (make element gi: "para"
	attributes: '(("class" "quotation"))
	(process-children-trim)))

(element verbatim
  (make element gi: "para"
	attributes: '(("class" "verbatim"))
	(process-children)))

(element attribution
  (span-type "emph"))

(element draftnote
  (make element gi: "span"
	attributes: '(("class" "strong"))
	(literal "Draft Note:")
	(process-children)))

(element update				; ignore in default mode
  (empty-sosofo))
