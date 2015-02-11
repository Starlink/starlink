<!--

Title:
  Starlink General DTD: XML stylesheet miscellaneous elements

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
  (make element gi: "figure"
	(process-children)))

(element caption
  (make element gi: "caption"
	(process-children)))

;; Changes here might need matching changes in 
;; mode make-manifest-mode in sl.dsl.
;;
;; NOTE that the ent-sysid which is used is _stripped_ of its path.
;; It is the responsibility of this stylesheet's harness to make sure
;; that the positions of entities like this are resolved post-hoc.
(element figurecontent
  (make element gi: "figurecontent"
	(literal "FIGURE")))

(element coverimage
  (make element gi: "coverimage"
	(process-children)))

<routine>
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
	(process-children-trim)))

(element px
  (make element gi: "para"
	(process-children-trim)))

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

;(element note
;  (make element gi: "note"
;	(process-children)))
;
;(element index
;  (make element gi: "index"
;	(process-children)))
;
;(element citation
;  (make element gi: "citation"
;	(process-children)))

(element note
  (copy-element))

(element index
  (copy-element))

(element citation
  (copy-element))

(element draftnote
  (make element gi: "span"
	attributes: '(("class" "strong"))
	(literal "Draft Note:")
	(process-children)))

;;; Angle element
; There's no error-checking here, so if the user includes an angle
; element with _no_ angles within it, or includes, say, just an angle
; and fractions of a second, this won't pick it up.
(element angle
  (copy-element))

