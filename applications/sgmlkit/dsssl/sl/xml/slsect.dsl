<![ ignore [

Title:
  Starlink General DTD -- XML stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<func>
<routinename>$xml-section$
<description>Simple function which should be called for all sectioning
commands.
<returnvalue type=sosofo>Produces a sosofo which creates the section heading.
<parameter>level
  <type>number
  <description>level of the section
<parameter keyword default="#t">children
  <type>boolean
  <description>If true, then invoke <funcname/process-children/, too. 
  It would be necessary to switch this off if there is some reason why
  the element contents should <em/not/ be taken to be the section contents.
<codebody>
(define ($xml-section$ level #!key (children #t))
  (let ((id (attribute-string (normalize "id")
			      (current-node))))
    (make element gi: "div"
	  attributes: (list (list "level" (number->string level))
			    ;(if id
				;(list "id" id)
				;#f)
			    )
	  (make element gi: "divtitle"
		(with-mode section-reference
		  (process-node-list (current-node))))
	  (if children
	      (make element gi: "divbody"
		    (process-children))
	      (empty-sosofo)))))

<misccode>
<description>
Section constructors

<codebody>
(element sect ($xml-section$ 1))
(element subsect ($xml-section$ 2))
(element subsubsect ($xml-section$ 3))
(element subsubsubsect ($xml-section$ 4))
;(element appendices ($xml-section$ "section"))

;; Discard the subhead, in general.
;; This is so we can call (process-children) on sections, and only process
;; the body.  We only need to go inside the subhead when we're in 
;; section-reference mode.
(element subhead
  (empty-sosofo))

