<![ ignore [
Title:
  Starlink stylesheet - sectioning

Author:
  Norman Gray, Glasgow (NG)

Revision history
  February 1999 (original version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<func>
<routinename>$section-separator$
<description>If we are not chunking, then we need to separate sections
visually -- this emits a suitable sosofo.
<returnvalue type=sosofo>Empty sosofo or <code/HR/, depending on whether
we're chunking or not
<argumentlist none>
<codebody>
(define ($section-separator$) 
  (if (or (chunking?)
	  (node-list=? (current-node) (document-element)))
      (empty-sosofo)
      (make empty-element gi: "HR")))

<func>
<routinename>$section$
<description>Simple function which should be called for all sectioning 
commandd which might be chunked
<returnvalue type=sosofo>Either an HTML document, or else the contents of the
section ready to flow into whatever contains this.
<parameter optional default='($section-body$)'>bod
  <type>sosofo
  <description>The body of the section.
<codebody>
(define ($section$ #!optional (bod ($section-body$)))
  (html-document (with-mode section-reference
		   (process-node-list (current-node)))
		 bod))

<func>
<routinename>$section-body$
<description>A standard section body
<returnvalue type=sosofo>A standard layout for the body of a section
<argumentlist none>
<codebody>
(define ($section-body$)
  (make sequence
    ($section-separator$)
    ($section-title$)
    (process-children)))

<func>
<routinename>$section-title$
<description>
Returns a sosofo with the section heading, consisting of an Hn
element, enclosing an A element with a NAME attribute, enclosing
the section title.
<returnvalue type=sosofo>The section's title, including numbering if any
<argumentlist none>
<codebody>
(define ($section-title$)
  (make sequence
    (make element
      gi: (string-append "H" (number->string (sectlevel)))
      (if (attribute-string (normalize "id") (current-node))
	  (make element
	    gi: "A"
	    attributes: (list (list "name"
				    (attribute-string (normalize "id")
						      (current-node))))
	    (with-mode section-reference
	      (process-node-list (current-node))))
	  (with-mode section-reference
	    (process-node-list (current-node)))))))

<misccode>
<description>Rules for the various section elements in the DTD
<codebody>
(element sect ($section$))
(element subsect ($section$))
(element subsubsect ($section$))
(element subsubsubsect ($section$))
(element appendices ($section$))

;; Discard the subhead, except when we're in in-section-head mode
(element subhead
  (empty-sosofo))
(mode in-section-head
  (element subhead
    (process-children-trim)))

