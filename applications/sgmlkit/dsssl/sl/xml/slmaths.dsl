<![ IGNORE [

Title:
  Starlink General DTD -- XML stylesheet for maths

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<routine>
<description>
Support for maths in XML documents.  This is easy, because all we do is 
remove the MLINE and MLABEL elements, taking care to record their equation
numbers.
<codebody>
;;; Note this is NOT COMPLETE because it avoids dealing with MLABEL
(element m
  (make element gi: "maths"
	attributes: '(("class" "inline"))
	(process-children)))
(element mequation
  (let ((mlabels (select-elements (children (current-node))
				  (normalize "mlabel"))))
  (make element gi: "maths"
	attributes: (list (list "class" "display"))
	(process-children))))
(element meqnarray
  (make element gi: "maths"
	attributes: '(("class" "display"))
	(process-children)))
(element mline
  (make sequence
    (process-children)
    (literal (if (last-sibling?) "" "\\\\"))))
(element mlabel
  (empty-sosofo))

(mode section-reference
  (element mlabel
    (literal "MLABEL-REFERENCE")))
;(mode section-reference
;  (element mlabel
;    (make command name: "Eqnref"
;	  (literal (get-equation-number)))))

