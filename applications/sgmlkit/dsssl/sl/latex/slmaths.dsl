<![ ignore [

Title:
  Starlink General DTD -- LaTeX stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<misccode>
<description>
Support for maths in LaTeX documents.  This is, obviously, very easy!
Set the inherited characteristic <code/escape-tex?/ to <code/#f/, so that 
raw LaTeX within the environments isn't escaped.
<codebody>
(element m
  (make environment brackets: '("\\(" "\\)")
	escape-tex?: #f
	(process-children)))
(element mequation
  (make environment brackets: '("\\[" "\\]")
	escape-tex?: #f
	(process-children)))
(element meqnarray
  (make environment name: "eqnarray*"
	escape-tex?: #f
	(process-children)))
