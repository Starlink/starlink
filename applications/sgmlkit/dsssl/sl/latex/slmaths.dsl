<!--

Title:
  Starlink General DTD: LaTeX stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
-->

<!--
  The following was an attempt to support equation numbering for maths
  environments.  This turns out to be bizarrely difficult!  The
  problem is that, to find an equation's number in the case where the
  equation elements have a `numbered' attribute, we need to find all
  the elements before it which are either of mline or mequation _and_
  which have the `numbered' attribute present.  I can find the number
  of elements before a specified one using element-number, but that
  doesn't tell me which ones have the attribute; I can find all the
  elements with a particular attribute using select-elements, but it's
  then hard to find the number of the `interesting' one within that
  (I can imagine a semi-complicated algorithm using tree-before?, but
  that's only supported in Open Jade, unless you want to use the
  sample definitions in the DSSSL standard, which look insanely
  inefficient).
-->

<routine>
<description>
Support for maths in LaTeX documents.  This is, obviously, very easy!
Set the inherited characteristic <code>escape-tex?</code> to <code>#f</code>, so that 
raw LaTeX within the environments isn't escaped.
<codebody>
(element m
  (make environment brackets: '("$" "$")
	escape-tex?: #f
	(process-children)))

(element meqnarray
  (make environment name: "eqnarray"
	escape-tex?: #f
	(process-children)))
(element mequation
  (make environment name: "equation"
	escape-tex?: #f
	(process-children)))
(element mline
  (make sequence
    (process-children)
    (literal (if (last-sibling?) "" "\\\\"))))
(element mdefs
  (make sequence
    (make environment brackets: '("
%% mdefs...
" "
%% ...mdefs
")
	  escape-tex?: #f
	  (process-children))))
(element mlabel
  (make empty-command name: "SetEqnNum"
	parameters: (list (get-equation-number))))
(mode section-reference
  (element mlabel
    (make command name: "Eqnref"
	  (literal (get-equation-number)))))

