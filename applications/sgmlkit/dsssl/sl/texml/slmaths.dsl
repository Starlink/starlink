<!--

Title:
  Starlink General DTD: TeXML stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, 2004, Council for the Central Laboratory of the Research Councils

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
Support for maths in TeXML documents.  Since the content of these
elements is defined to be LaTeX markup, we have to be a bit tricksy
about escaping just the right amount.
<codebody>
(element m
  (make element gi: "math"
        (make element gi: "TeXML" attributes: '(("escape" "0"))
              (process-children))))
(element meqnarray
  (make-latex-environment name: "eqnarray"
                          (make element gi: "TeXML"
                                attributes: '(("escape" "0"))
                                (process-children))))
(element mequation
  (make-latex-environment name: "equation"
                          (make element gi: "TeXML"
                                attributes: '(("escape" "0"))
                                (process-children))))
(element mline
  (make sequence
    (process-children)
    (if (last-sibling?)
        (literal "")
        (latex-newline))))
(element mdefs
  (make sequence
    (make-latex-environment brackets: '("
%% mdefs...
" "
%% ...mdefs
")
                            (make element gi: "TeXML"
                                  attributes: '(("mode" "math"))
                                  (process-children)))))
(element mlabel
  (make-latex-empty-command name: "SetEqnNum"
	parameters: (list (get-equation-number))))
(mode section-reference
  (element mlabel
    (make-latex-command name: "Eqnref"
	  (literal (get-equation-number)))))

