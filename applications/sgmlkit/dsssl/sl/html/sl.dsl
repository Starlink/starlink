<!DOCTYPE programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN" [
<!entity slparams.dsl		system "slparams.dsl">
<!entity sldocs.dsl		system "sldocs.dsl">
<!entity slall.dsl		system "slall.dsl">
<!entity slsect.dsl		system "slsect.dsl">
<!entity slhtml.dsl		system "slhtml.dsl">
<!entity slnavig.dsl		system "slnavig.dsl">
<!entity version.dsl		system "version.dsl">
<!entity slroutines.dsl		system "slroutines.dsl">

<!entity lib.dsl		system "../lib/sllib.dsl" subdoc>
<!entity slcommon.dsl		system "../common/slcommon.dsl" subdoc>
]>

<docblock>
<title>Starlink to HTML stylesheet
<description>
<p>This is the DSSSL stylesheet for converting the Starlink DTD to HTML.

<p>Requires Jade, for the non-standard functions.  Lots of stuff learned from 
Mark Burton's dbtohtml.dsl style file, and Norm Walsh's DocBook DSSSL
stylesheets. 

<authorlist>
<author id=ng attribution='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council
<history>
<change author=ng date='09-FEB-1999'>Released a version 0.1 to the other
Starlink programmers.
</history>

<codereference doc="lib.dsl" id="code.lib">
<!-- <codegroup id="code.lib"> -->
<title>Library code
<description>
<p>Some of this library code is from the standard, some from Norm
Walsh's stylesheet, other parts from me

<!-- &lib.dsl -->
<!-- </codereference> -->

<codereference doc="slcommon.dsl" id="code.common">
<!-- <codegroup id="code.common"> -->
<title>Common code
<description>
<p>Code which is common to both the HTML and print stylesheets.

<!-- &slcommon.dsl -->
<!-- </codereference> -->

<codegroup use="code.lib code.common" id=html>
<title>HTML-specific stylesheet code
<description>
This is the DSSSL stylesheet for converting the Starlink DTD to HTML.

<misccode>
<description>Declare the flow-object-classes to support the SGML
transformation extensions of Jade.</description>
</miscprologue>
<codebody>
(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")
(declare-flow-object-class empty-element
  "UNREGISTERED::James Clark//Flow Object Class::empty-element")
(declare-flow-object-class document-type
  "UNREGISTERED::James Clark//Flow Object Class::document-type")
(declare-flow-object-class processing-instruction
  "UNREGISTERED::James Clark//Flow Object Class::processing-instruction")
(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-flow-object-class entity-ref
  "UNREGISTERED::James Clark//Flow Object Class::entity-ref")
(declare-flow-object-class formatting-instruction
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))
</codebody>
</misccode>

<!-- include the other parts by reference -->

&slparams.dsl

&slroutines.dsl

&sldocs.dsl

&slhtml.dsl

<misccode>
<description>The other functions, which I'll get round to documenting
individually when I can
<codebody>
&version.dsl

&slsect.dsl
&slall.dsl
&slnavig.dsl
</codebody>
</misccode>

<misccode>
<description>
<p>Declare the handlers for the root element, and for the manifest
mode.
</description>
<codebody>
(root
 (make sequence
   (process-children)
   (with-mode manifest
     (process-children))))

(mode manifest
  ;; This mode is really just a hack to get at the root element.  
  ;; This is similar to the DocBook version, but simpler and, I believe,
  ;; more robust.
  (root (process-children))

  (default 
    (if (node-list=? (current-node) (document-element))
	(if %html-manifest%
	    (make entity
	      system-id: %html-manifest%
	      (process-node-list
	       (node-list-filter-by-gi (descendants (current-node))
				       (chunk-element-list))))
	    (empty-sosofo))
	(make sequence
	  (make formatting-instruction data: (html-file))
	  (make formatting-instruction data: "
")
	  ))))

(default
  (process-children))
;; Make text that comes from unimplemented tags easy to spot
;(default
;  (make element gi: "FONT"
;	attributes: '(("COLOR" "RED"))
;	(make entity-ref name: "lt")
;	(literal (gi))
;	(make entity-ref name: "gt")
;	(process-children)
;	(make entity-ref name: "lt")
;	(literal "/" (gi))
;	(make entity-ref name: "gt")
;	))
</codebody>
</misccode>

</codegroup>

