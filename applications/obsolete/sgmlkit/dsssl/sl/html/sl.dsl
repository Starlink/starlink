<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [
<!ENTITY sldocs.dsl		SYSTEM "sldocs.dsl">
<!ENTITY slmisc.dsl		SYSTEM "slmisc.dsl">
<!ENTITY slsect.dsl		SYSTEM "slsect.dsl">
<!ENTITY slroutines.dsl		SYSTEM "slroutines.dsl">
<!ENTITY slhtml.dsl		SYSTEM "slhtml.dsl">
<!ENTITY slnavig.dsl		SYSTEM "slnavig.dsl">
<!ENTITY sltables.dsl		SYSTEM "sltables.dsl">
<!ENTITY sllinks.dsl		SYSTEM "sllinks.dsl">

<!ENTITY commonparams.dsl	PUBLIC "-//Starlink//TEXT DSSSL Common Parameterisation//EN">
<!ENTITY slparams.dsl		PUBLIC "-//Starlink//TEXT DSSSL HTML Parameterisation//EN">
<!ENTITY slspecial.dsl		PUBLIC "-//Starlink//TEXT DSSSL HTML Special Code//EN">

<!ENTITY lib.dsl		SYSTEM "../lib/sllib.dsl" SUBDOC>
<!ENTITY common.dsl		SYSTEM "../common/slcommon.dsl" SUBDOC>
<!ENTITY maths.dsl		SYSTEM "slmaths.dsl" SUBDOC>
<!ENTITY slback.dsl		SYSTEM "slback.dsl" SUBDOC>
]>

<!-- $Id$ -->

<docblock>
<title>Starlink to HTML stylesheet
<description>
<p>This is the DSSSL stylesheet for converting the Starlink DTD to HTML.

<p>Requires Jade, for the non-standard functions.  Lots of stuff learned from 
Mark Burton's dbtohtml.dsl style file, and Norm Walsh's DocBook DSSSL
stylesheets. 

<authorlist>
<author id=ng affiliation='Starlink, University of Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council
<history>
<change author=ng date='09-FEB-1999'>Released a version 0.1 to the other
Starlink programmers.
</history>

<codereference doc="lib.dsl" id="code.lib">
<title>Library code
<description>
<p>Some of this library code is from the standard, some from Norm
Walsh's stylesheet, other parts from me

<codereference doc="common.dsl" id="code.common">
<title>Common code
<description>
<p>Code which is common to both the HTML and print stylesheets.

<codereference doc="maths.dsl" docid="code.maths.img" id="code.maths">
<title>Maths processing
<description>
Code to process the various maths elements.

<codereference doc="slback.dsl" id=code.back>
<title>Back-matter
<description>Handles notes, bibliography and indexing

<![ IGNORE [
<codereference doc="slnavig.dsl" id=code.navig>
<title>Navigation code
<description>
Code to support the generation of HTML filenames, and the
links which navigate between documents

<codereference doc="slhtml.dsl" id=code.html>
<title>HTML support
<description>
Code to support HTML generation

<codereference doc="sltables.dsl" id=code.tables>
<title>Tables support
<description>
Simple support for tables.

<codereference doc="sllinks.dsl" id=code.links>
<title>Inter- and Intra-document linking
<description>Handles <code>ref</code>, <code>docxref</code>, <code>webref</code> and <code>url</code>.
Imposes the link policy.

]]>


<!--codegroup 
  use="code.lib code.common code.maths code.back" 
  id=code.html-->
<codegroup 
  id=code.html>
<title>HTML-specific stylesheet code
<description>
This codegroup includes most of the stylesheet code.  This code is in
a separate codegroup so that the `main' codegroup below (id=html) can
include any definitions of other functions, which will override
definitions included here.


<!-- include the other parts by reference -->


&slroutines.dsl
&sldocs.dsl
&slsect.dsl
&slmisc.dsl
&commonparams.dsl
&slparams.dsl
&slhtml.dsl
&slnavig.dsl
&sltables.dsl
&sllinks.dsl


<codegroup
  use="code.lib code.common code.maths code.back code.html" 
  id=html>
<title>HTML stylesheet
<description>
This is the DSSSL stylesheet for converting the Starlink DTD to HTML.

<routine>
<description>Declare the flow-object-classes to support the SGML
transformation extensions of Jade.</description>
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
(define all-element-number
  (external-procedure "UNREGISTERED::James Clark//Procedure::all-element-number"))


<routine>
<description>
Include the list of hacks/overrides.  Since these are in the `main'
style-specification element (codegroup in DSSSLCODE terms) which
calls the other style-specifications and external-specifications
(codegroup and codereference), and so definitions here override
definitions elsewhere.
<codebody>
&slspecial.dsl;



<routine>
<description>
The root rule.  This generates the HTML documents, then generates the
manifest and extracts the maths to an external document for postprocessing.
<codebody>
(root
 (make sequence
   (process-children)
   (make-manifest)
   (get-maths)
   ))

<routine>
<routinename>make-manifest
<description>Construct a list of the HTML files generated by the main
processing.  Done only if <code>suppress-manifest</code> is false and 
<code>%html-manifest%</code> is true, giving the
name of a file to hold the manifest.
<p>Add certain elements to the list
of elements in (chunk-element-list), and treat them specially.
Those elements should have suitable FO construction rules defined within 
mode `make-manifest-mode'.
<p>The backmatter is treated entirely separately, by make-manifest-backmatter.
<p>The codecollection and docxref elements also have
attributes with ENTITY declared values.  Should I worry about those?
<argumentlist>
<parameter optional default='(current-node)'>nd<type>singleton-node-list
  <description>Node which identifies the grove to be processed.
<codebody>
(define (make-manifest #!optional (nd (current-node)))
  (if (and %html-manifest% (not suppress-manifest))
      (let ((element-list (append (chunk-element-list)
				  (list (normalize "figure")
					(normalize "coverimage")
					(normalize "m")
					(normalize "mequation")
					(normalize "meqnarray")
					;; codecollection is not
					;; chunked (programcode is
					;; instead), but that's the
					;; hook which allows us to
					;; list the file contents of
					;; codecollection.
					(normalize "codecollection")
					)))
	    (rde (document-element nd)))
	(make entity system-id: %html-manifest%
	      (with-mode make-manifest-mode
		(process-node-list
		 (node-list rde		;include current file
			    (node-list-filter-by-gi
			     (select-by-class (descendants rde)
					      'element)
			     element-list))
		 ))
	      (make-manifest-backmatter)))
      (empty-sosofo)))

(mode make-manifest-mode
  (default 
    (if (or (chunk?)
	    (node-list=? (current-node) (document-element)))
	(make formatting-instruction data: (string-append (html-file) "
"))
	(empty-sosofo)))

  ;; The selection here should match the processing in slmisc.dsl
  (element figure
    (let* ((kids (children (current-node)))
	   (content (get-best-figurecontent
		     (select-elements kids (normalize "figurecontent"))
		     '("gif89a" "jpeg"))))
      (if content
	  (process-node-list content)
	  (empty-sosofo))))
  (element coverimage
    (let* ((kids (children (current-node)))
	   (content (get-best-figurecontent
		     (select-elements kids (normalize "figurecontent"))
		     '("gif89a" "jpeg"))))
      (if content
	  (process-node-list content)
	  (empty-sosofo))))
  ;; the figurecontent element writes out TWO fields in the manifest:
  ;; the first is the sysid of the figure as referred to by the
  ;; generated HTML, which will have no path, and the second is the sysid as
  ;; declared in the entity, which may well have a path.  Locations may
  ;; need some post-processing.
  (element figurecontent
    (let* ((image-ent (attribute-string (normalize "image")
					(current-node)))
	   (full-sysid (and image-ent
			    (entity-system-id image-ent)))
	   (base-sysid (and image-ent
			    (car (reverse
				  (tokenise-string
				   (entity-system-id image-ent)
				   boundary-char?: (lambda (c)
						     (char=? c #\/))))))))
      (if image-ent
	  (make fi data: (string-append base-sysid " " full-sysid "
"))
	  (empty-sosofo))))
  (element m
    (make fi data: (string-append (img-equation-sysid (current-node)) "
")))
  (element mequation
    (make fi data: (string-append (img-equation-sysid (current-node)) "
")))
  (element meqnarray
    (make fi data: (string-append (img-equation-sysid (current-node)) "
")))
  (element mdefs
    (empty-sosofo))
  (element codecollection
    (make sequence
      (let* ((docent (attribute-string (normalize "doc")))
	     (de (and docent
		      (document-element-from-entity docent))))
	(if de
	    (make sequence
	      ;; find all the chunks and write their names
	      (node-list-reduce
	       (node-list de		;oddly, subtree isn't in jade-1.2.1
			  (node-list-filter-by-gi
			   (select-by-class (descendants de) 'element)
			   (chunk-element-list)))
	       (lambda (result i)
		 (sosofo-append
		  result
		  (make fi data: (string-append (html-file target_nd: i) "
"))))
	       (empty-sosofo))
	      ;; now find all the maths elements, and write out their
	      ;; equation-image filenames
	      (node-list-reduce
	       (node-list-filter-by-gi
		(select-by-class (descendants de) 'element)
		(maths-element-list))
	       (lambda (result i)
		 (sosofo-append
		  result
		  (process-node-list i)))
	       (empty-sosofo)))
       	    (error "codecollection: missing required doc attribute")))))
  )

<codegroup>
<title>The default rule
<description>This has to be in a separate group
(<code>style-specification</code> in the terms of the DSSSL architecture),
so that it doesn't take priority over mode-less rules in other process
specification parts.  See the DSSSL standard, sections 7.1 and 12.4.1.
<routine>
<description>The default rule
<codebody>
(default
  (process-children))
;; Make text that comes from unimplemented tags easy to spot
;(default
;  (make element gi: "font"
;	attributes: '(("color" "red"))
;	(make entity-ref name: "lt")
;	(literal (gi))
;	(make entity-ref name: "gt")
;	(process-children)
;	(make entity-ref name: "lt")
;	(literal "/" (gi))
;	(make entity-ref name: "gt")
;	))
