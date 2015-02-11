<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [
  <!ENTITY commonparams.dsl	PUBLIC "-//Starlink//TEXT DSSSL Common Parameterisation//EN">
  <!ENTITY slparams.dsl		PUBLIC "-//Starlink//TEXT DSSSL HTML Parameterisation//EN">
  <!ENTITY lib.dsl		SYSTEM "../lib/sllib.dsl" SUBDOC>
  <!ENTITY common.dsl		SYSTEM "../common/slcommon.dsl" SUBDOC>
]>
<!-- $Id$ -->

<docblock>
<title>Maths in the Starlink DTD
<description>This DSSSL program contains code to handle the maths
elements defined in the Starlink DTD, which have content in LaTeX
notation.  The code does not attempt to do anything with the contents
-- it simply sends it to an external file, where it is processed by 
an external program, ready to be reincorporated into the generated HTML.

<p>The code here is designed to be used within another stylesheet,
such as the stylesheet which does the main processing of the document.
At the end, however, is a codegroup which will do nothing except
extract all the maths from a document.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council.

<codegroup id="code.maths.img">
<title>Process maths elements into GIFs
<description>Unlike the <code>code.maths.tth</code> codegroup below, the
code here extracts the maths into a file ready for quite substantial
postprocessing using Perl, LaTeX, dvips, and convert, to turn the maths
fragments into GIFs (or JPEGs or PNGs, or whatever -- this code doesn't depend on the actual format).  We write out a file ready too be munged into a LaTeX 
file by a Perl script -- we use this extra step so that the Perl can try to
reuse equations wherever possible, the logic for which is probably rather
difficult in DSSSL.
<p>If there is nothing to write out, the code does not write an
empty output file. 
<p>The `interface' is the same as for the <code>tth</code> version below.
<p>Changes here might need corresponding changes in make-manifest-mode in
sl.dsl

<routine>
<routinename>get-maths
<description>Create an external entity, and write out a document ready
  for post-processing.  Process all the elements with GIs which match
  elements in <code>maths-element-list</code>, spitting them out into this
  document accompanied by directives to the post-processor.
  <p>If there are no maths elements, return <code>(empty-sosofo)</code>.
<returnvalue type=sosofo>SOSOFO containing external maths document
<argumentlist>
<parameter optional default='(current-node)'>
  nd
  <type>singleton-node-list
  <description>Indicates the node we want to extract the maths
  elements from.

<codebody>
(define (get-maths #!optional (nd (current-node)))
  (let* ((rde (document-element nd))
	 (mathels (node-list-filter-by-gi (select-by-class (descendants rde)
							   'element)
					  (maths-element-list))))
    (if (node-list-empty? mathels)
	(empty-sosofo)
	(make entity system-id: ($maths-extfile$ #t)
	      (with-mode get-maths-mode (process-node-list mathels))))))

<routine>
<description>Define the <code>get-maths-mode</code> mode, which need only be defined
for the elements in <funcname>maths-element-list</funcname>.  In it, the flow-object
construction rules simply call the <code>img-equation</code> function with
appropriate arguments

<codebody>
(define (maths-element-list)
  (list (normalize "m")
	(normalize "mequation")
	(normalize "meqnarray")
	(normalize "mdefs")
	(normalize "codecollection")))

(mode get-maths-mode
  (element m
    (img-equation "inline"))
  (element mequation
    (img-equation "equation" (get-equation-number)))
  (element mdefs
    (make sequence
      (make formatting-instruction data: (string-append "
%%startmdefs
"))
      (process-children)
      (make formatting-instruction data: (string-append "
%%endmdefs
"))))
  (element mline
    (let ((eqno (get-equation-number)))
      (make sequence
	(make formatting-instruction data: (data (current-node)))
	(if eqno
	    (make formatting-instruction data: (string-append "
%%eqno " eqno "
"))
	    (empty-sosofo))
	(literal (if (last-sibling?) "" "\\\\")))))
  (element meqnarray
    (make sequence
      (process-children)
      (make formatting-instruction data: (string-append "
%%imgmath eqnarray " (img-eqnref) "
"))))
  (element codecollection
    (let ((docent (attribute-string (normalize "doc"))))
      (if docent
	  (let ((mathels (node-list-filter-by-gi
			  (select-by-class
			   (descendants (document-element-from-entity docent))
			   'element)
			  (maths-element-list))))
	    (if (node-list-empty? mathels)
		(empty-sosofo)
		(with-mode get-maths-mode
		  (process-node-list mathels))))
	  (error "Codecollection has no document entity")))))

<routine>
<routinename>img-equation
<description>Process a single maths element, writing out a fragment of
  LaTeX for the post-processor.  This will indicate what type of maths
  element this is.
<returnvalue type=sosofo>LaTeX fragment containing maths
<argumentlist>
<parameter>eqn-type
  <type>string
  <description>The type of equation, `inline', `equation',
  `eqnarray', or `mdefs'.
<codebody>
(define (img-equation eqn-type #!optional (eqno #f))
  (make sequence
    (make formatting-instruction
      data: (string-append (data (current-node))
			   (if eqno
			       (string-append "
%%eqno " eqno "
")
			       "") "
%%imgmath " eqn-type " " (img-eqnref) "
"))))

<routine>
<description>
<p>Handle the maths elements in the normal run of text.
<codebody>
(element m
  (let ((sid (img-equation-details)))
    (if sid
	(make empty-element gi: "img"
	      attributes: (append sid
				  '(("align" "middle"))
				  `(("alt" ,(data (current-node))))))
	(make element gi: "em"
	      (literal "Equation not found")))))

(element mequation
  (let ((ref (img-eqnref))
	(sid (img-equation-details)))
    (if sid
	(make sequence
	  (make empty-element gi: "br")
	  (make element gi: "a"
		attributes: (list (list "name" ref))
		(make empty-element gi: "img"
		      attributes: (append sid
					  `(("alt" ,(data (current-node)))))))
	  (make empty-element gi: "br"))
	(make element gi: "em"
	      (literal "Equation not found")))))

;; meqnarray is identical to mequation
(element meqnarray
  (let ((ref (img-eqnref))
	(sid (img-equation-details)))
    (if sid
	(make sequence
	  (make empty-element gi: "br")
	  (make element gi: "a"
		attributes: (list (list "name" ref))
		(make empty-element gi: "img"
		      attributes: (append sid
					  `(("alt" ,(data (current-node)))))))
	  (make empty-element gi: "br"))
	(make element gi: "em"
	      (literal "Equation not found")))))

;; mdefs has no output in this, default, mode
(element mdefs
  (empty-sosofo))

;; cross-referencing
(define (href-to-fragid-mlabel target)
  (img-eqnref target))
(mode section-reference
  (element mlabel
    (literal (string-append "Eqn.(" (get-equation-number) ")"))))

<routine>
<routinename>img-equation-details
<description>Extracts from the <code>img-eqlist</code> document the sysid 
of the image which corresponds to the current element.  The sysid refers to
an image file which has been generated by the same process which
generated the <code>img-eqlist</code> document.
<returnvalue type='list of lists'>Returns a list containing a list
suitable for passing to the <code>attributes:</code> characteristic.
Return <code>#f</code> if no information is available.
<argumentlist>
<parameter optional default='(current-node)'>
  nd
  <type>node-list
  <description>The node we want the equation for.
<codebody>
(define (img-equation-details #!optional (nd (current-node)))
  (let* ((img-de (get-img-equations))
	 (id (img-eqnref nd))
	 (eq (element-with-id id img-de)))
    (if (node-list-empty? eq)
	#f
	(list-true (list (list "src"
			       (attribute-string (normalize "sysid") eq))
			 (if (attribute-string (normalize "width") eq)
			     (list "width"
				   (attribute-string (normalize "width") eq))
			     #f)
			 (if (attribute-string (normalize "height") eq)
			     (list "height"
				   (attribute-string (normalize "height") eq))
			     #f))))))


(define (img-equation-sysid #!optional (nd (current-node)))
  (let* ((img-de (get-img-equations))
	 (id (img-eqnref nd))
	 (eq (element-with-id id img-de)))
    (if (node-list-empty? eq)
	#f
	(attribute-string (normalize "sysid")
			  eq))))


<routine>
<routinename>get-img-equations
<description>Obtain the grove which contains the equations which are
to be inserted in the maths elements.  The document to be opened is
the one generated by <code>tth</code>, and is an instance of the
<code>system 'img-eqlist'</code> DTD.
<p>The system-id of the entity containing the equations is obtained
from the <funcname>$maths-extfile$</funcname> function.
<p>If the call to <funcname>document-element</funcname> fails (presumably because
the file doesn't exist) then signal an error.
<returnvalue type=node-list>The document element of the grove.
<codebody>
(define (get-img-equations)
  (let* ((mfile ($maths-extfile$ #f))
	 ;(mroot (sgml-parse mfile))
	 ;(rde (and mroot (document-element mroot)))
	 (rde (document-element-from-sysid mfile
					   prepend-decl: %xml-decl-entity%))
	 )
    (or rde
	(error (string-append "Can't find equations file: " mfile)))))

<routine>
<routinename>$maths-extfile$
<description>The name of the external file which is created by the
  <code>get-maths</code> mode, to hold fragments of maths.  Uses
  <funcname>root-file-name</funcname>.
<returnvalue type="string">filename
<argumentlist>
<parameter>img-input<type>boolean
	<description>True if we want the name of the file which is
	<em>input</em> to the equation processor, false if we want the name of the
	result.
<codebody>
(define ($maths-extfile$ img-input)
  (string-append (root-file-name) (if img-input
				      ".imgeq.list"
				      ".imgeq.xml")))



<codereference doc="lib.dsl" id="code.lib">
<title>Library code
<description>Required for various functions

<codereference doc="common.dsl" id="code.common">
<title>Common code
<description>
<p>Code which is common to both the HTML and print stylesheets.

<codegroup use="code.maths.img code.lib code.common" id=maths.main>
<title>Extract document maths
<description>This codegroup is used as a standalone stylesheet, to extract
the maths from a document without doing any further processing.

<routine>
<description>
Declare the Jade Transform extensions.
<codebody>
(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")
(declare-flow-object-class empty-element
  "UNREGISTERED::James Clark//Flow Object Class::empty-element")
(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-flow-object-class formatting-instruction
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")

&commonparams.dsl;
&slparams.dsl;

<routine>
<description>
Simply supply a root rule, which calls the <funcname>get-maths</funcname> function.
Also emit the root file name, as described in <funcname>process-document</funcname>
in the main stylesheet.
<codebody>
(root
    (make sequence
      (literal (string-append (root-file-name) ":"))
      (get-maths)))
