<!DOCTYPE programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN" [
  <!entity lib.dsl		system "../lib/sllib.dsl" subdoc>
  <!entity slnavig.dsl		system "slnavig.dsl" subdoc>
  <!entity common.dsl		system "../common/slcommon.dsl" subdoc>
]>

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
<description>Unlike the <code/code.maths.tth/ codegroup below, the
code here extracts the maths into a file ready for quite substantial
postprocessing using Perl, LaTeX, dvips, and convert, to turn the maths
fragments into GIFs.  We write out a file ready too be munged into a LaTeX 
file by a Perl script -- we use this extra step so that the Perl can try to
reuse equations wherever possible, the logic for which is probably rather
difficult in DSSSL.
<p>The `interface' is the same as for the <code/tth/ version below.

<func>
<routinename>get-maths
<description>Create an external entity, and write out a document ready
  for post-processing.  Process all the elements with GIs which math
  elements in <code/maths-element-list/, spitting them out into this
  document accompanied by directives to the post-processor.
  <p>If there are no maths elements, return <code/(empty-sosofo)/.
<returnvalue type=sosofo>
<parameter optional default='(current-node)'>
  nd
  <type>singleton-node-list
  <description>Indicates the node we want to extract the maths
  elements from.

<codebody>
(define (get-maths #!optional (nd (current-node)))
  (let* ((rde (document-element nd))
	 (mathels (node-list-filter-by-gi (descendants rde)
					  (maths-element-list))))
    (if (node-list-empty? mathels)
	(empty-sosofo)
	(make entity system-id: ($maths-extfile$ #t)
	      (with-mode get-maths-mode (process-node-list mathels))))))

<misccode>
<description>Define the <code/get-maths-mode/ mode, which need only be defined
for the elements in <funcname/maths-element-list/.  In it, the flow-object
construction rules simply call the <code/img-equation/ function with
appropriate arguments

<codebody>
(define (maths-element-list)
  (list (normalize "m")
	(normalize "mequation")
	(normalize "meqnarray")))

(mode get-maths-mode
  (element m
    (img-equation "inline"))
  (element mequation
    (img-equation "equation"))
  (element meqnarray
    (img-equation "eqnarray")))

<func>
<routinename>img-equation
<description>Process a single maths element, writing out a fragment of
  LaTeX for the post-processor.  This will indicate what type of maths
  element this is.
<returnvalue type=sosofo>
<parameter>eqn-type
  <type>string
  <description>The type of equation, `inline', `equation' or
  `eqnarray'.
<codebody>
(define (img-equation eqn-type)
  (make sequence
    (make formatting-instruction
      data: (string-append "%%imgmath " eqn-type " " (img-eqnref) "
" (data (current-node)) "
"))))

<func>
<routinename>img-eqnref
<description>Returns a unique reference to an equation.
<returnvalue type=string>String usable as an ID attribute value.
<parameter optional default='(current-node)'>nd
  <type>node-list
  <description>A singleton node-list containing the element which is
  to be referred to.
<codebody>
(define (img-eqnref #!optional (nd (current-node)))
  (string-append "IMGEQ" (number->string (all-element-number nd))))

<misccode>
<description>Handle the maths elements in the normal run of text
<codebody>
(element m
  (make element gi: "em"
	(literal (string-append "[ inline equation " (img-eqnref) " ]"))))
(element mequation
  (make element gi: "em"
	(literal (string-append "[ display equation " (img-eqnref) " ]"))))
(element meqnarray
  (make element gi: "em"
	(literal (string-append "[ equation array " (img-eqnref) " ]"))))



<func>
<routinename>$maths-extfile$
<description>The name of the external file which is created by the
  <code/get-maths/ mode, to hold fragments of maths.  Uses
  <funcname/root-file-name/.
<returnvalue type="string">filename
<parameter>tth-input<type>boolean
	<description>True if we want the name of the file which is
	<em/input/ to <code/tth/, false if we want the name of the
	result.
<codebody>
(define ($maths-extfile$ tth-input)
  (string-append (root-file-name) (if tth-input
				      ".img-eqlist"
				      ".img-html")))

<misccode>
<description>Declare the <code/all-element-number/ Jade extension
<codebody>
(define all-element-number
  (external-procedure "UNREGISTERED::James Clark//Procedure::all-element-number"))




<codegroup id="code.maths.tth">
<title>Process maths elements using tth
<description>This codegroup uses <code/tth/
(see <url>http://hutchinson.belmont.ma.us/tth/</url>)
to format the maths.  This is nifty, but isn't a complete solution since (a)
the browser needs some tweaking to show the results correctly, and (b) the
browser can't print the results at all.

<misccode>
<description>Declare the <code/all-element-number/ Jade extension
<codebody>
(define all-element-number
  (external-procedure "UNREGISTERED::James Clark//Procedure::all-element-number"))

<func>
<codeprologue>
<routinename>get-maths
<description>
  <p>Create an external entity, and write out a LaTeX document ready for
  conversion with <code/tth/.  Process all the elements with GIs
  which match elements in <code/maths-element-list/, spitting them out into 
  this document accompanied by the <code/%%tth:/ pragmas recognised by
  <code/tth/.
  <p>If there are no maths elements in the document, then return
  <code/(empty-sosofo)/.
<returnvalue type=sosofo>
<parameter optional default="(current-node)">
  nd
  <type>singleton-node-list
  <description>
  The node indicates the grove we want to extract the maths elements from
<codebody>
(define (get-maths #!optional (nd (current-node)))
  (let* ((rde (document-element nd))
	 (mathels (node-list-filter-by-gi (descendants rde)
					  (maths-element-list))))
    (if (node-list-empty? mathels)
	(empty-sosofo)
	(make entity
	  system-id: ($maths-extfile$ #t)
	  (make formatting-instruction data: 
"\\documentclass{article}
\\setcounter{secnumdepth}{0}
\\begin{document}
%%tth:\\begin{html}&lt;!doctype TTH-EQLIST public '-//Starlink//DTD tth equation list//EN'>\\end{html}
")
	  (with-mode get-maths-mode (process-node-list mathels))
	  (make formatting-instruction data:
		"\\end{document}
")
	  ))))

<misccode>
<description>Define the <code/get-maths-mode/ mode, which need only be defined
for the elements in <funcname/maths-element-list/.  In it, the flow-object
construction rules simply call the <code/tth-equation/ function with
appropriate bracketing expressions.

<codebody>
(define (maths-element-list)
  (list (normalize "m")
	(normalize "mequation")
	(normalize "meqnarray")))

(mode get-maths-mode
  (element m
    (tth-equation "\\(" "\\)"))
  (element mequation
    (tth-equation "\\begin{equation*}" "\\end{equation*}"))
  (element meqnarray
    (tth-equation "\\begin{eqnarray*}" "\\end{eqnarray*}")))

<func>
<codeprologue>
<routinename>
<name>tth-equation
<description>
<p>Process a single maths element, writing out a fragment of a LaTeX
file for tth to parse.  This requires us to put in directives to tth
to have it, in turn, emit SGML fragments, ready for us to read in again.

<returnvalue type=sosofo>A sosofo containing a fragment of LaTeX
suitable for being parsed by tth.

<parameter>latex-before
  <type>string
  <description>
  <p>A string to be inserted before the element contents -- it'll open
  the environment.
<parameter>latex-after
  <type>string
  <description>
  <p>A string to be inserted after the element contents -- it'll close
  the environment.

<codebody>
(define (tth-equation latex-before latex-after)
  (let ((id (tth-eqnref)))
    (make sequence
      (make formatting-instruction
	data: (string-append "%%tth:\\begin{html}&lt;tth-eq id=" id
			     ">&lt![ cdata [\\end{html}
" latex-before))
      ;;(process-children)
      (make formatting-instruction data: (data (current-node)))
      (make formatting-instruction
	data: (string-append latex-after "
%%tth:\\begin{html}]]" ">&lt;/tth-eq>\\end{html}
")))))

<func>
<routinename>tth-eqnref
<description>Returns a unique reference to an equation.
<returnvalue type=string>String usable as an ID attribute value
<argumentlist>
<parameter optional default='(current-node)'>
  nd
  <type>node-list
  <description>A singleton-node-list containing the element which is 
    to be referred to
<codebody>
(define (tth-eqnref #!optional (nd (current-node)))
  (string-append "TTHEQ"
		 (number->string (all-element-number nd))))

<misccode>
<miscprologue>
<description>
<p>Handle the maths elements in the normal run of text.
<codebody>
(element m
  (make element gi: "em"
	(insert-tth-equation)))

(element mequation
  (make element gi: "blockquote"
	(insert-tth-equation)))

(element meqnarray
  (make element gi: "blockquote"
	(insert-tth-equation)))

<func>
<routinename>insert-tth-equation
<description>Extracts from the <code/tth/ output the equation which
corresponds to the current element, and returns a SOSOFO
containing it.
<returnvalue type=sosofo>SOSOFO containing the formatted equation.
<argumentlist>
<parameter optional default='(current-node)'>
  nd
  <type>node-list
  <description>The node we want the equation for.
<codebody>
(define (insert-tth-equation #!optional (nd (current-node)))
  (let* ((tth-de (get-tth-equations))
	 (id (tth-eqnref nd))
	 (eq (element-with-id id tth-de)))
    (make formatting-instruction
      data: (if (node-list-empty? eq)
		"[[ No equation code found ]]"
		(data eq)))))


<func>
<routinename>get-tth-equations
<description>Obtain the grove which contains the equations which are
to be inserted in the maths elements.  The document to be opened is
the one generated by <code/tth/, and is an instance of the
<code>-//Starlink//DTD tth equation list//EN</code> DTD.
<p>The system-id of the entity containing the equations is obtained
from the <funcname/$maths-extfile$/ function.
<p>If the call to <funcname/document-element/ fails (presumably because
the file doesn't exist) then signal an error.
<returnvalue type=node-list>The document element of the grove.
<argumentlist none>
<codebody>
(define (get-tth-equations)
  (let* ((mfile ($maths-extfile$ #f))
	 (mroot (sgml-parse mfile))
	 (rde (document-element mroot)))
    (or rde
	(error (string-append "Can't find equations file: " mfile)))))

<func>
<codeprologue>
<routinename>
<name>$maths-extfile$
<description>
<p>The name of the external file which is created by the
<code/get-maths/ mode, to hold fragments of maths.  Uses
<funcname/root-file-name/.
<returnvalue type="string">filename
<argumentlist>
<parameter>tth-input<type>boolean
	<description>True if we want the name of the file which is
	<em/input/ to <code/tth/, false if we want the name of the
	result.
<codebody>
(define ($maths-extfile$ tth-input)
  (string-append (root-file-name) (if tth-input
				      ".tth-eqlist"
				      ".tth-html")))


<codereference doc="lib.dsl" id="code.lib">
<title>Library code
<description>Required for various functions

<codereference doc="slnavig.dsl" id=code.navig>
<title>Navigation code
<description>
Code to support the generation of HTML filenames, and the
links which navigate between documents

<codereference doc="common.dsl" id="code.common">
<title>Common code
<description>
<p>Code which is common to both the HTML and print stylesheets.

<codegroup use="code.maths.img code.lib code.common code.navig" id=maths.main>
<title>Extract document maths
<description>This codegroup is used as a standalone stylesheet, to extract
the maths from a document without doing any further processing.

<misccode>
<description>
Declare the Jade Transform extensions.
<codebody>
(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")
(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-flow-object-class formatting-instruction
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")

<misccode>
<description>
Simply supply a root rule, which calls the <funcname/get-maths/ function.
Also emit the root file name, as described in <funcname/process-document/
in the main stylesheet.
<codebody>
(root
    (make sequence
      (literal (string-append (root-file-name) ":"))
      (get-maths)))
