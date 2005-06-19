<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [
  <!ENTITY common.dsl		SYSTEM "../common/slcommon.dsl" SUBDOC>
  <!ENTITY lib.dsl		SYSTEM "../lib/sllib.dsl" SUBDOC>
  <!ENTITY maths.dsl		SYSTEM "slmaths.dsl" SUBDOC>
  <!ENTITY commonparams.dsl	PUBLIC "-//Starlink//TEXT DSSSL Common Parameterisation//EN">
  <!ENTITY params.dsl		PUBLIC "-//Starlink//TEXT DSSSL HTML Parameterisation//EN">
]>
<!-- $Id$ -->

<docblock>
<title>Preprocessing for HTML
<description>
<p>SGML documents need some preprocessing before the run which generates
the final HTML, to deal with bibliography information, maths, and
other LaTeX-notation material.  This stylesheet collects together this
processing.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

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
<title>Maths code
<description>
<p>Contains the get-maths function and supporting code

<codegroup id="preprocess.main" use="code.common code.lib code.maths">
<title>Preprocess for HTML
<description>This part of the stylesheet is standalone, and may be used
to process a document and extract those parts of the document (such as 
bibliography references) which require preprocessing.

<routine>
<description>
Extract the bibliography to a LaTeX .aux file, ready for processing
by BibTeX.  Extract maths, ready for processing with LaTeX and
conversion to GIFs.
<codebody>
(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")
(declare-flow-object-class empty-element
  "UNREGISTERED::James Clark//Flow Object Class::empty-element")
(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-flow-object-class formatting-instruction
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

;; Read in the parameter file
&commonparams.dsl;
&params.dsl;

(root
    (make sequence
      (make formatting-instruction data: (string-append (root-file-name) ":"))
      (get-maths)
      (get-bibliography)))

(define (get-bibliography)
  (let* ((kids (select-by-class (descendants (getdocbody)) 'element))
	 (citations (select-elements kids (normalize "citation")))
	 (bm (getdocbody 'backmatter))
	 (bibname (and bm
		       (attribute-string (normalize "bibliography")
					 bm)))
	 )
    (if (node-list-empty? citations)
	(empty-sosofo)
	(make entity system-id: (string-append (root-file-name)
					       ".htmlbib.aux")
	      (make formatting-instruction data: "\\relax
")
	      (process-node-list citations)
	      (if bibname
		  (make formatting-instruction
		    data: (string-append "\\bibdata{" bibname "}
\\bibstyle{plainhtml}
"))
		  (error "Citations present, but no BIBLIOGRAPHY attribute in document BACKMATTER"))))))

(element citation
  (make formatting-instruction
    data: (string-append "\\citation{" (trim-data (current-node)) "}
")))

