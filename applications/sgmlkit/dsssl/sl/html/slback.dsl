<!doctype programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN" [
  <!entity common.dsl		system "../common/slcommon.dsl" subdoc>
  <!entity lib.dsl		system "../lib/sllib.dsl" subdoc>
]>
<!-- $Id$ -->

<docblock>
<title>Back-matter in HTML
<description>
Support the back-matter elements.  Support notes as endnotes, 
support bibliography citations using BibTeX as an external processor,
and support indexing (soon!) using makeindex.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council

<codegroup id='code.back'>
<title>Support back-matter

<misccode>
<description>Declare Jade extension
<codebody>
(define read-entity
  (external-procedure "UNREGISTERED::James Clark//Procedure::read-entity"))
(declare-flow-object-class fi
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")

<misccode>
<description>
Support back-matter elements
<codebody>
(element backmatter
  ($html-section$))

(mode section-reference
  (element backmatter
    (make-section-reference title: (literal "Notes, etc..."))))
; (mode section-reference
;   (element backmatter
;     (let* ((kids (children (current-node)))
; 	   (cont (node-list-reduce kids
; 				   (lambda (result i)
; 				     (string-append result
; 						    (if (equal? result "")
; 							""
; 							", ")
; 						    (gi i)))
; 				   "")))
;     (make-section-reference title: (literal cont)))))

<misccode>
<description>
Support notes as endnotes.  
<codebody>
(element notecontents
  (let ((notelist (select-elements (select-by-class (descendants (getdocbody))
						    'element)
				   (normalize "note"))))
    (if (node-list-empty? notelist)
	(error "Have NOTECONTENTS but no NOTEs")
	($html-section$ (make element gi: "dl"
			      (with-mode make-notecontents
				(process-node-list notelist)))))))

(mode section-reference
  (element notecontents
    (make-section-reference title: (literal "Notes"))))

(element note
  (let ((notecontents-el (select-elements
			  (select-by-class (descendants (getdocbody))
					   'element)
			  (normalize "notecontents")))
	(en (number->string (element-number (current-node)))))
    (make element gi: "small"
      (literal "[")
      (if (node-list-empty? notecontents-el)
	  (error "Have NOTE but no NOTECONTENTS")
	  (make element gi: "a"
		attributes: (list (list "href" (string-append
						(href-to notecontents-el)
						"#NOTETEXT" en))
				  (list "name" (string-append "NOTEREF" en)))
		(literal (string-append "Note " en))))
      (literal "]"))))
(mode make-notecontents
  (element note
    (let ((en (number->string (element-number (current-node)))))
      (make sequence
	(make element gi: "dt"
	      (make element gi: "a"
		    attributes: (list (list "href" (string-append
						    (href-to (current-node))
						    "#NOTEREF" en))
				      (list "name" (string-append
						    "NOTETEXT" en)))
		    (literal (string-append "Note " en))))
	(make element gi: "dd"
	      (process-children))))))

<misccode>
<description>
Bibliography support.  The bibliography preprocessor (BibTeX) produces
an HTML DL element with entries referrable to by the bibkey, which is
the data of the CITATION element.
<codebody>
(element citation
  (let ((bib-el (select-elements (select-by-class (descendants (getdocbody))
						  'element)
				 (normalize "bibliography")))
	(cit-data (trim-data (current-node))))
    (if (node-list-empty? bib-el)
	(error "Have CITATION but no BIBLIOGRAPHY")
	(make element gi: "a"
	      attributes: (list (list "href" (string-append
					      (href-to bib-el)
					      "#"
					      cit-data)))
	      (literal (string-append "[" cit-data "]"))))))

(element bibliography
  (let ((bibcontents (read-entity (string-append (root-file-name)
						 ".htmlbib.bbl"))))
    ($html-section$ (if bibcontents
			(make fi data: bibcontents)
			(literal "No bibliography found")))))

(mode section-reference
  (element bibliography
    (make-section-reference title: (literal "Bibliography"))))

<misccode>
<description>
Linking support.  Create a page listing all the exported IDs in the document,
so that document authors can find them in once place.
<codebody>
; nothing yet

<codereference doc="lib.dsl" id="code.lib">
<title>Library code
<description>
<p>Some of this library code is from the standard, some from Norm
Walsh's stylesheet, other parts from me

<codereference doc="common.dsl" id="code.common">
<title>Common code
<description>
<p>Code which is common to both the HTML and print stylesheets.

<codegroup id=back.main use="code.common code.lib">
<title>Preprocess backmatter
<description>This part of the stylesheet is standalone, and may be used
to process a document and extract those parts of the document (such as 
bibliography references) which require preprocessing.

<misccode>
<description>
Extract the bibliography to a LaTeX .aux file, ready for processing
by BibTeX.
<codebody>
(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-flow-object-class fi
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))


(root
    (make sequence
      (make fi data: (string-append (root-file-name) ":"))
      (get-bibliography)))

(define (get-bibliography)
  (let* ((kids (select-by-class (descendants (document-element)) 'element))
	 (citations (select-elements kids (normalize "citation")))
	 (bibelement (select-elements kids (normalize "bibliography"))))
    (if (node-list-empty? citations)
	(empty-sosofo)
	(make entity system-id: (string-append (root-file-name)
					       ".htmlbib.aux")
	      (make fi data: "\\relax
")
	      (process-node-list citations)
	      (if (node-list-empty? bibelement)
		  (error "Citations but no BIBLIOGRAPHY in document")
		  (process-node-list bibelement))))))

(element citation
  (make fi data: (string-append "\\citation{" (trim-data (current-node)) "}
")))

(element bibliography
  (make sequence
    (make fi data: (string-append "\\bibdata{"
				  (attribute-string "BIB" (current-node))
				  "}
"))
    (make fi data: "\\bibstyle{plainhtml}
")))
