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
;(element backmatter
;  ($html-section$))
(element backmatter
  (html-document (with-mode section-reference
		   (process-node-list (current-node)))
		 (make sequence
		   (make-notecontents)
		   (make-bibliography)
		   (make-updatelist))))

(define (make-contents-backmatter)
  (make sequence
    (if (node-list-empty? (get-notelist))
	(empty-sosofo)
	(make element gi: "li"
	      (make element gi: "a"
		    attributes: (list (list "href" (notes-sys-id)))
		    (literal "Notes"))))
    (if (get-bibliography-name)
	(make element gi: "li"
	      (make element gi: "a"
		    attributes: (list (list "href" (bibliography-sys-id)))
		    (literal "Bibliography")))
	(empty-sosofo))
    (if (node-list-empty? (get-updates))
	(empty-sosofo)
	(make element gi: "li"
	      (make element gi: "a"
		    attributes: (list (list "href" (updatelist-sys-id)))
		    (literal "Changes"))))
    ))

(mode section-reference
  (element backmatter
    (make-section-reference title: (literal "Notes, etc..."))))

<misccode>
<description>
Support notes as endnotes.  
<codebody>
(define (notes-sys-id)
  (if (chunking?)
      (html-file uniq: "notes")
      ""))

;(mode section-reference
;  (element notecontents
;    (make-section-reference title: (literal "Notes"))))

(element note
  (let ((en (number->string (element-number (current-node)))))
    (make element gi: "small"
      (literal "[")
      (make element gi: "a"
	    attributes: (list (list "href" (string-append (notes-sys-id)
							  "#NOTETEXT" en))
			      (list "name" (string-append "NOTEREF" en)))
	    (literal (string-append "Note " en)))
      (literal "]"))))
(mode extract-notecontents
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

(define (get-notelist)
  (select-elements (select-by-class (descendants (getdocbody))
				    'element)
		   (normalize "note")))

(define (make-notecontents)
  (let ((notelist (get-notelist)))
    (if (node-list-empty? notelist)
	(empty-sosofo)
	(html-document (literal "Notes")
		       (make element gi: "dl"
			     (with-mode extract-notecontents
			       (process-node-list notelist)))
		       system-id: (notes-sys-id)))))

<misccode>
<description>
Bibliography support.  The bibliography preprocessor (BibTeX) produces
an HTML DL element with entries referrable to by the bibkey, which is
the data of the CITATION element.
<codebody>
(define (bibliography-sys-id)
  (if (chunking?)
      (html-file uniq: "bibliography")
      ""))

(define (get-bibliography-name)
  (attribute-string (normalize "bibliography")
		    (getdocbody 'backmatter)))

(element citation
  (let ((bib-name (get-bibliography-name))
	(cit-data (trim-data (current-node))))
    (if bib-name
	(make element gi: "a"
	      attributes: (list (list "href" (string-append
					      (bibliography-sys-id)
					      "#"
					      cit-data)))
	      (literal (string-append "[" cit-data "]")))
	(error "Have CITATION but no BIBLIOGRAPHY"))))

(define (make-bibliography)
  (let ((bibcontents (read-entity (string-append (root-file-name)
						 ".htmlbib.bbl"))))
    (if bibcontents
	(html-document (literal "Bibliography")
		       (make fi data: bibcontents)
		       system-id: (bibliography-sys-id))
	(empty-sosofo))))
;    ($html-section$ (if bibcontents
;			(make fi data: bibcontents)
;			(literal "No bibliography found")))))
;
;(mode section-reference
;  (element bibliography
;    (make-section-reference title: (literal "Bibliography"))))

<misccode>
<description>
Collect all the update elements
<codebody>
(define (updatelist-sys-id)
  (if (chunking?)
      (html-file uniq: "updates")
      ""))

(define (make-updatelist)
  (let ((updatelist (get-updates)))
    (if (node-list-empty? updatelist)
	(empty-sosofo)
	(html-document (literal "Change history")
		       (make element gi: "dl"
			     (with-mode extract-updatelist
			       (process-node-list updatelist)))
		       system-id: (updatelist-sys-id)))))

(mode extract-updatelist
  (element update
    (let* ((change (element-with-id (attribute-string (normalize "versionid"))))
	   (thisautid (attribute-string (normalize "author")))
	   (author (element-with-id (or thisautid
					(attribute-string (normalize "author")
							  change)))))
      (make sequence
	(make element gi: "dt"
	      (process-node-list author)
	      (literal ", "
		       (format-date (attribute-string (normalize "date")
						      change))))
	(make element gi: "dd"
	      (process-children))))))

(element update				; ignore in default mode
  (empty-sosofo))

<![ignore[
<misccode>
<description>
Linking support.  Create a page listing all the exported IDs in the document,
so that document authors can find them in once place.
<codebody>
; nothing yet
]]>

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
  (let* ((kids (select-by-class (descendants (getdocbody)) 'element))
	 (citations (select-elements kids (normalize "citation")))
	 ;(bibelement (select-elements kids (normalize "bibliography")))
	 (bibname (attribute-string (normalize "bibliography")
				    (getdocbody 'backmatter)))
	 )
    (if (node-list-empty? citations)
	(empty-sosofo)
	(make entity system-id: (string-append (root-file-name)
					       ".htmlbib.aux")
	      (make fi data: "\\relax
")
	      (process-node-list citations)
	      (if bibname
		  (make fi data: (string-append "\\bibdata{" bibname "}
\\bibstyle{plainhtml}
"))
		  (error "Citations but no BIBLIOGRAPHY in document"))
	      ;(if (node-list-empty? bibelement)
		;  (error "Citations but no BIBLIOGRAPHY in document")
		;  (process-node-list bibelement))
	      ))))

(element citation
  (make fi data: (string-append "\\citation{" (trim-data (current-node)) "}
")))

;(element bibliography
;  (make sequence
;    (make fi data: (string-append "\\bibdata{"
;				  (attribute-string "BIB" (current-node))
;				  "}
;"))
;    (make fi data: "\\bibstyle{plainhtml}
;")))
