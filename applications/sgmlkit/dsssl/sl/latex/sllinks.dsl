<!-- $Id$ -->

<!--
<docblock>
<title>Inter- and Intra-document cross references for LaTeX
<description>
Support the various cross reference elements

<p>Note that, despite the intricate HyTime setup in the DTD, the
implementations here currently have the semantics built in, and
ignore the HyTime attributes!

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council

<codegroup id='code.links'>
<title>Support cross references
-->

<misccode>
<description>REF is a simple reference to another element in the same document.
Check that the target is a member of the list <funcname/target-element-list/.
If the `text' attribute is present, then use that as the link text,
rather than generating it from the link target.  In this case, do not put
the link text in italics, as it has presumably been chosen to blend in with
the surrounding text, even though this will make it invisible in a 
presentation which has no links (the motivation for this was the fact that
`ref' elements <em/can/ occur within `verbatim' elements, in which a `textit'
command stands out somewhat).
<codebody>
(element ref
  (let ((target (element-with-id (attribute-string (normalize "id")
						   (current-node))))
	(linktext (attribute-string (normalize "text")
				    (current-node))))
    (if (member (gi target) (target-element-list))
	(if linktext
	    (literal linktext)	;override generation of link text
	    (make command name: "textit"
		  (if (member (gi target) (section-element-list))
		      (make-section-reference target: target specify-type: #t)
		      (with-mode section-reference
			(process-node-list target)))))
	(error (string-append
	     "The stylesheet is presently unable to link to elements of type "
	          (gi target))))))

<misccode>
<description>The <code/docxref/ element has a required attribute
giving the document which is to be referred to, and an optional
attribute giving an ID within that document.  The target of the link
should be a document marked up according to the <code/documentsummary/
DTD.  In fact, that DTD is a base architecture of the Starlink General
DTD, so we could get the same effect by linking to the actual document
and extracting the <code/documentsummary/ architectural instance.
However, the <funcname/sgml-parse/ function in DSSSL isn't defined as
being able to do that; there is a Jade patch which allows it to do
that, which I hope to build into a Starlink version of Jade when I
can.

<p>This rule invokes the <funcname/get-link-policy-target/ function to check
that the target of the link conforms to the policy -- if it doesn't,
it produces an <funcname/error/.

<p>The rule uses the <code/mk-docxref/ mode to process the target element.

<codebody>
(element docxref
  (let* ((xrefent (attribute-string (normalize "doc") (current-node)))
	 (docelem (and xrefent
		       (document-element
			(sgml-parse (entity-generated-system-id xrefent)))))
	 (xrefid (attribute-string (normalize "loc") (current-node)))
	 ;; xreftarget is the element the docxref refers to, or #f if
	 ;; attribute LOC is implied or the document doesn't have such
	 ;; an ID
	 (xreftarget (and xrefid
			  (node-list-or-false (element-with-id xrefid
							 docelem))))
	 (xrefurl (and xreftarget
		       (get-link-policy-target xreftarget no-urls: #t))))
    (if (string=? (gi docelem)
		  (normalize "documentsummary")) ; sanity check...
	(if xrefent
	    (if xreftarget
		(if (car xrefurl)	; link to element by id
		    (error (car xrefurl)) ; violated policy - complain
		    (make command name: "textit"
			  (with-mode mk-docxref
			    (process-node-list (document-element xreftarget)))
			  (literal ": ")
			  (with-mode section-reference
			    (process-node-list xreftarget))))
		(make command name: "textit" ; link to whole document
		      (with-mode mk-docxref
			(process-node-list docelem))))
	    (error "No value for docxref's DOC attribute"))
	(error (string-append "DOCXREF target " xrefent
			      " has document type " (gi docelem)
			      ": expected DOCUMENTSUMMARY")))))

;; possibly due to a bug in Jade, we need to provide dummy definitions
;; of %starlink-document-server% and the function href-to
(define %starlink-document-server% #t)
(define href-to (lambda (#!rest x) #t))

(mode mk-docxref
  (element documentsummary
    (process-matching-children 'docinfo))
  (element docinfo
    (let ((dn (getdocnumber))
	  (dtitle (getdocinfo 'title)))
      (make sequence
	(literal (string-append dn ", "))
	(make command name: "textit"
	      (process-node-list dtitle))))))

<misccode>
<description><code/webref/ elements are simply transformed into 
the element content followed by a footnote containing the URL.
<code/url/ elements have the URL printed.
<codebody>
(element url
  (make command name: "Url"
	(process-children-trim)))

(element webref
  (make sequence
    (process-children-trim)
    (make empty-command
      name: "UrlFootnote"
      parameters: (list (attribute-string (normalize "url")
					  (current-node))))))

