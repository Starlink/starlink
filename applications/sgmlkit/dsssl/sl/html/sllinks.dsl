<![ ignore [
<!doctype programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN">
<!-- $Id$ -->

<docblock>
<title>Inter- and Intra-document cross references for HTML
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

]]>

<misccode>
<description>
<p>REF is a simple reference to another element in the same document.
Check that the target is a member of the list <funcname/target-element-list/.
<p>If the target is a member of the list returned by
<funcname/section-element-list/, then make the section reference by
calling <funcname/make-section-reference/; otherwise, process the
target in mode <code/section-reference/.
<codebody>
(element ref
  (let* ((target-id (attribute-string (normalize "id")
				      (current-node)))
	 (target (element-with-id target-id))
	 (linktext (attribute-string (normalize "text")
				     (current-node))))
    (if (member (gi target) (target-element-list))
	(make element
	  gi: "A"
	  attributes: (list (list "href" (href-to target)))
	  ;(with-mode section-reference
	  ;  (process-node-list target))
	  (if linktext
	      (literal linktext)
	      (if (member (gi target) (section-element-list))
		  (make-section-reference target: target specify-type: #t)
		  (with-mode section-reference
		    (process-node-list target))))
	  )
	(error (string-append
		"stylesheet can't link to ID " target-id " of type "
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
			  (node-list-or-false (element-with-id xrefid docelem))))
	 (xrefurl (and xreftarget
		       (get-link-policy-target xreftarget)))
	 (linktext (attribute-string (normalize "text")
				     (current-node))))
    (if (string=? (gi docelem)
		  (normalize "documentsummary")) ; sanity check...
	(if xrefent
	    (if xreftarget
		(if (car xrefurl)	; link to element by id
		    (error (car xrefurl)) ; violated policy - complain
		    (make element gi: "A"
			  attributes: (list (list "HREF"
						  (cdr xrefurl)))
			  (if linktext
			      (literal linktext)
			      (make sequence
				(with-mode mk-docxref
				  (process-node-list (document-element
						      xreftarget)))
				(literal ": ")
				(with-mode section-reference
				  (process-node-list xreftarget))))))
		(make element gi: "A"	; link to whole document
		      attributes:
		      (list (list "HREF"
				  (let ((url (attribute-string
					      (normalize "urlpath") docelem)))
				    (if url
					(string-append %starlink-document-server%
						       url)
					(href-to docelem reffrag: #f)))))
		      (if linktext
			  (literal linktext)
			  (with-mode mk-docxref
			    (process-node-list docelem)))))
	    (error "No value for docxref's DOC attribute"))
	(error (string-append "DOCXREF target " xrefent
			      " has document type " (gi docelem)
			      ": expected DOCUMENTSUMMARY")))))

(mode mk-docxref
  (element documentsummary
    (process-matching-children 'docinfo))
  (element docinfo
    (let ((dn (getdocnumber))
	  (dtitle (getdocinfo 'title)))
      (make sequence
	(literal (string-append dn ", "))
	(make element gi: "CITE"
	      (process-node-list dtitle))))))

<misccode>
<description><code/webref/ elements are simply transformed into HTML
<code/A/ elements, and <code/url/ elements into <code/A/ elements with
the URL as link text
<codebody>
(element url
  (make element gi: "code"
	(make sequence
	  (literal "<")
	  (if (attribute-string (normalize "nolink") (current-node))
	      (process-children)
	      (make element gi: "a"
		    attributes: (list (list "href" (trim-data (current-node))))
		    (process-children)))
	  (literal ">"))))

(element webref
  (make element gi: "a"
    attributes: (list (list "href" (attribute-string (normalize "url")
						     (current-node)))
		      (list "title" (normalise-string (data (current-node)))))
    (process-children)))

