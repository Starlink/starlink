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

<routine>
<description>REF is a simple reference to another element in the same document.
Check that the target is a member of the list <funcname/target-element-list/.
If the `text' attribute is present, then use that as the link text,
rather than generating it from the link target.  In this case, do not put
the link text in italics, as it has presumably been chosen to blend in with
the surrounding text, even though this will make it invisible in a 
presentation which has no links (the motivation for this was the fact that
`ref' elements <em/can/ occur within `verbatim' elements, in which a `textit'
command stands out somewhat).
<p>Once we have obtained the element pointed to by the ID,
check to see if it is a <funcname/mapid/ element, and if it is,
immediately resolve the indirection.
<codebody>
(element ref
  (let* ((target-id (attribute-string (normalize "id")
				      (current-node)))
	 (tmp-target (node-list-or-false (element-with-id target-id)))
	 (target (if (and tmp-target
			  (string=? (gi tmp-target)
				    (normalize "mapid")))
		     (element-with-id (attribute-string (normalize "to")
							tmp-target))
		     tmp-target))
	 (linktext (attribute-string (normalize "text")
				     (current-node))))
    (if (and target
	     (member (gi target) (ref-target-element-list)))
	(if linktext
	    (literal linktext)	;override generation of link text
	    (if (member (gi target) (section-element-list))
		(make element gi: "ref"
		      (make-section-reference target: target
					      specify-type: #t
					      short-ref: %short-crossrefs%))
		(with-mode section-reference
			(process-node-list target))))
	(if target
	    (error (string-append
		    "The stylesheet is presently unable to link to elements of type "
		    (gi target)))
	    (error (string-append
		    "Can't find element with ID " target-id))))))

<routine>
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

<p>Once we have obtained the element pointed to by the ID,
check to see if it is a <funcname/mapid/ element, and if it is,
immediately resolve the indirection.

<p>This rule invokes the <funcname/get-link-policy-target/ function to check
that the target of the link conforms to the policy -- if it doesn't,
it produces an <funcname/error/.

<p>The rule uses the <code/mk-docxref/ mode to process the target element.

<codebody>
(element docxref
  (let* ((xrefent (attribute-string (normalize "doc") (current-node)))
	 ;; At one time, I extracted the entity's system id here.
	 ;; It's not clear why I did this, as the only apparent use to
	 ;; which I put it was to check whether it existed, and object
	 ;; vigourously if it did.  I don't know why I had such a
	 ;; downer on system ids in the entity declaration -- if I
	 ;; decide that this is, after all, a good thing to forbid,
	 ;; then perhaps I can put some explanation in next time.
	 ;;(xrefent-sysid (and xrefent
	 ;;		     (entity-system-id xrefent)))
	 (xrefent-gen-sysid (and xrefent
				 (entity-generated-system-id xrefent)))
	 (docelem (and xrefent-gen-sysid
		       (document-element-from-entity xrefent)))
	 (xrefid (attribute-string (normalize "loc") (current-node)))
	 ;; xreftarget is the element the docxref refers to, or #f if
	 ;; attribute LOC is implied or the document doesn't have such
	 ;; an ID
	 (tmp-xreftarget (and xrefid
			      docelem
			      (node-list-or-false (element-with-id xrefid
								   docelem))))
	 (xreftarget (if (and tmp-xreftarget
			      (string=? (gi tmp-xreftarget)
					(normalize "mapid")))
			 (element-with-id (attribute-string (normalize "to")
							    tmp-xreftarget)
					  docelem)
			 tmp-xreftarget))
	 (xrefurl (and xreftarget
		       (get-link-policy-target xreftarget no-urls: #t)))
	 (linktext (attribute-string (normalize "text")
				     (current-node))))
    (if (and xrefent-gen-sysid
	     docelem
	     (string=? (gi docelem)
		       (normalize "documentsummary"))) ; sanity check...
	(if linktext
	    (literal linktext)	;override generation of link text
	    (if xreftarget
		(if (car xrefurl)	; link to element by id
		    (error (car xrefurl)) ; violated policy - complain
		    (make element gi: "docxref"
			  (with-mode mk-docxref
			    (process-node-list (document-element
						xreftarget)))
			  (literal ": ")
			  (with-mode section-reference
			    (process-node-list xreftarget))))
		(make element gi: "docxref" ; link to whole document
		      (with-mode mk-docxref
			(process-node-list docelem)))))
	(let ((xrefent-pubid (and xrefent
				  (entity-public-id xrefent))))
	  (cond
	   (docelem (error (string-append "DOCXREF: target " xrefent
					  " has document type " (gi docelem)
					  ": expected "
					  (normalize "documentsummary"))))
	   ;;(xrefent-sysid (error (string-append "DOCXREF: entity " xrefent
	   ;;					" has a SYSTEM id")))
	   (xrefent-gen-sysid (error (string-append
				      "DOCXREF: Couldn't parse "
				      xrefent-gen-sysid)))
	   (xrefent-pubid (if linktext
			      (literal linktext)
			      (let* ((pfpi (parse-fpi xrefent-pubid))
				     (td (query-parse-fpi 'text-description
							  pfpi)))
				(if td
				    (literal
				     (car (reverse (tokenise-string td))))
				    (error (string-append
					    "DOCXREF: couldn't make sense of FPI '"
					    xrefent-pubid "'"))))))
	   (xrefent (error (string-append
			    "DOCXREF: entity " xrefent
			    " has no PUBLIC identifier")))
	   (else (error "DOCXREF: missing DOC attribute")))))))

;; possibly due to a bug in Jade, we need to provide dummy definitions
;; of %starlink-document-server% and the function href-to
(define %starlink-document-server% #t)
(define href-to (lambda (#!rest x) #t))

(mode mk-docxref
  (element documentsummary
    (process-matching-children 'docinfo))
  (element docinfo
    (if %short-crossrefs%
	(literal (getdocnumber))
	(let ((dn (getdocnumber))
	      (dtitle (getdocinfo 'title)))
	  (make sequence
	    (literal (string-append dn ", "))
	    (make element gi: "XXX"
		  (process-node-list dtitle)))))))

<routine>
<description><code/webref/ elements are simply transformed into 
the element content followed by a footnote containing the URL.
<code/url/ elements have the URL printed.
<codebody>
(element url
  (make element gi: "url"
	(process-children-trim)))

(element webref
  (make element gi: "webref"
	attributes: (copy-attributes)
	(process-children)))
