<!-- $Id$ -->

<!--
<docblock>
<title>Inter- and Intra-document cross references for TeXML
<description>
Support the various cross reference elements

<p>Note that, despite the intricate HyTime setup in the DTD, the
implementations here currently have the semantics built in, and
ignore the HyTime attributes!

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, 2004, Council for the Central Laboratory of the Research Councils

<codegroup id='code.links'>
<title>Support cross references
-->

<routine>
<description>REF is a simple reference to another element in the same document.
Check that the target is a member of the list <funcname>target-element-list</funcname>.
If the `text' attribute is present, then use that as the link text,
rather than generating it from the link target.  In this case, do not put
the link text in italics, as it has presumably been chosen to blend in with
the surrounding text, even though this will make it invisible in a 
presentation which has no links (the motivation for this was the fact that
`ref' elements <em>can</em> occur within `verbatim' elements, in which a `textit'
command stands out somewhat).
<p>Once we have obtained the element pointed to by the ID,
check to see if it is a <funcname>mapid</funcname> element, and if it is,
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
	 (linktext (data (current-node))))
    (if (and target
	     (member (gi target) (ref-target-element-list)))
	(if (string=? linktext "")
	    (if (member (gi target) (section-element-list))
		(make-latex-command name: "textit"
		      (make-section-reference target: target
					      specify-type: #t
					      short-ref: %short-crossrefs%))
		(with-mode section-reference
			(process-node-list target)))
	    (literal linktext)	;override generation of link text
	    )
	(if target
	    (make sequence
	      ;; Can't make this type of link.  Object, but try to
	      ;; emit something nonetheless.
	      (if (string=? linktext "")
		  (literal (string-append
			    "[reference to " (gi target) "]"))
		  (literal linktext))
	      (error (string-append
		      "The stylesheet is presently unable to link to elements of type "
		      (gi target)
		      " (ID " target-id ")")))
	    (error (string-append
		    "Can't find element with ID " target-id))))))

(define (gen-label #!optional (nd (current-node)))
  (if (attribute-string (normalize "id") nd)
      (string-append (gi nd) (attribute-string (normalize "id") nd))
      (error "Can't generate references to unlabelled elements")))

;; Given an FPI (a public identifier) of the form
;; "-//Starlink//DOCUMENT Summary SUN/123//EN", this returns a sosofo
;; which represents a reference to it
;;
;; Parameters:
;;    fpi the FPI which refers to the document
;;    linktext the content of the link, or #f, in which case a default
;;        text is generated
;;
;; Returns:
;;    A sosofo
(define ($make-dummy-reference$ fpi linktext)
  (if linktext
      (literal linktext)
      (let ((td (query-parse-fpi 'text-description
                                 (parse-fpi fpi))))
        (if td
            (literal (car (reverse (tokenise-string td))))
            #f))))


<routine>
<description>The <code>docxref</code> element has a required attribute
giving the document which is to be referred to, and an optional
attribute giving an ID within that document.  The target of the link
should be a document marked up according to the <code>documentsummary</code>
DTD.  In fact, that DTD is a base architecture of the Starlink General
DTD, so we could get the same effect by linking to the actual document
and extracting the <code>documentsummary</code> architectural instance.
However, the <funcname>sgml-parse</funcname> function in DSSSL isn't defined as
being able to do that; there is a Jade patch which allows it to do
that, which I hope to build into a Starlink version of Jade when I
can.

<p>Once we have obtained the element pointed to by the ID,
check to see if it is a <funcname>mapid</funcname> element, and if it is,
immediately resolve the indirection.

<p>This rule invokes the <funcname>get-link-policy-target</funcname> function to check
that the target of the link conforms to the policy -- if it doesn't,
it produces an <funcname>error</funcname>.

<p>The rule uses the <code>mk-docxref</code> mode to process the target element.

<codebody>
;; The code here is adapted from the html version of this, in
;; ../html/sllinks.dsl, and it's more fully explained there.  Only the
;; differences from that code are documented here.
(element docxref
  (let ((xrefent (attribute-string (normalize "doc") (current-node)))

        (linktext (if (not (string=? (data (current-node)) ""))
                      (data (current-node))
                      (attribute-string (normalize "text")
                                        (current-node))))

        (xrefid (attribute-string (normalize "loc") (current-node))))
    (cond
     ((not xrefent)
      (error "DOCXREF: missing DOC attribute"))

     ((not (entity-type xrefent))
      (literal xrefent))                ; just add literal
                                        ; -- no need to create dummy text 
     
     ((or (equal? (entity-type xrefent)
                  'subdocument)
          (and (equal? (entity-type xrefent)
                       'ndata)
               (string=? (notation-public-id  (entity-notation xrefent))
                         "-//Starlink//NOTATION Document Summary//EN")))
      (let* ((xrefent-gen-sysid (entity-generated-system-id xrefent))
             (docelem (and xrefent-gen-sysid
                           (document-element-from-entity xrefent)))

             ;; xreftarget is the element the docxref refers to, or #f if
             ;; attribute LOC is implied or the document doesn't have such
             ;; an ID
             (tmp-xreftarget (and xrefid
                                  docelem
                                  (node-list-or-false
                                   (element-with-id xrefid docelem))))

             (xreftarget
              (if (and tmp-xreftarget
                       (string=? (gi tmp-xreftarget)
                                 (normalize "mapid")))
                  (element-with-id (attribute-string (normalize "to")
                                                     tmp-xreftarget)
                                   docelem)
                  tmp-xreftarget))
             ;; no-urls: #t added
             (xrefurl (and xreftarget
                           (get-link-policy-target xreftarget no-urls: #t)))
             
             (xrefent-pubid (and xrefent
                                 (entity-public-id xrefent))))
        (cond
         ((not xrefent)
          (error "DOCXREF: missing DOC attribute"))
         
         ((not xrefent-pubid)
          (error (string-append "DOCXREF: entity " xrefent
                                " has no PUBLIC identifier")))
         
         ((not xrefent-gen-sysid)
          (or ($make-dummy-reference$ xrefent-pubid linktext)
              (error (string-append
                      "DOCXREF: couldn't make sense of FPI '"
                      xrefent-pubid "'"))))
         
         ((not docelem)
          (error (string-append
                  "DOCXREF: Couldn't parse " xrefent
                  " (gen-sys-id " xrefent-gen-sysid ")")))
         
         ((string=? (gi docelem)
                    (normalize "documentsummary"))
          ;; This is the normal case: everything's working
          ;; This is the main difference from ../html/sllinks.dsl for this element
          (if linktext
              (literal linktext)  ;override generation of link text
            (if xreftarget
                (if (car xrefurl)       ; link to element by id
                    (error (car xrefurl)) ; violated policy - complain
                  (make-latex-command name: "textit"
                        (with-mode mk-docxref
                                   (process-node-list (document-element
                                                       xreftarget)))
                        (literal ": ")
                        (with-mode section-reference
                                   (process-node-list xreftarget))))
              (make-latex-command name: "textit" ; link to whole document
                    (with-mode mk-docxref
                               (process-node-list docelem))))))

         (else
          ;; Ooops, failed at the last hurdle: the target document is the
          ;; wrong type
          (error (string-append "DOCXREF: target " xrefent
                                " has document type " (gi docelem)
                                ": expected "
                                (normalize "documentsummary")))))))

     ((and (equal? (entity-type xrefent) 'ndata)
           (string=? (notation-public-id (entity-notation xrefent))
                     "+//IDN www.w3.org/TR/1998/REC-xml-19980210//NOTATION XML//EN"))
      ;; A cross-reference to another XML document.  If there's a system id, 
      ;; use that; if the system id is absent or blank (it's required to be
      ;; present in XML), then generate a link based on the public id
      (let ((pubid (entity-public-id xrefent))
            (sysid (entity-system-id xrefent)))
        (make sequence
          (or ($make-dummy-reference$ pubid linktext)
              (error (string-append
                      "DOCXREF: couldn't make sense of FPI '" pubid "'")))
          (if (and sysid
                   (> (string-length sysid) 0))
              (make-latex-command name: "UrlFootnote"
                    (literal sysid))
              (empty-sosofo)))))

     (else
      (error (string-append "DOCXREF: can't make any sense of entity "
                            xrefent ", of type "
                            (symbol->string (entity-type xrefent))))))))


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
	    (make-latex-command name: "textit"
		  (process-node-list dtitle)))))))

<routine>
<description><code>webref</code> elements are simply transformed into 
the element content followed by a footnote containing the URL.
<code>url</code> elements have the URL printed.
<codebody>
(element url
  (make-latex-command name: "Url"
	(process-children-trim)))

(element webref
  (make sequence
    (process-children-trim)
    (make-latex-empty-command
      name: "UrlFootnote"
      parameters: (list (attribute-string (normalize "url")
					  (current-node))))))

