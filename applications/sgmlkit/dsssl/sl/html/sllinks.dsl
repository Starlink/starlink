<!-- 

$Id$

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

-->

<routine>
<description>
<p>REF is a simple reference to another element in the same document.
Check that the target is a member of the list <funcname>target-element-list</funcname>.
<p>If the target is a member of the list returned by
<funcname>section-element-list</funcname>, then make the section reference by
calling <funcname>make-section-reference</funcname>; otherwise, process the
target in mode <code>section-reference</code>.
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
	 (linktext (data (current-node)))
	 )
    (if (and target
	     (member (gi target) (ref-target-element-list)))
	(make element
	  gi: "a"
	  attributes: `(("href" ,(href-to target)))
	  ;(with-mode section-reference
	  ;  (process-node-list target))
	  (if (string=? linktext "")
	      (if (member (gi target) (section-element-list))
		  (make-section-reference target: target specify-type: #t
					  short-ref: %short-crossrefs%)
		  (with-mode section-reference
		    (process-node-list target)))
	      (literal linktext))
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
		      " (ID " target-id ")"
		      )))
	    (error (string-append
		    "Can't find element with ID " target-id " in this document"
		    ))))))

;;; The following mode section-reference definition of REF (and the
;;; similar ones for DOCXREF, WEBREF and URL) is for dealing with
;;; DISPLAYTITLEs.  There's a problem using these, since DISPLAYTITLEs
;;; within the section names which are used as link text make an extra
;;; link within the link text!  The plan is that these simpler
;;; versions of the cross-referencing elements would be used in these
;;; contexts.
;;;
;;; This works, but I've since tentatively removed the DISPLAYTITLE
;;; element from the DTD.
; (mode section-reference
;   (element ref
;     (let ((target (element-with-id (attribute-string (normalize "id")
; 						     (current-node))))
; 	  (linktext (attribute-string (normalize "text")
; 				      (current-node))))
;       (if (member (gi target) (ref-target-element-list))
; 	  (if linktext
; 	      (literal linktext)	;override generation of link text
; 	      (if (member (gi target) (section-element-list))
; 		  (make-section-reference target: target specify-type: #t)
; 		  (with-mode section-reference
; 		    (process-node-list target))))
; 	  (error (string-append
; 		  "The stylesheet is presently unable to link to elements of type "
; 	          (gi target)))))))

;; Given a docnum of the form 'SUN/123', for example, this returns a sosofo
;; which represents an "a" element, pointing to the correct place
;;
;; Parameters:
;;    docnum the document number, such as "SUN/123"
;;    xrefid a location within that document, or #f
;;    linktext the content of the link, or #f, in which case a default
;;        text is generated
;;
;; Returns:
;;    A sosofo
(define ($make-dummy-link$ docnum xrefid linktext)
  (let* ((docparts (and docnum
                        (tokenise-string docnum
                                         isbdy?: (lambda (l)
                                                   (if (char=? (car l) #\/)
                                                       (cdr l)
                                                       #f)))))
         (href (and docparts
                    (if (= (length docparts) 2)
                        (string-append %starlink-document-server%
                                       (case-fold-down (car docparts))
                                       (cadr docparts)
                                       ".htx/"
                                       (case-fold-down (car docparts))
                                       (cadr docparts)
                                       ".html#xref_"
                                       (or xrefid ""))
                        #f))))
    
    (if href
	(make element gi: "a"
	      attributes: (list (list "href" href))
	      (literal (or linktext docnum)))
	#f)))
;; Given an FPI (a public identifier) of the form
;; "-//Starlink//DOCUMENT Summary SUN/123//EN", this extracts and returns
;; the docnum "SUN/123" from it, or #f on error.
(define ($docnum-from-fpi$ fpi)
  (let ((descrip (query-parse-fpi 'text-description
                                  (parse-fpi fpi))))
    (and descrip
         (car (reverse (tokenise-string descrip))))))

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

<p>In a project meeting, it was decided that there were substantial
operational difficulties in guaranteeing that document-summary
target files would always exist.  Therefore the processing of
this element was required to work even if the target element
didn't exist, by parsing the public identifier, and making
intelligent guesses about the system's environment.  This is very
messy, not least because it requires messy gymnastics in here.

<p>Initially, the generated link text could be overridden by
supplying a value for the `text' attribute.  To make this element
more consistent with the REF element, however, this has been
changed so that the element's link text is given as content to
the DOCXREF element.  The `text' attribute is still present and
supported, for (very temporary) backward compatibility, but
undocumented.

<p>Once we have obtained the element pointed to by the ID,
check to see if it is a <funcname>mapid</funcname> element, and if it is,
immediately resolve the indirection.

<p>This rule invokes the <funcname>get-link-policy-target</funcname> function to check
that the target of the link conforms to the policy -- if it doesn't,
it produces an <funcname>error</funcname>.

<p>The rule uses the <code>mk-docxref</code> mode to process the target element.

<codebody>
(element docxref
  (let ((xrefent (attribute-string (normalize "doc") (current-node)))
        ;; At one time, I extracted the entity's system id here.
        ;; It's not clear why I did this, as the only apparent use to
        ;; which I put it was to check whether it existed, and object
        ;; vigourously if it did.  I don't know why I had such a
        ;; downer on system ids in the entity declaration -- if I
        ;; decide that this is, after all, a good thing to forbid,
        ;; then perhaps I can put some explanation in next time.
        ;;(xrefent-sysid (and xrefent
        ;;	     (entity-system-id xrefent)))

        ;; If the element has content, make that the link text, else
        ;; if the element has a `text' attribute (temporary, for
        ;; backward compatibility), make that the link
        ;; text, else generate the text below.
        (linktext (if (not (string=? (data (current-node)) ""))
                      (data (current-node))
                      (attribute-string (normalize "text")
                                        (current-node))))

        ;; xrefid is the ID of an element in the target document
        ;; which is to become the target of the link
        (xrefid (attribute-string (normalize "loc") (current-node))))
    (cond
     ((not xrefent)
      (error "DOCXREF: missing DOC attribute"))

     ((not (entity-type xrefent))
      (or ($make-dummy-link$ xrefent xrefid linktext)
          (error (string-append
                  "DOCXREF: doc value " xrefent
                  " does not appear to be the name of a document"))))
     
     ((or (equal? (entity-type xrefent)
                  'subdocument)
          (and (equal? (entity-type xrefent)
                       'ndata)
               (string=? (notation-public-id  (entity-notation xrefent))
                         "-//Starlink//NOTATION Document Summary//EN")))
      ;; This is a cross-reference to a documentsummary document
      (let* (
             ;; If the target document does not exist (which is deemed to
             ;; be OK, and from which we recover by parsing the public
             ;; ID below), then xrefent-gen-sysid will become false.
             (xrefent-gen-sysid (entity-generated-system-id xrefent))
             (docelem (and xrefent-gen-sysid
                           (document-element-from-entity xrefent)))
             ;; xreftarget is the element the docxref refers to, or docelem if
             ;; attribute LOC is implied (xrefid is #f) or the document
             ;; doesn't have such an ID (element-with-id returns #f).  If
             ;; the target document does not exist, then docelem will be
             ;; false, and hence so will tmp-xreftarget and xreftarget.
             (tmp-xreftarget (or (and xrefid
                                      docelem
                                      (node-list-or-false
                                       (element-with-id xrefid docelem)))
                                 docelem))
             ;; If the target element is a mapid element, then dereference it
             (xreftarget
              (if (and tmp-xreftarget
                       (string=? (gi tmp-xreftarget)
                                 (normalize "mapid")))
                  (element-with-id (attribute-string (normalize "to")
                                                     tmp-xreftarget)
                                   docelem)
                  tmp-xreftarget))
             ;; Call get-link-policy-target to decide whether or not we
             ;; are allowed, by the target document's own settings, to
             ;; link to this `xreftarget'.
             (xrefurl (and xreftarget
                           (get-link-policy-target xreftarget)))
             
             (xrefent-pubid (and xrefent
                                 (entity-public-id xrefent))))
        (cond
         ((not xrefent)
          (error "DOCXREF: missing DOC attribute"))
         
         ((not xrefent-pubid)
          (error (string-append "DOCXREF: entity " xrefent
                                " has no PUBLIC identifier")))
         
         ((not xrefent-gen-sysid)
          ;; Here's the guesswork -- call $make-dummy-link$ to guess
          ;; a link based on the public ID.
          (or ($make-dummy-link$ ($docnum-from-fpi$ xrefent-pubid)
                                 xrefid linktext)
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
          ;;
          ;; Target document exists, and has a document element of type
          ;; `documentsummary'....
          ;;
          ;; There used to be two cases here, depending on whether
          ;; xreftarget was true or not, which in turn depended on whether
          ;; xrefid was true (ie, if we're linking to an element within
          ;; the document, rather than the document as a whole).  Since
          ;; revision 1.21, however, these two paths have been unified, so that
          ;; xreftarget is either the element indicated by xrefid, or the
          ;; document element, making the second of the following
          ;; alternatives redundant.  I worry, however, that there's
          ;; a case I haven't thought of, but it'll always be possible
          ;; to reinstate the alternative case (or rewrite the damn
          ;; thing from scratch).
          (if (car xrefurl)
              (error (car xrefurl)) ; violated policy - complain
              (make element gi: "a"
                    attributes: (list (list "href"
                                            (cdr xrefurl)))
                    (if linktext
                        (literal linktext)
                        (make sequence
                          (with-mode mk-docxref
                            (process-node-list (document-element
                                                xreftarget)))
                          (if (node-list=? xreftarget
                                           (document-element
                                            xreftarget))
                              (empty-sosofo) ; nothing more...
                              (make sequence ; add in a section reference
                                (literal ": ")
                                (with-mode section-reference
                                  (process-node-list xreftarget)))))))))

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
        (if (and sysid
                 (> (string-length sysid) 0))
            (make element gi: "a"
                  attributes: `(("href" ,sysid))
                  ;; Use the linktext if specified, or the entity name otherwise.
                  ;; Latter isn't great, but all we can do
                  (literal (or linktext xrefent)))
            (or ($make-dummy-link$ ($docnum-from-fpi$ pubid)
                                   xrefid linktext)
                (error (string-append
                        "DOCXREF: couldn't make sense of FPI '" pubid "'"))))))

     (else
      (error (string-append "DOCXREF: can't make any sense of entity "
                            xrefent ", of type "
                            (symbol->string (entity-type xrefent))))))))


; (mode section-reference
;   (element docxref
;     (let* ((xrefent (attribute-string (normalize "doc") (current-node)))
; 	   (docelem (and xrefent
; 			 (document-element
; 			  (sgml-parse (entity-generated-system-id xrefent)))))
; 	   (linktext (attribute-string (normalize "text")
; 				       (current-node))))
;       (if (string=? (gi docelem)
; 		    (normalize "documentsummary")) ; sanity check...
; 	  (if xrefent
; 	      (if linktext
; 		  (literal linktext)	;override generation of link text
; 		  (with-mode mk-docxref
; 		    (process-node-list docelem)))
; 	      (error "No value for docxref's DOC attribute"))
; 	  (error (string-append "DOCXREF target " xrefent
; 				" has document type " (gi docelem)
; 				": expected DOCUMENTSUMMARY"))))))

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
	    (make element gi: "cite"
		  (process-node-list dtitle)))))))

<routine>
<description><code>webref</code> elements are simply transformed into HTML
<code>A</code> elements, and <code>url</code> elements into <code>A</code> elements with
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
		      (list "title"  (trim-data (current-node))))
    (process-children)))

; (mode section-reference
;   (element url
;     (make element gi: "code"
; 	(make sequence
; 	  (literal "<")
; 	  (process-children)
; 	  (literal ">"))))
;   (element webref
;     (process-children)))

