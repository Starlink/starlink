;; This stylesheet is pretty sketchy at present.  It needs to be
;; tidied up and made more modular, but that can wait until there's
;; more of it available.
;;
;; Forget about sketchy, this particular file is a MESS!
;;
;; Bits of this freely stolen from Norm Walsh's DocBook stylesheet.


;; Document Head

;(element title
;  (process-children))
(element title
  (literal (normalise-string (data (current-node)))))

(element authorlist
  (process-children))

(mode make-html-author-links
  (element author
    (make empty-element
      gi: "link"
      attributes: (list (list "rev" "made")
			(list "href" (attribute-string "email"))
			(list "title" (data (current-node)))
			)))
)

;; In the default mode, make author a link to the author details,
;; if either of the webpage or email attributes is present
(element author
  (let* ((webpage (attribute-string (normalize "webpage") (current-node)))
	 (link (cond (webpage)
		     ((attribute-string (normalize "email")
					 (current-node))
		       (string-append "mailto:"
				      (attribute-string (normalize "email")
							(current-node))))
		     (else #f))))
      (if link
	  (make element gi: "A"
		attributes: (list (list "HREF" link))
		(literal (normalise-string (data (current-node)))))
	  (literal (normalise-string (data (current-node)))))))

;  (let* ((webpage (attribute-string (normalize "webpage") (current-node)))
;	 (link (if webpage
;		   webpage
;		   (if (attribute-string (normalize "email")
;					 (current-node))
;		       (string-append "mailto:"
;				      (attribute-string (normalize "email")
;							(current-node)))
;		       #f))))
;    (make sequence
;      (if link
;	  (make element gi: "A"
;		attributes: (list (list "HREF" link))
;		(literal (data (current-node))))
;	  (literal (data (current-node))))
;       (make empty-element gi: "BR"))


;(element DOCNUMBER
;  (make sequence
;    (process-children)))

;(element HISTORY
;  (make display-group
;    (process-children)))

;(element VERSION
;  (make display-group
;    (process-children)))

;(element RELEASE
;  (make display-group
;    (process-children)))

;; Document Body starts here

;; Process the docbody element by creating a `title page', using
;; information from the docinfo element, then doing (process-children)
;; to format the document content.
(element docbody
  (let* ((tsosofo (process-node-list (getdocinfo 'title)))
	 (authors (children (getdocinfo 'authorlist)))
	 (rel (document-release-info))
	 (vers (car (cdr (cdr rel))))
	 (date (format-date (car rel)))
;	 (dn (getdocinfo 'docnumber))
;	 ; docref is a string document-reference, or false if none
;	 ; available (because docnumber isn't defined
;	 (docref (if dn
;		     (string-append (gi (document-element))
;				    "/"
;				    (if (attribute-string "UNASSIGNED" dn)
;					"??"
;					(trim-string (data dn))))
;		     #f	
;		 ))
	 (docref (getdocnumber))
	 )
    (make sequence
      (make element gi: "TABLE"
	    attributes: '(("WIDTH" "100%"))
	    (make sequence
	      (make element gi: "TR"
		    (make element gi: "TD"
			  attributes: (list (list "COLSPAN" "2")
					    (list "ALIGN" "CENTER"))
			  (make element gi: "H1"
				tsosofo)))
	      (if docref
		  (make element gi: "TR"
			(make sequence
			  (make element gi: "TD"
				attributes: (list (list "ALIGN"	"RIGHT")
						  (list "WIDTH" "50%"))
				(make element gi: "EM"
				      (literal "Document")))
;			  (make element gi: "TD"
;				  (literal docref))
			  (make element gi: "TD"
				(make sequence
				  (literal docref)
				  (literal (if vers
				      (string-append "." vers)
				      ""))))
			  ))
		  (empty-sosofo))
	      (make element gi: "TR"
		    (make sequence
		      (make element gi: "TD"
			    attributes: '(("ALIGN" "RIGHT"))
			    (make element gi: "EM"
				      (literal "Author")))
		      (make element gi: "TD"
			    (node-list-reduce authors
					      (lambda (result a)
						(sosofo-append
						 result
						 (make sequence
						   (process-node-list a)
						   (make empty-element gi: "BR"))))
					      (empty-sosofo)))))
	      (make element gi: "TR"
		    (make sequence
		      (make element gi: "TD"
			    attributes: '(("ALIGN" "RIGHT"))
			    (make element gi: "EM"
				      (literal "Release date")))
		      (make element gi: "TD"
			      (literal date))))
;	      (make element gi: "TR"
;		    (make sequence
;		      (make element gi: "TD"
;			    attributes: '(("ALIGN" "RIGHT"))
;			    (make element gi: "EM"
;				  (literal "Version / Date")))
;		      (make element gi: "TD"
;			    (if vers
;				(literal (string-append vers " / " date))
;				(literal date)))))
	      (if (and %starlink-banner% (not suppress-banner))
		  (make element gi: "TR"
			(make element gi: "TD"
			      attributes: (list (list "ALIGN" "CENTER")
						(list "COLSPAN" "2"))
			      (make element gi: "SMALL"
				    %starlink-banner%)))
		  (empty-sosofo))))
      (process-children))))

(element abstract
  (make sequence
    (make empty-element gi: "hr")
    (make element
      gi: "P"
      (make element
	gi: "STRONG"
	(literal "Abstract: ")))
    (make element
      gi: "BLOCKQUOTE"
      (process-children))
    (make empty-element gi: "hr")))

;; Phrase markup

(define (make-simple-element element-name)
  (make element
    gi: element-name
    (process-children)))

(element CODE
  (make-simple-element "code"))

(element EM
  (make-simple-element "em"))

(element KBD
  (make-simple-element "code"))

(element QUOTE
  (make sequence
    (literal "`")
    (process-children)
    (literal "'")))

(element STRONG
  (make-simple-element "strong"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; References
;;
;; Note that, despite the intricate HyTime setup in the DTD, the
;; implementations here currently have the semantics built in, and
;; ignore the HyTime attributes!  I'm not sure if this is the correct
;; way to do this....


(element ref
  (let ((target (element-with-id (attribute-string (normalize "id")
						   (current-node)))))
    (make element
      gi: "A"
      attributes: (list (list "href" (href-to target)))
      (with-mode section-reference
	(process-node-list target)))))

;; This is from the Jade SGML-transformations page
(define (copy-attributes #!optional (nd (current-node)))
  (let loop ((atts (named-node-list-names (attributes nd))))
    (if (null? atts)
        '()
        (let* ((name (car atts))
               (value (attribute-string name nd)))
          (if value
              (cons (list name value)
                    (loop (cdr atts)))
              (loop (cdr atts)))))))

(mode mk-docxref
  (element documentsummary
    (process-matching-children 'docinfo))
  (element docinfo
    (let ((dn (getdocnumber))
	  (dtitle (getdocinfo 'title)))
      (make sequence
	(literal (string-append dn ", "))
	(make element gi: "CITE"
	      (process-node-list dtitle)))))
;  (element docinfo
;    (process-matching-children 'docnumber 'title))
;  (element docnumber
;    (let ((docref (getdocnumber)))
;      (literal docref)))
)

;; Return nl if it is a non-empty node-list, otherwise #f
(define (non-empty-nl nl)
  (if (node-list-empty? nl)
      #f
      nl))

;; Check link policy, and return a pair.  The car of the pair is #f is
;; the policy is satisfied, and a string otherwise (this is an error,
;; which should be signalled with the string as an explanation, and no
;; link should be made); the
;; cdr is a string URL giving the URL to be used, or #f if the policy
;; is satisfied but no link should be made (ie, if the URLLINKPOLICY
;; is NONE).
;;
;; If the DOCUMENTSUMMARY element's EXPORTEDLINKPOLICY has the value
;; "EXPORTEDONLY", we may only link to targets which have the EXPORT
;; attribute present and set to "EXPORT": that is, if we have
;; "EXPORTEDONLY" but no EXPORT,
;; then the policy is trivially satisfied, but we do not return any
;; URL.  If the DOCUMENTSUMMARY element's URLLINKPOLICY attribute is
;; "AUTOMATIC", then the URLPATH attribute must not be present, and we
;; generate a URL based on the elements location in the hierarchy; if
;; it is "EXPLICIT", the URLPATH must be present; if it is "NONE",
;; then policy is satisfied, but no URL should be returned.
(define (get-link-policy nd)
  (let* (; onlyexported is true if only exported IDs may be linked to
	 (onlyexported (string=? (attribute-string (normalize
						    "exportedlinkpolicy")
						   (document-element nd))
				 (normalize "onlyexported")))
	 ; If veto-export is false, then we can link to this; if it's
	 ; true, that's because this element's ID isn't exported, and
	 ; this violates policy.
	 (ex (attribute-string (normalize "export") nd))
	 (veto-export (and onlyexported
			   (not (and ex
				     (string=? ex
					       (normalize "export"))))))
	 (urlpolicy (attribute-string (normalize "urllinkpolicy")
				      (document-element nd)))
	 (urlpath (attribute-string (normalize "urlpath") nd)))
    (if veto-export
	(cons (string-append "The element with id "
			     (attribute-string (normalize "id") nd)
			     " has not been exported, so may not be linked to")
	      #f)
	(case urlpolicy
	  (("NONE")
	   (cons #f #f))	; policy satisfied - no link
	  (("EXPLICIT")
	   (if urlpath
	       (cons #f (string-append %starlink-document-server%
				       urlpath))
	       (cons (string-append "element with id "
				    (attribute-string (normalize "id") nd)
				    " has no URLPATH attribute")
		     #f)))
	  (("AUTOMATIC")
	   (if urlpath
	       (cons (string-append "element with id "
				    (attribute-string (normalize "id") nd)
				    " has an URLPATH attribute present")
		     #f)
	       (cons #f (href-to nd full-url: #t))))
	  (else
	   (cons (string-append "Unknown URLPOLICY: " urlpolicy) #f))))))

(element docxref
  (let* ((xrefent (attribute-string (normalize "doc") (current-node)))
	 (docelem (and xrefent
		       (document-element
			(sgml-parse (entity-generated-system-id xrefent)))))
	 (xrefid (attribute-string (normalize "loc") (current-node)))
	 ; xreftarget is the element the docxref refers to, or #f if
	 ; attribute LOC is implied or the document doesn't have such
	 ; an ID
	 (xreftarget (and xrefid
			  (non-empty-nl (element-with-id xrefid
							 docelem))))
	 (xrefurl (and xreftarget
		       (get-link-policy xreftarget))))
    (if (string=? (gi docelem)
		  (normalize "documentsummary")) ; sanity check...
	(if xrefent
	    (if xreftarget
		(if (car xrefurl)	; link to element by id
		    (error (car xrefurl)) ; violated policy - complain
		    (make element gi: "A"
			  attributes: (list (list "HREF"
						  (cdr xrefurl)))
			  (with-mode mk-docxref
			    (process-node-list (document-element xreftarget)))
			  (literal ": ")
			  (with-mode section-reference
			    (process-node-list xreftarget))))
		(make element gi: "A"	; link to whole document
		      attributes: (list (list "HREF"
					      (href-to docelem reffrag: #f)))
		      (with-mode mk-docxref
			(process-node-list docelem))))
	    (error "No value for docxref's DOC attribute"))
	(error (string-append "DOCXREF target " xrefent
			      " has document type " (gi docelem)
			      ": expected DOCUMENTSUMMARY")))))

;(element docxref
;  (let* ((xrefent (attribute-string "DOC" (current-node))))
;    (if xrefent
;        (process-node-list
;         (document-element
;          (sgml-parse
;           (entity-generated-system-id xrefent)
;           )))
;        (error "No value for docxref's DOC attribute"))))

;(element URL
;  (if (attribute-string "nolink" (current-node))
;      (make element
;	gi: "CODE"
;	(process-children))
;      (make element
;	gi: "A"
;	attributes: (list (list "href" (trim-string (data (current-node)))))
;	(make element
;	  gi: "CODE"
;	  (process-children)))))
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

(element WEBREF
  (make element
    gi: "A"
    attributes: (list (list "href" (attribute-string (normalize "url")
						     (current-node)))
		      (list "title" (normalise-string (data (current-node)))))
    (process-children)))

(element figure
  (let ((kids (children (current-node))))
  (make element gi: "div"
	attributes: '(("ALIGN" "CENTER"))
	(make sequence
	  (process-node-list (select-elements kids 'figurecontent))
	  (process-node-list (select-elements kids 'caption))))))

(element caption
  (make element gi: "p"
	(make sequence
	  (literal "Figure: ")
	  (process-children))))

(element figurecontent
  (let* ((alt-text (attribute-string (normalize "alt")
				     (current-node)))
	 (image-ents (attribute-string (normalize "image")
				       (current-node)))
	 (best-ent (and image-ents
			(get-sysid-by-notation image-ents '("JPEG" "GIF87A")))))
    (if image-ents
	(if best-ent
	    (make empty-element gi: "img"
		  attributes: (list (list "src" best-ent)
				    (list "alt" alt-text)))
	    (error "No suitable entity in figurecontent"))
	(process-children))))

;; Lists

(element DL
  (make-simple-element "dl"))

(element DT
  (make-simple-element "dt"))

(element DD
  (make-simple-element "dd"))

(element OL
  (make-simple-element "ol"))

(element UL
  (make-simple-element "ul"))

(element LI
  (make-simple-element "li"))

(element p
  (make element
    gi: "p"
    (process-children-trim)))

(element px
  (make element
    gi: "p"
    (process-children-trim)))

;(element CITATION
;  (make sequence
;    (process-children)))
;
(element CITE
  (make-simple-element "cite"))


(element BLOCKQUOTE
  (make-simple-element "blockquote"))

(element ATTRIBUTION
  (make element
    gi: "em"
    (make sequence
      (literal "[ ")
      (process-children)
      (literal " ]"))))

(element DRAFTNOTE
  (make element
    gi: "strong"
    (literal "Draft Note:")
    (process-children)))

;(element UPDATE
;  (make display-group
;    (process-children)))
;
;(element INDEX
;  (make sequence
;    (process-children)))
;
;(element NOTE
;  (make sequence
;    (process-children)))


;; Table and Figure stuff -- remove for now

;(element TABLE
;  (make sequence
;    (process-children)))

;(element COL
;  (make sequence
;    (process-children)))

;(element COLGROUP
;  (make display-group
;    (process-children)))

;(element TBODY
;  (make display-group
;    (process-children)))

;(element TR
;  (make display-group
;    (process-children)))

;(element TD
;  (make display-group
;    (process-children)))

;(element TH
;  (make display-group
;    (process-children)))

;(element TFOOT
;  (make display-group
;    (process-children)))

;(element THEAD
;  (make display-group
;    (process-children)))

(element VERBATIM
  (make element
    gi: "pre"
    (process-children)))

;(element BIBLIOGRAPHY
;  (make sequence
;    (process-children)))

;(element INDEXCONTENTS
;  (make sequence
;    (process-children)))

;(element NOTECONTENTS
;  (make sequence
;    (process-children)))


;; Routine list in SUN documents

;(element ROUTINELIST
;  (make display-group
;    (process-children)))

;(element ROUTINE
;  (make display-group
;    (process-children)))

;(element DESCRIPTION
;  (make display-group
;    (process-children)))

;(element EXAMPLES
;  (make display-group
;    (process-children)))

;(element PARAMETERLIST
;  (make display-group
;    (process-children)))

;(element PARAMDESC
;  (make sequence
;    (process-children)))

;(element PARAMETER
;  (make sequence
;    (process-children)))

;(element ROUTINENAME
;  (make sequence
;    (process-children)))

;(element ROUTINENOTES
;  (make display-group
;    (process-children)))

;(element SUMMARY
;  (make sequence
;    (process-children)))

;(element USAGE
;  (make sequence
;    (process-children)))




