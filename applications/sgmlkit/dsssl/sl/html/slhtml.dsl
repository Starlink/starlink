<!-- 

$Id$ 

<docblock>
<title>HTML-specific support
<description>
This section supports the generation of HTML.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray
<otherauthors>
<author id=nw>Norman Walsh
<authornote>
The structure of the support in this file, and some of the code, draws
on file <code/dbhtml.dsl/ from Norm Walsh's 
DocBook Stylesheet distribution.

<copyright>Copyright 1999, Particle Physics and Astronomy Research
Council.

<codegroup id=code.html>
<title>HTML-specific support

-->

<routine>
<routinename>html-document
<description>
<p>Every element which is potentially a chunk should be passed to
<funcname/html-document/ as its body-sosofo argument.  If it is in fact to
be chunked, then this creates a new entity with that body as its
contents, if not, it evaluates simply to body-sosofo.
<returnvalue type="sosofo">Either a new entity, or <code/body-sosofo/,
depending on whether the element is to be chunked.
<argumentlist>
<parameter>title-sosofo
  <type>sosofo
  <description>A sosofo which will form the title of the document, if it
  is made into a separate entity
<parameter>body-sosofo
  <type>sosofo
  <description>The body of the element
<parameter keyword default='#f'>system-id
  <type>string
  <description>If present, this gives the name of the system-id which is too 
  be used for the output file (if a file is in fact output), overriding the
  name obtained from <funcname/html-file/.  This is need in the case of those
  files (such as the notes) which have no associated element.
  <p>If this is present, and chunking has not been turned off
  completely, then the document <em/will/ be chunked, because it
  wouldn't make much sense otherwise.
<codebody>
(define (html-document title-sosofo body-sosofo #!key (system-id #f))
  (let* ((is-de? (node-list=? (current-node) (document-element)))
	 (doc-sosofo 
	  (if (or (chunk?) is-de?)
	      (make element gi: "HTML"
		    (make element gi: "HEAD"
			  (make element gi: "TITLE" title-sosofo)
			  ($standard-html-header$))
		    (make element gi: "BODY" 
			  attributes: %body-attr%
			  (header-navigation (current-node))
			  body-sosofo
			  (footer-navigation (current-node))
			  ))
	      body-sosofo)))
    (if stream-output
	(make sequence
	  (if is-de?
	      (make document-type
		name: "HTML"
		public-id: %html-pubid%)
	      (empty-sosofo))
	  doc-sosofo)
	(if (or (chunk?)		;if this is a chunk
		is-de?			;or this is the document-element
		(and (chunking?) system-id) ;or we've specified a
					    ;sysid (and we haven't
					    ;turned off chunking)
		)
	    (make entity system-id: (or system-id (html-file))
	      (make document-type
		name: "HTML"
		public-id: %html-pubid%)
	      doc-sosofo)
	    doc-sosofo))))

<routine>
<routinename>href-to
<purpose>Return the HTML HREF for the given node.  If chunking has been
turned off, just return the fragment identifier.
<description>Return the HTML HREF for the given node.  If chunking has been
turned off, just return the fragment identifier.
<p>There can be a number of special cases in the determination of the fragid.
Each one of these at present consists of a call to a function 
<funcname/href-to-fragid-giname/, which returns a fragment prefixed by `#'.
<returnvalue type="string">An HTML HREF which can be used to refer to
the current node.
<argumentlist>
<parameter>target
  <type>node-list
  <description>The href returned refers to the node passed as <code/target/
<parameter keyword>reffrag
  <type>boolean
  <description>If reffrag is false, don't append the fragid to the HREF.  
<parameter keyword>full-url
  <type>boolean
  <description>If full-url is true, prefix
  <code/%starlink-document-server%/ to the returned URL.
<parameter keyword default='#f'>force-frag
  <type>string
  <description>If not <code/#f/, force the fragment id to be this string.
<codebody>
(define (href-to target #!key (reffrag #t) (full-url #f) (force-frag #f))
  (let* ((id (attribute-string (normalize "id") target))
	 (entfile (html-file target_nd: target))
	 (url (string-append (if full-url %starlink-document-server% "")
			     entfile))
	 (fragid (cond
		  (force-frag force-frag)
		  ((node-list=? target (document-element))
		   "#xref_")
		  (else (case (gi target)
			  (("MLABEL") (href-to-fragid-mlabel target))
			  (else (if (or (chunk? target)
					(not id))
				    ""
				    (string-append "#xref_" id)))))))
	 ;(fragid (if (or (chunk? target)
		;	 (not id))
		;     ""
		;     (string-append "#xref_" id)))
	 )
    (if reffrag
	(string-append url fragid)
	url)))
;; Following is an attempt to (neatly) suppress the entfile if the reference
;; would be local, but (chunk?) as currently written returns false for any
;; nodes within the `master' chunk.
;    (debug (if (chunk? target) ;(or nochunks stream-output)
;	(if reffrag
;	    (string-append url fragid)
;	    url)
;	fragid))))

<routine>
<routinename>$standard-html-header$
<description>
A hook function to add additional tags to the HEAD of your HTML files
<returnvalue type=sosofo>Sosofo inserted in the head of HTML files
<codebody>
(define ($standard-html-header$)
  (let* (
	 ;(home (nav-home (current-node)))
	 (up   (parent (current-node)))
	 ;(prev (prev-chunk-element (current-node)))
	 ;(next (next-chunk-element (current-node)))
	 (onpair (onwards))
	 (next (cdr onpair))
	 (kws  (if (getdocinfo 'keyword) ; node-list, possibly empty
		   (getdocinfo 'keyword)
		   (empty-node-list)))
	 )
    (make sequence
      ;; Add the META NAME=GENERATOR tag
      (make empty-element gi: "META"
	    attributes: (list (list "NAME" "GENERATOR")
			      (list "CONTENT" %stylesheet-version%)))

      (with-mode make-html-author-links
	(process-node-list (getdocinfo 'authorlist)))

;      ;; Add the LINK REL=PREVIOUS tag
;      (if (node-list-empty? prev)
;	  (empty-sosofo)
;	  (make empty-element gi: "LINK"
;		attributes: (list (list "REL" "PREVIOUS")
;				  ;;(list "TITLE"  (element-title prev))
;				  (list "HREF" (href-to prev)))))

      ;; Add the LINK REL=NEXT tag
      (if (node-list-empty? next)
	  (empty-sosofo)
	  (make empty-element gi: "LINK"
		attributes: (list (list "REL" "NEXT")
				  (list "TITLE" (car onpair))
				  (list "HREF" (href-to next)))))

;      ;; Add the LINK REL=HOME tag
;      (if (nav-home? (current-node))
;	  (make empty-element gi: "LINK"
;		attributes: (list (list "REL" "HOME")
;				  ;;(list "TITLE"  (element-title home))
;				  (list "HREF" (href-to home))))
;	  (empty-sosofo))

      ;; Add the LINK REL=UP tag
      (if (nav-up? (current-node))
	  (if (or (node-list-empty? up)
		  (node-list=? up (document-element)))
	      (empty-sosofo)
	      (make empty-element gi: "LINK"
		    attributes: (list (list "REL" "UP")
				      (list "HREF" (href-to up)))))
	  (empty-sosofo))

      ;; Add META NAME=KEYWORD tags
      (let loop ((nl kws))
	(if (node-list-empty? nl)	; nl=empty-node-list if no keywords
	    (empty-sosofo)
	    (make sequence
	      (make empty-element gi: "META"
		    attributes: (list (list "NAME" "KEYWORD")
				      (list "CONTENT" (data (node-list-first nl)))))
	      (loop (node-list-rest nl)))))

;      ;; Add LINK REL=STYLESHEET tag
;      (if %stylesheet%
;	  (make empty-element gi: "LINK"
;		attributes: (list (list "REL" "STYLESHEET")
;				  (list "TYPE" %stylesheet-type%)
;				  (list "HREF" %stylesheet%)))
;	  (empty-sosofo))

;      ($user-html-header$ home up prev next)

      )))

<routine>
<description>
<p>Various other hooks for inserting code within the HTML file
<codebody>
(define ($user-html-header$ #!optional 
			    (home (empty-node-list))
			    (up (empty-node-list))
			    (prev (empty-node-list))
			    (next (empty-node-list)))
  ;; Add additional header tags.
  (let loop ((tl %html-header-tags%))
    (if (null? tl)
	(empty-sosofo)
	(make sequence
	  (make empty-element gi: (car (car tl))
		attributes: (cdr (car tl)))
	  (loop (cdr tl))))))

(define ($html-body-start$)
  (empty-sosofo))

(define ($html-body-content-start$)
  (empty-sosofo))

(define ($html-body-content-end$)
  (empty-sosofo))

(define ($html-body-end$)
  (empty-sosofo))
