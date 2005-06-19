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
on file <code>dbhtml.dsl</code> from Norm Walsh's 
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
<funcname>html-document</funcname> as its body-sosofo argument.  If it is in fact to
be chunked, then this creates a new entity with that body as its
contents, if not, it evaluates simply to body-sosofo.
<returnvalue type="sosofo">Either a new entity, or <code>body-sosofo</code>,
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
  name obtained from <funcname>html-file</funcname>.  This is need in the case of those
  files (such as the notes) which have no associated element.
  <p>If this is present, and chunking has not been turned off
  completely, then the document <em>will</em> be chunked, because it
  wouldn't make much sense otherwise.
<parameter keyword default='#f'>force-chunk?
  <type>boolean
  <description>If true, then treat the body as a chunk, even if it
  wouldn't otherwise be taken to be one.
<parameter keyword default='#t'>navbars?
  <type>boolean
  <description>If true (default) then include the navigation aids at 
  the beginning and end of the page.
<codebody>
(define (html-document title-sosofo body-sosofo #!key (system-id #f)
						      (force-chunk? #f)
						      (navbars? #t)
						      (uplink #f))
  (let* ((is-de? (node-list=? (current-node) (document-element)))
	 (chunk-it? (or force-chunk?
			(chunk?)	;if this is a chunk
			is-de?		;or this is the document-element 
			(and (chunking?) system-id) ;or we've specified a
					;sysid (and we haven't
					;turned off chunking)
			))
	 (doc-sosofo 
	  (if chunk-it? ; (or force-chunk? (chunk?) is-de?)
	      (make element gi: "html"
		    (make element gi: "head"
			  (make element gi: "title" title-sosofo)
			  ($standard-html-header$))
		    (make element gi: "body" 
			  attributes: %body-attr%
			  (if navbars?
			      (header-navigation (current-node)
						 uplink: uplink)
			      (empty-sosofo))
			  body-sosofo
			  (if navbars?
			      (footer-navigation (current-node)
						 uplink: uplink)
			      (empty-sosofo))
			  ))
	      body-sosofo)))
    (if stream-output
	(make sequence
	  (if is-de?
	      (make document-type
		name: "html"
		public-id: %html-pubid%)
	      (empty-sosofo))
	  doc-sosofo)
	(if chunk-it?
	    (make entity system-id: (or system-id (html-file))
	      (make document-type
		name: "html"
		public-id: %html-pubid%)
	      doc-sosofo)
	    doc-sosofo))))

<routine>
<routinename>href-to
<purpose>Return the HTML HREF for the given node.
<description>Return the HTML HREF for the given node.  If chunking has
been turned off, just return the fragment identifier.
<p>There can be a number of special cases in the determination of the fragid.
Each one of these at present consists of a call to a function 
<funcname>href-to-fragid-giname</funcname>, which returns a fragment not
prefixed by `#'.
<p>HTTP hrefs are described in
<webref url='http://sunsite.org.uk/rfc/rfc1945.txt'
>RFC 1945</webref> and
URLs are described in <webref
url='http://sunsite.org.uk/rfc/rfc1738.txt' >RFC 1738</webref>.
<returnvalue type="string">An HTML HREF which can be used to refer to
the current node.  If the function was called with <code>frag-only</code>
set to <code>#t</code> and no fragment ID could be generated, then it
returns <code>#f</code>.  This <code>#f</code> return value might well
generate an error such as `invalid value for "attributes"
characteristic' -- it should really be tested for whenever href-to is
invoked in this way.
<argumentlist>
<parameter>target
  <type>node-list
  <description>The href returned refers to the node passed as <code>target</code>
<parameter keyword default='#t'>reffrag
  <type>boolean
  <description>If reffrag is true (default), then append the fragid to
  the HREF.
<parameter keyword default='#f'>frag-only
  <type>boolean
  <description>If true, then only the fragment identifier (without the
  <code>#</code> character) is returned.  Calling <funcname>href-to</funcname>
  with this set to <code>#t</code> is the best way of generating IDs within
  the stylesheet.
<parameter keyword default='#f'>full-url
  <type>boolean
  <description>If full-url is true, prefix
  <code>%starlink-document-server%</code> to the returned URL.
<parameter keyword default='#f'>force-frag
  <type>string
  <description>If not <code>#f</code>, force the fragment id to be this string.
<codebody>
(define (href-to target #!key (reffrag #t)
			      (frag-only #f)
			      (full-url #f)
			      (force-frag #f))
  (let* ((exportatt (attribute-string (normalize "export") target))
         (idatt (attribute-string (normalize "id") target))
         (id (cond
	      ;; First, a couple of special cases.
	      ;;
	      ;; `codecollection' elements have special (and
	      ;; complicated) rules for linking.  See slroutines.dsl
	      ((equal? (gi target) (normalize "codecollection"))
	       #f			; no fragment identifier
	       )
	      ;; `mlabel' elements can't be exported.  Also, the
	      ;; href-to-fragid-mlabel function can deal with
	      ;; equations inside programcode documents.
	      ((equal? (gi target) (normalize "mlabel"))
	       (href-to-fragid-mlabel target))
	      ;; Everything else: use an xref target suitable for
	      ;; later processing with HTX.
	      (idatt 
	       (if exportatt
		   (string-append "xref_" idatt)
		   idatt))
	      ;; Following ones aren't used when referring to targets
	      ;; with IDs -- used instead for generating
	      ;; HTX-compatible `name' attributes for certain special
	      ;; elements...
	      ((node-list=? target (document-element)) 
	       "xref_")
	      ((equal? (gi target) (normalize "abstract"))
	       "xref_abstract")
	      ;; `routine' elements
	      ;((equal? (gi target) (normalize "routine"))
	      ; (href-to-fragid-routine target))
	      ;; ...and for generating HREFs and NAMEs for
	      ;; internally generated cross-references (ie, link
	      ;; targets such as routine cross-references, which
	      ;; aren't linked by a REF element: note that
	      ;; section-element-list includes both `codegroup' and
	      ;; `routine').
	      ((member (gi target) (section-element-list))
	       (string-append "_ID" 
			      (number->string (all-element-number target))))
	      (else #f)))
	 (entfile (and (not frag-only)
		       (html-file target_nd: target)))
	 (url (and entfile
		   (string-append (if full-url %starlink-document-server% "")
				  entfile)))
         (fragid (or force-frag id)))
    (cond (frag-only fragid)
	  (reffrag (string-append url (if fragid
					  (string-append "#" fragid)
					  "")))
	  (else url))))

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
         (next (nav-forward-element (current-node)))
         (prev (nav-backward-element (current-node)))
	 (kws  (if (getdocinfo 'keyword) ; node-list, possibly empty
		   (getdocinfo 'keyword)
		   (empty-node-list)))
	 )
    (make sequence
      ;; Add the META NAME=GENERATOR tag
      (make empty-element gi: "meta"
	    attributes: (list (list "name" "generator")
			      (list "content" %stylesheet-version%)))

      (with-mode make-html-author-links
	(if (getdocinfo 'authorlist)
	    (process-node-list (getdocinfo 'authorlist))
	    (empty-sosofo)))

      ;; Add the LINK REL=PREVIOUS tag
      (if (node-list-empty? prev)
	  (empty-sosofo)
	   (make empty-element gi: "link"
	 	attributes: (list (list "rel" "previous")
			    (list "href" (href-to prev)))))

      ;; Add the LINK REL=NEXT tag
      (if (node-list-empty? next)
	  (empty-sosofo)
	  (make empty-element gi: "link"
		attributes: (list (list "rel" "next")
				  (list "href" (href-to next)))))

;      ;; Add the LINK REL=HOME tag
;      (if (nav-home? (current-node))
;	  (make empty-element gi: "link"
;		attributes: (list (list "rel" "home")
;				  ;;(list "title"  (element-title home))
;				  (list "href" (href-to home))))
;	  (empty-sosofo))

      ;; Add the LINK REL=UP tag
	(if (or (node-list-empty? up)
	  (node-list=? up (document-element)))
	    (empty-sosofo)
	    (make empty-element gi: "link"
		  attributes: (list (list "rel" "up")
				    (list "href" (href-to up)))))

      ;; Add META NAME=KEYWORD tags
      (let loop ((nl kws))
	(if (node-list-empty? nl)	; nl=empty-node-list if no keywords
	    (empty-sosofo)
	    (make sequence
	      (make empty-element gi: "meta"
		    attributes: (list (list "name" "keyword")
				      (list "content" (data (node-list-first nl)))))
	      (loop (node-list-rest nl)))))

;      ;; Add LINK REL=STYLESHEET tag
;      (if %stylesheet%
;	  (make empty-element gi: "link"
;		attributes: (list (list "rel" "stylesheet")
;				  (list "type" %stylesheet-type%)
;				  (list "href" %stylesheet%)))
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
