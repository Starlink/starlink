<!doctype programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN">

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

<func>
<codeprologue>
<routinename>
<name>html-document
<description>
<p>Every element which is potentially a chunk should be passed to
<funcname/html-document/ as its body-sosofo argument.  If it is in fact to
be chunked, then this creates a new entity with that body as its
contents, if not, it evaluates simply to body-sosofo.
</description>
<returnvalue type="sosofo">Either a new entity, or <code/body-sosofo/,
depending on whether the element is to be chunked.
<argumentlist>
<parameter>
<name>title-sosofo
<type>sosofo
<description>
<p>A sosofo which will form the title of the document, if it is made
into a separate entity
</description>
</parameter>
<parameter>
<name>body-sosofo
<type>sosofo
<description>
<p>The body of the element
</description>
</parameter>
</argumentlist>
</codeprologue>
(define (html-document title-sosofo body-sosofo)
  (let ((doc-sosofo 
	 (if (or (chunk?) (node-list=? (current-node) (document-element)))
	     (make element gi: "HTML"
		   (make element gi: "HEAD"
			 (make element gi: "TITLE" title-sosofo)
			 ($standard-html-header$))
		   (make element gi: "BODY" 
			 attributes: %body-attr%
			 (header-navigation (current-node))
			 body-sosofo
			 ;(whereami "html-document")
			 (footer-navigation (current-node))
			 ))
	     body-sosofo)))
    (if stream-output
	(make sequence
	  (make document-type
	    name: "HTML"
	    public-id: %html-pubid%)
	  doc-sosofo)
	(if (or (chunk?)
		(node-list=? (current-node) (document-element)))
	    (make entity
	      system-id: (html-file)
	      (make document-type
		name: "HTML"
		public-id: %html-pubid%)
	      doc-sosofo)
	    doc-sosofo))))
</func>

<func>
<codeprologue>
<routinename>
<name>href-to
<description>
<p>Return the HTML HREF for the given node.  If chunking has been
turned off, just return the fragment identifier.
<returnvalue type="string">An HTML HREF which can be used to refer to
the current node.
<argumentlist>
<parameter>
<name>target
<type>node-list
<description>
<p>The href returned refers to the node passed as <code/target/
</description>
</parameter>
<parameter keyword>
<name>reffrag
<type>boolean
<description>
<p>If reffrag is false, don't append the fragid to the HREF.  
</parameter>
<parameter keyword>
<name>full-url
<type>boolean
<description>
<p>If full-url is true, prefix
<code/%starlink-document-server%/ to the returned URL.
</parameter>
</argumentlist>
</codeprologue>
(define (href-to target #!key (reffrag #t) (full-url #f))
  (let* ((id (attribute-string (normalize "id") target))
	 (entfile (html-file target))
	 (fragid (if (or (chunk? target)
			 (not id))
		     ""
		     (string-append "#" id))))
    (if (chunk? target) ;(or nochunks stream-output)
	(if reffrag
	    (string-append (if full-url %starlink-document-server% "")
			   entfile fragid)
	    entfile)
	fragid)))
</func>

<func>
<codeprologue>
<routinename>
<name>$standard-html-header$
<description>
<p>A hook function to add additional tags to the HEAD of your HTML files
<returnvalue type=sosofo>Sosofo inserted in the head of HTML files
<argumentlist none>
</codeprologue>
(define ($standard-html-header$)
  (let* (
;	 (home (nav-home (current-node)))
;	 (up   (parent (current-node)))
;	 (prev (prev-chunk-element (current-node)))
;	 (next (next-chunk-element (current-node)))
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

;      ;; Add the LINK REL=HOME tag
;      (if (nav-home? (current-node))
;	  (make empty-element gi: "LINK"
;		attributes: (list (list "REL" "HOME")
;				  (list "TITLE"  (element-title home))
;				  (list "HREF" (href-to home))))
;	  (empty-sosofo))

;      ;; Add the LINK REL=UP tag
;      (if (nav-up? (current-node))
;	  (if (or (node-list-empty? up)
;		  (node-list=? up (document-element)))
;	      (empty-sosofo)
;	      (make empty-element gi: "LINK"
;		    attributes: (list (list "REL" "UP")
;				      (list "TITLE"  (element-title up))
;				      (list "HREF" (href-to up)))))
;	  (empty-sosofo))

;      ;; Add the LINK REL=PREVIOUS tag
;      (if (node-list-empty? prev)
;	  (empty-sosofo)
;	  (make empty-element gi: "LINK"
;		attributes: (list (list "REL" "PREVIOUS")
;				  (list "TITLE"  (element-title prev))
;				  (list "HREF" (href-to prev)))))

;      ;; Add the LINK REL=NEXT tag
;      (if (node-list-empty? next)
;	  (empty-sosofo)
;	  (make empty-element gi: "LINK"
;		attributes: (list (list "REL" "NEXT")
;				  (list "TITLE"  (element-title next))
;				  (list "HREF" (href-to next)))))

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
</func>

<misccode>
<miscprologue>
<description>
<p>Various other hooks for inserting code within the HTML file
</miscprologue>
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
</misccode>
