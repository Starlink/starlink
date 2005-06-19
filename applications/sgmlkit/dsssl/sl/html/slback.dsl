<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [
  <!ENTITY common.dsl		SYSTEM "../common/slcommon.dsl" SUBDOC>
  <!ENTITY lib.dsl		SYSTEM "../lib/sllib.dsl" SUBDOC>
  <!ENTITY commonparams.dsl	PUBLIC "-//Starlink//TEXT DSSSL Common Parameterisation//EN">
  <!ENTITY params.dsl		PUBLIC "-//Starlink//TEXT DSSSL HTML Parameterisation//EN">
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

<copyright>Copyright 1999, 2000,
Council for the Central Laboratory of the Research Councils

<codegroup id='code.back'>
<title>Support back-matter

<routine>
<description>Declare Jade extension
<codebody>
(define read-entity
  (external-procedure "UNREGISTERED::James Clark//Procedure::read-entity"))
(declare-flow-object-class fi
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

;; Language objects are available only in OpenJade.
;; See http://sources.redhat.com/ml/dssslist/2002-03/msg00007.html
;; Uses POSIX locales, but there's no apparent way of getting the default
;; or, say, POSIX locale.
;; See http://openjade.sourceforge.net/jadedoc-1.3/index.htm#extensions
;; Using (define-language ...) would presumably be useful, but the
;; documentation makes it sound very hard.....
(define language
  (external-procedure "UNREGISTERED::OpenJade//Procedure::language"))
(declare-default-language (or (language 'en 'gb)
                              (language 'en 'us)))




<routine>
<description>
Support back-matter elements.  Changes here might need matching changes 
in mode make-manifest-mode in sl.dsl
<codebody>
(element backmatter
  (empty-sosofo))

(define (hasbackmatter?)
  (or (hasnotes?)
      (hasbibliography?)
      (hashistory?)
      (hasidindex?)
      (haskeywordindex?)))

;; Do NOT call this within the context of the document-element.  It
;; messes up big-time if (current-node) is the document-element,
;; because (I think -- I'm adding this note a little while after this
;; injunction) that results in a recursive call to this routine.
(define (process-backmatter)
  (if (node-list=? (current-node) (document-element))
      (error "(process-backmatter) called within document-element")
    (if (hasbackmatter?)
	(let ((body (make sequence
			  (make element gi: "ul"
				(make-contents-backmatter))
			  (make-notecontents)
			  (make-bibliography)
			  (make-updatelist)
			  (make-idindex)
                          (make-keywordindex))))
	  (if (chunking?)
	      (html-document (literal "Backmatter")
			     body
			     system-id: (backmatter-sys-id)
			     force-chunk?: #t
			     navbars?: #f)
	    (make sequence
		  (make element gi: "h1"
			(literal "Backmatter"))
		  body)))
      (empty-sosofo))))

(define (make-manifest-backmatter)
  (make sequence
    (if separate-toc
	;; Not really _back_matter, but a special case which is most
	;; conveniently handled here.
	(make fi data: (string-append %toc-file-root% %html-ext% "
"))
	(empty-sosofo))
    (if (hasbackmatter?)
	(make fi data: (string-append (backmatter-sys-id) "
"))
	(empty-sosofo))
    (if (hasnotes?)
	(make fi data: (string-append (notes-sys-id) "
"))
	(empty-sosofo))
    (if (hasbibliography?)
	(make fi data: (string-append (bibliography-sys-id) "
"))
	(empty-sosofo))
    (if (hashistory?)
	(make fi data: (string-append (updatelist-sys-id) "
"))
	(empty-sosofo))
    (if (hasidindex?)
	(make fi data: (string-append (idindex-sys-id) "
"))
	(empty-sosofo))
    (if (haskeywordindex?)
        (make fi data: (string-append (keywordindex-sys-id) "
"))
        (empty-sosofo))))

;; This function caters for the possibility that _no_ backmatter 
;; needs to be generated.  Generally, the history
;; element requires at least one version, but a MUD may have 
;; no history element?
(define (make-contents-backmatter)
  (let* ((noteslist
	  (and (hasnotes?)
               (make element gi: "li"
                     (make element gi: "a"
                           attributes:
                           `(("href" ,(string-append (notes-sys-id)
                                                     "#" (notes-frag-id))))
                           (literal "Notes")))))
	 (biblist
	  (and (get-bibliography-name)
               (make element gi: "li"
                     (make element gi: "a"
                           attributes:
                           `(("href" ,(string-append (bibliography-sys-id)
                                                     "#" (bibliography-frag-id))))
                           (literal "Bibliography")))))
	 (updateslist
	  (and (hashistory?) ;(get-updates)
               (make element gi: "li"
                     (make element gi: "a"
                           attributes:
                           `(("href" ,(string-append (updatelist-sys-id)
                                                     "#" (updatelist-frag-id))))
                           (literal "Changes")))))
	 (idindex
	  (and (hasidindex?)
               (make element gi: "li"
                     (make element gi: "a"
                           attributes:
                           `(("href" ,(string-append (idindex-sys-id)
                                                     "#" idindex-frag-id)))
                           (literal "ID index")))))
         (keywordindex
          (and (haskeywordindex?)
               (make element gi: "li"
                     (make element gi: "a"
                           attributes:
                           `(("href" ,(string-append (keywordindex-sys-id)
                                                     "#" keywordindex-frag-id)))
                           (literal "Keyword index")))))
	 (contentslist (if (or noteslist biblist updateslist
                               idindex keywordindex)
			   (sosofo-append (or noteslist (empty-sosofo))
					  (or biblist (empty-sosofo))
					  (or updateslist (empty-sosofo))
					  (or idindex (empty-sosofo))
                                          (or keywordindex (empty-sosofo)))
			   #f)))
    (if contentslist
	(make element gi: "li"
	      (make element gi: "a"
		    attributes: (list (list "href" (backmatter-sys-id)))
		    (literal "Backmatter"))
	      (make element gi: "ul"
		    contentslist))
	(empty-sosofo))))

(define (backmatter-sys-id)
  (if (chunking?)
      (html-file uniq: "backmatter")
      (html-file target_nd: (document-element))))
(define (notes-sys-id)
  (if (chunking?)
      (html-file uniq: "notes")
      (html-file target_nd: (document-element))
      ))
(define (bibliography-sys-id)
  (if (chunking?)
      (html-file uniq: "bibliography")
      (html-file target_nd: (document-element))))
(define (updatelist-sys-id)
  (if (chunking?)
      (html-file uniq: "updates")
      (html-file target_nd: (document-element))))

(define (notes-frag-id) "xref__NOTES")
(define (bibliography-frag-id) "xref__BIBLIOGRAPHY")
(define (updatelist-frag-id) "xref__UPDATELIST")

(mode section-reference
  (element backmatter
    (empty-sosofo)))

<routine>
<description>
Support notes as endnotes.  
<codebody>
(define (hasnotes?)
  (not (node-list-empty? (get-notelist))))

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
	(let ((body (make sequence
		      (make element gi: "h2"
			    (make element gi: "a"
				  attributes: (list (list "name"
							  (notes-frag-id)))
				  (literal "Notes")))
		      (make element gi: "dl"
			    (with-mode extract-notecontents
			      (process-node-list notelist))))))
	  (if (chunking?)
	      (html-document (literal "Notes")
			     body       
			     system-id: (notes-sys-id)
			     force-chunk?: #t
			     navbars?: #f)
	      body)))))

<routine>
<description>
Bibliography support.  The bibliography preprocessor (BibTeX) produces
an HTML DL element with entries referrable to by the bibkey, which is
the data of the CITATION element.
<codebody>
(define (hasbibliography?)
  (get-bibliography-name))

;; Return bibliography name or #f
(define (get-bibliography-name)
  (let ((bm (getdocbody 'backmatter)))
    (and bm
	 (attribute-string (normalize "bibliography") bm))))

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
  (let* ((bibcontents (and (hasbibliography?)
			   (read-entity (string-append (root-file-name)
						       ".htmlbib.bbl")))))
    (if bibcontents
	(let ((body (make sequence
		      (make element gi: "h2"
			    (make element gi: "a"
				  attributes: (list (list "name"
							  (bibliography-frag-id)))
				  (literal "Bibliography")))
		      (make fi data: bibcontents))))
	  (if (chunking?)
	      (html-document (literal "Bibliography")
			     body       
			     system-id: (bibliography-sys-id)
			     force-chunk?: #t
			     navbars?: #f)
	      body))
	(empty-sosofo))))

<routine>
<description>
Process the history element in the docbody.  Present it in reverse order
(ie, newest first), including in the distribution and change elements any
update elements which refer to them.
<codebody>
(define (hashistory?)
  (or (getdocinfo 'history)
      (get-updates)))

(define (make-updatelist)
  (if (hashistory?)
      (let ((body (make sequence
		    (make element gi: "h2"
			  (make element gi: "a"
				attributes: (list (list "name"
							(updatelist-frag-id)))
				(literal "Change history")))
		    (with-mode extract-updatelist
		      (process-node-list (getdocinfo 'history))))))
	(if (chunking?)
	    (html-document (literal "Change history")
			   body
			   system-id: (updatelist-sys-id)
			   force-chunk?: #t
			   navbars?: #f)
	    body))
      (empty-sosofo)))

;(define (make-updatelist)
;  (let ((updatelist (get-updates)))
;    (if (node-list-empty? updatelist)
;	(empty-sosofo)
;	(html-document (literal "Change history")
;		       (make sequence
;			 (make element gi: "h1"
;			       (literal "Change history"))
;			 (make element gi: "dl"
;			       (with-mode extract-updatelist
;				 (process-node-list updatelist))))
;		       system-id: (updatelist-sys-id)))))

;; Instead of 
;;
;;   (process-node-list
;;    (element-with-id (attribute-string (normalize "author"))))
;;
;; I tried using
;;
;;   (process-node-list (referent (attribute (normalize "author")
;;                                           (current-node))))
;;
;; but that didn't work, and (referent) always seemed to return an empty
;; node list.  I scoured through the property set, and referent seems to
;; have the value `node', so I don't understand what's wrong.  It's likely
;; related to the fact that if the "author" attribute had IDREFS declared
;; value, then this would have to return a list -- perhaps the return
;; value of referent has subnodes, or perhaps I'm just not understanding
;; property sets properly.  Note that the procedures used here are not
;; implemented as primitives in Jade, and have to be inserted: 
;; (node-list-property) from clause 10.2.3, (attribute) and (referent)
;; from clause 10.2.5.

(mode extract-updatelist
  (element history
      (node-list-reduce (children (current-node))
			(lambda (last el)
			  (sosofo-append (process-node-list el)
					 last))
			(empty-sosofo)))
  (element version
    (make sequence
      (make element gi: "h2"
	    (literal "Version " (attribute-string (normalize "number"))))
      (make element gi: "p"
	    (process-node-list (element-with-id
				(attribute-string (normalize "author"))))
	    (literal ", "
		     (format-date (attribute-string (normalize "date")))))
      (process-children)
      (collect-updates (literal "Changes in this version"))))
  (element distribution
    (collect-updates (literal "Distribution "
			      (attribute-string (normalize "string")))))
  (element change
    (collect-updates (literal
		      "Change "
		      (format-date (attribute-string (normalize "date"))))))
  (element update
    (make sequence
      (make element gi: "dt"
	    (let ((linktarget (ancestor-member (current-node)
					       (target-element-list))))
	      (make element gi: "a"
		    attributes: (list (list "href"
					    (href-to linktarget)))
		    (with-mode section-reference
		      (process-node-list linktarget)))))
      (make element gi: "dd"
	    (process-children)))
    )
  )

(element update				; ignore in default mode
  (empty-sosofo))

;; collect-updates can be called only within distribution or change
;; FO constructor.
(define (collect-updates title)
  (let* ((allupdates (get-updates))
	 (myvid (attribute-string (normalize "versionid")))
	 (selupdates (and myvid
			  allupdates
			  (node-list-or-false
			   (select-elements
			    allupdates
			    (list (normalize "update")
				  (list (normalize "versionid")
					myvid)))))))
    (make sequence
      (make element gi: "h3"
	    title)
      (make element gi: "p"
	    (process-node-list (element-with-id
				(attribute-string (normalize "author"))))
	    (literal ", "
		     (format-date (attribute-string (normalize "date")))))
      (process-children)
      (if selupdates
	  (make element gi: "dl"
		(process-node-list selupdates))
	  (empty-sosofo)))))

<routine>
<description>
Linking support.  Create a page listing all the exported IDs in the document,
so that document authors can find them in one place.
Note that we use <funcname>idindex-element-list</funcname> rather than 
<funcname>target-element-list</funcname>, since the former has had
removed from it elements which are expensive to work with.
<codebody>
(define (hasidindex?)
  (not suppress-idindex))
(define (idindex-sys-id)
  (if (chunking?)
      (html-file uniq: "idindex")
      (html-file target_nd: (document-element))))
(define idindex-frag-id "xref__IDINDEX")

;; The all-els variable in this function is bound to (select-by-class
;; (descendants (getdocbody)) 'element), and the resulting node-list
;; is processed by handlers which check whether (current-node) is a
;; member of (idindex-element-list) .  It's very important that this
;; is _not_ optimised to (node-list-filter-by-gi
;; ... (idindex-element-list)).  You'd think that would be faster, but
;; it seems to slow this function down by at least one order of
;; magnitude (306s runtime in this version, and 759s in the old).
;;
;; I don't understand this -- what can be happening in there?  I
;; thought it might be due to taking a long time to find routine
;; elements cross-document, so had idindex-element-list consist of
;; everything in target-element-list except "routine", but that didn't
;; make any difference, so now the two are identical.  Tracing the
;; output (by putting (debug) statements in the handlers in
;; make-idindex-mode) suggests that every element in the idindex is
;; taking a couple of seconds or so to process.  Bizarre!  Further
;; experiments suggest that it's not some weird late-binding issue, as
;; exactly the same seems to happen if you process the all-els
;; node-list twice in succession with trivially different modes.  It
;; does, however, appear to be linked to the size of the element in
;; question, with elements which have more element content taking
;; longer to process, even though I'd have thought that the
;; node-list-filter-by-gi step would avoid this.
;;
(define (make-idindex)
  (if suppress-idindex
      (empty-sosofo)
      (let ((body (let (;(all-els (node-list-filter-by-gi
			;	  (select-by-class (descendants (getdocbody))
			;			   'element)
			;	  (idindex-element-list)))
			(all-els (select-by-class (descendants (getdocbody))
						  'element)))
		    (make sequence
		      (make element gi: "h2"
			    (make element gi: "a"
				  attributes: `(("name" ,idindex-frag-id))
				  (literal "ID Index")))
		      (make element gi: "p"
			    (literal "Index of IDs in this document. "
				     "Exported IDs indicated ")
			    (make element gi: "strong"
				  (literal "like this.")))
		      (with-mode make-idindex-mode
			(process-node-list all-els))))))
	(if (chunking?)
	    (html-document (literal "ID Index")
			   body
			   system-id: (idindex-sys-id)
			   force-chunk?: #t
			   navbars?: #f)
	    body))))

(mode make-idindex-mode
  (default
    (let ((id (attribute-string (normalize "id") (current-node))))
      (if (and (member (gi (current-node)) (idindex-element-list))
	       id)
	  (let* ((export (attribute-string (normalize "export")
					   (current-node)))
		 (target (element-with-id id)))
	    (make element gi: "p"
		  (make sequence
		    (make element gi: (if export "strong" "em")
			  (literal "<" (gi target) " id=")
			  (make element gi: "a"
				attributes:
				`(("href" ,(href-to target)))
				(literal id)))
		    (literal ">")
		    (with-mode section-reference
			       (process-node-list target))
		    )))
	  (empty-sosofo))))
  (element sect
    (let ((id (attribute-string (normalize "id") (current-node))))
      (if (and (member (gi (current-node)) (idindex-element-list))
	       id)
	  (let* ((export (attribute-string (normalize "export")
					   (current-node)))
		 ;(format (if export "strong" "em"))
		 (target (element-with-id id)))
	    (make sequence
	      (make empty-element gi: "hr")
	      (make element gi: "h3"
		    (make sequence
		      (make element gi: (if export "strong" "em")
			    (literal "<" "sect id=")
			    (make element gi: "a"
				  attributes: `(("href" ,(href-to target))
						("name" ,(string-append
							  idindex-frag-id
							  "_" id)))
				  (literal id)))
		      (literal ">")
		      (with-mode section-reference
				 (process-node-list target))
		      ))))
	  (empty-sosofo)))))

<routine>
<description>
Indexing support.
<codebody>
;; Extract all the explicit "index" elements, returning a node-list
(define (*get-all-index-elements*)
  (select-elements (select-by-class (descendants (getdocbody)) 'element)
                   (normalize "index")))
;; Extract all the sectioning elements which have an "indexkey" attribute,
;; returning a list of nodes
(define (*get-all-indexed-sections*)
  (let ((allsects (node-list-filter-by-gi (select-by-class
                                           (descendants (getdocbody))
                                           'element)
                                          (section-element-list))))
    (node-list-reduce allsects
                      (lambda (result n)
                        (if (attribute-string "indexkey" n)
                            (cons n result)
                            result))
                      '())))
(define (haskeywordindex?)
  (not (or suppress-keywordindex
           (and (node-list-empty? (*get-all-index-elements*))
                (null? (*get-all-indexed-sections*))))))
(define (keywordindex-sys-id)
  (if (chunking?)
      (html-file uniq: "keywordindex")
      (html-file target_nd: (document-element))))
(define keywordindex-frag-id "xref__KWDIDX")

;; Evaluates to a sosofo comprising the keyword index, either as a separate
;; page, or as the index body, to go inline.
(define (make-keywordindex)
  (if suppress-keywordindex
      (empty-sosofo)
      (let ((body (make sequence
                    (make element gi: "h2"
                          (make element gi: "a"
                                attributes: `(("name" ,keywordindex-frag-id))
                                (literal "Keyword index")))
                    (make-keywordindex-sosofo))))
        (if (chunking?)
            (html-document (literal "Keyword index")
                           body
                           system-id: (keywordindex-sys-id)
                           force-chunk?: #t
                           navbars?: #f)
            body))))

;; Given a key in "topic!subtopic!detail" form and a node, return the 
;; index key which corresponds to this.  This is an improper list
;; consisting of the components of the key split at '!', terminated by
;; the all-element-number, which keeps the sort stable, and avoids
;; duplicate keys
(define (*index-string-to-key* key nd)
  (let loop ((l (tokenise-string
                 key
                 boundary-char?: (lambda (c)
                                   (char=? c #\!)))))
    (cond ((<= (length l) 0)
           (error "Impossible length in *reduce-one-index-entry*"))
          ((= (length l) 1)
           (cons (car l) (all-element-number nd)))
          (else
           (cons (car l) (loop (cdr l)))))))

;; *reduce-one-index-entry*: node-list-reduce procedure.
;; Make a list of index entry pairs.  Each pair has (key . '(ref-sosofo))
(define (*reduce-one-index-entry* result n)
  (let ((key (trim-data n)))
    (if (= (string-length key) 0)       ; user error, but don't fail
        result
        (cons (cons
               ;; index key
               (*index-string-to-key* key n)
               ;; reference -- an "a" element
               (list
                (if (attribute-string "seealso" n)
                    (literal (string-append "See also: "
                                            (attribute-string "seealso" n)))
                    (make element gi: "a"
                          attributes: `(("href" ,(href-to n)))
                          (make-section-reference
                           target: (ancestor-member
                                    n
                                    (section-element-list)))))))
              result))))


;; The same, but from a list of sub*section nodes which have an
;; "indexkey" attribute.
(define (*index-entry-from-section* sectionlist)
  (if (null? sectionlist)
      '()
      (cons (let ((nd (car sectionlist)))
              (cons
               ;; index key
               (*index-string-to-key* (attribute-string "indexkey" nd) nd)
               ;; reference -- an "a" element
               (list (make element gi: "a"
                           attributes: `(("href" ,(href-to nd)))
                           (make-section-reference target: nd)))))
            (*index-entry-from-section* (cdr sectionlist)))))

;; Test ordering of two index lists.
;; Arguments are lists where the first argument is an improper list consisting
;; of strings terminated by a single number (as generated by
;; *reduce-one-index-entry*).  Return #t if the first should be regarded as
;; "less than or equal" the second.
(define (*idx<=?* i1 i2)
  (let loop ((k1 (car i1))
             (k2 (car i2)))
    (cond ((and (number? k1) (number? k2))
           (<= k1 k2))
          ((number? k1)
           #t)
          ((number? k2)
           #f)
          ((string=? (car k1) (car k2))
           (loop (cdr k1) (cdr k2)))
          (else
           (string-ci<=? (car k1) (car k2))))))

;; Return true if the two index entries have different text, and false if
;; they represent the same test.  The true value encodes how they are
;; different, returning +1/-1/0 according as the second has more/fewer/same
;; number of levels (this last isn't currently used anywhere).
(define (*idx-different-text* i1 i2)
  (let loop ((k1 (car i1))
             (k2 (car i2)))
    (cond ((and (number? k1) (number? k2))
           #f)
          ((and (pair? k1) (pair? k2))
           (if (string=? (car k1) (car k2))
               (loop (cdr k1) (cdr k2))
               0))
          (else
           (if (pair? k1)               ; then k2 is a number
               -1
               +1)))))

;; Collapses an input index list (a list of (key . '(sosofo)) pairs) by,
;; at present, simply appending the (list of) sosofos of those entries
;; which have string-identical keys (according to *idx-different-text*).
(define (*collapse-index* full-list)
  (if (null? full-list)
      '()                               ; an error, probably
      (let ((first (car full-list))
            (rest (cdr full-list)))
        (cond ((null? rest)             ; finished
               (list first))
              ((*idx-different-text* first (car rest))
               (cons first
                     (*collapse-index* rest)))
              (else                     ; first has the same index text as next
               (let ((next (car rest))
                     (rrest (cdr rest)))
                 (*collapse-index* (cons (cons (car first)
                                               (append (cdr first)
                                                       (cdr next)))
                                         rrest))))))))

;; Evaluates to a sosofo comprising the formatted index entries
(define (make-keywordindex-sosofo)
  (let ((indexents (*get-all-index-elements*)))
    (if (node-list-empty? indexents)
        (empty-sosofo)
        (apply sosofo-append
               (map (lambda (idxpair)
                      (make element gi: "p"
                            (literal
                             (let strs-to-str ((strs (car idxpair)))
                               (if (number? (cdr strs))
                                   (car strs)
                                   (string-append
                                    (car strs)
                                    "/"
                                    (strs-to-str (cdr strs))))))
                            (literal ": ")
                            (make element gi: "ul"
                                  (apply sosofo-append
                                         (map (lambda (s)
                                                (make element gi: "li"
                                                      (make element gi: "em"
                                                            s)))
                                              (cdr idxpair))))))
                    (*collapse-index*
                     (sort-list (append
                                 ;; list of sections with indexkey attributes
                                 (*index-entry-from-section*
                                  (*get-all-indexed-sections*))
                                 ;; all the "index" elements
                                 (node-list-reduce indexents
                                                   *reduce-one-index-entry*
                                                   '()))
                                *idx<=?*)))))))


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

<routine>
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

;; Read in the parameter file
&commonparams.dsl
&params.dsl;

(root
    (make sequence
      (make fi data: (string-append (root-file-name) ":"))
      (get-bibliography)))

(define (get-bibliography)
  (let* ((kids (select-by-class (descendants (getdocbody)) 'element))
	 (citations (select-elements kids (normalize "citation")))
	 ;(bibelement (select-elements kids (normalize "bibliography")))
	 (bibname (get-bibliography-name))
	 )
    (if bibname
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
		  ;;(if (node-list-empty? bibelement)
		  ;;  (error "Citations but no BIBLIOGRAPHY in document")
		  ;;  (process-node-list bibelement))
	      ))
	(error "No backmatter in document, or bibliography not specified"))))

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
