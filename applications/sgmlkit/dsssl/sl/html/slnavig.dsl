;; This module supports navigation.

;; List of element types which should be broken into chunks.
;; Because of the way that section-footer-navigation finds its
;; subsections, I think there should be no `missing levels' in the set
;; of elements.  Ie, ("sect" "subsubsect") would be bad, since
;; "subsect" is missing.
;;
;; There are two constraints on the elements in this list.  (1) no
;; elements appear which are in the documentsummary DTD but not in the
;; General DTD, since (main-html-base) relies on this to be able to
;; generate the same HTML file name in both cases.  It doesn't matter
;; if there are elements here which don't appear in the summary DTD,
;; since elements with those names will necessarily never be found
;; when processing an instance of the summary DTD.  (2) the list
;; must be a subset of the return value of (section-element-list).
(define (chunk-element-list)
  (list (normalize "sect")
	(normalize "subsect")
	(normalize "appendices")
	(normalize "routinelist")
	(normalize "codecollection")
	))

(define (whereami str #!optional (gitype "P") (nd (current-node)))
  (make element gi: gitype
	  (literal (string-append
		    str ": "
		    (gi nd)))))


;; I don't know what this `skipping' stuff is, but I've changed the
;; list to just "sect" rather than "sect1"
;; (I _think_ the `skipping' is that if a section starts with a
;; subsection, then the two are put on the same page, but you'd have
;; to burrow through the DocBook stylesheet to confirm that).
(define (chunk-skip-first-element-list)
  (list (normalize "sect")))

(define (subset testlist memberlist)
  ;; Returns #t if all the elements of testlist are also elements of memberlist
  (let loop ((l testlist))
    (if (null? l)
	#t
	(if (not (member (car l) memberlist))
	    #f
	    (loop (cdr l))))))

(define (nodelist-to-gilist nodelist) 
  (let loop ((nl nodelist) (gilist '()))
    (if (node-list-empty? nl)
	gilist
	(loop (node-list-rest nl) (append gilist (list (gi (node-list-first nl))))))))

(define (is-first-element nd)
  (equal? (child-number nd) 1))

;;; make trivial version of (combined-chunk?)
;(define (combined-chunk? #!optional (nd (current-node)))
;  #f)
;(define (combined-chunk? #!optional (nd (current-node)))
;  (or
;   ;; if it's the first skipped chunk in a chunk
;   (and (not (node-list-empty? nd))
;	(member (gi nd) (chunk-element-list))
;	(is-first-element nd)
;	(member (gi nd) (chunk-skip-first-element-list)))
;   ;; of if it's a chunk in a partintro
;   (and (member (gi nd) (chunk-element-list))
;	(has-ancestor-member? nd (list (normalize "partintro"))))))

;; Return #t if the given node is a chunk, taking account of whether
;; chunking has been turned off (that is, it's not enough for the node
;; to have a GI which is in (chunk-element-list)).
(define (chunk? #!optional (nd (current-node)))
  (if (or nochunks stream-output)
      #f ;; (node-list=? nd (document-element))
      (if (member (gi nd) (chunk-element-list))
	  #t ;; (if (combined-chunk? nd) #f #t)
	  #f)))

;; Return a string which describes the path to the given node through
;; nodes which are members of (chunk-element-list).  Returns an empty
;; string if the (chunk-level-parent) of the given node is empty.
;; Note that (chunk-level-parent nd) returns nd if nd is a member of
;; (chunk-element-list). 
(define (chunk-path nd)
  (let loop ((this-node (chunk-level-parent nd))
	     (path-string ""))
    (if (node-list-empty? this-node)
	path-string
	(loop (chunk-level-parent (parent this-node))
	      (string-append (gi this-node)
			     (number->string (child-number this-node))
			     path-string)))))

;; (html-file) substantially simplified by removing all the PI stuff
;; present in the DocBook stylesheets (not least because I couldn't
;; really work out what it was for - some mechanism for forcing the
;; filename?)  

;; Return a string containing the name of the file which will hold the
;; given node.  Since this must work both for the general DTD and the
;; documentsummary DTD, we can't use (all-element-number), and can use
;; only elements which exist within both DTDs.  Assume that all the
;; elements in (chunk-element-list) appear in both DTDs.
(define (main-html-base nd)
  (let* ((node-name-suffix (chunk-path nd))
	 (idbase (if (and %use-id-as-filename%
			  (attribute-string (normalize "id") nd))
		     (case-fold-down (attribute-string (normalize "id") nd))
		     #f)))
    (if idbase
	(string-append (root-file-name nd) idbase)
	(string-append (root-file-name nd)
		       "-"
		       (case-fold-down node-name-suffix)))))

;; Returns the filename of the html file that contains elemnode
;;
(define (html-file #!optional (input_nd (current-node)))
  (let* ((nd (chunk-parent input_nd))
	 (base (cond ((member (gi nd) (section-element-list))
		      (main-html-base nd))
		     ((node-list-empty? nd)
				; if the node-list nd is
				; empty, then this is because
				; chunk-parent couldn't find a
				; parent chunk.  This means
				; either that we're not
				; chunking, or else that this
				; is the root chunk.
		      (root-file-name input_nd)
				; give input_nd as argument - this is
				; a singleton-node-list (required
				; argument for document-element), but
				; chunk-parent produces a node-list (mmm?)
		      )
		     ;; Following gives the same behaviour.  More rational?
		     ;((node-list=? input_nd (document-element))
		     ; (root-file-name))
		     (else "xxx1"))))
    (string-append base %html-ext%)))

; Returns the filename to be used for the root HTML file, based on
; document type and DOCNUMBER if present (which need not be the case for
; all document types). Another way to set this might be through a
; processing-instruction or (less good) an entity, but I can't work
; out how to get pi content!
(define (root-file-name #!optional (nd (current-node)))
  (let* ((dn (getdocinfo 'docnumber nd))
	 (docelemtype (if dn
			  (if (attribute-string (normalize "documenttype") dn)
			      (attribute-string (normalize "documenttype") dn)
			      (error "DOCNUMBER has no DOCUMENTTYPE"))
			  (gi (document-element))))
	 (docref (if dn
		     (if (attribute-string "UNASSIGNED" dn)
			 "unassigned"	; is there a better alternative?
			 (trim-data dn))
		     (trim-data (getdocinfo 'docdate nd))
		 )))
    (string-append docelemtype ;(gi (document-element))
		   (if docref (string-append "-" docref) ""))))

;; Return an id for the element, either the element's ID if it has one, 
;; or one obtained from (generate-anchor).
;; Don't want this any more (partly because I've discarded (generate-anchor)
;; However, I might want to resurrect the more complicated version below, 
;; when and if I want to refer to titles (which don't have IDs in the General
;; DTD.
;(define (element-id #!optional (nd (current-node)))
;    (if (attribute-string (normalize "id") nd)
;	(attribute-string (normalize "id") nd)
;	(generate-anchor nd)))
;(define (element-id #!optional (nd (current-node)))
;  ;; IDs of TITLEs are the IDs of the PARENTs
;  (let ((elem (if (equal? (gi nd) (normalize "title"))
;		  (parent nd)
;		  nd)))
;    (if (attribute-string (normalize "id") elem)
;	(attribute-string (normalize "id") elem)
;	(generate-anchor elem))))

;; Return the node-list for the element whose chunk nd is in, or an
;; empty node list if there is none such (which might happen if
;; chunking is turned off).
(define (chunk-parent #!optional (nd (current-node)))
  (let loop ((p (chunk-level-parent nd)))
    (if (or (node-list-empty? p) (chunk? p))
	p
	(chunk-level-parent (parent p)))))

;; Return (a node-list containing) the nearest ancestor which is a
;; member of (chunk-element-list).  The difference between this and
;; (chunk-parent) is that (chunk-parent) tests whether the node is
;; actually chunked (ie, it also uses (chunk?)), whereas this one just
;; tests for membership of (chunk-element-list).
(define (chunk-level-parent #!optional (nd (current-node)))
  (ancestor-member nd (chunk-element-list)))

;; Return the children of the current chunk, or an empty node-list if
;; there are none.
(define (chunk-children #!optional (nd (current-node)))
  (node-list-filter-by-gi (children nd) (chunk-element-list)))

; ----------------------------------------------------------------------
; (header-navigation) substantially simplified!
(define (header-navigation nd)
  (make sequence
    ($html-body-start$)
    (cond ((node-list=? nd (document-element))
	   (root-header-navigation nd))
	  ((member (gi nd) (section-element-list))
	   (section-header-navigation nd))
	  (else (empty-sosofo)))
    ($html-body-content-start$)))

(define (footer-navigation nd)
  (make sequence
    ($html-body-content-end$)
    ;(whereami "footer-navigation" "P" nd)
    (cond ((node-list=? nd (document-element))
	   (root-footer-navigation nd))
	  ((member (gi nd) (section-element-list))
	   (section-footer-navigation nd))
	  (else (empty-sosofo)))
    (nav-footer nd)
    ($html-body-end$)))

(define (root-header-navigation elemnode)
  (empty-sosofo))

;; This is like (ancestors), except that we list only ancestors which
;; are present in (chunk-element-list) or DOCBODY
(define (chunk-ancestors nl)
  (node-list-filter-by-gi (ancestors nl) (append (chunk-element-list)
						 (list (normalize "docbody")))))

(define (section-header-navigation elemnode)
  (let ((anc (chunk-ancestors elemnode)))
    (make element gi: "TABLE" attributes: %nav-header-table-attr%
	  (make sequence
	    (make element gi: "TR"
		  (make element gi: "TD" attributes: '(("ALIGN" "LEFT"))
			(make sequence
			  (node-list-reduce
			   anc ; (node-list-reduce) 10.2.2
			   (lambda (curr el)
			     (sosofo-append
			      curr
			      (make sequence
				(make element gi: "A"
				      attributes: (list (list "HREF"
							      (href-to el)))
				      (with-mode section-reference
					(process-node-list el)))
				(literal " / "))))
			   (empty-sosofo))
			  (with-mode section-reference
			    (process-node-list (current-node))))))
	    (make element gi: "TR"
		  (make element gi: "TD" attributes: '(("ALIGN" "RIGHT"))
			(make sequence
			  (if (nav-home? elemnode)
			      (make sequence
				(nav-home-link elemnode)
				(literal " / "))
			      (empty-sosofo))
			  (if (nav-up? elemnode)
			      (nav-up-link elemnode)
			      (make element gi: "EM"
				    (literal "Up")))
			  (literal " / ")
			  (if (nav-prev? elemnode)
			      (nav-prev-link elemnode)
			      (make element gi: "EM"
				    (literal "Prev")))
			  (literal " / ")
			  (if (nav-next? elemnode)
			      (nav-next-link elemnode)
			      (make element gi: "EM"
				    (literal "Next"))))))))))

(define (make-subcontents nl)
  (if (node-list-empty? nl)
      (empty-sosofo)
      (make sequence
	;(make empty-element gi: "HR")
	(make element gi: "H3"
	      (literal "Contents"))
	(make element gi: "UL"
	      (node-list-reduce
	       nl
	       (lambda (last el)
		 (sosofo-append last
				(make element gi: "LI"
				      (make element gi: "A"
					    attributes: (list (list "HREF"
								    (href-to el)))
					    (with-mode section-reference
					      (process-node-list el))))))
	       (empty-sosofo))))))


;; We're producing the footer for the root element (SUN or MUD, or
;; whatever).  This doesn't have any element of (chunk-element-list)
;; as a child, so we have to give it some help.
;;
;; If we're not chunking, then we don't want to produce a table of
;; contents.  This is the only place we need to worry about this --
;; the section-footer-navigation functions will be invoked only if
;; we're chunking.
(define (root-footer-navigation elemnode)
  (if (or nochunks stream-output)
      (empty-sosofo)
      (let ((subsects (chunk-children (select-elements (children elemnode)
						       (normalize "docbody")))))
	(make-subcontents subsects))))

;(define (section-footer-navigation elemnode)
;  (let ((subsects (chunk-children elemnode)))
;    (make-subcontents subsects)))
(define (section-footer-navigation elemnode)
  (empty-sosofo))

(define (nav-footer elemnode)
  (let* ((authors (children (getdocinfo 'authorlist)))
	 (rel (document-release-info))
	 (subsects (chunk-children elemnode))
	 ; If this is the root element, and we _are_ chunking, then
	 ; return a list of the children of the root element.
	 ; Otherwise, return an empty-node-list
	 ; (this is because (chunk-children) doesn't find sections from within
	 ; the root element, and so needs a special case).
	 (root-subsects (if (and (node-list=? elemnode
					      (document-element))
				 (not (or nochunks stream-output)))
			    (chunk-children (select-elements
					     (children elemnode)
					     (normalize "docbody")))
			    (empty-node-list)))
	 ; nextchunk is set to a pair consisting of a node-list with
	 ; the `next' chunk to go to, and a legend for it.
	 (nextchunk (cond ((not (node-list-empty? root-subsects))
			   (cons
			    "Begin" (node-list-first root-subsects)))
			  ((not (node-list-empty? subsects))
					; first of any children
			   (cons
			    "Down to" (node-list-first subsects)))
			  ((nav-next? elemnode)	; next at this level
			   (cons
			    "Next" (nav-next-element elemnode)))
			  ((not (node-list-empty?
				 (nav-next-element
				  (nav-up-element elemnode))))
			   (cons
			    "Next up" (nav-next-element
				       (nav-up-element elemnode))))
;                         Don't go UP - when there's no longer a `next
;                         up', then we've got to the end of the trail.
;			  ((nav-up? elemnode) ; up
;			   (cons
;			    "Up"
;			    (nav-up-element elemnode)))
			  (else (cons "None" (empty-node-list)))))
	 )
    (make sequence
      (make element gi: "TABLE" attributes: %nav-footer-table-attr%
	    (make sequence
	      (if (node-list-empty? subsects)
		  (empty-sosofo)
		  (make element gi: "TR"
			(make element gi: "TD"
			      (make-subcontents subsects))))
	      (make element gi: "TR"
		    (make element gi: "TD" attributes: '(("ALIGN"
							  "RIGHT"))
			;(whereami "nav-footer")
			;(literal (car nextchunk))
			(if (node-list-empty? (cdr nextchunk))
			    (literal "END")
			    (make sequence
			      (make element gi: "EM"
				    (literal (string-append (car nextchunk)
							    ": ")))
			      (make element gi: "A"
				    attributes: (list
						 (list "HREF"
						       (href-to (cdr nextchunk))))
				    (with-mode section-reference
				      (process-node-list (cdr nextchunk))))))
			))))
      (make element gi: "P" attributes: '(("ALIGN" "RIGHT"))
	    (make element gi: "EM"
		  (make sequence
		    (node-list-reduce authors
				      (lambda (result a)
					(sosofo-append
					 result
					 (make sequence
					   (process-node-list a)
					   (make empty-element gi: "BR"))))
				      (empty-sosofo))
		    (literal (format-date (car rel)))
		    ;(literal (append-strings rel "[" "==" "]"))
		    )))
      )))

;; Quickie function to collapse a list of strings into one
(define (append-strings sl begin-string link end-string)
  (let loop ((l sl)
	     (res begin-string))
    (if (null? l)
	res
	(loop (cdr l)
	      (string-append res (if (car l) (car l) "#f")
			     (if (null? (cdr l)) end-string link))))))


;(define (nav-footer elemnode)
;  (let* ((authors (getdocinfo 'authorlist))
;	 (hist (getdocinfo 'history))
;	 (date (if hist			; HISTORY element present
;		   (attribute-string "date"
;				     (select-elements (descendants hist)
;						      'release))
;		   (data (getdocinfo 'docdate))))) ; must be docdate instead
;    (make sequence
;      (make element gi: "TABLE" attributes: %nav-footer-table-attr%
;	    (make element gi: "TR"
;		  (make element gi: "TD" attributes: '(("ALIGN"
;							"RIGHT"))
;			(whereami "nav-footer")
;			(literal "Next location..."))))
;      (make element gi: "P" attributes: '(("ALIGN" "RIGHT"))
;	    (make element gi: "EM"
;		  (process-node-list authors)
;		  (literal "Date: " date))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation macros.  These navigate around the document, giving
;; parents, siblings, and so on

;; General navigation link, which nav-{up,home,next,prev}-link use
(define (nav-gen-link thisnode target title linktext)
  (if (node-list=? thisnode target)
      (literal (string-append "This node is " title))
      (make sequence
	(make element gi: "A"
	      attributes: (list (list "HREF" (href-to target))
				(list "TITLE" title))
	      linktext)
	;(literal (string-append " (" (href-to target) ")"))
	)))

;; Is there an Up definable?
(define (nav-up? elemnode)
  (let ((up (parent elemnode)))
    (if (or (node-list-empty? up)
	    (node-list=? up (document-element)))
	#f
	#t)))
(define (nav-up-element elemnode)
  (parent elemnode))

(define (nav-up-link elemnode)
  (let ((up (parent elemnode)))
    (nav-gen-link elemnode up "Up" (gentext-nav-up up))))

;; Returns a sosofo with the up link (or empty-sosofo if none)
;(define (nav-up-link elemnode)
;  (let ((up (parent elemnode)))
;    (if (or (node-list-empty? up)
;	    (node-list=? up (document-element)))
;	;(make entity-ref name: "nbsp")
;	;(empty-sosofo)
;	(literal "No up element")
;	(make element gi: "A"
;	      attributes: (list
;			   (list "HREF" (href-to up))
;			   (list "TITLE" "Up"))
;	      (gentext-nav-up up)))))

;; Is there a home link (say no, if we're currently the root element)
(define (nav-home? elemnode)
  (not (node-list=? elemnode (document-element))))

(define (nav-home elemnode)
  (document-element))

(define (nav-home-link elemnode)
  (let ((home (nav-home elemnode)))
    (nav-gen-link elemnode home "Home" (gentext-nav-home home))))

;;; make the home link (or empty-sosofo if none)
;(define (nav-home-link elemnode)
;  (let ((home (nav-home elemnode)))
;    (if (node-list=? elemnode home)
;	;(make entity-ref name: "nbsp")
;	;(empty-sosofo)
;	(literal "No home element")
;	(make element gi: "A"
;	      attributes: (list
;			   (list "HREF" (href-to home))
;			   (list "TITLE" "Home"))
;	      (gentext-nav-home home)))))

;; ifollow-by-gi and ipreced-by-gi return the next sibling node which
;; is in the list gilist, which need not be the same as the next
;; sibling (though it probably will be, in the present case)
(define (ifollow-by-gi nd gilist)
  (let loop ((next (ifollow nd)))
    (if (node-list-empty? next)
	(empty-node-list)
	(if (member (gi next) gilist)
	    next
	    (loop (ifollow next))))))

(define (ipreced-by-gi nd gilist)
  (let loop ((prev (ipreced nd)))
    (if (node-list-empty? prev)
	(empty-node-list)
	(if (member (gi prev) gilist)
	    prev
	    (loop (ipreced prev))))))

;; Is there a next link?
(define (nav-next-element elemnode)
  (ifollow-by-gi elemnode (chunk-element-list)))

(define (nav-next? elemnode)
  (not (node-list-empty? (nav-next-element elemnode))))

(define (nav-next-link elemnode)
  (let ((next (nav-next-element elemnode)))
    (nav-gen-link elemnode next "Next" (gentext-nav-next next))))

;;; return sosofo for 'next' link (or empty-sosofo if none)
;(define (nav-next-link elemnode)
;  (let ((next (ifollow-by-gi elemnode (chunk-element-list))))
;    (if (node-list-empty? next)
;	;(empty-sosofo)
;	(literal "No next element")
;	(make element gi: "A"
;	      attributes: (list (list "HREF" (href-to next))
;				(list "TITLE" "Next"))
;	      (gentext-nav-next next)))))

;; Is there a prev link?
(define (nav-prev-element elemnode)
  (ipreced-by-gi elemnode (chunk-element-list)))
(define (nav-prev? elemnode)
  (not (node-list-empty? (nav-prev-element elemnode))))

(define (nav-prev-link elemnode)
  (let ((prev (nav-prev-element elemnode)))
    (nav-gen-link elemnode prev "Prev" (gentext-nav-prev prev))))

;(define (nav-prev-link elemnode)
;  (let ((prev (ipreced-by-gi elemnode (chunk-element-list))))
;    (if (node-list-empty? prev)
;	;(empty-sosofo)
;	(literal "No previous element")
;	(make element gi: "A"
;	      attributes: (list (list "HREF" (href-to prev))
;				(list "TITLE" "Prev"))
;	      (gentext-nav-prev prev)))))



;(define (section-header-navigation elemnode)
;  (let ((prev (prev-chunk-element elemnode))
;	(next (next-chunk-element elemnode)))
;    (default-header-navigation elemnode prev next)))

;(define (section-footer-navigation elemnode)
;  (let ((prev (prev-chunk-element elemnode))
;	(next (next-chunk-element elemnode)))
;    (default-footer-navigation elemnode prev next)))

;;; ----------------------------------------------------------------------

;(define (default-header-navigation elemnode prev next)
;  (let* ((r1? (nav-banner? elemnode))
;	 (r1-sosofo (make element gi: "TR"
;			  (make element gi: "TH"
;				attributes: (list
;					     (list "COLSPAN" "3")
;					     (list "ALIGN" "center"))
;				(nav-banner elemnode))))
;	 (r2? (or (not (node-list-empty? prev))
;		  (not (node-list-empty? next))
;		  (nav-context? elemnode)))
;	 (r2-sosofo (make element gi: "TR"
;			  (make element gi: "TD"
;				attributes: (list
;					     (list "WIDTH" "10%")
;					     (list "ALIGN" "left")
;					     (list "VALIGN" "bottom"))
;				(if (node-list-empty? prev)
;				    (make entity-ref name: "nbsp")
;				    (make element gi: "A"
;					  attributes: (list
;						       (list "HREF" 
;							     (href-to 
;							      prev)))
;					  (gentext-nav-prev prev))))
;			  (make element gi: "TD"
;				attributes: (list
;					     (list "WIDTH" "80%")
;					     (list "ALIGN" "center")
;					     (list "VALIGN" "bottom"))
;				(nav-context elemnode))
;			  (make element gi: "TD"
;				attributes: (list
;					     (list "WIDTH" "10%")
;					     (list "ALIGN" "right")
;					     (list "VALIGN" "bottom"))
;				(if (node-list-empty? next)
;				    (make entity-ref name: "nbsp")
;				    (make element gi: "A"
;					  attributes: (list
;						       (list "HREF" 
;							     (href-to
;							      next)))
;					  (gentext-nav-next next)))))))
;    (if (or r1? r2?)
;	(make element gi: "DIV"
;	      attributes: '(("CLASS" "NAVHEADER"))
;	  (make element gi: "TABLE"
;		attributes: (list
;			     (list "WIDTH" %gentext-nav-tblwidth%)
;			     (list "BORDER" "0")
;			     (list "CELLPADDING" "0")
;			     (list "CELLSPACING" "0"))
;		(if r1? r1-sosofo (empty-sosofo))
;		(if r2? r2-sosofo (empty-sosofo)))
;	  (make empty-element gi: "HR"
;		attributes: (list
;			     (list "ALIGN" "LEFT")
;			     (list "WIDTH" %gentext-nav-tblwidth%))))
;	(empty-sosofo))))

;(define (default-footer-navigation elemnode prev next prevsib nextsib)
;  (let ((r1? (or (not (node-list-empty? prev))
;		 (not (node-list-empty? next))
;		 (nav-home? elemnode)))
;	(r2? (or (not (node-list-empty? prev))
;		 (not (node-list-empty? next))
;		 (nav-up? elemnode)))

;	(r1-sosofo (make element gi: "TR"
;			 (make element gi: "TD"
;			       attributes: (list
;					    (list "WIDTH" "33%")
;					    (list "ALIGN" "left")
;					    (list "VALIGN" "top"))
;			       (if (node-list-empty? prev)
;				   (make entity-ref name: "nbsp")
;				   (make element gi: "A"
;					 attributes: (list
;						      (list "HREF" (href-to
;								    prev)))
;					 (gentext-nav-prev prev))))
;			 (make element gi: "TD"
;			       attributes: (list
;					    (list "WIDTH" "34%")
;					    (list "ALIGN" "center")
;					    (list "VALIGN" "top"))
;			       (nav-home-link elemnode))
;			 (make element gi: "TD"
;			       attributes: (list
;					    (list "WIDTH" "33%")
;					    (list "ALIGN" "right")
;					    (list "VALIGN" "top"))
;			       (if (node-list-empty? next)
;				   (make entity-ref name: "nbsp")
;				   (make element gi: "A"
;					 attributes: (list
;						      (list "HREF" (href-to
;								    next)))
;					 (gentext-nav-next next))))))
;	(r2-sosofo (make element gi: "TR"
;			 (make element gi: "TD"
;			       attributes: (list
;					    (list "WIDTH" "33%")
;					    (list "ALIGN" "left")
;					    (list "VALIGN" "top"))
;			       (if (node-list-empty? prev)
;				   (make entity-ref name: "nbsp")
;				   (element-title-sosofo prev)))
;			 (make element gi: "TD"
;			       attributes: (list
;					    (list "WIDTH" "34%")
;					    (list "ALIGN" "center")
;					    (list "VALIGN" "top"))
;			       (if (nav-up? elemnode)
;				   (nav-up elemnode)
;				   (make entity-ref name: "nbsp")))
;			 (make element gi: "TD"
;			       attributes: (list
;					    (list "WIDTH" "33%")
;					    (list "ALIGN" "right")
;					    (list "VALIGN" "top"))
;			       (if (node-list-empty? next)
;				   (make entity-ref name: "nbsp")
;				   (element-title-sosofo next))))))
;    (if (or r1? r2?)
;	(make element gi: "DIV"
;	      attributes: '(("CLASS" "NAVFOOTER"))
;	  (make empty-element gi: "HR"
;		attributes: (list
;			     (list "ALIGN" "LEFT") 
;			     (list "WIDTH" %gentext-nav-tblwidth%)))
;	  (make element gi: "TABLE"
;		attributes: (list
;			     (list "WIDTH" %gentext-nav-tblwidth%)
;			     (list "BORDER" "0")
;			     (list "CELLPADDING" "0")
;			     (list "CELLSPACING" "0"))
;		(if r1? r1-sosofo (empty-sosofo))
;		(if r2? r2-sosofo (empty-sosofo))))
;	(empty-sosofo))))

