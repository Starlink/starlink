<!--
Title:
  Starlink stylesheet - routine list

Author:
  Norman Gray, Glasgow (NG)

Revision history
  February 1999 (original version)

Copyright 1999, 2000, Council for the Central Laboratories of the Research Councils

$Id$
-->

<routine>
<description>Constructors for the ROUTINELIST element
<codebody>
(mode section-reference
  ;;(element routinelist
  ;;  (make-section-reference title: (literal "Routine list")))
  (element routinelist
    (literal "Routine list"))
  (element codecollection
    (make-section-reference
     set-prefix: (literal (number->string (child-number)) " ")
     title: (literal "CodeCollection")
     ))
  (element codegroup
    (with-mode routine-ref-get-reference
      (process-node-list (current-node))))
  (element routine
    (with-mode routine-ref-get-reference
      (process-node-list (current-node))))
  (element programcode
    (with-mode routine-ref-get-reference
      (process-node-list (current-node))))
  )


;; Routinelist is simple
(element routinelist
  (make sequence
    (process-matching-children 'p)
    (make element gi: "ul"
	  (process-matching-children 'codecollection))))

;; Supporting the codecollection chunking/sectioning isn't as easy as with
;; the other such elements, because it doesn't have any children in this
;; document.  We have to do it rather more by hand, therefore.
;; Don't yet support the INCLUDEONLY attribute.
(element codecollection
  (let ((docent (attribute-string (normalize "doc"))))
    (if docent
	(with-mode routine-ref
	  (process-codecollection docent))
	(error "codecollection: missing required doc attribute"))))

(mode html-contents
  (element codecollection
    (let ((docent (attribute-string (normalize "doc"))))
      (if docent
	  (make element gi: "a"
		attributes: `(("href" , (href-to (current-node))))
		(with-mode routine-ref-get-reference
		  (process-codecollection docent)))
	  (error "codecollection: missing required doc attribute")))))

(define (process-codecollection docent)
  (if docent
      (let ((de (document-element-from-entity docent)))
	(if (or (not de)
		(node-list-empty? de))
	    (error (string-append "Couldn't get document element from doc "
				  docent))
	    (make sequence
	      (make element gi: "li"
		    (make element gi: "a"
			  attributes: `(("href" , (href-to de)))
			  (with-mode routine-ref-get-reference
			    (process-node-list de))))
	      (process-programcode de (current-node)))))
      (error "No docent in process-codecollection")))

;; Process a list consisting of all the "author" elements in the
;; programcode document.  Split the author list into those with the
;; same ID as the first author, and those with a different ID: call
;; process-one-author on the first set, and call process-authorlist
;; again on the second.
(define (process-authorlist nl)
  (if (node-list-empty? nl)
      (empty-sosofo)
      (let* ((first-author (attribute-string (normalize "id")
					     (node-list-first nl)))
	     (alists (let loop ((anl nl)
				(thisaut (empty-node-list))
				(otherauts (empty-node-list)))
		       (if (node-list-empty? anl)
			   (cons thisaut otherauts)
			   (if (string=?
				first-author
				(attribute-string (normalize "id")
						  (node-list-first anl)))
			       (loop (node-list-rest anl)
				     (node-list thisaut
						(node-list-first anl))
				     otherauts)
			       (loop (node-list-rest anl)
				     thisaut
				     (node-list otherauts
						(node-list-first anl))))))))
	(make sequence
	  (process-one-author first-author (car alists))
	  (process-authorlist (cdr alists))))))

;; Process a single-author node-list.  Collect the relevant
;; information from the nodes in the list.  Use the first attribute
;; present, and don't check or warn if the information is
;; inconsistent.
;;
;; The processing here is less sophisticated than it could be, as
;; there's no real cross-referencing between individual author
;; elements and the aggregate information.
(define (process-one-author aut-id nl)
  (let ((autprops (let loop ((auts nl)
			     (name #f)
			     (aff #f)
			     (email #f)
			     (web #f))
		    (if (or (node-list-empty? auts)
			    (and name aff email web))
			(list name aff email web)
			(let ((aut (node-list-first auts)))
			  (loop (node-list-rest auts)
				(or name
				    (data aut))
				(or aff
				    (attribute-string (normalize "affiliation")
						      aut))
				(or email
				    (attribute-string (normalize "email")
						      aut))
				(or web
				    (attribute-string (normalize "webpage")
						      aut))))))))
    (make sequence
      (make element gi: "dt"
	    (make element gi: "a"
		  attributes: `(("name" , (string-append "AUTHOR_" aut-id)))
		  (literal (or (car autprops)
			       "Mystery programmer"))))
      (make element gi: "dd"
	    (make element gi: "ul"	;Assume we have at least one of these
		  (if (cadr autprops)
		      (make element gi: "li"
			    (literal "Affiliation: " (cadr autprops)))
		      (empty-sosofo))
		  (if (caddr autprops)
		      (make element gi: "li"
			    (make sequence
			      (literal "Email: ")
			      (make element gi: "code"
				    (literal (caddr autprops)))))
		      (empty-sosofo))
		  (if (cadddr autprops)
		      (make element gi: "li"
			    (make sequence
			      (literal "URL: ")
			      (make element gi: "code"
				    (make sequence
				      (literal "<")
				      (make element gi: "a"
					    attributes: `(("href" ,(cadddr autprops)))
					    (literal (cadddr autprops)))
				      (literal ">")))))
		      (empty-sosofo)))))))

;; The following are essentially equivalent to (process-node-list),
;; with an argument of a list of programcode, codegroup and routines.
;; However, they have an extra uplink parameter so that, when they are
;; called from process-codecollection, it can provide a link back to itself.
(define (process-programcode docelem #!optional (uplink #f))
  (let ((kids (children docelem)))
    (html-document (literal "Programcode document")
		   (make sequence
		     (process-node-list (select-elements kids 'docblock))
		     ;(process-matching-children 'docblock)
		     (make element gi: "ul"
			   ;(process-codegroup-list (select-elements kids 'codegroup) uplink)
			   ;(process-routine-list (select-elements kids 'routine) uplink)))
			   (process-node-list (select-elements kids 'codegroup))
			   (process-node-list (select-elements kids 'routine))
			   ))
		   force-chunk?: #t
		   system-id: (html-file target_nd: docelem)
		   uplink: uplink)))

(mode routine-ref

  ;; programcode element is dealt with `manually', in process-programcode

  (element docblock
    ;; docblock always has a title element, but this is suppressed in
    ;; this mode, because we want to treat it specially (inside the h1
    ;; element).  If there's _more_ than one element, then other
    ;; docblock elements are present, so should be put into a
    ;; description list.
    (make sequence
      (make element gi: "h1"
	    (with-mode routine-ref-get-reference
	      (process-node-list (current-node))))
      (let ((allauthors (if (string=? (gi (parent (current-node)))
				      (normalize "programcode"))
			    ;; Only prepare an authorlist for the top-level
			    ;; docblock, and not for the docblock elements in
			    ;; codegroups, for example.
			    (select-elements
			     (select-by-class
			      (descendants (parent (current-node)))
			      'element)
			     (normalize "author"))
			    (empty-node-list))))
	(if (or (> (node-list-length (children (current-node))) 1)
		(not (node-list-empty? allauthors)))
	    (make element gi: "dl"
		  (make sequence
		    (with-mode docblock
		      (process-children))
		    ;; process any authors
		    (if (node-list-empty? allauthors)
			(empty-sosofo)
			(make sequence
			  (make element gi: "dt"
				(make element gi: "strong"
				      (literal "Authors")))
			  (make element gi: "dd"
				(make element gi: "dl"
				      (process-authorlist allauthors)))))))
	    (empty-sosofo)))))

  (element codegroup
    (let* ((db (select-elements (children (current-node)) 'docblock))
	   (title (select-elements (children db) 'title)))
      (make sequence
	(make element gi: "li"
	      (make element gi: "a"
		    attributes: `(("href" ,(href-to (current-node))))
		    (make sequence
		      (literal "Code group: ")
		      (with-mode routine-ref-get-reference
			(process-node-list (current-node))))))
	(html-document (literal (data title))
		       (make sequence
			 (process-node-list db)
			 (make element gi: "ul"
			       (process-matching-children 'routine)))))))

  (element codereference
    (let ((ref-docelem (document-element-from-entity
			(attribute-string (normalize "doc")))))
      ;; possibly make this a link, in future
      (make sequence
	(make empty-element gi: "hr")
	(make element gi: "h2"
	      (literal "Code reference"))
	(make element gi: "p"
	      (make sequence
		(literal "Refers to ")
		(make element gi: "cite"
		      (with-mode routine-ref-get-reference
			(process-node-list ref-docelem)))))
	(process-children))))

  (element title
    (make element gi: "h1"
	  (process-children)))
  (element (codereference docblock title) ; discard, in this mode
    (empty-sosofo))			; (see mode routine-ref-get-reference)
  (element (codegroup docblock title)	; discard, in this mode
    (empty-sosofo))			; (see mode routine-ref-get-reference)
  (element authorlist
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Authors")))
      (make element gi: "dd"
	    (make element gi: "ul"
		  (process-children)))))
  (element author
    (let ((id (attribute-string (normalize "id"))))
      (make element gi: "li"
	    (make element gi: "a"
		  attributes: `(("href" , (href-to
					   (document-element)
					   force-frag: (string-append "#AUTHOR_" id))))
		  (process-children)))))

  (element authornote
    (make sequence
      (literal "[ ")
      (process-children)
      (literal " ]")))
  (element otherauthors
    (make element gi: "li"
	  (make element gi: "p"
		(literal "Other contributors")
		(make element gi: "ul"
		      (process-children)))))
  (element copyright
    (make sequence
      (make element gi: "h3"
	    (literal "Copyright"))
      (process-children)))
  (element history
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Change history")))
      (make element gi: "dd"
	    (make element gi: "dl"
		  (process-children)))))
  (element change
    (let ((auth-id (attribute-string (normalize "author")))
	  (date-str (attribute-string (normalize "date"))))
      (make sequence
	(make element gi: "dt"
	      (make element gi: "strong"
		    (literal (string-append (data (element-with-id auth-id))
					    ", "
					    (format-date date-str)))))
	(make element gi: "dd"
	      (process-children)))))
  (element routineprologue
    (let ((kids (nl-to-pairs (children (current-node)))))
      (apply sosofo-append
	     (map (lambda (thisgi)
		    (let ((gi-and-nd (assoc (normalize thisgi) kids)))
		      (if gi-and-nd	; there is an element with this gi
			  (if (string=? (normalize thisgi)
					(normalize "diytopic"))
			      ;; process multiple diytopics all together
			      (node-list-reduce
			       (children (current-node))
			       (lambda (result i)
				 (if (string=? (gi i) (normalize "diytopic"))
				     (sosofo-append result
						    (process-node-list i))
				     result))
			       (empty-sosofo))
			      ;; else just process this node
			      (process-node-list (cdr gi-and-nd)))
			  (empty-sosofo))))
		  %display-programcode-elements%))))

  (element routine
    (let* ((rp (select-elements (children (current-node)) 'routineprologue))
	   (rnm (and (not (node-list-empty? rp))
		     (select-elements (children rp) 'routinename)))
	   (purp (and (not (node-list-empty? rp))
		      (select-elements (children rp) 'purpose)))
	   (title (if (node-list-empty? rnm)
			 (literal "Anonymous routine")
			 (with-mode routine-ref-get-reference
			   (process-node-list rp)))))
      (make sequence
	(make element gi: "li"
	      (make sequence
		(make element gi: "a"
		      ;; make a link to the document we're about to generate
		      attributes: `(("href" ,(href-to (current-node))))
		      title)
		(if (node-list-empty? purp)
		    (empty-sosofo)
		    (literal (string-append " - " (data purp))))))
	(html-document title
		       (make sequence
			 (make element gi: "h1"
			       (make element gi: "a"
				     attributes: `(("name"
						    ,(href-to (current-node)
							      frag-only: #t)))
				     ;; We don't really need this, since
				     ;; the routine is in a chunk by
				     ;; itself, but since routine is in
				     ;; section-element-list, be
				     ;; consistent with the other
				     ;; elements in that.
				     (make sequence
				       title
				       (if (node-list-empty? purp)
					   (empty-sosofo)
					   (literal (string-append 
						     " - "
						     (data purp)))))))
			 (make element gi: "dl"
			       (process-node-list rp)))))))

  (element routinename
    (empty-sosofo))			; discard, in this mode.  See
					; mode routine-ref-get-reference  
  (element name
    (make element gi: "b"
	  (process-children)))
  (element othernames
    (let* ((names (children (current-node)))
	   (namelist (node-list-reduce
		      names
		      (lambda (res i)
			(if (string=? res "")
			    (data i)
			    (string-append res ", " (data i))))
		      "")))
      (literal (string-append " (also: " namelist ")"))))
  (element purpose
    (make element gi: "p"
	  attributes: '(("align" "center"))
	  (make sequence
	    (make element gi: "b"
		  (literal "Purpose: "))
	    (process-children))))
  (element description
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Description")))
      (make element gi: "dd"
	    (make element gi: "dl"	; hack text further in, to line up
		  (make element gi: "dd"
			(process-children))))))
  (element userkeywords
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Keywords")))
      (make element gi: "p"
	    (process-children))))
  (element softwarekeywords
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Software group")))
      (make element gi: "p"
	    (process-children))))
  (element returnvalue
    (let ((none-att (attribute-string (normalize "none")))
	  (type-att (attribute-string (normalize "type"))))
      (make sequence
	(make element gi: "dt"
	      (make element gi: "strong"
		    (literal "Return value")))
	(make element gi: "dd"
	      (if none-att
		  (literal "None") ;...and discard any data
		  (make sequence
		    (if type-att
			(make element gi: "p"
			      (literal (string-append "Type: " type-att)))
			(empty-sosofo))
		    (process-children)))))))
  (element argumentlist
    (let ((none-att (attribute-string (normalize "none"))))
      (make sequence
	(make element gi: "dt"
	      (make element gi: "strong"
		    (literal "Argument list")))
	(make element gi: "dd"
	      (if none-att
		  (make element gi: "p"
			(literal "None"))
		  (make element gi: "dl"
			(process-children)))))))
  (element parameterlist
    (let ((none-att (attribute-string (normalize "none"))))
      (make sequence
	(make element gi: "dt"
	      (make element gi: "strong"
		    (literal "Parameter list")))
	(make element gi: "dd"
	      (if none-att
		  (make element gi: "p"
			(literal "None"))
		  (make element gi: "dl"
			(process-children)))))))
  (element parameter
    (let* ((kids (children (current-node)))
	   (name (select-elements kids (normalize "name")))
	   (type (select-elements kids (normalize "type")))
	   (desc (select-elements kids (normalize "description")))
	   (opt-att (attribute-string (normalize "optional")))
	   (given-att (attribute-string (normalize "given")))
	   (returned-att (attribute-string (normalize "returned"))))
      (make sequence
	(make element gi: "dt"
	      (make element gi: "strong"
		    (process-node-list name)
		    (literal (string-append " = " (data type) " ("
					    (cond
					     ((and given-att returned-att)
					      "Given and Returned")
					     (given-att "Given")
					     (returned-att "Returned")
					     (else ;default is given
					      "Given"))
					    (if opt-att
						(string-append "), " opt-att)
						")")))))
	(make element gi: "dd"
	      (with-mode routine-ref-plain
		(process-node-list desc))))))
  (element usage
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Usage")))
      (make element gi: "dd"
	    (process-children))))
  (element moduletype
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Type of Module")))
      (make element gi: "dd"
	    (process-children))))
  (element invocation
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Invocation")))
      (make element gi: "dd"
	    (process-children))))
  (element implementationstatus
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Implementation Status")))
      (make element gi: "dd"
	    (process-children))))
  (element bugs
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Bugs")))
      (make element gi: "dd"
	    (process-children))))
  (element diytopic
    (let ((kids (children (current-node))))
      (make sequence
	(make element gi: "dt"
	      (make element gi: "strong"
		    (literal (data (node-list-first kids)))))
	(make element gi: "dd"
	      (process-node-list (node-list-rest kids))))))
  (element examplelist
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Examples")))
      (make element gi: "dd"
	    (make empty-element gi: "br")
	    (process-children))))
  (element example
    (make sequence
      (make element gi: "p"
	    (literal "Example "
		     (number->string (child-number (current-node))) ":"))
      (make element gi: "pre"
	    (process-children))))
  (element examplenote
    (process-children))
  ;; The funcname element could be made more sophisticated, so that
  ;; it includes a link (possibly using the source-code browser) to
  ;; the function definition/documentation.
  (element funcname
    (make element gi: "code"
          (literal (string-append "(" (data (current-node)) ")"))))
  ;; discard the following elements, at present
  (element codebody
    (empty-sosofo))
  ;(element misccode
  ;  (empty-sosofo))
  )

;; Mode of exceptions from the above
(mode routine-ref-plain
  (element description
    (process-children)))

;; Mode which includes assorted variants of the handlers above, designed
;; to extract information to which other handlers have made cross-reference.
(mode routine-ref-get-reference
  (default (empty-sosofo))
  (element programcode
    (process-matching-children 'docblock))
  (element codegroup
    (process-matching-children 'docblock))
  (element docblock
    (process-matching-children 'title))
  (element title
    (process-children))
  (element author
    (let ((note (attribute-string (normalize "authornote"))))
    (make sequence
	  (process-matching-children 'name)
	  (if note
	      (literal (string-append " (" note ")"))
	      (empty-sosofo)))))
  (element name
    (process-children))
  (element routine
    (process-matching-children 'routineprologue))
;   (element routineprologue
;     (let* ((kids (children (current-node)))
; 	   (rn (select-elements kids 'routinename))
; 	   (rp (select-elements kids 'purpose)))
;       (if (node-list-empty? rn)
; 	  (literal "Anonymous routine")
; 	  (sosofo-append (process-node-list rn)
; 			 (if (node-list-empty? rp)
; 			     (empty-sosofo)
; 			     (literal (string-append " - "
; 						     (data rp))))))))
  (element routineprologue
    (let ((rn (select-elements (children (current-node)) 'routinename)))
      (if (node-list-empty? rn)
	  (literal "Anonymous routine")
	  (process-node-list rn))))
  (element routinename
    (process-children)))

(mode docblock
  (element title			; ignore in this mode -- title
					; is taken care of by
					; $latex-section$ in element
					; codecollection
    (empty-sosofo))
  (element description
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Description")))
      (make element gi: "dd"
	    (process-children))))
  (element userkeywords
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "User keywords")))
      (make element gi: "dd"
	    (process-children))))
  (element softwarekeywords
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Software category")))
      (make element gi: "dd"
	    (process-children))))
  (element copyright
    (make sequence
      (make element gi: "dt"
	    (make element gi: "strong"
		  (literal "Copyright")))
      (make element gi: "dd"
	    (process-children))))
  (element history
    (empty-sosofo))
  (element authorlist			; authorlist in docblock is
					; assembled from author
					; elements in content.  Don't
					; produce separate set of links.
    (empty-sosofo))
  ;; Following works, it's just that I don't want to produce this output.
;   (element history
;     (make sequence
;       (make empty-command name: "item"
; 	    parameters: '("?History"))
;       (make environment name: "description"
; 	    (process-children))))
;   (element change
;     (let ((heading (string-append (attribute-string (normalize "date")
; 						    (current-node))
; 				  " by "
; 				  (attribute-string (normalize "author")
; 						    (current-node)))))
;       (make sequence
; 	(make empty-command name: "item"
; 	      parameters: `(,(string-append "?" heading)))
; 	(process-children))))
  )


(define (html-file-routine #!optional (nd (current-node)))
  (string-append (html-file-programcode nd)
		 (chunk-path nd)))

(define (html-file-codegroup #!optional (nd (current-node)))
  (string-append (html-file-programcode nd)
		 (chunk-path nd)))

;; Construct a programcode filename, using only information within the
;; document.  In the absence of any hash function, and with no way of
;; converting characters to integers to construct one, just do
;; something based on the number of children, and the number of
;; characters in the title.
(define (html-file-programcode #!optional (nd (current-node)))
  (let* ((de (document-element nd))
	 (db (select-elements (children de) 'docblock))
	 (ti (select-elements (children db) 'title)))
;    (string-append (root-file-name de)
;		   "-P-"
;		   (number->string (node-list-length (children de)))
;		   "-"
;		   (number->string (string-length (data ti))))
    (string-append (root-file-name de)
		   "-P"
		   (hash-of-tree de))))


(element coderef
  (let* ((cc (node-list-or-false
	      ;; If there's a collection attribute, then it
	      ;; provides the ID of a codecollection element.  If
	      ;; this attribute isn't present, then use instead
	      ;; the _first_ (which includes the only)
	      ;; codecollection element in the document.
	      (if (attribute-string (normalize "collection"))
		  (element-with-id
		   (attribute-string (normalize "collection")))
		  (node-list-first
		   (select-elements
		    (select-by-class (descendants (getdocbody))
				     'element)
		    'codecollection)))))
	 (ccdoc (and cc
		     (document-element-from-entity
		      (attribute-string (normalize "doc") cc))))
	 (target (and ccdoc
		      (node-list-or-false
		       (element-with-id (attribute-string (normalize "id"))
					ccdoc))))
	 (targetroutine (and target
			     (if (equal? (gi target) (normalize "routine"))
				 target
				 (ancestor (normalize "routine")
					   target))))
	 (linktext (data (current-node)))
	 )
    (if targetroutine
	(make element gi: "a"
	      attributes: `(("href" ,(href-to targetroutine)))
	      (if (string=? linktext "")
		  (with-mode section-reference
		    (process-node-list targetroutine))
		  (literal linktext)))
	(error (string-append "Can't locate routine with ID "
			      (attribute-string (normalize "id"))
			      (if (attribute-string (normalize "collection"))
				  (string-append
				   " within codecollection with ID "
				   (attribute-string
				    (normalize "collection")))
				  " in default codecollection"))))))

