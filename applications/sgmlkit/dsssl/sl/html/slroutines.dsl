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
     title: (with-mode routine-ref-get-reference
	      (process-codecollection (attribute-string (normalize "doc"))))))
  )


;; Routinelist is simple
(element routinelist ($html-section$ (process-children)))

;; Supporting the codecollection chunking/sectioning isn't as easy as with
;; the other such elements, because it doesn't have any children in this
;; document.  We have to do it rather more by hand, therefore.
;; Don't yet support the INCLUDEONLY attribute.
(element codecollection
  (let ((docent (attribute-string (normalize "doc"))))
    (if docent
	($html-section$ (with-mode routine-ref
			  (process-codecollection docent)))
	(error "codecollection: missing required doc attribute"))))

(define (process-codecollection docent)
  (if docent
      (let ((de (document-element-from-entity docent)))
	(if (or (not de)
		(node-list-empty? de))
	    (error (string-append "Couldn't get document element from doc "
				  docent))
	    (process-node-list de)))
      (empty-sosofo)))

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

(mode routine-ref
  (element programcode
    (process-children))
  (element docblock
    ;; Only prepare an authorlist for the top-level docblock, and not
    ;; for the docblock elements in codegroups, for example.
    (if (string=? (gi (parent (current-node)))
		  (normalize "programcode"))
	(let ((allauthors (select-elements
			   (descendants (parent (current-node)))
			   (normalize "author"))))
	  ;; docblock always has a title element, but this is suppressed in
	  ;; this mode.  If there's _more_ than one element, then other
	  ;; docblock elements are present, so should be put into a
	  ;; description list.
	  (make sequence
	    (if (or (> (node-list-length (children (current-node))) 1)
		    (not (node-list-empty? allauthors)))
		(make element gi: "dl"
		      (make sequence
			(with-mode docblock
			  (process-children))
			;; process authors
			(if (node-list-empty? allauthors)
			    (empty-sosofo)
			    (make sequence
			      (make element gi: "dt"
				    (make element gi: "strong"
					  (literal "Authors")))
			      (make element gi: "dd"
				    (make element gi: "dl"
					  (process-authorlist allauthors)))))))
		(empty-sosofo))))
	(empty-sosofo)))
  (element codegroup
    (make sequence
      (make empty-element gi: "hr")
      (make element gi: "h2"
	    (literal "Code group: ")
	    (with-mode routine-ref-get-reference
	      (process-children)))
      (process-children)))
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
		  attributes: `(("href" , (string-append "#AUTHOR_" id)))
		  (process-children)))))
;   (element author
;     (let ((affil (attribute-string (normalize "affiliation")))
; 	  (id (attribute-string (normalize "id")))
; 	  (kids (children (current-node)))
; 	  (link (or (attribute-string (normalize "webpage"))
; 		    (and (attribute-string (normalize "email"))
; 			 (string-append "mailto:"
; 					(attribute-string (normalize
; 							   "email")))))))
;       (make element gi: "li"
; 	    (let ((attlist
; 		   (if link
; 		       (list (list "name" (string-append "AUTHOR_" id))
; 			     (list "href" link))
; 		       (list (list "name" (string-append "AUTHOR_" id))))))
; 	      (make element gi: "a"
; 		    attributes: attlist
; 		    (process-node-list (node-list-first kids))))
; 	    (process-node-list (node-list-rest kids))
; 	    (if affil
; 		(literal (string-append " (" affil ")"))
; 		(empty-sosofo)))))
;   (element authorref
;     (let* ((aut-id (attribute-string (normalize "id")))
; 	   (aut-el (and aut-id
; 			(element-with-id aut-id)))
; 	   (note (attribute-string (normalize "note"))))
;       (if (and (not (node-list-empty? aut-el))
; 	       (string=? (gi aut-el) (normalize "author")))
; 	  (make element gi: "li"
; 		(make element gi: "a"
; 		      attributes: (list (list "href"
; 					      (string-append "#AUTHOR_"
; 							     aut-id)))
; 		      (make sequence
; 			(with-mode routine-ref-get-reference
; 			  (process-node-list aut-el))
; 			(if note
; 			    (literal (string-append " (" note ")"))
; 			    (empty-sosofo)))))
; 	  (error (string-append "ID " aut-id " is not an AUTHOR element")))))
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
      (make sequence
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
		    %display-programcode-elements%))
	;; now collect together all the diytopics
	;;(apply sosofo-append
	;;       (map (lambda (gi-and-nd)
	;;	      (if (string=? (normalize (car gi-and-nd))
	;;			    (normalize "diytopic"))
	;;		  (process-node-list (cdr gi-and-nd))
	;;		  (empty-sosofo)))
	;;	    kids))
	)))
  (element routine
    (let* ((rp (select-elements (children (current-node)) 'routineprologue))
	   (rn (and (not (node-list-empty? rp))
		    (select-elements (children rp) 'routinename))))
      (make sequence
	(make empty-element gi: "hr")
	(make element gi: "h2"
	      (make element gi: "a"
		    attributes: `(("name" ,(href-to (current-node)
						    frag-only: #t)))
		    (if (node-list-empty? rn)
			(literal "Anonymous routine")
			(with-mode routine-ref-get-reference
			  (process-node-list rp)))))
	(make element gi: "dl"
	      (process-children)))))
;  (element routinename
;    (make sequence
;      (make empty-element gi: "hr")
;      (make element gi: "h3"
;	    attributes: '(("align" "center"))
;	    (process-children))))
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
		    (literal (string-append " (" (data type) ") "
					    (cond
					     ((and given-att returned-att)
					      "given and returned")
					     (given-att "given")
					     (returned-att "returned")
					     (else ;default is given
					      "given"))
					    (if opt-att
						(string-append ", " opt-att)
						"")))))
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
	    (make element gi: "dl"
		  (process-children)))))
  ;;(element example
  ;;  (make sequence
  ;;    (make element gi: "dt"
  ;;	  (literal "Example " (number->string (child-number (current-node)))))
  ;;    (make empty-element gi: "br")
  ;;    (make element gi: "code"
  ;;	    (process-children))))
  (element example
    (make sequence
      (make element gi: "dt"
	    (make element gi: "code"
		  (process-children)))))
  (element examplenote
    (make element gi: "dd"
	  (process-children)))
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
  (element routineprologue
    (let* ((kids (children (current-node)))
	   (rn (select-elements kids 'routinename))
	   (rp (select-elements kids 'purpose)))
      (if (node-list-empty? rn)
	  (literal "Anonymous routine")
	  (sosofo-append (process-node-list rn)
			 (if (node-list-empty? rp)
			     (empty-sosofo)
			     (literal (string-append " - "
						     (data rp))))))))
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
	    (literal "Description"))
      (make element gi: "dd"
	    (process-children))))
  (element userkeywords
    (make sequence
      (make element gi: "dt"
	    (literal "User keywords"))
      (make element gi: "dd"
	    (process-children))))
  (element softwarekeywords
    (make sequence
      (make element gi: "dt"
	    (literal "Software category"))
      (make element gi: "dd"
	    (process-children))))
  (element copyright
    (make sequence
      (make element gi: "dt"
	    (literal "Copyright"))
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

;(define (href-to-fragid-routine)
;  (let ((id (or (attribute-string (normalize "id"))
;		(select-elements (children (current-node)) 'routineprologue))))
;    (string-append "_R")))
;; Define a href-to function.  Note that this will generate a fragid
;; which is unique in the target file, but which wouldn't be unique
;; document-wide.  That doesn't matter in this application, because
;; routinelists are _always_ in separate files.
(define (href-to-fragid-routine nd)
  (string-append "_R" (number->string (element-number nd))))

(element coderef
  (let* ((cc (node-list-or-false
	      (element-with-id (attribute-string (normalize "collection")))))
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
	 (targetfragid (and targetroutine
			    (href-to targetroutine frag-only: #t))))
    (if targetfragid
	(make element gi: "a"
	      attributes: `(("href" ,(href-to cc force-frag: targetfragid)))
	      (process-children))
	(error (string-append "Can't find one of collection "
			      (attribute-string (normalize "collection"))
			      " or routine "
			      (attribute-string (normalize "id")))))))
