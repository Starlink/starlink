;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the part of the stylesheet which looks after sectioning.
;;
;;
;; Draws on dbsect.dsl


(define ($section-separator$) 
  (if (or (not (or nochunks stream-output))
	  (node-list=? (current-node) (document-element)))
      (empty-sosofo)
      (make empty-element gi: "HR")))

(define ($section$ #!optional (bod ($section-body$)))
  (html-document (with-mode section-reference (process-node-list (current-node)))
		 bod))

(define ($section-body$)
  (make sequence
    ($section-separator$)
    ($section-title$)
    (process-children)))


;; Returns a sosofo with the section heading, consisting of an Hn
;; element, enclosing an A element with a NAME attribute, enclosing
;; the section title.
;(define ($section-title$)
;  (let* ((sect (current-node))
;	 (hlevel (sectlevel))
;	 (h1elem
;	  (string-append "H" (number->string hlevel)))
;	 (name (element-id))
;	 (isep (%gentext-intra-label-sep% (gi sect)))
;	 (nsep (%gentext-label-title-sep% (gi sect))))
;    (make sequence
;      (make element gi: h1elem
;	    (make element gi: "A"
;		  attributes: (list (list "NAME" name))
;		  (if (string=? (element-label (current-node)) "")
;		      (empty-sosofo)
;		      (literal (element-label (current-node)) nsep))
;		  (element-title-sosofo sect)))
;      )))

(define ($section-title$)
  (make sequence
    (make element
      gi: (string-append "H" (number->string (sectlevel)))
      (if (attribute-string (normalize "id") (current-node))
	  (make element
	    gi: "A"
	    attributes: (list (list "name"
				    (attribute-string (normalize "id")
						      (current-node))))
	    (with-mode section-reference
	      (process-node-list (current-node))))
	  (with-mode section-reference
	    (process-node-list (current-node)))))))


(element sect ($section$))
(element subsect ($section$))
(element subsubsect ($section$))
(element subsubsubsect ($section$))
(element appendices ($section$))




;; Old stuff - still works?
;; Return a sosofo with the contents of a section, complete with header
;(define (make-section level)
;  (make sequence
;    (make element
;      gi: (string-append "H"(number->string (+ level 1)))
;      (if (attribute-string (normalize "id") (current-node))
;	  (make element
;	    gi: "A"
;	    attributes: (list (list "name"
;				    (attribute-string (normalize "id")
;						      (current-node))))
;	    (with-mode section-reference
;	      (process-node-list (current-node))))
;	  (with-mode section-reference
;	    (process-node-list (current-node)))))
;    (process-children)))
;
;(element sect
;  (make-section 1))
;(element subsect
;  (make-section 2))
;(element subsubsect
;  (make-section 3))
;(element subsubsubsect
;  (make-section 4))
;


;; Discard the subhead, except when we're in in-section-head mode
(element subhead
  (empty-sosofo))
(mode in-section-head
  (element subhead
    (process-children-trim)))

