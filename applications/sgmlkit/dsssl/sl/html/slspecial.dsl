;;; This file contains DSSSL code which overrides selected parts of
;;; the DSSSL stylesheets.  It's included in the `main'
;;; style-specification element (codegroup in DSSSLCODE terms) which
;;; calls the other style-specifications and external-specifications
;;; (codegroup and codereference), and so definitions here override
;;; definitions elsewhere.
;;;
;;; This is intended for back-door hacking, so this is all the
;;; documentation you get.  If it doesn't make sense, don't use it.
;;;
;;; If a particular release of the stylesheet breaks your hacks,
;;; tough!

;;; For example, the following definition of root-footer-navigation
;;; overrides the standard one, to produce a TOC on a separate page.
;(define (root-footer-navigation elemnode)
;  (if (chunking?)
;      (let ((tocfname (string-append "TOC" %html-ext%)))
;	(make sequence
;	      (make element gi: "a"
;		    attributes: `(("HREF" ,tocfname))
;		    (literal "Table of Contents"))
;	      (html-document
;	       (literal "Table of Contents")
;	       (make sequence
;		 (make element gi: "h1"
;		       (literal "Table of Contents"))
;		 (make-contents (getdocbody) 4 #t))
;	       system-id: tocfname
;	       navbars?: #f		; This is essential.  If not
;					; present, we get into an
;					; infinite loop, since
;					; html-document calls
;					; footer-navigation if
;					; navbars? is true 
;	       )))
;      (empty-sosofo)))
;
;;; The above TOC file wouldn't appear in the MANIFEST.  Piggyback a
;;; reference to it in a make-manifest-mode entry for sect (and hope
;;; that there's at least one sect element in the document).
;(mode make-manifest-mode
;  (element sect
;    (make sequence
;      (if (or (chunk?)
;	      (node-list=? (current-node) (document-element)))
;	  (make formatting-instruction data: (string-append (html-file) "
;"))
;	  (empty-sosofo))
;      (make formatting-instruction data: (string-append "TOC" %html-ext% "
;")))))
