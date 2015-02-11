<!--

Title:
  Starlink General DTD: LaTeX stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
-->

<routine>
<routinename>process-latex-document
<description>Generates an entity to hold the LaTeX file.
<returnvalue type=sosofo>A sosofo which generates logging output to
stdout, then generates an entity which contains the entire LaTeX file.
<codebody>
(define (process-latex-document)
  (make sequence
    (literal (string-append (root-file-name) ":"))
    (make entity system-id: (string-append (index-file-name) ".tex")
	  (make empty-command name: "documentclass"
		parameters: %latex-document-class%)
	  (make fi data: %latex-document-general-preamble%)
	  (if onepass-latex
	      (make fi data: %latex-onepass-toc%)
	      (make fi data: %latex-ordinary-toc%))
	  (make fi data: %latex-sst-preamble%)
	  (make fi data: %latex-end-preamble%)
	  (make empty-command name: "usepackage"
		parameters: '("graphics"))
          (let ((indexents (select-elements
                            (select-by-class (descendants (document-element))
                                             'element)
                            (normalize "index"))))
            (if (node-list-empty? indexents)
                (empty-sosofo)
                (make empty-command name: "makeindex")))
	  (make environment name: "document"
		(process-matching-children 'docinfo)
		(process-matching-children 'docbody)
		(make-backmatter)))))

<routine>
<description>These are the document element types.
<codebody>
(element sug (process-latex-document))
(element sun (process-latex-document))
(element ssn (process-latex-document))
(element mud (process-latex-document))
(element sc  (process-latex-document))
(element sg  (process-latex-document))
(element sgp (process-latex-document))


<routine>
<description>Flow-object constructors for the document head
<p>See the documentation of <funcname>%latex-document-preamble%</funcname> for the 
interface with the `style file' defined there.
<codebody>
(element docinfo
  (let* ((title (getdocinfo 'title))
	 (authorlist (getdocinfo 'authorlist))
	 (rel (document-release-info))
	 (copy (getdocinfo 'copyright))
	 (coverimage (getdocinfo 'coverimage))
	 (date (format-date (car rel)))
	 (docref (getdocnumber))
	 (mantype (getdocinfo 'manualtype))
	 (swvers (getdocinfo 'softwareversion))
	 (abstract (getdocbody 'abstract)))
  (make environment name: "FrontMatter"
    (make command name: "setTitle"
	  (process-node-list title))
    (if (or mantype swvers)
	(make command name: "setSubTitle"
	      (if mantype
		  (process-node-list mantype)
		  (empty-sosofo))
	      (if (and mantype swvers)
		  (literal " -- ")
		  (empty-sosofo))
	      (if swvers
		  (process-node-list swvers)
		  (empty-sosofo)))
	(empty-sosofo))
    (make command name: "setDate"  (literal date))
    (make command name: "setAuthorlist"
	  (with-mode in-docinfo (process-node-list authorlist)))
    (if abstract
	(make command name: "setAbstract"
	      (with-mode in-docinfo (process-node-list abstract)))
	(empty-sosofo))
    (if docref
	(make sequence
	  (if (caddr rel)
	      (make empty-command name: "setDocCode"
		    parameters: `(,(string-append docref "." (caddr rel))))
	      (make empty-command name: "setDocCode"
		    parameters: `(,docref)))
	  (make empty-command name: "setDocRef"
		parameters: `(,(getdocnumber (current-node) #t))))
	(empty-sosofo))
    (if copy
	(make command name: "setCopyright" (process-node-list copy))
	(if %copyright-string%
	    (make command name: "setCopyright" (literal %copyright-string%))
	    (empty-sosofo)))
    (if coverimage
	(make command name: "setCoverimage" (process-node-list coverimage))
	(empty-sosofo))
    (make empty-command name: "MakeTitle")
    (make environment name: "VersoTitlepage"
	  (make environment name: "tabular"
		parameters: '("rl")
		(make fi data:
		      "\\multicolumn{2}{l}{\\emph{Document information}}\\\\")
		(if (car rel)
		    (make fi data: (string-append "Document date&"
						  (format-date (car rel))
						  "\\\\"))
		    (empty-sosofo))
		(if (cadr rel)
		    (make fi data: (string-append "Last revised&"
						  (format-date (cadr rel))
						  "\\\\"))
		    (empty-sosofo))
		(if (caddr rel)
		    (make fi data: (string-append "Version number&"
						  (caddr rel)
						  "\\\\"))
		    (empty-sosofo))
		(if (cadddr rel)
		    (make fi data: (string-append "Distribution ID&"
						  (cadddr rel)
						  "\\\\"))
		    (empty-sosofo))
		(if swvers
		    (make sequence
		      (make fi data: "Software version&")
		      (process-node-list swvers)
		      (make fi data: "\\\\"))
		    (empty-sosofo))
		))
    (make empty-command name: "TableOfContents"))))


(element abstract			;dealt with in docinfo element above
  (empty-sosofo))
(mode in-docinfo
  (element abstract
    (process-children))
  (element authorlist
    (make environment name: "fmtAuthorlist"
	  (process-children-trim)))
  (element author
    (make command name: "fmtAuthor"
	  (process-children-trim)))
  (element otherauthors
    (make environment name: "fmtOtherAuthors"
	  (process-children-trim))))

(element manualtype
   (case (case-fold-down (attribute-string (normalize "type")))
      (("users") (literal "User's Guide"))
      (("programmers") (literal "Programmer's Guide"))
      (("programmers.c") (literal "Programmer's Guide (C version)"))
      (("programmers.fortran") (literal "Programmer's Guide (Fortran version)"))
      (("other") (process-children))
      (else (error "manualtype: unrecognised manualtype"))))

(element softwareversion
   (make sequence
     (literal "Software Version ")
     (process-children)))
