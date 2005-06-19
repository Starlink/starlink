<!--

Title:
  Starlink General DTD: TeXML for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, 2004, Central Laboratory for the Research Councils

$Id$
-->

<routine>
<routinename>process-texml-document
<description>Generates an entity to hold the LaTeX file.
<returnvalue type=sosofo>A sosofo which generates logging output to
stdout, then generates an entity which contains the entire LaTeX file.
<codebody>
(define (process-texml-document)
  (make sequence
    (literal (string-append (index-file-name) ":"))
    (make entity system-id: (string-append (index-file-name) ".xml")
          (make processing-instruction data: "xml version=\"1.0\"")
          ;; Don't generate doctype -- it's generated in a form 
          ;; `SYSTEM"texml.dtd"', which is legal, but which the
          ;; Python XML parser doesn't like.
          ;; (make document-type name: "TeXML"
          ;;    system-id: "texml.dtd")
	  (make element gi: "TeXML"
                (make-latex-empty-command name: "documentclass"
                                          parameters: %latex-document-class%)
                (make element gi: "TeXML"
                      attributes: '(("escape" "0"))
                      (make fi data: "&lt;![CDATA[")
                      (make fi data: %latex-document-general-preamble%)
                      (if onepass-latex
                          (make fi data: %latex-onepass-toc%)
                          (make fi data: %latex-ordinary-toc%))
                      (make fi data: %latex-sst-preamble%)
                      (make fi data: %latex-end-preamble%)
                      (make fi data: "]]<!-- x -->>") ;mustn't look like MSC
                      )
                (make-latex-empty-command name: "usepackage"
                                          parameters: '("graphics"))
                (let ((indexents (select-elements
                                  (select-by-class (descendants (document-element))
                                                   'element)
                                  (normalize "index"))))
                  (if (node-list-empty? indexents)
                      (empty-sosofo)
                      (make-latex-empty-command name: "makeindex")))
                (make-latex-environment name: "document"
                                        (process-matching-children 'docinfo)
                                        (process-matching-children 'docbody)
                                        (make-backmatter))))))

<routine>
<description>These are the document element types.
<codebody>
(element sug (process-texml-document))
(element sun (process-texml-document))
(element ssn (process-texml-document))
(element mud (process-texml-document))
(element sc  (process-texml-document))
(element sg  (process-texml-document))
(element sgp (process-texml-document))


<routine>
<description>Flow-object constructors for the document head
<p>See the documentation of <funcname>%latex-document-preamble%</funcname> for the 
interface with the `style file' defined there.
<codebody>
(define (make-latex-table-row entry-sosofos)
  (let ((sosofo-or-string
         (lambda (s)
           (cond
            ((sosofo? s)
             s)
            ((string? s)
             (literal s))
            (else
             (error "sosofo-or-string did not get a string"))))))
    (cond
     ((null? entry-sosofos)
      (empty-sosofo))
     ((= (length entry-sosofos) 1)
      (sosofo-append (sosofo-or-string (car entry-sosofos))
                     (latex-newline)))
     (else
      (sosofo-append (sosofo-or-string (car entry-sosofos))
                     (make empty-element
                       gi: "spec" attributes: '(("cat" "align")))
                     (make-latex-table-row (cdr entry-sosofos)))))))
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
  (make-latex-environment name: "FrontMatter"
    (make-latex-command name: "setTitle"
	  (process-node-list title))
    (if (or mantype swvers)
	(make-latex-command name: "setSubTitle"
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
    (make-latex-command name: "setDate"  (literal date))
    ;; setAuthorlist takes an argument which is an environment -- TeXML
    ;; doesn't like that, so make it an empty command and a group
    (make-latex-empty-command name: "setAuthorlist")
    (make element gi: "group"
          (with-mode in-docinfo (process-node-list authorlist)))
    (if abstract
        ;; as for setAuthorlist, so with setAbstract
	(make sequence
          (make-latex-empty-command name: "setAbstract")
          (make element gi: "group"
                (with-mode in-docinfo (process-node-list abstract))))
	(empty-sosofo))
    (if docref
	(make sequence
	  (if (caddr rel)
	      (make-latex-empty-command name: "setDocCode"
		    parameters: `(,(string-append docref "." (caddr rel))))
	      (make-latex-empty-command name: "setDocCode"
		    parameters: `(,docref)))
	  (make-latex-empty-command name: "setDocRef"
		parameters: `(,(getdocnumber (current-node) #t))))
	(empty-sosofo))
    (if copy
	(make-latex-command name: "setCopyright" (process-node-list copy))
	(if %copyright-string%
	    (make-latex-command name: "setCopyright" (literal %copyright-string%))
	    (empty-sosofo)))
    (if coverimage
	(make-latex-command name: "setCoverimage" (process-node-list coverimage))
	(empty-sosofo))
    (make-latex-empty-command name: "MakeTitle")
    (make-latex-environment
     name: "VersoTitlepage"
     (make-latex-environment
      name: "tabular"
      parameters: '("rl")
      (make-latex-table-row
       (list (make sequence
               (make-latex-command name: "multicolumn"
                                   parameters: '("2" "l"))
               (make-latex-command name: "emph"
                                   (literal "Document information")))))
      (if (car rel)
          (make-latex-table-row `("Document date"
                                  ,(format-date (car rel))))
          (empty-sosofo))
      (if (cadr rel)
          (make-latex-table-row `("Last revised"
                                  ,(format-date (cadr rel))))
          (empty-sosofo))
      (if (caddr rel)
          (make-latex-table-row `("Version number"
                                  ,(caddr rel)))
          (empty-sosofo))
      (if (cadddr rel)
          (make-latex-table-row `("Distribution ID"
                                  ,(cadddr rel)))
          (empty-sosofo))
      (if swvers
          (make-latex-table-row `("Software version"
                                  ,(process-node-list swvers)))
          (empty-sosofo))
      ))
    (make-latex-empty-command name: "TableOfContents"))))


(element abstract			;dealt with in docinfo element above
  (empty-sosofo))
(mode in-docinfo
  (element abstract
    (process-children))
  (element authorlist
    (make-latex-environment name: "fmtAuthorlist"
	  (process-children-trim)))
  (element author
    (make-latex-command name: "fmtAuthor"
	  (process-children-trim)))
  (element otherauthors
    (make-latex-environment name: "fmtOtherAuthors"
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
