<![ ignore [

Title:
  Starlink General DTD -- LaTeX stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<func>
<routinename>process-latex-document
<description>Generates an entity to hold the LaTeX file.
<returnvalue type=sosofo>A sosofo which generates logging output to
stdout, then generates an entity which contains the entire LaTeX file.
<argumentlist none>
<codebody>
(define (process-latex-document)
  (make sequence
    (literal (string-append (root-file-name) ":"))
    (make entity system-id: (string-append (root-file-name) ".tex")
	  (make empty-command name: "documentclass"
		parameters: %latex-document-class%)
	  (make fi data: %latex-document-preamble%)
	  (make empty-command name: "usepackage"
		parameters: '("graphics"))
	  (make environment name: "document"
		(process-matching-children 'docinfo)
		(process-matching-children 'docbody)))))

<misccode>
<description>These are the document element types.
<codebody>
(element sun (process-latex-document))
(element ssn (process-latex-document))
(element mud (process-latex-document))


<misccode>
<description>Flow-object constructors for the document head
<codebody>
(element docinfo
  (let* ((title (getdocinfo 'title))
	 (authorlist (children (getdocinfo 'authorlist)))
	 (rel (document-release-info))
	 (vers (car (cdr (cdr rel))))
	 (date (format-date (car rel)))
	 (docref (getdocnumber))
	 (abstract (getdocbody 'abstract)))
  (make sequence
    (make empty-command name: "thispagestyle" parameters: '("empty"))
    (make fi data: "\\noindent CCLRC / {\\sc Rutherford Appleton Laboratory}")
    (make fi data: (if docref (string-append "\\hfill{\\bf " docref "}") ""))
    (make fi data: "\\\\{\\large Particle Physics \\& Astronomy Research Council}\\\\{\\large Starlink Project\\\\}")
    (make fi data: (if docref
		 (string-append "{\\large "
				(getdocnumber (current-node) #t)
				"}")
		 ""))
    (make environment name: "flushright"
	  (let loop ((nl authorlist)
		     (sep ""))
	    (if (node-list-empty? nl)
		(empty-sosofo)
		(sosofo-append (literal sep)
			       (process-node-list (node-list-first nl))
			       (loop (node-list-rest nl)
				     ", "))))
	  (make fi data: (string-append "\\\\" date)))
    (make empty-command name: "vspace" parameters: '("-4mm"))
    (make empty-command name: "rule" parameters: '("\\textwidth" "0.5mm"))
    (make empty-command name: "vspace" parameters: '("5mm"))
    (make environment name: "center"
	  (make fi data: (string-append "{\\Huge\\bf "
				  (data title)
				  "}\\\\[4ex]")))
    (make empty-command name: "vspace" parameters: '("5mm"))
    (if abstract
	(make sequence
	  (make empty-command name: "vspace" parameters: '("10mm"))
	  (make environment name: "center"
		(make fi data: "{\\Large\\bf Abstract}"))
	  (make environment name: "flushleft"
		(with-mode in-docinfo
		  (process-node-list abstract))))
	(empty-sosofo))
    (make empty-command name: "cleardoublepage")
    (make empty-command name: "renewcommand"
	  parameters: '("\\thepage" "\\arabic{page}"))
    (make empty-command name: "setcounter" parameters: '("page" "1")))))

(element abstract			;dealt with in docinfo element above
  (empty-sosofo))
(mode in-docinfo
  (element abstract
    (process-children)))
(element author
  (process-children))
