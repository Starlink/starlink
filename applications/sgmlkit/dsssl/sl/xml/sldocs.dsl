<![ ignore [

Title:
  Starlink General DTD -- XML stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
]]>

<func>
<routinename>process-xml-document
<description>Generates an entity to hold the XML file.
<returnvalue type=sosofo>A sosofo which generates logging output to
stdout, then generates an entity which contains the entire XML file.
<argumentlist none>
<codebody>
(define (process-xml-document)
  (make sequence
    (literal (string-append (root-file-name) ":"))
    (make entity system-id: (string-append (root-file-name) ".xml")
	  (process-matching-children 'docinfo)
	  (process-matching-children 'docbody))))

<misccode>
<description>These are the document element types.
<codebody>
(element sug (process-xml-document))
(element sun (process-xml-document))
(element ssn (process-xml-document))
(element mud (process-xml-document))
(element sc  (process-xml-document))
(element sg  (process-xml-document))
(element sgp (process-xml-document))
(element mud (process-xml-document))


<misccode>
<description>Flow-object constructors for the document head
<codebody>
(element docinfo
  (let* ((title (getdocinfo 'title))
	 (authorlist (getdocinfo 'authorlist))
	 (rel (document-release-info))
	 (copy (getdocinfo 'copyright))
	 (coverimage (getdocinfo 'coverimage))
	 ;(date (format-date (car rel)))
	 (docref (getdocnumber))
	 (abstract (getdocbody 'abstract)))
  (make element gi: "docinfo"
    (make element gi: "title" (process-node-list title))
    (make empty-element gi: "docdates"
	  attributes: (list
		       (if (car rel)
			   (list "date" (format-date (car rel)))
			   '())
		       (if (cadr rel)
			   (list "revised" (format-date (cadr rel)))
			   '())
		       (if (caddr rel)
			   (list "version" (caddr rel))
			   '())
		       (if (cadddr rel)
			   (list "distribution" (cadddr rel))
			   '())
		       ))
    (make element gi: "authorcollection"
	  (with-mode in-docinfo (process-node-list authorlist)))
    (if abstract
	(make element gi: "abstract"
	      (with-mode in-docinfo (process-node-list abstract)))
	(empty-sosofo))
    (if docref
	(make sequence
	  (make element gi: "doccode"
		(literal docref))
	  (make element gi: "docref"
		(literal (getdocnumber (current-node) #t))))
	(empty-sosofo))
    (if copy
	(make element gi: "copyright" (process-node-list copy))
	(empty-sosofo))
    (if coverimage
	(make element gi: "coverimage" (process-node-list coverimage))
	(empty-sosofo)))))


(element history
  (empty-sosofo))
(element abstract			;dealt with in docinfo element above
  (empty-sosofo))
(mode in-docinfo
  (element abstract
    (process-children))
  (element authorlist
    (make element gi: "authorlist"
	  (process-children-trim)))
  (element author
    (make element gi: "author"
	  (process-children-trim)))
  (element otherauthors
    (make element gi: "otherauthors"
	  (process-children-trim))))
