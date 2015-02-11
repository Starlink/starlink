<!--

Title:
  Starlink General DTD: XML stylesheet for document elements

Author:
  Norman Gray, Glasgow (NG)

History:
  19 April 1999 (initial version)

Copyright 1999, Particle Physics and Astronomy Research Council

$Id$
-->

<routine>
<routinename>process-xml-document
<description>Generates an XML file on stdout.
<returnvalue type=sosofo>A sosofo which contains the output file.
<codebody>
(define (process-xml-document)
  (make sequence
    (make processing-instruction data: "xml version=\"1.0\"")
    (make document-type name: "slsimple"
	  system-id: "/home/norman/s/src/sgml/w/sgml/dtd/simple-0.7.dtd"
	  ;public-id: "-//Starlink//DTD Something//EN"
	  )
    (make element gi: "slsimple"
	  (process-matching-children 'docinfo)
	  (process-matching-children 'docbody))
    ;(make-backmatter)
    ))

<routine>
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


<routine>
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
	 (mantype (getdocinfo 'manualtype))
	 (swvers (getdocinfo 'softwareversion))
	 (history (getdocinfo 'history))
	 (abstract (getdocbody 'abstract)))
    (make element gi: "docinfo"
	  (make element gi: "title" (process-node-list title))
	  (if (or mantype swvers)
	      (make element gi: "subtitle"
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
	  (if docref
	      (make sequence
		(make element gi: "doccode"
		      (literal docref))
		(make element gi: "docref"
		      (literal (getdocnumber (current-node) #t))))
	      (empty-sosofo))
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
	  (with-mode in-docinfo (process-node-list authorlist))
	  (if abstract
	      (make element gi: "abstract"
		    (with-mode in-docinfo (process-node-list abstract)))
	      (empty-sosofo))
	  (if copy
	      (make element gi: "copyright" (process-node-list copy))
	      (empty-sosofo))
	  (if coverimage
	      (process-node-list coverimage)
	      (empty-sosofo))
	  (if history
	      (process-node-list history)
	      (empty-sosofo)))))

(element docbody
  (copy-element))

;; history elements -- copy unchanged
(element history
  (copy-element))
(element version
  (copy-element))
(element distribution
  (copy-element))
(element change
  (copy-element))

(element abstract			;dealt with in docinfo element above
  (empty-sosofo))
(mode in-docinfo
  (element abstract
    (process-children-trim))
  (element authorlist
    (copy-element))
  (element otherauthors
    (copy-element))
  (element editors
    (copy-element))
  (element author
    (copy-element)))

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
