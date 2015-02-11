;; Starlink text strings.  Based on docbook/common/dbl1usen.dsl...

;; $Id$
;;
;; This file is part of the Modular DocBook Stylesheet distribution.
;; See ../README or http://nwalsh.com/docbook/dsssl/
;;

;; ----------------------------- Localization -----------------------------

;; If you create a new version of this file, please send it to
;; Norman Walsh, ndw@nwalsh.com

;; The generated text for cross references to elements.  See dblink.dsl
;; for a discussion of how substitution is performed on the %x and #x
;; keywords.
;;

(define (gentext-xref-strings giname)
  (case giname
    (("APPENDIX") (if %chapter-autolabel%
		      "Appendix %n"
		      "the appendix called %t"))
    (("ARTICLE") (string-append %gentext-start-quote%
				"%t"
				%gentext-end-quote%))
    (("BIBLIOGRAPHY") "%t")
    (("BOOK") "%t")
    (("CHAPTER") (if %chapter-autolabel%
		     "Chapter %n"
		     "the chapter called %t"))
    (("EQUATION") "Equation %n")
    (("EXAMPLE") "Example %n")
    (("FIGURE") "Figure %n")
    (("INFORMALTABLE") "%kg %kn")
    (("LISTITEM") "%n")
    (("PART") "Part %n")
    (("PREFACE") "%t")
    (("PROCEDURE") "Procedure %n, %t")
    (("SECT1") (if %section-autolabel% 
		   "Section %n" 
		   "the section called %t#c"))
    (("SECT2") (if %section-autolabel% 
		   "Section %n" 
		   "the section called %t#c"))
    (("SECT3") (if %section-autolabel% 
		   "Section %n" 
		   "the section called %t#c"))
    (("SECT4") (if %section-autolabel% 
		   "Section %n" 
		   "the section called %t#c"))
    (("SECT5") (if %section-autolabel% 
		   "Section %n" 
		   "the section called %t#c"))
    (("STEP") "step %n#k")
    (("TABLE") "Table %n")
    (("TITLE") "%kg %kn")
    (else (string-append "[xref to " 
			 (if giname 
			     giname
			     "non-existant element")
			 " unsupported]"))))

;; Indirection in the generated text for cross references.  The first
;; element of the list is used for cross references across element
;; boundries, the second element is used for cross references within
;; the same element
;;
(define (gentext-xref-strings-indirect key)
  (case key
    (("b") '("" ""))
    (("c") '(" in %cg %cn" ""))
    (("d") '("" ""))
    (("k") '(" in %kg %kn" ""))
    (("s") '("" ""))))

;; Should the TOC come first or last?
;;
(define %generate-toc-in-front% #t)

;; gentext-element-name returns the generated text that should be 
;; used to make reference to the selected element.
;;
(define (gentext-element-name giname)
  (case giname
    (("ABSTRACT") "Abstract")
    (("APPENDIX") "Appendix")
    (("BIBLIOGRAPHY") "Bibliography")
    (("CAUTION") "CAUTION")
    (("CHAPTER") "Chapter")
    (("COPYRIGHT") "Copyright")
    (("DEDICATION") "Dedication")
    (("EDITION") "Edition")
    (("EQUATION") "Equation")
    (("EXAMPLE") "Example")
    (("FIGURE") "Figure")
    (("GLOSSARY") "Glossary")
    (("GLOSSSEE") "See")
    (("GLOSSSEEALSO") "See Also")
    (("IMPORTANT") "IMPORTANT")
    (("INDEX") "Index")
    (("ISBN") "ISBN")
    (("LEGALNOTICE") "")
    (("MSGAUD") "Audience")
    (("MSGLEVEL") "Level")
    (("MSGORIG") "Origin")
    (("NOTE") "NOTE")
    (("PART") "Part")
    (("PREFACE") "Preface")
    (("PROCEDURE") "Procedure")
    (("PUBDATE") "Published")
    (("REFERENCE") "Reference")
    (("REFNAME") "Name")
    (("REVHISTORY") "Revision History")
    (("REVISION") "Revision")
    (("SECT1") "Section")
    (("SECT2") "Section")
    (("SECT3") "Section")
    (("SECT4") "Section")
    (("SECT5") "Section")
    (("SIDEBAR") "")
    (("STEP") "step")
    (("TABLE") "Table")
    (("TIP") "TIP")
    (("TOC") "Table of Contents")
    (("WARNING") "WARNING")
    (else (string-append "UNEXPECTED-ELEMENT-NAME: " giname))))

;; gentext-element-name-space returns gentext-element-name with a 
;; trailing space, if gentext-element-name isn't "".
;;
(define (gentext-element-name-space giname)
  (string-with-space (gentext-element-name giname)))

;; gentext-intra-label-sep returns the seperator to be inserted
;; between multiple occurances of a label (or parts of a label)
;; for the specified element.  Most of these are for enumerated
;; labels like "Figure 2-4", but this function is used elsewhere
;; (e.g. REFNAME) with a little abuse.
;;
(define (gentext-intra-label-sep giname)
  (case giname
    (("EQUATION") "-")
    (("EXAMPLE") "-")
    (("FIGURE") "-")
    (("PROCEDURE") ".")
    (("REFENTRY") ".")
    (("REFERENCE") ".")
    (("REFNAME") ", ")
    (("REFSECT1") ".")
    (("REFSECT2") ".")
    (("REFSECT3") ".")
    (("SECT1") ".")
    (("SECT2") ".")
    (("SECT3") ".")
    (("SECT4") ".")
    (("SECT5") ".")
    (("STEP") ".")
    (("TABLE") "-")
    (("_PAGENUMBER") "-")  ;; page number psuedo element
    (else "")))

;; gentext-label-title-sep returns the seperator to be inserted
;; between a label and the text following the label for the
;; specified element.  Most of these are for use between
;; enumerated labels and titles like "1. Chapter One Title", but
;; this function is used elsewhere (e.g. NOTE) with a little
;; abuse.
;;
(define (gentext-label-title-sep giname)
  (case giname
    (("ABSTRACT") ": ")
    (("APPENDIX") ". ")
    (("CAUTION") "")
    (("CHAPTER") ". ")
    (("EQUATION") ". ")
    (("EXAMPLE") ". ")
    (("FIGURE") ". ")
    (("FOOTNOTE") ". ")
    (("GLOSSSEE") ": ")
    (("GLOSSSEEALSO") ": ")
    (("IMPORTANT") ": ")
    (("NOTE") ": ")
    (("ORDEREDLIST") ". ")
    (("PART") ". ")
    (("PROCEDURE") ". ")
    (("PREFIX") ". ")
    (("REFENTRY") "")
    (("REFERENCE") ". ")
    (("REFSECT1") ". ")
    (("REFSECT2") ". ")
    (("REFSECT3") ". ")
    (("SECT1") ". ")
    (("SECT2") ". ")
    (("SECT3") ". ")
    (("SECT4") ". ")
    (("SECT5") ". ")
    (("STEP") ". ")
    (("TABLE") ". ")
    (("TIP") ": ")
    (("WARNING") "")
    (else "")))

(define ($lot-title$ lotgi)
  (cond ((string=? "TABLE"   lotgi) "List of Tables")
	((string=? "EXAMPLE" lotgi) "List of Examples")
	((string=? "FIGURE"  lotgi) "List of Figures")
	((string=? "EQUATION" lotgi) "List of Equations")
	(else "List of ???")))

(define %gentext-start-quote% (dingbat "ldquo"))

(define %gentext-end-quote% (dingbat "rdquo"))

(define %gentext-by% "by") ;; e.g. Copyright 1997 "by" A. Nonymous
                           ;; Authored "by" Jane Doe

(define %gentext-edited-by% "Edited by")
                           ;; "Edited by" Jane Doe

(define %gentext-page% "")

(define %gentext-and% "and")

(define %gentext-bibl-pages% "Pgs.")

(define %gentext-endnotes% "Notes")

(define %gentext-table-endnotes% "Notes:")

(define %gentext-index-see% "See")

(define %gentext-index-seealso% "See Also")




;; Following from dbl1usen.dsl
(define (gentext-nav-prev prev) 
  (make sequence (literal "Prev")))

(define (gentext-nav-prevsib prevsib) 
  (make sequence (literal "Fast Backward")))

(define (gentext-nav-nextsib nextsib)
  (make sequence (literal "Fast Forward")))

(define (gentext-nav-next next)
  (make sequence (literal "Next")))

(define (gentext-nav-up up)
  (make sequence (literal "Up")))

(define (gentext-nav-home home)
  (make sequence (literal "Home")))
