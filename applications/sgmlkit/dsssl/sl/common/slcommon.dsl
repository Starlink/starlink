<!DOCTYPE programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN" [
  <!entity sl-gentext.dsl	system "sl-gentext.dsl">
]>

<title>Common functions for the Starlink stylesheets

<codegroup>
<title>Common functions for the Starlink stylesheets
<description>
<p>These are functions and handlers common to both the HTML and print
versions of the Starlink stylesheets.

<p>Note that Jade only supports the DSSSL Online subset of DSSSL, so some
more advanced features will be missing.
See:
<url>http://sunsite.unc.edu/pub/sun-info/standards/dsssl/dssslo/do960816.htm</url> 
and
<url>http://www.jclark.com/jade/#limitations</url> for further details.

<authorlist>
<author id=ng attribution='Glasgow'>Norman Gray

<func>
<codeprologue>
<routinename>
<name>getdocinfo
<description>
<p>Return a node-list consisting of the specified child of the docinfo element
for the current grove, or false if there is no such child.
That is, <code>(getdocinfo 'title)</code> returns the current document's title
<returnvalue type="node-list">
<argumentlist>
<parameter>
<name>type
<type>symbol
<description>
<p>Symbol giving the name of one of the children of the docinfo element
</description>
</parameter>
<parameter optional="optional">
<name>nd
<type>node-list
<description>
<p>A node-list which indicates the grove which is to supply the
document element.  If omitted, it defaults to the current node, and
the document element corresponding to the current grove is obtained
and returned.
</description>
</parameter>
</argumentlist>
</codeprologue>
(define (getdocinfo type #!optional (nd (current-node)))
  (let* ((docelem (document-element nd))
	 (dinl (select-elements (children (select-elements (children docelem)
							            'docinfo))
				type)))
    (if (node-list-empty? dinl)
	#f
	dinl)))
</func>

<func>
<codeprologue>
<routinename>
<name>getdocnumber
<description>
<p>Return the current node's document number as a string, or <code/#f/ if
unavailable (because DOCNUMBER isn't defined)
</description>
<returnvalue type=string>The document number as a string
<argumentlist>
<parameter optional>
<name>nd
<type>node-list
<description>
<p>If present, this indicates which grove is to be used to find the
document element.  Defaults to <code/(current-node)/.
</parameter>
</argumentlist>
</codeprologue>
(define (getdocnumber #!optional (nd (current-node)))
  (let* ((dn (getdocinfo 'docnumber nd))
	 (docelemtype (and dn
			   (if (attribute-string (normalize "documenttype") dn)
			       (attribute-string (normalize "documenttype") dn)
			       (error "DOCNUMBER has no DOCUMENTTYPE")))))
    (if dn
	(string-append docelemtype
		       "/"
		       (if (attribute-string "UNASSIGNED" dn)
			   "??"
			   (trim-string (data dn))))
	#f	
	)))
</func>


<misccode>
<miscprologue>
<description>
<p>Section number formatting
<p>Various routines for obtaining and formatting section headings.
</description></miscprologue>
; Define the formats for section and appendix numbering, in order
; (subsubsubsect, subsubsect, subsect, sect)
(define %section-fmts '("1" "1" "1" "1"))
(define %appendix-fmts '("A" "1" "1" "1"))

; Returns an array with the first n elements of "sect", "subsect", ...
(define (section-hierarchy n)
  (let loop ((l '("subsubsubsect" "subsubsect" "subsect" "sect")))
    (if (or (<= (length l) n) (null? l))
	l
	(loop (cdr l)))))

; Turn a list of strings into one string, with elements separated by sep
(define (stringlist->string list sep)
  (let loop ((l list)
	     (allstrings ""))
    (if (null? l)
	allstrings
	(loop (cdr l)
	      (string-append allstrings (car l) sep)))))

;; Return a sosofo with the title of the current node,
;; prefixed by its section number.  If the optional argument ts is given, it
;; is used as the title sosofo, rather than extracting it from the node.
(define (make-section-reference level #!optional (ts #f))
  (let* ((inappendix (have-ancestor? "appendices" (current-node)))
					; (have-ancestor?) 10.2.4.4
	 (hier-list (reverse (section-hierarchy (- level 1))))
	 (hier-nums (hierarchical-number hier-list (current-node)))
					; (hierarchical-number) see 10.2.4.2
	 (hier-strs (map (lambda (n f)
			   (format-number n f))
			 (append hier-nums (list (child-number)))
					; make a list of the hierarchy nos,
					; including current child number
			 (if inappendix	; ...formatted appropriately
			     %appendix-fmts
			     %section-fmts)))
	 (sn (stringlist->string hier-strs "."))
	 )
    (make sequence
      (literal ;(if inappendix "Appendix " "Section ")
	       sn
	       " ")
      (if ts
	  ts
	  (process-first-descendant 'title)))))

(define (sectlevel #!optional (sect (current-node)))
  (cond
   ((equal? (gi sect) (normalize "subsubsubsect")) 4)
   ((equal? (gi sect) (normalize "subsubsect")) 3)
   ((equal? (gi sect) (normalize "subsect")) 2)
   ((equal? (gi sect) (normalize "sect")) 1)
   (else 1)))

;; Return a list of elements which are allowable targets for a link.
;; This will be used when linking to (an ID attribute within) an
;; object which can't be linked to directly.  It doesn't matter if
;; there's redundancy in this - the appropriate element is selected as
;; the first member of this list amongst the target element's ancestors.
;;
;; For example, the LABEL
;; element in the DocumentSummary DTD requires this.
(define (target-element-list)
  (list (normalize "sect")
	(normalize "subsect")
	(normalize "subsubsect")
	(normalize "appendices")))

(mode section-reference
  (element sect
    (make-section-reference 1))
  (element subsect
    (make-section-reference 2))
  (element subsubsect
    (make-section-reference 3))
  (element subsubsubsect
    (make-section-reference 4))
  (element appendices
    (literal "Appendices"))
  (element title
    (process-children-trim))
  (element docbody
    (process-node-list (getdocinfo 'title)))
  (element label
    ;; label doesn't appear in the General DTD, but it does appear in
    ;; the summary one.  Refer to it, at least at present, by
    ;; referring to the section it lives in
    (process-node-list (ancestor-member (current-node)
					(target-element-list)))))


;; return the title of a section
(define (section-title nd)
  (let* ((subhead (select-elements (children nd) (normalize "subhead")))
	 (title (select-elements (children subhead) (normalize "title"))))
    (if (node-list-empty? title)
	""
	(data (node-list-first title)))))

;; Returns the data of the title of the element
(define (element-title nd)
  (if (note-list-empty? nd)
      ""
      (cond
       ((equal? (gi nd) (normalise "sect")) (section-title nd))
       ((equal? (gi nd) (normalise "subsect")) (section-title nd))
       ((equal? (gi nd) (normalise "subsubsect")) (section-title nd))
       ((equal? (gi nd) (normalise "subsubsubsect")) (section-title nd))
       (else (literal "UNKNOWN TITLE!")))))


;; Element lists.
;; These have to be functions because they have to be evaluated when
;; there is a current-node so that normalize can know what declaration
;; is in effect.
;; (I don't really know what the *-element-list functions are for, but
;; they're in the docbook stylesheets, and needed in (eg) the
;; dbnavig.dsl sheet.)  See also (chunk-element-list)
(define (section-element-list)
  (list (normalize "sect")
	(normalize "subsect")
	(normalize "subsubsect")
	(normalize "subsubsubsect")
	(normalize "appendices")
	(normalize "routinelist")
	(normalize "codecollection")))
</misccode>

<misccode>
<description>
<p>A couple of functions for trimming strings.  I need these because,
for example, I generate the output filename based on the document type
and number, and this goes wrong if they were specified with their
end-tags omitted, so that the string has returns or the like within
it.

<p>At one point, I thought to use <code/input-whitespace?/, defined in
the DSSSL spec, but it turns out that this is defined only during 
FOT construction.  There seems potential milage in defining whitespace
properties for characters using <code/(declare-char-property)/ (8.5.8.1),
but little ultimate point, as Jade does not fully support
char-property - see notes at
<url>http://www.jclark.com/jade/#limitations</url>.  That code
<em/would/ have been
<code>
(define (trim-leading-whitespace charlist)
  (let loop ((cl charlist))
    (if (not (debug (char-property 'input-whitespace? (debug (car cl)))))
	(debug cl)
	(loop (cdr cl)))))</code>
<p>Instead, brute-force it:
</description>
;; Given a list of characters, return the list with leading whitespace 
;; characters removed
(define (trim-leading-whitespace charlist)
  (let loop ((cl charlist))
    (let ((firstchar (car cl)))
      (if (not (or (equal? firstchar #\space)
		   (equal? firstchar #\&#TAB)
		   (equal? firstchar #\&#RE)))
	  cl
	  (loop (cdr cl))))))

;; Trim both ends of a string by converting it to a list, trimming
;; leading whitespace, then reversing it and doing it again, then
;; reversing it.
(define (trim-string s)
  (let* ((cl (string->list s))
	 (rl (reverse (trim-leading-whitespace cl))))
    (list->string (reverse (trim-leading-whitespace rl)))))

;; Shorthand
(define (trim-data nd)
  (trim-string (data nd)))

;; Normalise a list of characters by replacing non-space whitespace by
;; space.
(define (normalise-character-list cl)
   (let loop ((l cl) (result '()))
     (if (null? l)
	 result
	 (let* ((fc (car l))
		(replacement (cond ((equal? fc #\&#TAB) #\space)
				   ((equal? fc #\&#RE)  #\space)
				   (else fc))))
	   (loop (cdr l) (append result (list replacement)))))))

(define (normalise-string s)
  (let* ((cl (string->list s))
	 (rl (reverse (trim-leading-whitespace cl)))
	 (rrl (reverse (trim-leading-whitespace rl))))
    (list->string (normalise-character-list rrl))))
</misccode>

<func>
<codeprologue>
<routinename>
<name>document-release-info
<description>
<p>
Return a list containing
<ul>
<li>version date: contents of the date attribute of the last
    VERSION element in the history, or false if there's none.
<li>last change date: contents of the date attribute of the last
    CHANGE or DISTRIBUTION element, or false if there's none.
<li>version number: contents of the number attribute of the last
    VERSION element.
<li>distribution id: contents of the string attribute of the last
    DISTRIBUTION element.
</ul>
If there's no history element, then the first two elements will be
the same, with the contents of the DOCDATE element, and the last
two will be <code/#f/.
We don't check whether the dates are sensible (ie, whether the last
element really does have the latest date).
<returnvalue type=list>(version date version-number distribution-id)
<argumentlist>
<parameter>
<name>nd
<type>node-list
<description>
<p>A node which identifies the grove we want the document release info for.
</parameter>
</argumentlist>
</codeprologue>
(define (document-release-info #!optional (nd (current-node)))
  (let* ((histkids (node-list-reverse (children (getdocinfo 'history nd))))
	 ; vers-and-change returns a list:
	 ; (last-version last-distribution last-distribution-or-change)
	 ; Either of the last two may be false
	 (vers-and-change (let loop ((nl histkids)
				     (distrib #f)
				     (lastchange #f))
			    (if (string=? (gi (node-list-first nl))
					  (normalize "version")) ; VERSION
				(list (node-list-first nl)
				      distrib
				      lastchange)
				(if (string=? (gi (node-list-first nl))
					      (normalize "distribution"))
				    (loop (node-list-rest nl) ; DISTRIBUTION
					  (or distrib
					      (node-list-first nl))
					  (or lastchange
					      (node-list-first nl)))
				    (loop (node-list-rest nl) ; CHANGE
					  distrib
					  (or lastchange
					      (node-list-first nl)))))))
	 (dist-or-change-date (and (caddr vers-and-change)
				   (attribute-string (normalize "date")
						     (caddr vers-and-change))))
	 (docdate (getdocinfo 'docdate nd))) ;false if no docdate element
    (list (or (and (car vers-and-change)
		   (attribute-string (normalize "date")
				     (car vers-and-change)))
	      dist-or-change-date
	      (and docdate
		   (trim-data docdate)))
	  (or dist-or-change-date
	      (and docdate
		   (trim-data docdate)))
	  (and (car vers-and-change)
	       (attribute-string (normalize "number")
				 (car vers-and-change)))
	  (and (cadr vers-and-change)
	       (attribute-string (normalize "string")
				 (cadr vers-and-change))))))
</func>


<func>
<codeprologue>
<routinename>
<name>format-date
</name></routinename>
<description>
<p>Returns a string with the formatted version of the date.  If the
string is not in the correct format,  
it returns the input string, and evaluates the <code/(error)/
function.  I'd like to use (error) at the end, rather 
than silently returning just d, but I cannot work out how to
evaluate more than one expression one after another!
<returnvalue type="string">Formatted into english</returnvalue>
<argumentlist>
<parameter>
<name>d
<type>string
<description>
<p>The string should be in the form dd-MMM-yyyy (two-digit day,
3-uppercase-character month appreviation, four-digit year)
</description>
</parameter>
</argumentlist>
<history>
<change author="ng" date="19-MAR-1999">
<p>Altered from original yyyymmdd format.
</change>
</history>
</codeprologue>
(define (format-date d)
  (let* ((strok (and d
		     (string? d)
		     (equal? (string-length d) 11)))
	 (year (and strok (substring d 7 11)))
	 (month (and strok (case (substring d 3 6)
			     (("JAN") (cons "January" 31))
			     (("FEB") (cons "February" 29))
			     (("MAR") (cons "March" 31))
			     (("APR") (cons "April" 30))
			     (("MAY") (cons "May" 31))
			     (("JUN") (cons "June" 30))
			     (("JUL") (cons "July" 31))
			     (("AUG") (cons "August" 31))
			     (("SEP") (cons "September" 30))
			     (("OCT") (cons "October" 31))
			     (("NOV") (cons "November" 31))
			     (("DEC") (cons "December" 31))
			     (else #f))))
	 (day (and strok (string->number (substring d 0 2)))))
    (if (and strok year month (and (<= day (cdr month)) (>= day 1)))
	(string-append (number->string day) " "
		       (car month) " "
		       year)
	(let ((nothing (error (if (string? d)
				  (string-append "Malformed date: " d)
				  "No string for format-date"))))
	  d))))
</func>


<func>
<codeprologue>
<routinename>
<name>format-date-old
</name></routinename>
<description>
<p>Returns a string with the formatted version of the date, which
should be in the form yyyymmdd.  If the string is not in this format, 
it returns a string indicating this (if only there were a 
(warning) primitive.  I'd like to use (error) at the end, rather
than silently returning just d, but I cannot work out how to
evaluate more than one expression one after another!
<p>Replaced by <code/(format-date)/, which parses dates in the form
dd-MMM-yyyy.
</p></description>
<returnvalue type="string">Formatted into english</returnvalue>
<argumentlist>
<parameter>
<name>d
<type>string
<description>
<p>The string should be in the form yyyymmdd
</codeprologue>
(define (format-date-old d)
  (let* ((strok (and d
		     (string? d)
		     (equal? (string-length d) 8)))
	 (year (and strok (substring d 0 4)))
	 (month (and strok (case (substring d 4 6)
			     (("01") (cons "January" 31))
			     (("02") (cons "February" 29))
			     (("03") (cons "March" 31))
			     (("04") (cons "April" 30))
			     (("05") (cons "May" 31))
			     (("06") (cons "June" 30))
			     (("07") (cons "July" 31))
			     (("08") (cons "August" 31))
			     (("09") (cons "September" 30))
			     (("10") (cons "October" 31))
			     (("11") (cons "November" 31))
			     (("12") (cons "December" 31))
			     (else #f))))
	 (day (and strok (string->number (substring d 6 8)))))
    (if (and strok year month (and (<= day (cdr month)) (>= day 1)))
	(string-append (number->string day) " "
		       (car month) " "
		       year)
	(let ((nothing (error (if (string? d)
				  (string-append "Malformed date: " d)
				  "No string for format-date"))))
	  d))))
</func>

<!-- now scoop up the remaining common functions, from sl-gentext.dsl -->
<misccode>
<miscprologue>
<description>
<p>Various strings (document in more detail!)
</description>
</miscprologue>
&sl-gentext.dsl
</misccode>
