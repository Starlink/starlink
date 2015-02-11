<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [
  <!ENTITY params.dsl PUBLIC "-//Starlink//TEXT DSSSL Common Parameterisation//EN">
  <!ENTITY sl-gentext.dsl	SYSTEM "sl-gentext.dsl">
]>
<!-- $Id$ -->

<title>Common functions for the Starlink stylesheets

<codegroup>
<title>Common functions for the Starlink stylesheets
<description>
<p>These are functions and handlers common to both the HTML and print
versions of the Starlink stylesheets.  Library functions are in another file.

<p>Note that Jade only supports the DSSSL Online subset of DSSSL, so some
more advanced features will be missing.
See:
<url>http://sunsite.unc.edu/pub/sun-info/standards/dsssl/dssslo/do960816.htm</url> 
and
<url>http://www.jclark.com/jade/#limitations</url> for further details.

<authorlist>
<author id=ng affiliation='Starlink, Glasgow'>Norman Gray</author>

&params.dsl;

<routine>
<routinename>getdocinfo
<description>
<p>Obtain the specified child of the docinfo element for the current grove.
That is, <code>(getdocinfo 'title)</code> returns the current document's title
<returnvalue type="node-list">Return a node-list consisting of the
  specified child, or false if there is no such child.
<argumentlist>
<parameter>type
  <type>symbol
  <description>
  <p>Symbol giving the name of one of the children of the docinfo element
<parameter optional default='(current-node)'>nd
  <type>node-list
  <description>
  <p>A node-list which indicates the grove which is to supply the
  document element.  If omitted, it defaults to the current node, and
  the document element corresponding to the current grove is obtained
  and returned.
<codebody>
(define (getdocinfo type #!optional (nd (current-node)))
  (let* ((docelem (document-element nd))
	 (dinl (select-elements (children (select-elements (children docelem)
							            'docinfo))
				type)))
    (if (node-list-empty? dinl)
	#f
	dinl)))

<routine>
<routinename>getdocbody
<description>
<p>Obtain the specified child of the docbody element for the current grove.
That is, <code>(getdocbody 'abstract)</code> returns the current document's 
abstract.
<returnvalue type="node-list">Return a node-list consisting of the 
  specified child, or false if there is no such child.
<argumentlist>
<parameter optional default='#f'>type
  <type>symbol
  <description>
  <p>Symbol giving the name of one of the children of the docbody element.  
  If this is given as <code>#f</code>, or omitted, then the docbody element itself
  is returned.
<parameter optional default='(current-node)'>nd
  <type>node-list
  <description>
  <p>A node-list which indicates the grove which is to supply the
  document element.  If omitted, it defaults to the current node, and
  the document element corresponding to the current grove is obtained
  and returned.
<codebody>
(define (getdocbody #!optional (type #f) (nd (current-node)))
  (let* ((docelem (document-element nd))
	 (db (and docelem
		  (select-elements (children docelem) 'docbody)))
	 (dinl (if db
		   (if type
		       (select-elements (children db) type)
		       db)
		   (empty-node-list))))
    (if (node-list-empty? dinl)
	#f
	dinl)))

<routine>
<routinename>getdocnumber
<description>
Return the current node's document number as a string.  If the second optional
argument is true, then return the document number as a longer description
rather than a code.  That is, as `Starlink Cookbook n' rather than `SC/n'
<returnvalue type=string>The document number as a string, or <code>#f</code> 
if <code>docnumber</code> isn't defined.
<argumentlist>
<parameter optional default='(current-node)'>
  <name>nd
  <type>node-list
  <description>
  <p>If present, this indicates which grove is to be used to find the
  document element.
<parameter optional default='#f'>
  <name>asString
  <type>boolean
  <description>If true, return a longer descriptive string rather than a code.
<codebody>
(define (getdocnumber #!optional (nd (current-node)) (asString #f))
  (let* ((dn (getdocinfo 'docnumber nd))
	 (doctype (and dn
		       (attribute-string (normalize "documenttype") dn)))
	 (docelemtype (and dn
			   (if doctype
			       (if asString
				   (string-append (code-to-string doctype)
						  " ")
				   (string-append (case-fold-up doctype) "/"))
			       (error "DOCNUMBER has no DOCUMENTTYPE")))))
    (if dn
	(string-append docelemtype
		       (if (attribute-string "UNASSIGNED" dn)
			   "??"
			   (normalize-string (data dn))))
	#f	
	)))

<routine>
<routinename>getdocdate
<description>Return the document's release date as a string.
<argumentlist>
<parameter optional default='(current-node)'>
  <name>nd
  <type>node-list
  <description>
  <p>If present, this indicates which grove is to be used to find the
  document element.
<codebody>
(define (getdocdate #!optional (nd (current-node)))
  (let ((rel (document-release-info)))
    (if (or (car rel)
	    (cadr rel)
	    (cadddr rel))
	(string-append (if (car rel)
			   (format-date (car rel))
			   "not released")
		       ". "
		       (if (cadddr rel)
			   (string-append
			    "Release "
			    (cadddr rel)
			    ". ")
			   "")
		       (if (cadr rel)
			   (string-append
			    "Last updated "
			    (format-date (cadr rel)))
			   ""))
	"not released")))

<routine>
<routinename>getdocauthors
<description>Return a list of the author names as a sosofo.
<argumentlist>
<parameter optional default='(current-node)'>
  <name>nd
  <type>node-list
  <description>
  <p>If present, this indicates which grove is to be used to find the
  document element.
<codebody>
(define (getdocauthors #!optional (nd (current-node)))
   (let ((authors (children (getdocinfo 'authorlist))))
         (let loop ((to (empty-sosofo))
                    (from authors))
            (if (node-list-empty? from)
                to
                (loop
                   (sosofo-append
                      to
                      (process-node-list (node-list-first from))
                      (if (node-list-empty? (node-list-rest from))
                          (empty-sosofo)
                          (literal ", ")))
                   (node-list-rest from))))))


<routine>
<routinename>code-to-string
<description>Map a document code (SC, SUN, etc) to a string
<returnvalue type=string>Document class
<argumentlist>
<parameter>doccode
  <type>string
  <description>One of the Starlink document codes
<codebody>
(define (code-to-string doccode)
  (case (case-fold-down doccode)
    (("sun") "Starlink User Note")
    (("ssn") "Starlink System Note")
    (("mud") "Miscellaneous User Document")
    (("sc") "Starlink Cookbook")
    (("sg") "Starlink Guide")
    (("sgp") "Starlink General Paper")
    (("sug") "Starlink Users' Guide")
    (else (error (string-append "Unknown document code " doccode)))))

<routine>
<routinename>stringlist->string
<description>Turn a list of strings into one string, 
with elements separated by a constant string.
<returnvalue type=string>Concatenated string
<argumentlist>
<parameter>list
  <type>list of strings
  <description>List of strings to be joined
<parameter>begin-string
  <type>string
  <description>String to start the list
<parameter>sep-string
  <type>string
  <description>String to be inserted between each of the elements
  in <code>list</code>.
<parameter>end-string
  <type>string
  <description>String to end the list
<codebody>
(define (stringlist->string list begin-string sep-string end-string)
  (let loop ((l list)
	     (allstrings begin-string))
    (if (null? l)
	allstrings
	(loop (cdr l)
	      (string-append allstrings
			     (car l)
			     (if (null? (cdr l))
				 end-string
				 sep-string))))))

<routine>
<routinename>make-section-reference
<description>
<p>Return a sosofo with the title of the given node,
prefixed by its section number.  If the optional argument ts is given, it
is used as the title sosofo, rather than extracting it from the node.
<p>You can generate a reference to a node either by calling this 
function with that node as argument, or by processing the node in
the <code>section-reference</code> mode (which might well call this function in
turn.  In the former case, you can include keyword attributes.
<p>This function should be able to generate a reference to at least
all the functions listed in the returnvalue of function
<funcname>section-element-list</funcname>
(mmm, how about <funcname>target-element-list</funcname>?).
<p>In the body of the function, we call <funcname>process-node-list</funcname> on the 
first child of the target node.  This is because we call this function for 
elements both within the Starlink General DTD and within the DocumentSummary
DTD.  The former has sub*sect enclosing subhead enclosing title, and the
latter has sub*sect enclosing title, and in both cases it is the title we 
wish to get at.
<p>Note that, if you specify both the <code>title</code> and <code>set-prefix</code>
keywords, you seem to be doing all the work.  You should still call this
routine in that case, however, so that it can do any other processing
required.
<returnvalue type=sosofo>Name of section, prefixed by section number
<argumentlist>
<parameter keyword default='(current-node)'>target
  <type>singleton-node-list
  <description>The node we wish to generate a reference to
<parameter keyword default='extracted from node'>title
  <type>sosofo
  <description>A sosofo specifying a title to be used, rather than
  extracting it from the node
<parameter keyword default='#f'>specify-type
  <type>boolean
  <description>If true, then include the section's type (ie, appendix or
  section) in the reference.
<parameter keyword default='#f'>set-prefix
  <type>sosofo
  <description>If provided, this overrides the section-number prefix
<parameter keyword default='#f'>short-ref
  <type>boolean
  <description>If true, then generate only a short reference (which
  currently means, just the prefix)
<codebody>
(define (make-section-reference #!key (target (current-node))
				      (title #f)
				      (specify-type #f)
				      (set-prefix #f)
				      (short-ref #f))
  (let* ((in-backmatter (or (equal? (gi target)
				    (normalize "backmatter"))
			    (have-ancestor? (normalize "backmatter")
					    target)))
	 (prefix (or set-prefix		; set-prefix overrides
		     (if in-backmatter
		      (empty-sosofo)
		      (let* ((hier-nums (element-number-list
					 (list
					  (normalize "appendices")
					  (normalize "sect")
					  (normalize "subsect")
					  (normalize "subsubsect")
					  (normalize "subsubsubsect"))
					 target))
			     (inappendix (> (car hier-nums) 0))
			     (sn (let loop ((res "")
					    (nums (cdr hier-nums))
					    (fmts (if inappendix
						      %appendix-fmts
						      %section-fmts))
					    (sep ""))
				   (if (or (null? nums)
					   (= (car nums) 0))
				       res
				       (loop (string-append
					      res
					      sep
					      (format-number (car nums)
							     (car fmts)))
					     (cdr nums)
					     (cdr fmts)
					     ".")))))
			(literal (if specify-type
				     (if inappendix "Appendix " "Section ")
				     "")
				 sn))))))
    (make sequence
      prefix
      (if short-ref
	  (empty-sosofo)
	  (make sequence
	    (literal " ")
	    (if title
		title
		(with-mode section-reference ; in case we're not already
		  ;; process the first child (see note above)
		  (process-node-list (node-list-first (children target)))
		  ))))
      (if show-element-ids
	  (literal (display-element-ids target))
	  (empty-sosofo))
      )))

<routine>
<routinename>display-element-ids
<description>Called (typically) when show-element-ids is true, this displays
the ID of the indicated element.
<returnvalue type=string>The ID, if available, or empty.
<argumentlist>
<parameter optional default='(current-node)'>nd
  <type>singleton-node-list<description>The node we want the IDs of
<codebody>
(define (display-element-ids #!optional (nd (current-node)))
  (let ((id (attribute-string (normalize "id") nd))
	(export? (attribute-string (normalize "export") nd)))
    (if id
	(if export?
	    (string-append " ["  (case-fold-down id) "]")
	    (string-append " [[" (case-fold-down id) "]]"))
	"")))

<routine>
<routinename>section-element-list
<purpose>List the elements classified as `sections'.
<description>
<p>Element lists.
<p>The <funcname>section-element-list</funcname> is the list elements which are to be
taken to be `sections', for the purposes of navigation, and for
generating tables of contents.  For the <funcname>make-contents</funcname>
function to work, there should be no `missing levels' -- anything
in this list should have its parent also in this list, apart from
elements which have docbody as their parent.
<p>References to the functions in this list should be generated by
function <funcname>make-section-reference</funcname>.
<p>See also <funcname>chunk-element-list</funcname>.
<p>This has to be a function because it's evaluated when
there is a current-node so that normalize can know what declaration
is in effect.
<returnvalue type=list>List of GIs
<codebody>
(define (section-element-list)
  (list (normalize "abstract")
        (normalize "sect")
	(normalize "subsect")
	(normalize "subsubsect")
	(normalize "subsubsubsect")
	(normalize "appendices")
	;(normalize "routinelist")
	;(normalize "codecollection")
	;(normalize "backmatter")
	;(normalize "notecontents")
	;(normalize "bibliography")
	;(normalize "programcode")	;in programcode DTD
	(normalize "codegroup")
	(normalize "routine")
	))

<routine>
<description>
<p>Section number formatting
<p>Various routines for obtaining and formatting section headings.
<codebody>
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

(define (sectlevel #!optional (sect (current-node)))
  (cond
   ((equal? (gi sect) (normalize "subsubsubsect")) 4)
   ((equal? (gi sect) (normalize "subsubsect")) 3)
   ((equal? (gi sect) (normalize "subsect")) 2)
   ((equal? (gi sect) (normalize "sect")) 1)
   (else 1)))

;; This mode is added to elsewhere, for example, the code for the back-matter
;; defines FO constructors in this mode.
(mode section-reference
  (element abstract
    (literal "Abstract"))
  (element sect
    (make-section-reference))
  (element subsect
    (make-section-reference))
  (element subsubsect
    (make-section-reference))
  (element subsubsubsect
    (make-section-reference))
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
					(target-element-list))))
  (element p
    (process-node-list (ancestor-member (current-node)
					(target-element-list))))
  (element table
    (literal (car (get-caption-details))))
  (element figure
    (literal (car (get-caption-details))))
  ;; Following definition of SUBHEAD appropriate for the DISPLAYTITLE
  ;; element which I've currently tentatively removed from the DTD
  ; (element subhead			;so we _do_ get inside the subhead
  ;     (let ((disp (select-elements (children (current-node))
  ; 				 (normalize "displaytitle"))))
  ;       (if (node-list-empty? disp)
  ; 	  (process-children-trim)
  ; 	  (process-node-list disp))))
  (element subhead			; so we _do_ get inside the subhead
    (process-children-trim))
  )

<routine>
<routinename>get-caption-details
<purpose>Return generated details for a caption.
<description>
<p>Return generated details for a caption.
<p>The identifier which is returned must be consistent with the
fragment which would be generated by function <funcname>href-to</funcname> in
the HTML stylesheet.  This is satisfied by returning simply the
element's ID.
<returnvalue type='list of strings'>A list containing, in order,
   the caption legent (eg, `Fig. 1.3'); the caption number as a string (eg,
   `1.3'); either a unique identifying string  
  which is usable as a link target, or <code>#f</code> if the element's ID attribute
  is implied.  The figure/table number is returned separately so it
  can be put to different uses (eg, as an entry in a list of figures). 
<argumentlist>
<parameter optional default='(current-node)'>nd
  <type>singleton node-list<description>An element (typically the
  <code>TABLE</code> or <code>FIGURE</code> element the caption is a member of)
  which has an ID attribute, and which will be used to generate the 
  caption reference.
<codebody>
(define (get-caption-details #!optional (nd (current-node)))
  (let ((num (number->string (element-number nd)))
	(id (attribute-string (normalize "id") nd)))
    (list (string-append (case-fold-capitalise (gi nd)) " " num)
	  num
	  id)))

<routine>
<routinename>case-fold-capitalise
<description>Make all of a string lowercase, but with first char uppercase.
Modelled after <funcname>(case-fold-down)</funcname> and friends in dblib.dsl
<returnvalue type=string>Capitalised version of argument
<argumentlist>
<parameter>str
  <type>string<description>string to be modified
<codebody>
(define (case-fold-capitalise str)
  (if (string? str)
      (let ((sl (string->list str)))
	(apply string
	       (case-fold-up-char (car sl))
	       (case-fold-down-charlist (cdr sl))))
      str))

<routine>
<description>
<p>A couple of functions for trimming strings.  I need these because,
for example, I generate the output filename based on the document type
and number, and this goes wrong if they were specified with their
end-tags omitted, so that the string has returns or the like within
it.

<p>Given a list of characters, return the list with leading whitespace 
characters removed

<p>This function requires that <code>input-whitespace?</code> work.  This is
true in OpenJade but not in Jade 1.2.1 (see notes at
<url>http://www.jclark.com/jade/#limitations</url>).  In that case,
the char-property whitespace test has to be replaced by the
brute-force version:
<code>(not (or (equal? firstchar #\space)
	       (equal? firstchar #\&#TAB)
	       (equal? firstchar #\&#RE)))</code>
However this doesn't appear to work within openjade, for some reason
I don't fully undertand, to do with parentheses and ampersands, I think.

<codebody>
<!--
;; jade-1.2.1 version:
(define (*is-whitespace* c)
  (or (equal? c #\space)
      (equal? c #\&#TAB;)
      (equal? c #\&#RE;)
      (equal? c #\&#RS;)))
-->
;; openjade version:
(define (*is-whitespace* c)
  (char-property 'input-whitespace? c))


;; Remove any leading and trailing space, and collapse intermediate
;; whitespace to a single space character
(define (*normalize-character-list* l seen-space seen-nonspace)
  (if (null? l)
      '()
      (let ((firstchar (car l)))
        (cond ((*is-whitespace* firstchar)
               (*normalize-character-list* (cdr l) #t seen-nonspace))
              ((and seen-space seen-nonspace)
               (cons #\space
                     (cons firstchar
                           (*normalize-character-list* (cdr l) #f #t))))
              (else
               (cons firstchar
                     (*normalize-character-list* (cdr l) #f #t)))))))

(define (normalize-string s)
  (list->string
   (*normalize-character-list* (string->list s) #f #f)))

;; Shorthand: return trimmed data, or false if argument is false
;; (ie, don't just fail in this second case).  This would be better called
;; (normalize-data), but it isn't.
(define (trim-data nd)
  (if nd
      (normalize-string (data nd))
      #f))


<routine>
<routinename>index-file-name
<purpose>Return the filename of the document index.
<description>
Returns the filename to be used as entry point of the collection of
documents.  Note that this is different from the
<funcname>root-file-name</funcname>, which is a prefix applied to generated file
names.  In some cases, it might be sensible for
<funcname>index-file-name</funcname> to simply call <funcname>root-file-name</funcname>,
but that's not what we want to do in this case.
<p>This can be generated using the
document type and DOCNUMBER if present (which need not be the case for
all document types). Another way to set this might be through a
processing-instruction.
<p>An arguably better way to obtain a root/index file name is to 
root around in the grove and find the name of the source file,
then use that information to generate the output file name.
That should be possible, and it 
almost certainly involves the SosSequence property class (cf,
HyTime A.6.1, for example, or
<url>http://www.hytime.org/materials/sgmlpropset/classes/sosseq/index.htm</url>),
which is in the `formal system identifier abstract' (fsiabs) module of the
SGML property set.  This isn't, however, implemented in Jade.
<returnvalue type=string>Complete filename for the `entry-point' HTML file.
<argumentlist>
<parameter optional default='(current-node)'>
  nd<type>node-list<description>Node which identifies the grove 
  we want the root file name of
<codebody>
(define (index-file-name #!optional (nd (current-node)))
  (let* ((dn (getdocinfo 'docnumber nd))
	 (docelemtype (case-fold-down
		       (if dn
			   (if (attribute-string (normalize "documenttype") dn)
			       (attribute-string (normalize "documenttype") dn)
			       (error "DOCNUMBER has no DOCUMENTTYPE"))
			   (gi (document-element)))))
	 (docref (cond (dn (if (attribute-string (normalize "unassigned") dn)
			       "unassigned" ; better alternative?
			       (trim-data dn)))
		       ((getdocinfo 'docdate nd)
			(trim-data (getdocinfo 'docdate nd)))
		       (else  ; if no date, at least it should have a history
			(let ((rel (document-release-info)))
			  (car rel)))))
	 )
    (string-append docelemtype ;(gi (document-element))
		   (if docref docref ""))))

<routine>
<routinename>root-file-name
<purpose>Return the filename root.
<description>
Returns the filename to be used as the `root part' of the name when
generating HTML file names.  Note that this is not the same as the
<funcname>index-file-name</funcname>, which is the name of the `index' file
which is the entry point of the collection of documents.
<p>This is also used by the LaTeX stylesheet, when generating the names
of various auxiliary files.
<p>Note that the parameter <funcname>%override-root-file-name%</funcname> does <em>not</em>
override within this function, since this function is not merely used for
the name of the single root file.  See <funcname>html-file</funcname>.
<returnvalue type=string>Complete filename for the `entry-point' HTML file.
<argumentlist>
<parameter optional default='(current-node)'>
  nd<type>node-list<description>Node which identifies the grove 
  we want the root file name of
<codebody>
(define (root-file-name #!optional (nd (current-node)))
  "N")

<routine>
<routinename>document-release-info
<purpose>Extract release numbers and dates from history
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
Each element of the list is <code>#f</code> (as opposed to blank) if the
corresponding information is unavailable.

<p>If there's no history element, then the first two elements will be
the same, with the contents of the DOCDATE element, and the last
two will be <code>#f</code>.

<p>We don't check whether the dates are sensible (ie, whether the last
element really does have the latest date).

<returnvalue type=list>(version date version-number distribution-id)
<argumentlist>
<parameter optional default="(current-node)">
  <name>nd
  <type>node-list
  <description>A node which identifies the grove we want the document
    release info for.
<codebody>
(define (document-release-info #!optional (nd (current-node)))
  (let* ((hist (getdocinfo 'history nd))
	 (histkids (and hist
			(node-list-reverse (children hist))))
	 ;; vers-and-change returns a list:
	 ;; (last-version last-distribution last-distribution-or-change)
	 ;; Either of the last two may be false
	 (vers-and-change
	  (and hist
	       (let loop ((nl histkids)
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
				   (node-list-first nl))))))))
	 (dist-or-change-date (and hist
				   (caddr vers-and-change)
				   (attribute-string (normalize "date")
						     (caddr vers-and-change))))
	 (docdate (getdocinfo 'docdate nd))) ;false if no docdate element
    (list (or (and hist
		   (car vers-and-change)
		   (attribute-string (normalize "date")
				     (car vers-and-change)))
	      dist-or-change-date
	      (and docdate
		   (trim-data docdate)))
	  (or dist-or-change-date
	      (and docdate
		   (trim-data docdate)))
	  (and hist
	       (car vers-and-change)
	       (attribute-string (normalize "number")
				 (car vers-and-change)))
	  (and hist
	       (cadr vers-and-change)
	       (attribute-string (normalize "string")
				 (cadr vers-and-change))))))


<routine>
<routinename>get-link-policy-target
<purpose>Check that the `link policy' is satisfied by a target element.
<description>This function expresses the `link policy' we wish this
stylesheet to impose on the element (in a separate document) which is
the putative target of a link.

<p>Check link policy, and return a pair.  The <code>car</code> of the pair
is <code>#f</code> if the policy is satisfied, and a string otherwise (this
is an error, which should be signalled with the string as an
explanation, and no link should be made); the <code>cdr</code> is a string
URL giving the URL to be used, or <code>#f</code> if the policy is satisfied
but no link should be made (ie, if the <code>urllinkpolicy</code> is NONE).

<p>The link policy is as follows.
If the <code>documentsummary</code> element's <code>exportedlinkpolicy</code>
has the value <code>"exportedonly"</code>, we may only link to targets which
have the <code>export</code> attribute present and set to <code>"export"</code>:
that is, if we have <code>"exportedonly"</code> but no <code>export</code>, then
the policy is trivially satisfied, but we do not return any URL.  If
the <code>documentsummary</code> element's <code>urllinkpolicy</code> attribute is
<code>"automatic"</code>, then the <code>urlpath</code> attribute must not be
present, and we generate a URL based on the element's location in the
hierarchy; if it is <code>"explicit"</code>, the <code>urlpath</code> must be
present; if it is <code>"none"</code>, then the policy is satisfied, but no URL
should be returned.

<p>Irrespective of the link policy settings, we take it that it is
always permissable to link to the document element.

<p>Note that this function is designed to emit URLs as part of its response.
That is, it is partly specific to the stylesheet which generates HTML.  
The link policy, however, is <em>not</em> specific to HTML, and the link with
the HTML stylesheet is only because that stylesheet is the most sophisticated
one, to which is delegated such tasks as checking the link policy by calling
this function.  For these reasons, this function should indeed be in this
common set of functions.  It may be called from the LaTeX stylesheet, however,
in which case the keyed argument <code>no-urls</code> should be set true, to
prevent the function referencing the undefined (in that stylesheet)
variables <code>%starlink-document-server</code> and <code>href-to</code> (actually,
this fails to work, due to what may be either a bug, or
implementation-defined behaviour in Jade, so the calling file should specify
dummy values for these two variables).

<returnvalue type=pair>If <code>car</code> is true, the policy has been
violated and the <code>car</code> contains an error.  If <code>cdr</code> is true,
then a link should be made, using the URL in <code>cdr</code>.

<argumentlist>
<parameter>nd
  <type>singleton-node-list
  <description>The node we want to check.
<parameter keyword default='#f'>no-urls
  <type>boolean
  <description>If true, simply return <code>#t</code> or <code>#f</code> in the <code>cdr</code>
  of the pair, rather than returning a URL.

<codebody>
(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))

(define (get-link-policy-target nd #!key (no-urls #f))
  (let* (;; onlyexported is true if only exported IDs may be linked to
	 (linkpol (attribute-string (normalize "exportedlinkpolicy")
				    (document-element nd)))
	 (onlyexported (and linkpol
			    (string=? linkpol
				      (normalize "onlyexported"))))
	 ;; If veto-export is false, then we can link to this; if it's
	 ;; true, that's because this element's ID isn't exported, and
	 ;; this violates policy.
	 (ex (attribute-string (normalize "export") nd))
	 ;; If nd is the document-element, then permit the link
	 ;; always, by setting veto-export to #f.
	 (veto-export (if (node-list=? nd (document-element nd))
			  #f		;never veto link to document element
			  (and onlyexported
			       (not (and ex
					 (string=? ex
						   (normalize "export")))))))
	 (urlpolicy (attribute-string (normalize "urllinkpolicy")
				      (document-element nd)))
	 (urlpath (attribute-string (normalize "urlpath") nd))
	 (id (or (attribute-string (normalize "id") nd)
		 "")))
    (if veto-export
	(cons (string-append "The element with id " id
			     " has not been exported, so may not be linked to")
	      #f)
	(case (case-fold-down urlpolicy) ;compare case-insensitively
	  (("none")
	   (cons #f #f))	; policy satisfied - no link
	  (("explicit")
	   (if urlpath
	       (cons #f (or no-urls
			    (string-append %starlink-document-server%
					   urlpath
					   ;; include xref for hlink
					   "#xref_"
					   (or id ""))))
	       (cons (string-append "element with id " id
				    " has no URLPATH attribute")
		     #f)))
	  (("automatic")
	   (if urlpath
	       (cons (string-append "element with id " id
				    " has an URLPATH attribute present")
		     #f)
	       (cons #f (or no-urls
			    (href-to nd full-url: #t)))))
	  (else
	   (cons (string-append "Unknown URLPOLICY: " urlpolicy) #f))))))

<routine>
<routinename>target-element-list
<purpose>A list of elements which are allowable targets for a link.
<description>
Return a list of elements which are allowable targets for a link.
This list primarily expresses those elements which this stylesheet is able
to generate a link to -- it's more to do with this stylesheet's capabilities
than with any fundamental property of the document type.

<p>This will be used when linking to (an ID attribute within) an
object which can't be linked to directly.  It doesn't matter if
there's redundancy in this -- the appropriate element is selected as
the first member of this list amongst the target element's ancestors.

<p>Note that we include "routine" in the list, even though that
element doesn't appear in the General DTD, so that links to routines from
that DTD must be done via the coderef element.  This allows us to link
within a programcode element, to other elements <em>within the same
document</em>.  There is currently no means of referring, from a
programcode document, to elements in another programcode document.

<p>The link text will be generated by function
<funcname>make-section-reference</funcname>.

<p>For example, the LABEL element in the DocumentSummary DTD requires this.
<returnvalue type='list of strings'>List of allowable link targets
<codebody>
(define (target-element-list)
  (list (normalize "abstract")
        (normalize "sect")
	(normalize "subsect")
	(normalize "subsubsect")
	(normalize "subsubsubsect")
	(normalize "appendices")
	(normalize "figure")
	(normalize "table")
	(normalize "mlabel")
	(normalize "routine")
	(normalize "dt")
	(normalize "li")))

<routine>
<routinename>idindex-element-list
<description><p>Return a list of elements which are to be included in the 
idindex generated in the backmatter.  In principle, this is identical
to <funcname>target-element-list</funcname>, but in practice it
appears to be extremely expensive to do this for the
<code>routine</code> elements, so we here generate a different list
which has those elements removed.  In general, we wouldn't much want
to include these in the idindex in any case.
<p>No, just use <funcname>target-element-list</funcname>.  Function
<funcname>make-idindex</funcname> is amazingly slow, but for some
different, unknown, reason than this.
<returnvalue type='list of strings'>List of elements to include in the
idindex
<codebody>
(define (idindex-element-list)
  (target-element-list))

<routine>
<routinename>ref-target-element-list
<description>This is almost the same as
  <funcname>target-element-list</funcname>, except that it lists those elements
  which are an allowable target of a REF element.  The difference is
  that when we link to a P, the target of the REF is the P element,
  but the link that is generated is to the first ancestor of the P
  which is within <funcname>target-element-list</funcname>.  If there isn't
  this distinction, we get a processing loop.
<returnvalue type='list of strings'>List of ref targets
<codebody>
(define (ref-target-element-list)
  (append (target-element-list)
	  (list (normalize "p"))))

<routine>
<routinename>document-element
<description>
<p>Returns the document element of the document containing the given
node.
<p>Only the <code>SgmlDocument</code> node class
exibits a <code>DocumentElement</code> property, so to find the document element
we first have to find the grove root, which we do by examining the 
<code>grove-root</code> property of the current node.  The only node which doesn't
have a <code>grove-root</code> property (so that the <funcname>node-property</funcname>
routine will correctly return <code>#f</code> -- ie, it exhibits the property, but
with the value <code>#f</code>) is the root node, but in that case,
<funcname>current-node</funcname> returns the grove root directly (this isn't clear
from the standard -- see the discussion on `Finding the root element' in
the dssslist archive at
<url>http://www.mulberrytech.com/dsssl/dssslist/</url>).
<p>The subsequent calls to <funcname>node-property</funcname> default <code>#f</code> if 
the property is not exhibited by the node.  This catches the case where the
grove root doesn't have any <code>document-element</code> property, for example if
the grove is malformed because it resulted from a call to <funcname>sgml-parse</funcname>
with a non-existent file.
<returnvalue type="singleton-node-list">The document element, or
<code>#f</code> if not found.
<argumentlist>
<parameter optional default='(current-node)'>
  <name>node
  <type>node-list
  <description>this node indicates the grove we want the document element
  of.
<codebody>
(define (document-element #!optional (node (current-node)))
  (let ((gr (node-property 'grove-root node)))
    (if gr				; gr is the grove root
	(node-property 'document-element gr default: #f)
	;; else we're in the root rule now
	(node-property 'document-element node default: #f))))

<routine>
<routinename>document-element-from-fsi
<description>
  <p>Return the document element of the document referred to by the
  FOSI passed as argument.  
  Uses <funcname>sgml-parse</funcname>: see 10179, 10.1.7.
  <p>This is <em>complicated</em>!  When <funcname>sgml-parse</funcname> is called, it
  is a completely different parse from the main one.  That means that
  it has the default SGML declaration, which has <code>NAMECASE
  GENERAL NO</code>.  Unless we prepend the correct declaration, this isn't
  parsed properly.  The entity <funcname>%starlink-decl-entity%</funcname> is
  defined in the General DTD to point to the declaration, and we must
  prepend this entity to the entity we have been asked to parse.  Do
  this by calling <funcname>entity-generated-system-id</funcname> on the entity.
  <p>The crucial thing here is to realise that a `formal system identifier'
  (the argument of <funcname>sgml-parse</funcname>) is <em>not</em> necessarily a
  single file.  As described in clause A.6 (specifically A.6.4.3) of
  the HyTime spec, and also on the <webref
  url='http://www.jclark.com/sp/sysid.htm' >SP pages</webref>, it can
  consist of several <em>storage object identifiers</em>, which are
  concatenated.  A simple filename, lacking the SOS start-tag, is an
  `informal system identifier'.
  <p>This will most often be used via
  <funcname>document-element-from-entity</funcname> or
  <funcname>document-elememt-from-sysid</funcname>.
<returnvalue type="node-list">Document element, or <code>#f</code> on error.
<argumentlist>
<parameter>fsi
  <type>string
  <description>String containing FSI.
<parameter keyword default='%starlink-decl-entity%'>prepend-decl
  <type>string
  <description>If true, prepend the given SGML declaration
<codebody>
(define (document-element-from-fsi fsi
				   #!key
				   (prepend-decl %starlink-decl-entity%))
  (let* ((decl-sysid (and prepend-decl
			  (entity-generated-system-id prepend-decl)))
	 (full-fsi (if decl-sysid
		       (string-append decl-sysid fsi)
		       (if prepend-decl ;should have been able to get sysid
			   (error (string-append
				   "Can't get sysid from entity "
				    prepend-decl))
			    fsi))))
    (if full-fsi
	(document-element (sgml-parse full-fsi))
	(error (string-append "Can't generate file from system-id " fsi)))))

(define (document-element-from-entity entname
				      #!key
				      (prepend-decl %starlink-decl-entity%))
  (document-element-from-fsi (entity-generated-system-id entname)
			      prepend-decl: prepend-decl))

(define (document-element-from-sysid str
				     #!key
				     (prepend-decl %starlink-decl-entity%))
  (document-element-from-fsi (string-append "<" "OSFILE>" str)
			      prepend-decl: prepend-decl))

<routine>
<routinename>isspace?
<description>Returns true if the argument is a whitespace character, or if
  it has the value <code>#f</code>.
<returnvalue type=boolean>True if whitespace
<argumentlist>
<parameter>c<type>character<description>Character to be tested
<codebody>
(define (isspace? c)
  (or (not c)
      (char=? c #\space)
      (char=? c #\&#TAB)))


<routine>
<routinename>format-date
<description>
<p>Returns a string with the formatted version of the date.  If the
string is not in the correct format,  
it returns <code>#f</code>.
<returnvalue type="string">Formatted into english</returnvalue>
<argumentlist>
<parameter>
  <name>d
  <type>string
  <description>
  <p>The string should be either in the form dd-MMM-yyyy (one- or two-digit day,
  3-character month abbreviation, four-digit year), and may include whitespace.
  Alternatively, it may be in the form of an RCS/CVS date:
  <code>$Date$</code>
<history>
<change author="ng" date="19-MAR-1999">
<p>Altered from original yyyymmdd format.
</change>
<change author="ng" date="20050727">
<p>Altered/extended to allow dates in multiple formats
</history>
<codebody>
(define (format-date d)
  (or (format-date-ddmmmyyyy d)
      (format-date-rcs d)
      (format-date-yyyymmdd d)))

(define (format-date-rcs d)
  ;; $Date$
  (and (string=? (substring d 0 6) "$Date:")
       (let ((date (cadr (tokenise-string d))))
         (string-append (substring date 8 10)
                        " "
                        (case (substring date 5 7)
                          (("01") "January")
                          (("02") "February")
                          (("03") "March")
                          (("04") "April")
                          (("05") "May")
                          (("06") "June")
                          (("07") "July")
                          (("08") "August")
                          (("09") "September")
                          (("10") "October")
                          (("11") "November")
                          (("12") "December"))
                        " "
                        (substring date 0 4)))))

(define (format-date-ddmmmyyyy d)
  ;; dd-MMM-yyyy (where MMM is a three-character month abbreviation)
  (let* ((parts (tokenise-string d boundary-char?: (lambda (c)
						     (or (char=? c #\-)
							 (char=? c #\space)))))
	 (cpts (if (string=? (car parts) "")
		   (cdr parts)
		   parts))
	 (strok (equal? (length cpts) 3))
	 (day (and strok (string->number (car cpts))))
	 (month-str (and strok (cadr cpts)))
	 (year (and strok (caddr cpts)))
 	 (month (and strok (case (case-fold-up month-str)
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
	 )
    (if (and strok year month (and (<= day (cdr month)) (>= day 1)))
	(string-append (number->string day) " "
		       (car month) " "
		       year)
        #f)))

(define (format-date-yyyymmdd d)
  ;; yyyymmdd
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
        #f)))


<routine>
<routinename>tokenise-string
<description>Tokenises a string, breaking at arbitrary character classes
<returnvalue type=list>List of strings, each containing a single token
<argumentlist>
<parameter>str<type>String<description>String to be tokenised
<parameter keyword default='break at spaces'>boundary-char?
  <type>function
  <description>Character-class function, which takes a single
  character as argument, and returns true if it should be categorised
  as whitespace, and false otherwise.  If this is insufficiently
  flexible, then the <funcname>isbdy?</funcname> function can be used.  The
  default is the function <funcname>isspace?</funcname>
<parameter keyword default='break at spaces'>isbdy?
  <type>function
  <description>General character-class function, which takes as
  argument a list of characters, and returns <code>#f</code>
  if the string should <em>not</em> be broken here (ie, if the string does
  not begin with separator characters).  If the string should
  be broken here, it returns the list of characters which consists of
  the remainder of the string with the leading separators removed.  For
  example, the function <code>(lambda (l) (if (and (char=? (car l)
  #\/) (char=? (cadr l) #\/)) (cddr l) #f))</code> breaks strings at
  double-slashes, but not single ones.  By default, the function
  removes strings of characters for which <funcname>boundary-char?</funcname> is true.
<parameter keyword default='no max'>max
  <type>integer
  <description>Integer which specifies the maximum number of splits
  which should be made.  Thus, for example, if max is passed as 1, the
  string will be split into a maximum of two pieces; if this is
  negative (the default) the string will be completely tokenised.
  Note that removal of initial whitespace counts as a `split'.
<codebody>
(define (tokenise-string str
			 #!key
			 (boundary-char? isspace?)
			 (isbdy? (lambda (l)
				   (if (boundary-char? (car l))
				       (let loop ((rest l))
					 (if (null? rest)
					     '()
					     (if (boundary-char? (car rest))
						 (loop (cdr rest))
						 rest)))
				       #f)))
			 (max -1))
  (let loop ((charlist (string->list str))
	     (wordlist '())
	     (currword '())
	     (splits max))
    (if (or (= splits 0)		;reached max split
	    (null? charlist))		;nothing more to do
	(let ((cw (append currword charlist)))
	  (if (null? cw)
	      wordlist
	      (append wordlist (list (list->string cw)))))
	(let ((nextword (isbdy? charlist)))
	  (if nextword
	      (loop nextword		;word just ended - add to list
		    (append wordlist (list (list->string currword)))
		    '()
		    (- splits 1))
	      (loop (cdr charlist)	;within word
		    wordlist
		    (append currword (list (car charlist)))
		    splits))))))

<routine>
<routinename>parse-fpi
<purpose>Parse a formal public identifier
<description>Breaks an FPI into its component parts. 
  <p>Returns an object which can be passed to the
  <funcname>query-parse-fpi</funcname> function.
  <p>It should be fairly robust, but it won't detect nonsense, so that
  it won't, for example, object to <code>hello//MyCorp//...</code> as
  an FPI.  Note that the separator
  between the text class and description is a <em>single</em> space.
  <p>See productions 79--90 in ISO 8879.
<returnvalue type='opaque object'>Object to be passed to
  <funcname>query-parse-fpi</funcname>.
<argumentlist>
<parameter>str<type>string<description>FPI to be parsed
<codebody>
(define (parse-fpi str)
  (let* ((frag-list (tokenise-string str
 				     isbdy?: (lambda (l)
 					       (if (and (char=? (car l) #\/)
 							(char=? (cadr l) #\/))
 						   (cddr l)
 						   #f))))
	 (reg (car frag-list))
	 (frag-list2 (if (or (string=? reg "+")
			     (string=? reg "-"))
			 ;; glue the first two strings back together
			 (append (list (string-append (car frag-list)
						      "//"
						      (cadr frag-list)))
				 (cddr frag-list))
			 frag-list))
	 (owner-id (if (>= (length frag-list2) 1)
		       (car frag-list2)
		       #f))
	 (tc-and-d (if (>= (length frag-list2) 2) ;text class and description
		       (tokenise-string (cadr frag-list2)
					;;split at _single_ space
					isbdy?: (lambda (l)
						  (if (isspace? (car l))
						      (cdr l)
						      #f))
					max: 1)
		       #f))
	 (text-class (and tc-and-d
			  (car tc-and-d)))
	 (uti (and tc-and-d
		   (>= (length tc-and-d) 2)
		   (string=? (cadr tc-and-d) "-"))) ;unavailable text indicator
	 (frag-list3 (if (>= (length frag-list2) 2)
			 (if uti
			     (cddr frag-list2)
			     (append (cdr tc-and-d) (cddr frag-list2)))
			 '()))
	 )
    (list (cons 'registered reg)
	  (cons 'owner-id owner-id)
	  (cons 'text-class text-class)
	  (cons 'unavailable uti)
	  (cons 'text-description (if (>= (length frag-list3) 1)
				      (car frag-list3)
				      #f))
	  (cons 'lang-or-des-seq (if (>= (length frag-list3) 2)
				     (cadr frag-list3)
				     #f))
	  (cons 'display-version (if (>= (length frag-list3) 3)
				     (caddr frag-list3)
				     #f))
	  )))


<routine>
<routinename>query-parse-fpi
<purpose>Query an FPI parsed using <funcname>parse-fpi</funcname>
<description>
  Given an object returned by function <funcname>parse-fpi</funcname>, this
  returns elements of it.
<returnvalue type=string>Requested part of FPI, or <code>#f</code> if not
  available.
<argumentlist>
<parameter>symbol
  <type>symbol
  <description>One of the symbols <code>'registered</code> (`+', `-' or
  `ISO...', or any other (wrong) text found here), <code>'owner-id</code>
  (the owner identifier, production [80]), <code>'text-class</code>
  ([86]), <code>'unavailable</code> (<code>#t</code>or <code>#f</code>,
  depending on whether the `unavailable text indicator' [85] was
  present), <code>'text-description</code> ([87]), <code>'lang-or-des-seq</code>
  (`public text language' [88] or 
  `public text designating sequence' [89]), or <code>'display-version</code>
  ([90]).
<parameter>parse-list
  <type>opaque
  <description>Object returned from <funcname>parse-fpi</funcname>
<codebody>
(define (query-parse-fpi symbol parse-list)
  (let ((p (assoc symbol parse-list)))
    (if p
	(cdr p)
	#f)))




<![ IGNORE [
<routine>
<routinename>tokenise-string
<description>Tokenises a string, breaking at arbitrary character classes
<returnvalue type=list>List of strings, each containing a single word
<argumentlist>
<parameter>str<type>String<description>String to be tokenised
<parameter optional default='isspace?'>isbdy?
  <type>procedure<description>Character-class function, which takes
  a single character argument, and returns true if the character
  is a token-separating character.  <funcname>isspace?</codebody> is a suitable
  such function, and is the default.
<codebody>
(define (tokenise-string str #!optional (isbdy? isspace?))
  (let ((sl (string->list str)))
    (let loop ((charlist sl)
	       (wordlist '())
	       (currword '()))
      (if (null? charlist)		;nothing more to do
	  (if (null? currword)
	      wordlist
	      (append wordlist (list (list->string currword))))
	  (if (isbdy? (car charlist))
	      (if (null? currword)
		  (loop (cdr charlist)	;skipping blanks
			wordlist
			'())
		  (loop (cdr charlist)	;word just ended - add to list
			(append wordlist (list (list->string currword)))
			'()))
	      (loop (cdr charlist)	;within word
		    wordlist
		    (append currword (list (car charlist)))))))))
]]>

<routine>
<routinename>get-sysid-by-notation
<purpose>Select an entity whose declared content is one of a set of
  different notations.
<description>Given a string which has a list of entities,
  this tokenises the list (at
  whitespace), then works through the list and returns the system-id of
  the first entity which has a declared notation which
  matches a string in the argument <code>req-not</code>.  This merely
  returns the first match: if you have a hierarchy of preferences, then
  call the function repeatedly.
<returnvalue type=string>System ID (ie, filename) of the first entity whose
  notation matches a notation in <code>req-not</code>.  Returns <code>#f</code> if none
  can be found.  There is a potential problem here, when an entity is
  referenced using a public ID -- should that go in the manifest?
<argumentlist>
<parameter>ent-list-string
  <type>string<description>A string containing a list of entities (such
  as an attribute value with a value prescription of ENTITIES), each of which
  has been declared to have a notation.
<parameter>req-not
  <type>list of strings<description>A list of notations, as defined in the DTD.
<codebody>
(define (get-sysid-by-notation ent-list-string req-not)
  (let loop ((ent-list (tokenise-string ent-list-string isspace?)))
    (cond ((null? ent-list)		;end of list - nothing found
	   #f)
	  ((member (entity-notation (car ent-list) (current-node))
		   req-not)
	   (entity-system-id (car ent-list) (current-node)))
	  (else (loop (cdr ent-list))))))

<routine>
<routinename>get-best-figurecontent
<purpose>Extract the `best' figure content.

<description>The FIGURE element has a content model which includes
  <code>FIGURECONTENT*, PX*</code>.  Each of the FIGURECONTENT
  elements has an `image' and a `notation' attribute.  If the `image'
  attribute is present, it refers to an entity which itself has a
  notation, and the element's content is that entity (the attribute
  has a CONREF default value).  If that attribute is not present, then
  the element's content is the image, and the `notation' attribute
  should be present (it is an application error if it is not).  This
  function should work through the list of elements it is presented,
  returning the first element which has a notation in
  <funcname>req-not</funcname>, or the list of paragraphs if none matches.  This
  merely returns the first match: if you have a hierarchy of
  preferences, then call the function repeatedly.

<returnvalue type='node-list'>Either a selected figurecontent node, or the
  collection of paragraphs that follow them.  Or <funcname>#f</funcname> if
  <em>nothing</em> works!

<argumentlist>
<parameter>nl
  <type>node-list
  <description>The list of FIGURECONTENT elements and paragraphs which is
    part of the content of the FIGURE element.
<parameter>req-not
  <type>list of strings
  <description>A list of notations, as defined in the DTD.

<codebody>
(define (get-best-figurecontent nl req-not)
  (if (or (not nl) (node-list-empty? nl))
      #f
      (let* ((this-one (node-list-first nl))
	     (is-figurecontent? (string=? (gi this-one)
					  (normalize "figurecontent")))
	     (ent (and is-figurecontent?
		       (attribute-string (normalize "image") this-one)))
	     (ent-notation (and ent
				(entity-notation ent (current-node))))
	     (cont-notation (and is-figurecontent?
				 (attribute-string (normalize "notation")
						   this-one))))
	(if is-figurecontent?
	    (if ent
		(if ent-notation	; entity is present and has notation
		    (if (member ent-notation req-not)
			this-one
			(get-best-figurecontent (node-list-rest nl) req-not))
		    (error (string-append "Entity " ent " has no notation")))
		(if cont-notation
		    (if (member cont-notation req-not)
			this-one
			(get-best-figurecontent (node-list-rest nl) req-not))
		    (error "FIGURECONTENT has neither image nor notation")))
	    nl				; return the trailing paragraphs
	    ))))

      </codebody>
    </routine>

    <routine>
      <routineprologue>
        <routinename>
          <name>figurecontent-to-notation-map</name>
        </routinename>
        <description>
          <p>The FIGURE element has a content model which includes
            <code>FIGURECONTENT*, PX*</code>.  Each of the FIGURECONTENT
            elements has an `image' and a `notation' attribute.  If the `image'
            attribute is present, it refers to an entity which itself has a
            notation, and the element's content is that entity (the attribute
            has a CONREF default value).  If that attribute is not
            present, then the element's content is the image, and the
            `notation' attribute should be present (it is an
            application error if it is not).</p>
          <p>This function takes a node-list NL, each node of which is
            either a FIGURECONTENT element or PX, and produces a list of
            pairs.  The car of each pair is the notation of the node,
            such as "eps" or "pdf", or "XML" in the case of PX; and
            the cdr is the node itself.  Note that if there is more
            than one node with a given notation, or more than one PX element,
            then the returned assoc list will have duplicates in the car
            fields of the pairs.</p>
        </description>
        <returnvalue type='list of pairs'>
          <p>A list of pairs, of the form <code>(notation
              . node-list)</code>, or <funcname>#f</funcname> if the
            node-list is empty.</p>
        </returnvalue>
        <argumentlist>
          <parameter>
            <name>nl</name>
            <type>node-list</type>
            <description>
              <p>The list of FIGURECONTENT elements and paragraphs
                which is part of the content of the FIGURE element.</p>
            </description>
          </parameter>
        </argumentlist>
      </routineprologue>
      <codebody>
(define (figurecontent-to-notation-map nl)
  (reverse
   (node-list-reduce
    nl
    (lambda (l n)
      (let* ((is-figurecontent? (string=? (gi n)
                                          (normalize "figurecontent")))
             (ent (and is-figurecontent?
                       (attribute-string (normalize "image") n)))
             (ent-notation (and ent
                                (entity-notation ent (current-node))))
             (cont-notation (and is-figurecontent?
                                 (attribute-string (normalize "notation") n))))
        (cons (if is-figurecontent?
                  (if ent
                      (if ent-notation   ;entity is present and has notation
                          (cons ent-notation n)
                          (error
                           (string-append "Entity " ent " has no notation")))
                      (if cont-notation  ;content has declared notation
                          (cons cont-notation n)
                          (error
                           "FIGURECONTENT has neither image nor notation")))
                  (cons "XML" n))
              
              l)))
    '())))
      </codebody>
    </routine>

<![INCLUDE[
<!-- redundant for ../latex/sltables.dsl, but not for ../html/sltables.dsl -->
<routine>
<routinename>table-colno
<purpose>Return a list of numbers, indicating the current column number and the
total number of columns.
<description>Checks only the <code>ENTRY</code> and <code>TGROUP</code> elements,
the first to find the column number, and the second to find the total
number of columns.
<returnvalue type='list of numbers'><p>A list of numbers, where
  the <code>car</code> is the current column number, 
  and the <code>cadr</code> is the total number of columns.
  <p>If the argument is not for an <code>ENTRY</code> element,
  then the current column number will be returned as zero.
  <p>If the node doesn't have a <code>TABLE</code> in its ancestry,
  then return <code>#f</code>.
<argumentlist>
<parameter optional default='(current-node)'>nd
  <type>singleton node-list<description>Node we want the column number of.
  If this is not an <code>ENTRY</code> element, then the column number will be
  returned as zero.
<codebody>
(define (table-colno #!optional (nd (current-node)))
  (let ((isentry (string=? (gi nd) (normalize "entry")))
	(tgroup-cols (inherited-element-attribute-string (normalize "tgroup")
							 (normalize "cols")
							 nd)))
    (if tgroup-cols
	(list (if isentry
		   (child-number (current-node))
		   0)
	       (string->number tgroup-cols))
	#f				; we're not in a table
	)))
]]>

<routine>
<routinename>get-updates
<description>
Scoop up all the update elements.
<returnvalue type='node-list'>All the update elements in the document, or #f
  if that would be an empty-node-list
<codebody>
(define (get-updates)
  (let ((nl (select-elements (select-by-class (descendants (document-element))
					      'element)
			     (normalize "update"))))
    (if (node-list-empty? nl)
	#f
	nl)))

<routine>
<routinename>node-list-or-false
<description>
Return a non-empty node-list, or false
<returnvalue type='node-list'>argument, if it's non-empty, <code>#f</code> otherwise.
<argumentlist>
<parameter>nl
  <type>node-list
  <description>node-list to be tested
<codebody>
(define (node-list-or-false nl)
  (if (node-list-empty? nl)
      #f
      nl))

<routine>
<routinename>get-equation-number
<description>
Return the equation number corresponding to the given equation node.
The node may be either an MLABEL element, or an element which has an
MLABEL child in the document instance.
<returnvalue type='string'>Equation number, or <code>#f</code> if the parameter
  is neither a MLABEL element, nor has an MLABEL child.
<argumentlist>
<parameter optional default='(current-node)'>nl
  <type>node-list
  <description>Equation to be numbered.
<codebody>
(define (get-equation-number #!optional (nl (current-node)))
  (let* ((mlabel-gi (normalize "mlabel"))
	 (mlabel (if (string=? (gi nl) mlabel-gi)
		     nl
		     (node-list-or-false
		      (select-elements (children nl) mlabel-gi)))))
    (if mlabel
	(number->string (element-number mlabel))
	#f)))

<routine>
<routinename>img-eqnref
<description>Returns a unique reference to an equation.  If the element does
  not have one of MEQUATION or MEQNARRAY in its ancestry (for example because
  it is an M element), then we won't need to refer to this label.  It must,
  however, be generated and be unique.
<returnvalue type=string>String usable as an ID attribute value.
<argumentlist>
<parameter optional default='(current-node)'>nd
  <type>node-list
  <description>A singleton node-list containing the element which is
  to be referred to.
<codebody>
(define (img-eqnref #!optional (nd (current-node)))
  (let* ((refable-ancestor (ancestor-member nd `(,(normalize "mequation")
						 ,(normalize "meqnarray"))))
	 (de (document-element nd))
	 (docprefix (if (string=? (gi de) (normalize "programcode"))
			(hash-of-tree de)
			"X")))
    (if (node-list-empty? refable-ancestor)
	(string-append docprefix "DEQ" (gi nd)
		       (number->string (element-number nd)))
	(string-append docprefix "EQ" (gi refable-ancestor)
		       (number->string (element-number refable-ancestor))))))

<routine>
<routinename>nl-to-pairs
<description>
Transform a node-list into a list of pairs, where each pair consists of the 
GI of the node, and the node.  This is useful when combined with
<funcname>assoc</funcname>.
<returnvalue type='list of pairs'>List of pairs of GI plus node.
<argumentlist>
<parameter>nl
  <type>node-list
  <description>A node-list to be transformed.
<codebody>
(define (nl-to-pairs nl)
  (node-list-reduce nl
		    (lambda (result nd)
		      (append result (list (cons (gi nd) nd))))
		    '()))

<routine>
<routinename>node-list-split-by-gi
<description>Split a node-list into a list of node-lists.
<returnvalue type='list of node-lists'>List of node-lists, each
one of which is either a single node whose GI is a member of `gilist',
or a non-empty sequence of nodes whose GIs are not members of
`gilist'.  Preserves order.
<argumentlist>
<parameter>nodelist
  <type>node-list
  <description>The list of nodes to be split
<parameter>gilist
  <type>list of strings
  <description>A list of GIs at which to split the node-list
<codebody>
(define (node-list-split-by-gi nodelist gilist)
  (let loop ((nllist '())		;list of node-lists
	     (tnl (empty-node-list))	;current node-list of non-matched nodes
	     (nl nodelist))		;unprocessed node-list
    (if (node-list-empty? nl)
	(if (node-list-empty? tnl) nllist (append nllist (list tnl)))
	(let ((g (gi (node-list-first nl))))
	  (if (and g (member g gilist))
	      (loop (append nllist (if (node-list-empty? tnl)
				       (list (node-list-first nl))
				       (list tnl (node-list-first nl))))
		    (empty-node-list)
		    (node-list-rest nl))
	      (loop nllist
		    (node-list tnl (node-list-first nl))
		    (node-list-rest nl)))))))

<routine>
<routinename>get-mediatypes
<purpose>Get any media attribute, and parse it.
<description>If the specified node has a `media' attribute, then this
routine extracts and parses it, according to the scheme described in
the HTML4 spec at
<url>http://www.w3.org/TR/REC-html40/types.html#h-6.13</url>. That is,
media types are separated by commas plus whitespace, and each media
type may have an arbitrary parameter string separated from it by
whitespace.
<returnvalue type='list of pairs'>A list of pairs, with the car of
each one the media type, and the cdr any parameterstring present.  If
there is no `media' attribute, return <code>#f</code>.
<argumentlist>
<parameter default='(current-node)'>nd
  <type>singleton-node-list
  <description>The node to be examined for a media attribute.
<codebody>
(define (get-mediatypes #!optional (nd (current-node)))
  (let ((medstr (attribute-string (normalize "media") nd)))
    (if medstr
	(map (lambda (s)
	       (let ((l (tokenise-string (normalize-string s) max: 1)))
		 (cons (car l) (if (> (length l) 1)
				   (cadr l)
				   #f))))
	     (tokenise-string (normalize-string medstr)
			      boundary-char?: (lambda (c) (char=? c #\,))))
	#f)))

<routine>
<routinename>hash-of-tree
<purpose>Return some hash of a tree
<description>Return some hash of a tree.  This is used, for example,
when we are generating filenames or equation labels within 
programcode documents: we can't look `up' into the parent document, so
we have to use this function to generate something unique to the
document. Returns a string which
should be reasonably unique to a node sub-tree.  Works by going down a
number of levels in the node tree, counting the number of children at
each level, and munging them with an RNG.
<p>Note that the (select-by-class) of the children of the node is
***extremely*** important: missing this out results in a substantial
slowdown in this function, resulting in a massive (factor of 10?!)
slowdown of the down-converter as a whole.  The (select-by-class)
disables a Jade optimisation which treats the characters in a string
as just that, rather than the formally correct node-list.  I can't
really detect this slowdown in the conversion of a trial document, but
perhaps it disables the optimisation on a wider scale, or frustrates
caching, or something similar.  The depth of the hash
doesn't *seem* to make much difference, but I've kept it smallish
just in case.
<argumentlist>
<parameter>nd<type>singleton-node-list<description>The node to be
hashed
<parameter>depth<type>number<description>The number of levels to go
down.  This should not be supplied by external uses of this function.
<returnvalue type='string'>A string which should be characteristic of
the node subtree.  Starts with a letter.
<codebody>
(define (hash-of-tree nd #!optional (depth #f))
  (if depth
      ;; this is an `internal' call
      (let ((kids (select-by-class (children nd) 'element)))
	(if (node-list-empty? kids)
	    1
	    (if (<= depth 0)
		(node-list-length kids)
		(node-list-reduce
		 kids
		 (lambda (res n)
		   (hash-mult res (hash-of-tree n (- depth 1))))
		 1))))
      ;; This is an `external' call -- call myself, and return number
      ;; as hex string.
      (string-append "X" (number->string (hash-of-tree nd 4) 16))))

;; Combine two numbers using a pseudo RNG.  This is a simple
;; linear-congruence generator, which finds the term in the sequence
;; next after the sum of the two arguments.
;; The numbers here are from the table of constants for the Quick and Dirty
;; RNGs in Numerical Recipes, section 7.1.
;; Other possibilities:
;;     ((   m    a    c )
;;      ( 6075  106 1283)
;;      ( 7875  211 1663)
;;      ( 7875  421 1663)
;;      ( 6075 1366 1283)
;;      ( 6655  936 1399)
;;      (11979  430 2531))
;;
(define (hash-mult x1 x2)
  (let ((m 11979)
	(a 430)
	(c 2531))
    (modulo (+ (* (+ x1 x2)
		  a)
	       c)
	    m)))


<routine>
<routinename>list-true
<description>Given a list, return the list with all <code>#f</code> elements deleted.
<codebody>
(define (list-true l)
  (if (null? l)
      l
      (if (car l)
	  (append (list (car l)) (list-true (cdr l)))
	  (list-true (cdr l)))))

<routine>
<routinename>list-difference
<description>
Given two lists <code>l</code> and <code>xl</code>, return a list
containing just those elements of <code>l</code> which are not present 
in <code>xl</code> (I'd have thought this was a standard function, but 
apparently not)
<returnvalue type="list">Filtered list of elements
<codebody>
(define (list-difference l xl)
  (cond ((null? l)
	 l)
	((member (car l) xl)
	 (list-difference (cdr l) xl))
	(else
	 (append (list (car l)) (list-difference (cdr l) xl)))))


<routine>
<routinename>sort-list
<description>
Given a list and a sorting predicate (which returns true if its first argument
should be taken to be less-than-or-equal to its second), return a sorted list.
Fairly simple shell sort, but seems to work!
<returnvalue type="list">Sorted list of the elements in the input list.
<codebody>
(define (*merge-lists* ina inb <=)
  (let loop ((res '())
             (a ina)
             (b inb))
    (cond
     ((null? a)
      (append (reverse res) b))
     ((null? b)
      (append (reverse res) a))
     (else
      (if (<= (car a) (car b))
          (loop (cons (car a) res)
                (cdr a)
                b)
          (loop (cons (car b) res)
                a
                (cdr b)))))))

(define (*partition-list* pe inl <=)
  (let loop ((pa '())
             (pb '())
             (l inl))
    (if (null? l)
        (cons pa pb)
        (if (<= (car l) pe)
            (loop (cons (car l) pa) pb (cdr l))
            (loop pa (cons (car l) pb) (cdr l))))))


(define (sort-list l <=)
  (case (length l)
    ((0 1) l)
    ((2) (if (<= (car l) (cadr l))
             l
             (list (cadr l) (car l))))
    (else
     (let* ((pe (car l))
            (twolists (*partition-list* pe (cdr l) <=)))
       (*merge-lists* (sort-list (car twolists) <=)
                      (cons pe
                            (sort-list (cdr twolists) <=))
                      <=)))))


<!-- now scoop up the remaining common functions, from sl-gentext.dsl -->
<routine>
<description>Various strings (document in more detail!)
<codebody>
&sl-gentext.dsl

