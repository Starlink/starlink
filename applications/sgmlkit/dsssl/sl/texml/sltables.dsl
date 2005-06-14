<!-- $Id$ -->

<!--
<docblock>
<title>LaTeX/TeXML Tables
<description>The Starlink General DTD uses the OASIS Exchange Table
Model subset of the CALS table model (see
<url>http://www.oasis-open.org/html/a503.htm</url> for discussion and
<url>http://www.oasis-open.org/html/publtext.htm</url> for public
texts).

<p>The Exchange Table Model can be customised.  The only such
customisations at present are: replace the optional TITLE element with
a required CAPTION; add implied ID and EXPORT attributes to the TABLE;
extend the table entry model to include phrase markup.


<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, 2004, Council for the Central Laboratory of the Research Councils

<codegroup id=code.tables>
<title>Tables
<description>I've aimed to support all of the <em>structure</> of this table
model below, but not necessarily to support all of the attributes at
first.
-->


;; Return the node-list of colspecs, or #f if we're not in a tgroup
(define (get-colspecs #!optional (nd (current-node)))
  (let ((tg (if (string=? (gi nd) (normalize "tgroup"))
		nd
		(ancestor (normalize "tgroup") nd))))
    (if (node-list-empty? tg)
	#f
	(select-elements (children tg)
			 (normalize "colspec")))))
;; get column number of column with supplied name, or #f if none such exists
(define (get-column-number #!key (colspec #f)
				 (name #f)
				 (nd (current-node)))
  (let* ((cs-l (and (not colspec)
		    (get-colspecs nd)))
	 (cs (or colspec
		 (and cs-l
		      (node-list-first ;just in case there are two
		       (node-list-filter
			(lambda (n)
			  (string=? name
				    (or (attribute-string
					 (normalize "colname") n)
					"XXX"; no colname, so don't match
					)))
			cs-l))))))
    (if (or (not cs)
	    (node-list-empty? cs))
	#f				;no such node
    (let loop ((n cs)
	       (inc 0))
      (if (node-list-empty? n)
	  inc
	  (if (attribute-string (normalize "colnum") n)
	      (+ (string->number (attribute-string (normalize "colnum") n))
		 inc)
	      (loop (ipreced n)
		    (+ inc 1))))))))

(element table
  (let ((float (attribute-string (normalize "float"))))
    (if (and float
	     (string=? float "float"))
	(make-latex-environment name: "table"
	      parameters: `(,%latex-float-spec%)
	      (process-matching-children 'tabular)
	      (process-matching-children 'caption))
	(make element gi: "group"
	      (make-latex-empty-command name: "SetCapType"
		    parameters: '("table"))
	      (process-matching-children 'tabular)
	      (process-matching-children 'caption)))))

;; TABULAR
;; Supported attributes colsep, frame (in tgroup), rowsep (in row)
;; Unsuported: pgwide
(element tabular
  (process-matching-children 'tgroup))

;; Use the tokenise-string routine, with a special isbdy? function, to
;; split off the leading digits from the trailing non-digits
;; characters.  Returns a list of either one or two strings,
;; containing the number and the unit.
(define (get-digits-from-string s)
  (tokenise-string s
		   isbdy?: (lambda (l)
			     (case (car l)
			       ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\.) #f)
			       (else l)))
		   max: 1))

;; From OASIS TR 9503:1995, the colwidth has the following grammar:
;;
;;   colwidthspec := "" | propmeasure | fixedmeasure
;;   propmeasure  := number? `*'
;;   fixedmeasure := number (`pt' | `cm' | `mm' | `pi' | `in')?
;;
;; If the number is omitted from `propmeasure' it is interpreted as `1*', 
;; and if the `colwidthspec' is given as the empty string, it is interpreted
;; as `1*'.  If the units are omitted from the `fixedmeasure' they are
;; taken to be `pt'.  The fixed measure units are case insensitive.
;; The following interprets a bare unit such as `pt' as `1pt', which
;; goes beyond the standard, but won't be documented.  "\TabMod" is a
;; unit which will be defined as pagewidth/n-columns.
;;
;; Returns a sosofo for the column width
(define (parse-colwidth spec)
  (let* ((l (get-digits-from-string spec))
	 ;; num is non-zero-length number, or #f
	 (num (and (> (string-length (car l)) 0) (car l)))
	 ;; dim is dimension (inc `*'), or #f
	 (dim (and (> (length l) 1) (cadr l))))
    (sosofo-append (literal (or num "1"))
		   (if dim
		       (case (case-fold-down dim)
			 (("*") (make-latex-command name: "TabMod"))
			 (("pt" "in" "cm" "mm") (literal dim))
			 (("pi") (literal "pc"))
			 (else (error (string-append "bad unit in " spec))))
		       (literal "pt")))))

;; process a colspec `cs' given a pair default `def'.
;; Result should be a pair ("l|r|c" . "| or empty"), where the car is a sosofo
(define (proc-colspec cs def)
  (let ((colsep (attribute-string (normalize "colsep") cs))
	(align (attribute-string (normalize "align") cs))
	(colwidth (attribute-string (normalize "colwidth") cs)))
    (cons (if colwidth
	      (make sequence
                (literal "p")
                (make element gi: "group"
                      (parse-colwidth colwidth)))
	      (if align
                  (literal 
                   (case align
                     (("left" "center" "justify") "l")
                     (("right") "r")
                     (("center") "c")
                     (("char")
                      (error "colspec: align=char not supported"))
                     (else
                      (error (string-append
                              "colspec: unrecognised alignment type ("
                              align ")")))))
                   (car def)))
	  (if colsep
	      (if (= (string->number colsep) 0)
		  ""
		  "|")
	      (cdr def)))))


;; Produce either "\\" or "&" depending on whether the (current-node)
;; is the last sibling
(define (newline-or-align)
  (if (last-sibling? (current-node))
      (latex-newline)
      (make empty-element gi: "spec" attributes: '(("cat" "align")))))

;; TGROUP
;; Supported attributes: colsep, cols, align, rowsep
;; Unsupported: (none)
(element tgroup
  (let* ((colno-str (attribute-string (normalize "cols") (current-node)))
	 (colno (if colno-str
		    (string->number colno-str)
		    (error "tgroup: required cols attribute missing")))
	 (tgroup-colsep (attribute-string (normalize "colsep")))
	 (colsep-str (or tgroup-colsep
			 (attribute-string (normalize "colsep")
					   (parent (current-node)) ;tabular
					   )))
	 (def-colsep (if (and colsep-str (= (string->number colsep-str) 0))
			 ""
			 "|"))		;default colsep present
	 (def-align-att (attribute-string (normalize "align") (current-node)))
	 ;; FIXME include p column specifiers somehow
	 (def-align
           (literal (if def-align-att
			(case (case-fold-down def-align-att)
			  (("left" "justify") "l")
			  (("right") "r")
			  (("center") "c")
			  (("char")
			   (error "tgroup: char alignment not supported"))
			  (else
			   (error (string-append
				   "tgroup: unrecognised alignment ("
				   def-align-att ")"))))
			"l")))		;default
	 ;; form a list of pairs of (colspec-sosofo . "| or nothing")
	 (def-colspec  (let loop ((n colno)
                                  (res '()))
                            (if (<= n 0)
                                (reverse res)
                              (loop (- n 1)
                                (cons (cons def-align def-colsep)
                                      res)))))
         (colspec-l-tmp (let loop ((res '()) ;result list of sosofos
                                   (def-l def-colspec) ;list of spec to process
                                   (cs-l	;list of colspecs
                                    (node-list->list (get-colspecs))))
                          (cond
                           ((null? def-l)
                            res)
                           ((and (not (null? cs-l))
                                 (= (get-column-number colspec: (car cs-l))
                                    (+ (length res) 1)))
                            (loop (cons (proc-colspec (car cs-l)
                                                      (car def-l))
                                        res)
                                  (cdr def-l)
                                  (cdr cs-l)))
                           (else
                            (loop (cons (car def-l)
                                        res)
                                  (cdr def-l)
                                  cs-l)))))
	 (colspec-l (let ((l colspec-l-tmp))
                      (reverse (cons (cons (caar l) "")
                                     (cdr l)))))
	 (colspec-sosofo (apply sosofo-append
				(map (lambda (p)
				       (sosofo-append (car p)
                                                      (literal (cdr p))))
				     colspec-l)))
	 (border (let ((bspec (inherited-element-attribute-string
			       (normalize "tabular")
			       (normalize "frame")
			       (current-node))))
		   ;; produce a list containing #t when the (top bottom sides)
		   ;; are to have a border
		   (if bspec
		       (case bspec
			 (("top")    '(#t #f #f))
			 (("bottom") '(#f #t #f))
			 (("topbot") '(#t #t #f))
			 (("all")    '(#t #t #t))
			 (("sides")  '(#f #f #t))
			 (("none")   '(#f #f #f))
			 (else
			  (error (string-append
				  "tabular: illegal frame spec ("
				  bspec ")"))))
		       '(#f #f #f))))	;no frame by default
	 )
    (make sequence
      (make-latex-empty-command name: "TabMod")
      (make-latex-empty-command name: "textwidth")
      (make-latex-empty-command name: "divide")
      (make-latex-empty-command name: "TabMod")
      (literal (string-append (number->string colno) " "))
      (make-latex-environment name: "tabular"
                              (make element
                                gi: "TeXML"
                                attributes: '(("escape" "0"))
                                (make element gi: "group"
                                      colspec-sosofo))
                              (if (car border)
                                  (make-latex-empty-command name: "hline")
                                  (empty-sosofo))
                              (process-children)
                              (if (cadr border)
                                  (make-latex-empty-command name: "hline")
                                  (empty-sosofo))))))

;; COLSPEC
;; Supported attributes: align, colname, colnum, colsep,
;; Unsupported: char, charoff, colwidth, rowsep
(element colspec
  ;; Nothing to display
  (empty-sosofo))

;; ROW
;; Supported attributes: rowsep
;; Unsupported: valign
(element row
  (let ((tgroup-cols (string->number
		      (inherited-element-attribute-string (normalize "tgroup")
							  (normalize "cols")
							  (current-node))))
	(actual-elements (node-list-length (children (current-node))))
	(rowsep-string
	 (or (attribute-string (normalize "rowsep"))
	     (attribute-string (normalize "rowsep")
			       (ancestor (normalize "tgroup")))
	     (attribute-string (normalize "rowsep")
			       (ancestor (normalize "tabular"))))))
    (if (<= actual-elements tgroup-cols)
	(make sequence
	  (process-children-trim)
	  (if (and (not (last-sibling?)) ;do we add a line below
		   rowsep-string
		   (not (= (string->number rowsep-string) 0)))
	      (make-latex-empty-command name: "hline")
	      (empty-sosofo)))
	(error (string-append
		"Row "
		(number->string (child-number (current-node)))
		" of table has more than "
		(number->string tgroup-cols)
		" rows, as declared")))))

;; THEAD & TBODY
;; Supported attributes: none
;; Unsupported: valign
(element thead
  (make sequence
    (process-children)
    (make-latex-empty-command name: "hline")))

(element tbody
  (process-children))

;; ENTRY
;; Supported attributes: colname, namest, nameend
;; Unsupported: align, char, charoff, colsep, morerows, rowsep, valign
(element entry
  (let* ((colname (attribute-string (normalize "colname")))
	 (namest (attribute-string (normalize "namest")))
	 (nameend (attribute-string (normalize "nameend")))
	 (start-col (or (and colname
			     (get-column-number name: colname))
			(and namest
			     (get-column-number name: namest)))))
    (make sequence
      (if start-col
            (let loop ((n start-col))
              (if (> n (child-number (current-node)))
                  (sosofo-append
                   (make empty-element gi: "spec" attributes: '(("cat" "align")))
                   (loop (- n 1)))
                  (empty-sosofo)))
            (empty-sosofo))
      (if (and namest nameend)
	  (if (and (get-column-number name: nameend)
		   (get-column-number name: namest))
	      (let ((sep (- (get-column-number name: nameend)
			    (get-column-number name: namest)
			    -1)))
		(make-latex-command name: "multicolumn"
		      parameters: `(,(number->string sep) "c")
		      (process-children-trim)))
	      (error (string-append
		      "entry refers to unknown columns "
		      namest " and " nameend)))
	  (process-children-trim))
      (newline-or-align))))




<![IGNORE[
<routine>
<description>All the flow-object constructors
<codebody>
(element table
  (let ((float (attribute-string (normalize "float"))))
    (if float
	(make-latex-environment name: "table"
	      (process-matching-children 'tabular)
	      (process-matching-children 'caption))
	(make sequence
	  (process-matching-children 'tabular)
	  (process-matching-children 'caption)))))

(element tabular
  (process-matching-children 'tgroup))

(element tgroup
  (let* ((colno (table-colno))
	 (colspec (and colno
		       (let loop ((n (cadr colno))
				  (str ""))
			 (if (<= n 0)
			     str
			     (loop (- n 1) (string-append str "l")))))))
    (if colno
	(make-latex-environment name: "tabular"
	      parameters: (list colspec)
	      (process-children))
	(error "Can't find column number"))))

(element colspec
  ;; Merely discard these at present
  (empty-sosofo))

(element row
  (let ((colno (table-colno))
	(actual-elements (node-list-length (children (current-node)))))
    (if (<= actual-elements (cadr colno))
	(process-children-trim)
	(error (string-append
		"Row "
		(number->string (child-number (current-node)))
		" of table has more than "
		(number->string (cadr colno))
		" rows, as declared")))))

(element thead
  (make sequence
    (process-children)
    (make-latex-empty-command name: "hline")))

(element tbody
  (process-children))

(element entry
  (make sequence
    (process-children-trim)
    (newline-or-align)
;     (if (last-sibling? (current-node))
; 	(make fi data: "\\\\")
; 	(make fi data: "&"))
    ))


]]>
