<!-- 
$Id$ 

<docblock>
<title>HTML Tables
<description>The Starlink General DTD uses the OASIS Exchange Table
Model subset of the CALS table model (see
<url>http://www.oasis-open.org/html/a503.htm</url> for discussion and
<url>http://www.oasis-open.org/html/publtext.htm</url> for public
texts).

<p>The Exchange Table Model can be customised.  The only such
customisations at present are: replace the optional TITLE element with
a required CAPTION; add implied ID and EXPORT attributes to the TABLE.


<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council

<codegroup id=code.tables>
<title>Tables
<description>I've aimed to support all of the <em>structure</> of this table
model below, but not necessarily to support all of the attributes at
first.
-->

<routine>
<description>All the flow-object constructors
<codebody>
(element table
  (let* ((caption-details (get-caption-details (current-node)))
	 (caption-id (caddr caption-details)))
    (make sequence
      (process-matching-children 'tabular)
      (make element gi: "blockquote"
	    (make element gi: "p"
		  (if caption-id
		      (make element gi: "a"
			    attributes:
			    (list (list "name"
					(string-append "xref_" caption-id)))
			    (literal (car caption-details)))
		      (literal (car caption-details)))
		  (if show-element-ids
		      (literal (display-element-ids (current-node)))
		      (empty-sosofo)))
	    (process-matching-children 'caption)))))

(element tabular
  (make element gi: "table"
	attributes: '(("border" "1"))
	(process-matching-children 'tgroup)))

(element tgroup
  (make sequence
    (process-children)
    (if (last-sibling? (current-node))
	(empty-sosofo)
	(let ((colno (table-colno)))
	  (if colno
	      (make element gi: "tr"
		    (make element gi: "td"
			  attributes: (list
				       (list
					"colspan"
					(number->string (cadr colno))))
			  (make empty-element gi: "hr")))
	      (error "Can't find column number"))))))

(element colspec
  ;; Merely discard these at present
  (empty-sosofo))

(element thead
  (with-mode thead-mode
    (process-children)))

(element tbody
  (process-children))

(element row
  (let ((colno (table-colno))
	(actual-elements (node-list-length (children (current-node)))))
    (if (<= actual-elements (cadr colno))
	(make element gi: "tr"
	      (process-children))
	(error (string-append
		"Row "
		(number->string (child-number (current-node)))
		" of table has "
		actual-elements
		" columns, rather than "
		(number->string (cadr colno))
		", as declared")))))

(element entry
  (make element gi: "td"
	(process-children)))

(mode thead-mode
  (element entry
    (make element gi: "th"
	  (process-children))))

