<!-- $Id$ -->

<!--
<docblock>
<title>LaTeX Tables
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

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council

<codegroup id=code.tables>
<title>Tables
<description>I've aimed to support all of the <em/structure/ of this table
model below, but not necessarily to support all of the attributes at
first.
-->

<routine>
<description>All the flow-object constructors
<codebody>
(element table
  (make environment name: "table"
	(process-matching-children 'tabular)
	(process-matching-children 'caption)))

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
	(make environment name: "tabular"
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
    (make empty-command name: "hline")))

(element tbody
  (process-children))

(element entry
  (make sequence
    (process-children-trim)
    (if (last-sibling? (current-node))
	(make fi data: "\\\\")
	(make fi data: "&"))))


