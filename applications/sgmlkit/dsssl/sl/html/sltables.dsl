<!DOCTYPE programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN">
<![ ignore [ $Id$ ]]>

<docblock>
<title>HTML Tables
<description>The Starlink General DTD uses the OASIS Exchange Table
Model subset of the CALS table model (see
<url>http://www.oasis-open.org/html/a503.htm</url> for discussion and
<url>http://www.oasis-open.org/html/publtext.htm</url> for public
texts).

<p>The Exchange Table Model can be customised.  The only such
customisations at present are: replace the optional TITLE element with
a required CAPTION, and add an implied ID attribute to the TABLE.


<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council

<codegroup id=code.tables>
<title>Tables
<description>I've aimed to support all of the <em/structure/ of this table
model below, but not necessarily to support all of the attributes at
first.

<misccode>
<description>All the flow-object constructors
<codebody>
(element table
  (let* ((caption-details (get-caption-details (current-node)))
	 (caption-id (cadr caption-details)))
    (make sequence
      (make element gi: "table"
	    attributes: '(("border" "1"))
	    (process-matching-children 'tgroup))
      (make element gi: "blockquote"
	    (make element gi: "p"
		  (if caption-id
		      (make element gi: "a"
			    attributes: (list (list "name" caption-id))
			    (literal (car caption-details)))
		      (literal (car caption-details))))
	    (process-matching-children 'caption)))))

(element caption
  (process-children))

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
  (make element gi: "tr"
	(process-children)))

(element entry
  (make element gi: "td"
	(process-children)))

(mode thead-mode
  (element entry
    (make element gi: "th"
	  (process-children))))

<func>
<routinename>table-colno
<purpose>Return a list of numbers, indicating the current column number and the
total number of columns.
<description>Checks only the <code/ENTRY/ and <code/TGROUP/ elements,
the first to find the column number, and the second to find the total
number of columns.
<returnvalue type='list of numbers'><p>A list of numbers, where
  the <code/car/ is the current column number, 
  and the <code/cadr/ is the total number of columns.
  <p>If the argument is not for an <code/ENTRY/ element,
  then the current column number will be returned as zero.
  <p>If the node doesn't have a <code/TABLE/ in its ancestry,
  then return <code/#f/.
<parameter optional default='(current-node)'>nd
  <type>singleton node-list<description>Node we want the column number of.
  If this is not an <code/ENTRY/ element, then the column number will be
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
