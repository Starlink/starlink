<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [
  <!ENTITY dblib.dsl	SYSTEM "dblib.dsl">
]>

<title>Library code for Starlink DSSSL stylesheets

<codegroup>
<title>Library code for Starlink DSSSL stylesheets

<description>
This file contains functions defined in 10179, but not in Jade, and not
in <code>lib/dblib.dsl</code> in Norm Walsh's DocBook stylesheet.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray
<otherauthors>
<author id=iso10179>Text of ISO 10179:1996
<authornote>The DSSSL standard defines many functions without mandating
that they be included as primitives in an implementation.  Some
such functions are defined here.
<author id=nw>Norman Walsh
<authornote>This stylesheet drew heavily on the structure and some of
the code of version 1.12 of Norm Walsh's DocBook stylesheets.  See
<url>http://nwalsh.com/docbook/dsssl/</url>.

<routine>
<description>
<p><code>dblib.dsl</code> contains useful functions lifted wholesale
from Norm Walsh's DocBook stylesheet
<codebody>
&dblib.dsl


<routine>
<routinename>ancestors
<description><p>
Return the mapping over nl of the function on a node that returns
the ancestors of the node, where the ancestors of a node are an
empty node-list if the node is a tree root, and otherwise are the
result of appending the ancestors of the parent of the node and the
parent of the node.
<returnvalue type="node-list">List of ancestors
<argumentlist>
<parameter>
  <name>nl
  <type>node-list
  <description>list of nodes we want the ancestors of
<authorlist>
<author id=iso10179>ISO 10179    <!-- 10.2.3 -->
<codebody>
(define (ancestors nl)
  (node-list-map (lambda (snl)
		   (let loop ((cur (parent snl))
			      (result (empty-node-list)))
		     (if (node-list-empty? cur)
			 result
			 (loop (parent cur)
			       (node-list cur result)))))
		 nl))

<![ IGNORE [
<!-- Don't need these after all -->
<routine>
<routinename>node-list-contains?
<description>
Returns <code>#t</> if nl contains a node equal to the member of snl,
and otherwise returns <code>#f</>.
<returnvalue type=boolean>True if the node-list contains the specified element
<argumentlist>
<parameter>
  <name>nl
  <type>node-list
  <description>Node-list to be tested
<parameter>
  <name>snl
  <type>singleton-node-list
  <description>node to be tested against
<authorlist>
<author id=iso10179>ISO 10179 <!-- 10.2.2 -->
<codebody>
(define (node-list-contains? nl snl)
  (node-list-reduce nl
		    (lambda (result i)
		      (or result
			  (node-list=? snl i)))
		    #f))

<routine>
<routinename>node-list-remove-duplicates
<description>
Returns a node-list which is the same as nl except that any member of
nl which is equal to a preceding member of nl is removed.
<returnvalue type=node-list>Filtered node-list
<argumentlist>
<parameter>
  <name>nl
  <type>node-list
  <description>node-list to be filtered
<authorlist>
<author id=iso10179>ISO 10179 <!-- 10.2.2 -->
<codebody>
(define (node-list-remove-duplicates nl)
  (node-list-reduce nl
		    (lambda (result snl)
		      (if (node-list-contains? result snl)
			  result
			  (node-list result snl)))
		    (empty-node-list)))

<routine>
<routinename>node-list-difference
<description>
Returns a node-list containing the set deifference of all the
arguments, which shall be node-lists.  The set difference is defined
to be those members of the first argument that are not members of any
of the other arguments.  The result shall contain no duplicates.  With
no arguments, an empty node-list shall be returned.
<returnvalue type=node-list>List of nodes
<authorlist>
<author id=iso10179>ISO 10179 <!-- 10.2.2 -->
<codebody>
(define (node-list-difference #!rest args)
  (if (null? args)
      (empty-node-list)
      (node-list-reduce (cdr args)
	      (lambda (nl1 nl2)
		(node-list-reduce nl1
				  (lambda (result snl)
				    (if (node-list-contains? nl2 snl)
					result
					(node-list result snl)))
				  (empty-node-list)))
	      (node-list-remove-duplicates (car args)))))
]]>

<routine>
<routinename>node-list-filter
<description>
Returns a node-list containing just those members of nl for which proc
applied to a singleton node-list containing just that member does not
return #f.
<returnvalue type=node-list>List of matching nodes
<argumentlist>
<parameter>proc
  <type>procedure
  <description>The filtering procedure, retrning #t when a node is to
  be included in the result.
<parameter>nl
  <type>node-list
  <description>The node-list which is to be filtered
<authorlist>
<author id=iso10179>ISO 10179 <!-- 10.2.2 -->
<codebody>
(define (node-list-filter proc nl)
  (node-list-reduce nl
		    (lambda (result snl)
		      (if (proc snl)
			  (node-list snl result)
			  result))
		    (empty-node-list)))

<routine>
<routinename>node-list->list
<description>
Returns a list containing, for each member of nl, a singleton
node-list containing just that member
<returnvalue type=node-list>List of singleton node-lists
<argumentlist>
<parameter>nl
  <type>node-list
  <description>The node-list which is to be split
<authorlist>
<author id=iso10179>ISO 10179 <!-- 10.2.2 -->
<codebody>
(define (node-list->list nl)
  (reverse (node-list-reduce nl
			     (lambda (result snl)
			       (cons snl result))
			     '())))

<routine>
<description>The old favourites, beloved of Lisp folk
<codebody>
(define caar (lambda (x) (car (car x))))
(define cadr (lambda (x) (car (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define caddddr (lambda (x) (car (cdr (cdr (cdr (cdr x)))))))
(define cddr (lambda (x) (cdr (cdr x))))

