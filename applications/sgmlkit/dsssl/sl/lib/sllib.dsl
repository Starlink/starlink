<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.6//EN" [
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
<authorref id=iso10179 note='10.2.3'>
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

<routine>
<description>The old favourites, beloved of Lisp folk
<codebody>
(define cadr (lambda (x) (car (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
(define caddddr (lambda (x) (car (cdr (cdr (cdr (cdr x)))))))
(define cddr (lambda (x) (cdr (cdr x))))

