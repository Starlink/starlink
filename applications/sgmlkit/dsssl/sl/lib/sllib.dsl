<!DOCTYPE programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN" [
  <!entity dblib.dsl	system "dblib.dsl">
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

<misccode>
<miscprologue>
<description>
<p><code>dblib.dsl</code> contains useful functions lifted wholesale
from Norm Walsh's DocBook stylesheet
</description>
</miscprologue>
<codebody>
&dblib.dsl
</codebody>
</misccode>


<func>
<name>ancestors
<description><p>
Return the mapping over nl of the function on a node that returns
the ancestors of the node, where the ancestors of a node are an
empty node-list if the node is a tree root, and otherwise are the
result of appending the ancestors of the parent of the node and the
parent of the node.
<returnvalue type="node-list">
<parameter>
  <name>nl
  <type>node-list
  <description>list of nodes we want the ancestors of
</parameter>
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
</codebody></func>

<func>
<name>document-element
<description><p>
Returns the document element of the document containing the given
node.

<p>Only the <code/SgmlDocument/ node class
exibits a <code/DocumentElement/ property, so to find the document element
we first have to find the grove root, which we do by examining the 
<code/grove-root/ property of the current node.  The only node which doesn't
have a <code/grove-root/ property (so that the <funcname/node-property/
routine will correctly return <code/#f/ -- ie, it exhibits the property, but
with the value <code/#f/) is the root node, but in that case,
<funcname/current-node/ returns the grove root directly (this isn't clear
from the standard -- see the discussion on `Finding the root element' in
the dssslist archive at
<url>http://www.mulberrytech.com/dsssl/dssslist/</url>).
<p>The subsequent calls to <funcname/node-property/ default <code/#f/ if 
the property is not exhibited by the node.  This catches the case where the
grove root doesn't have any <code/document-element/ property, for example if
the grove is malformed because it resulted from a call to <funcname/sgml-parse/
with a non-existent file.
<returnvalue type="singleton-node-list">The document element, or
<code/#f/ if not found.
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
</codebody>
</func>

<func>
<routinename>document-element-from-entity
<description>
Return the document element of the document referred to by the
entity string passed as argument.  
Uses <funcname/sgml-parse/: see 10179, 10.1.7.
<returnvalue type="node-list">Document element, or <code/#f/ on error.
<parameter>
  ent-name
  <type>string
  <description>string containing entity declared in current context

<codebody>
(define (document-element-from-entity str)
  (let ((sysid (entity-generated-system-id str)))
    (and sysid
	 (document-element (sgml-parse sysid)))))
</codebody>
</func>


<misccode>
<description>The old favourites, beloved of Lisp folk
<codebody>
(define cadr (lambda (x) (car (cdr x))))
(define caddr (lambda (x) (car (cdr (cdr x)))))
</codebody>
</misccode>
