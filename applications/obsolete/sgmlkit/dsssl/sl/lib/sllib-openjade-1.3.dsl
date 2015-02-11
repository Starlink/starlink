<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN">

<title>Library code for Starlink DSSSL stylesheets

<codegroup>
<title>Library code for Starlink DSSSL stylesheets

<description>
This file contains functions defined in 10179, but not in OpenJade.
Jade 1.2.1 has many fewer built-in functions, and the corresponding
file for that version of Jade is substantially larger.

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
<routinename>node-list-filter-by-gi
<description>
Returns a node list containing all the nodes from 'nodelist' whose
GIs are members of 'gilist'.  The order of nodes in the node list
is preserved.
<argumentlist>
<parameter>nodelist
  <type>node list
  <description>List of nodes which is to be filtered
<parameter>gilist
  <type>list of strings
  <description>List of GIs which indicates which elements are to be
  selected from the node list.
<authorlist>
<author id="nw">Norman Walsh
<authornote>This stylesheet drew heavily on the structure and some of
the code of version 1.12 of Norm Walsh's DocBook stylesheets.  See
<url>http://nwalsh.com/docbook/dsssl/</url>.
</authorlist>
<codebody>
(define (node-list-filter-by-gi nodelist gilist)
  (let loop ((result (empty-node-list)) (nl nodelist))
    (if (node-list-empty? nl)
	result
	(if (member (gi (node-list-first nl)) gilist)
	    (loop (node-list result (node-list-first nl)) 
		  (node-list-rest nl))
	    (loop result (node-list-rest nl))))))

<routine>
<routinename>normalize
<purpose>Normalize the str according to the SGML declaration in effect
<description>
Performs SGML general name normalization on the string;
used to compare attribute names and generic identifiers correctly
according to the SGML declaration in effect; this is necessary
since XML is case-sensitive but the reference concrete syntax and
many SGML DTDs are not.  Author, Chris Maden.
<argumentlist>
<parameter>str
  <type>string
  <description>String to be normalised
<codebody>
(define (normalize str)
  (if (string? str)
      (general-name-normalize str
			      (current-node))
      str))


<routine>
<description>
Case folding routines.  Author unknown.
<codebody>
(define default-uppercase-list
  ;; REFENTRY
  ;; PURP The default list of uppercase characters
  ;; DESC
  ;; The default list of uppercase characters.  The order and sequence
  ;; of characters
  ;; in this list must match the order and sequence in 
  ;; 'default-lowercase-list'.
  ;; /DESC
  ;; /REFENTRY
  '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(define default-lowercase-list
  ;; REFENTRY
  ;; PURP The default list of lowercase characters
  ;; DESC
  ;; The default list of lowercase characters.  The order and sequence
  ;; of characters
  ;; in this list must match the order and sequence in 
  ;; 'default-uppercase-list'.
  ;; /DESC
  ;; /REFENTRY
  '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))


(define (case-fold-down-char ch #!optional (uc-list default-uppercase-list)
					   (lc-list default-lowercase-list))
  ;; REFENTRY
  ;; PURP Return the lowercase form of a single character
  ;; DESC
  ;; Returns the lowercase form of 'ch' if 'ch' is a member of
  ;; the uppercase list, otherwise return 'ch'.
  ;;
  ;; The implied mapping from uppercase to lowercase in the two lists is
  ;; one-to-one.  The first element of the uppercase list is the uppercase
  ;; form of the first element of the lowercase list, and vice versa.
  ;; ARGS
  ;; ARG 'ch'
  ;; The character to fold down.
  ;; /ARG
  ;; ARG 'uc-list' o
  ;; The list of uppercase letters. The default is the list of English 
  ;; uppercase letters.
  ;; /ARG
  ;; ARG 'lc-list' o
  ;; The list of lowercase letters. The default is the list of English 
  ;; lowercase letters.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (let ((idx (list-member-find ch uc-list)))
    (if (>= idx 0)
	(list-ref lc-list idx)
	ch)))

(define (case-fold-up-char ch #!optional (uc-list default-uppercase-list)
					 (lc-list default-lowercase-list))
  ;; REFENTRY
  ;; PURP Return the uppercase form of a single character
  ;; DESC
  ;; Returns the uppercase form of 'ch' if 'ch' is a member of
  ;; 'lowercase-list', otherwise return 'ch'.
  ;;
  ;; The implied mapping from uppercase to lowercase in the two lists is
  ;; one-to-one.  The first element of the uppercase list is the uppercase
  ;; form of the first element of the lowercase list, and vice versa.
  ;; ARGS
  ;; ARG 'ch'
  ;; The character to fold down.
  ;; /ARG
  ;; ARG 'uc-list' o
  ;; The list of uppercase letters. The default is the list of English 
  ;; uppercase letters.
  ;; /ARG
  ;; ARG 'lc-list' o
  ;; The list of lowercase letters. The default is the list of English 
  ;; lowercase letters.
  ;; /ARG
  ;; /ARGS
  ;; /DESC
  ;; /REFENTRY
  (let ((idx (list-member-find ch lc-list)))
    (if (>= idx 0)
	(list-ref uc-list idx)
	ch)))

(define (case-fold-down-charlist charlist)
  ;; REFENTRY lib-case-fold-down-charlist
  ;; PURP Return the list of characters, shifted to lowercase
  ;; DESC
  ;; Shifts all of the characters in 'charlist' to lowercase with
  ;; 'case-fold-down-char'.
  ;; /DESC
  ;; /REFENTRY
  (if (null? charlist)
      '()
      (cons (case-fold-down-char (car charlist)) 
	    (case-fold-down-charlist (cdr charlist)))))

(define (case-fold-up-charlist charlist)
  ;; REFENTRY lib-case-fold-up-charlist
  ;; PURP Return the list of characters, shifted to uppercase
  ;; DESC
  ;; Shifts all of the characters in 'charlist' to uppercase with
  ;; 'case-fold-up-char'.
  ;; /DESC
  ;; /REFENTRY
  (if (null? charlist)
      '()
      (cons (case-fold-up-char (car charlist)) 
	    (case-fold-up-charlist (cdr charlist)))))

(define (case-fold-down str)
  ;; REFENTRY lib-case-fold-down
  ;; PURP Shift a string to lowercase
  ;; DESC
  ;; Returns 'str' in lowercase.
  ;; /REFENTRY
  (if (string? str)
      (apply string (case-fold-down-charlist (string->list str)))
      str))

(define (case-fold-up str)
  ;; REFENTRY lib-case-fold-up
  ;; PURP Shift a string to uppercase
  ;; DESC
  ;; Returns 'str' in uppercase.
  ;; /REFENTRY
  (if (string? str)
      (apply string (case-fold-up-charlist (string->list str)))
      str))


<routine>
<routinename>ancestor-member
<purpose>
Returns the first ancestor in a list of GIs
<description>
Returns the first ancestor of 'nd' whose GI that is a member of 'gilist'.
<argumentlist>
<parameter>nd
  <type>node
  <description>A node whose ancestor we want
<parameter>gilist
  <type>list of strings
  <description>List of strings indicating the set of GIs we want to
  select
<authorlist>
<author id=nw>Norman Walsh
<authornote>This stylesheet drew heavily on the structure and some of
the code of version 1.12 of Norm Walsh's DocBook stylesheets.  See
<url>http://nwalsh.com/docbook/dsssl/</url>.
<codebody>
(define (ancestor-member nd gilist)
  (if (node-list-empty? nd)
      (empty-node-list)
      (if (member (gi nd) gilist)
	  nd
	  (ancestor-member (parent nd) gilist))))

<routine>
<routinename>list-member-find
<purpose>Returns the index of an element in a list
<description>Returns the index of 'element' in the list 'elementlist'. The
first element in a list has index 0.
<argumentlist>
<parameter>element
  <type>singleton-node-list
  <description>A node which is to be searched for
<parameter>elementlist
  <type>node-list
  <description>A list of nodes which is to be examined
<returnvalue>The index of `element' in `elementlist'.  The first
  element has index 0.  If the element is not found, returns -1.
<codebody>
(define (list-member-find element elementlist)
  (let loop ((elemlist elementlist) (count 0))
    (if (null? elemlist)
	-1
	(if (equal? element (car elemlist))
	    count
	    (loop (cdr elemlist) (+ count 1))))))
