<!-- 
$Id$ 

<docblock>
<title>HTML navigation
<description>
<p>This module supports navigation.
<p>Some of the functions in this file are imported more-or-less intact from 
Norm Walsh's DocBook stylesheet, but have been substantially simplified.
This file was one of the first I wrote, so there are likely several
redundant functions in here which I don't yet have the time to 
winnow out.
<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray
<author id=mbt>Mark Taylor
<otherauthors>
<author id=nw>Norman Walsh
<authornote>The general structure of this file, plus some of the code,
has been adapted from version v1.12 (or thereabouts) of Norman Walsh's 
DocBook stylesheet. 

<codegroup>
<title>HTML navigation
-->

<routine>
<routinename>chunk-element-list
<description>
<p>List of element types which should be broken into chunks.
Because of the way that section-footer-navigation, and nav-next-chunk,
for example, find their subsections, there should be no `missing
levels' in the set of elements.  Ie, ("sect" "subsubsect") would be
bad, since "subsect" is missing.  That is, (chunk-parent) should
generally be equal to (parent).  The cases where this isn't true (ie,
abstract, and the boundary between codecollection and programcode)
are dealt with separately.

<p>There are two constraints on the elements in this list.  (1) no
elements appear which are in the documentsummary DTD but not in the
General DTD, since <funcname>main-html-base</funcname> relies on this to be able to
generate the same HTML file name in both cases.  It doesn't matter
if there are elements here which don't appear in the summary DTD,
since elements with those names will necessarily never be found
when processing an instance of the summary DTD.  (2) the list
must be a subset of the return value of <funcname>section-element-list</funcname>.
<codebody>
(define (chunk-element-list)
  (list (normalize "abstract")
        (normalize "sect")
	(normalize "subsect")
	(normalize "subsubsect")
	;(normalize "subsubsubsect")
	(normalize "appendices")
	;(normalize "routinelist")
	;(normalize "codecollection")
	(normalize "programcode")	;in programcode DTD
	(normalize "codegroup")
	(normalize "routine")
	))

<routine>
<routinename>chunking?
<description>
Returns true if chunking is enabled.
<p>Currently, this simply returns <code>(not (or nochunks stream-output))</code>,
but could be more general in future.
<returnvalue type=boolean>True if chunking is enabled.
<codebody>
(define (chunking?)
  (not (or nochunks stream-output)))

<routine>
<routinename>chunk?
<description>
Return <code>#t</code> if the given node is a chunk, taking account of whether
chunking has been turned off.
Given that chunking is on, this simply tests whether the
node is a member of <funcname>chunk-element-list</funcname>.
<returnvalue type=boolean>True if the node is a chunk
<p>Do <em>not</em> modify this so that the document element is deemed to
be a chunk.  This may seem like a good idea, but it's important that
<funcname>chunk?</funcname> <em>always</em> returns false if chunking is off.
<argumentlist>
<parameter optional default='(current-node)'>
  nd
  <type>node-list
  <description>The node to test
<codebody>
(define (chunk? #!optional (nd (current-node)))
  (and (chunking?)
       (member (gi nd) (chunk-element-list))))

<routine>
<routinename>chunk-path
<description>
Return a string which describes the path to the given node through
nodes which are members of <funcname>chunk-element-list</funcname>.  Returns an empty
string if the <funcname>chunk-level-parent</funcname> of the given node is empty.
Note that <funcname>chunk-level-parent nd</funcname> returns nd if nd is a member of
<funcname>chunk-element-list</funcname>. 
<returnvalue type=string>String without spaces, listing the chunk-type
elements on the way to the current chunk
<argumentlist>
<parameter>nd<type>node-list<description>Node we want the path to
<codebody>
(define ($chunk-path-abbrev$ nd)
  (let* ((gi-map (list (cons (normalize "sect") "a")
		       (cons (normalize "subsect") "b")
		       (cons (normalize "subsubsect") "c")
		       (cons (normalize "subsubsubsect") "d")
		       (cons (normalize "appendices") "x")
		       (cons (normalize "routinelist") "r")
		       (cons (normalize "codecollection") "f")
		       (cons (normalize "codegroup") "G")
		       (cons (normalize "routine") "R")
		       ;; the programcode number will always be one,
		       ;; but we do nonetheless need this here, since
		       ;; programcode is in chunk-element-list (so
		       ;; that nav-next-element and friends work).
		       (cons (normalize "programcode") "P")
		       ))
	 (nd-gi-map (assoc (gi nd) gi-map)))
    (if nd-gi-map
	(cdr nd-gi-map)
	(gi nd))))
(define (chunk-path nd)
  (let loop ((this-node (chunk-level-parent nd))
	     (path-string ""))
    (if (node-list-empty? this-node)
	path-string
	(loop (chunk-level-parent (parent this-node))
	      (string-append ($chunk-path-abbrev$ this-node) ;(gi this-node)
			     (number->string (child-number this-node))
			     path-string)))))

<routine>
<routinename>main-html-base
<description>
Return a string containing the name of the file which will hold the
given node.  Since this must work both for the general DTD and the
documentsummary DTD, we can't use <funcname>all-element-number</funcname>.  Since 
this current version uses <funcname>chunk-path</funcname>, it will break (in the sense
that different filenames will be generated for the same element when it
appears in the general and in the documentsummary DTD) if the elements in
<funcname>chunk-element-list</funcname> (which <funcname>chunk-path</funcname> uses) produce
different hierarchies in the two DTDs.
<returnvalue type=string>Base of filename
<argumentlist>
<parameter>nd<type>node-list<description>We want the basename of the file
which will hold this node
<codebody>
(define (main-html-base nd)
  (let* ((node-name-suffix (chunk-path nd))
	 (idbase (if (and %use-id-as-filename%
			  (attribute-string (normalize "id") nd))
		     (case-fold-down (attribute-string (normalize "id") nd))
		     #f)))
    (if idbase
	(string-append (root-file-name nd) idbase)
	(string-append (root-file-name nd)
		       "-"
		       (case-fold-down node-name-suffix)))))

<routine>
<routinename>html-file
<description>
Returns the filename of the html file that contains the given node.
<returnvalue type=string>Complete filename.  Will not return an empty string.
<argumentlist>
<parameter keyword default='(current-node)'>
  target_nd<type>node-list<description>Node whose file we want
<parameter keyword default='#f'>
  uniq<type>string<description>If present, this gives a unique string which 
  will be used to construct the file name.  This is need in the case of,
  for example, the note contents file, which has no associated element.
<codebody>
(define (html-file #!key (target_nd (current-node)) (uniq #f))
  (let* ((nd (chunk-parent target_nd))
	 (base (cond (uniq
		      (string-append (root-file-name target_nd) "-" uniq))
		     ((string=? (gi target_nd) "routine")
		      (html-file-routine target_nd))
		     ((string=? (gi target_nd) "programcode")
		      (html-file-programcode target_nd))
		     ((string=? (gi target_nd) "codegroup")
		      (html-file-codegroup target_nd))
		     ((member (gi nd) (section-element-list))
		      (main-html-base nd))
		     ((node-list-empty? nd)
				; if the node-list nd is empty, then
				; this is because chunk-parent
				; couldn't find a parent chunk.  This
				; means either that we're not
				; chunking, or else that this is the
				; root chunk.
		      (if %override-root-file-name%
			  %override-root-file-name%
			  (index-file-name target_nd))
				; give target_nd as argument - this is
				; a singleton-node-list (required
				; argument for document-element), but
				; chunk-parent produces a node-list (mmm?)
		      )
		     ;; Following gives the same behaviour.  More rational?
		     ;((node-list=? input_nd (document-element))
		     ; (root-file-name))
		     ;; Catch-all.  It's probably better to return
		     ;; _something_ here, even if it's nonsense,
		     ;; rather than a confusing empty-string
		     (else "xxx1")
		     )))
    (string-append base %html-ext%)
    ))

<routine>
<routinename>chunk-parent
<description>
Return the node-list for the element whose chunk nd is in, or an
empty node list if there is none such (which might happen if
chunking is turned off).
<returnvalue type=singleton-node-list>An element which is the `top level'
of a particular chunk
<argumentlist>
<parameter optional default='(current-node)'>
  nd<type>node-list<description>This node identifies the chunk we want
  the top level of.
<codebody>
(define (chunk-parent #!optional (nd (current-node)))
  (let loop ((p (chunk-level-parent nd)))
    (if (or (node-list-empty? p) (chunk? p))
	p
	(loop (chunk-level-parent (parent p))))))

<routine>
<routinename>chunk-level-parent
<description>
Return (a node-list containing) the nearest ancestor which is a
member of <funcname>chunk-element-list</funcname>.  The difference between this and
<funcname>chunk-parent</funcname> is that <funcname>chunk-parent</funcname> tests whether the 
node is 
actually chunked (ie, it also uses <funcname>chunk?</funcname>), whereas this one just
tests for membership of <funcname>chunk-element-list</funcname>.
<returnvalue type=singleton-node-list>`Top level' of the current chunk.
<argumentlist>
<parameter optional default='(current-node)'>
  nd<type>node-list<description>This node identifies the chunk we want the 
  parent of
<codebody>
(define (chunk-level-parent #!optional (nd (current-node)))
  (ancestor-member nd (chunk-element-list)))

<routine>
<routinename>chunk-children
<description>
Return the children of the current chunk, or an empty node-list if
there are none.
<returnvalue type=node-list>Children of the current chunk.
<argumentlist>
<parameter optional default='(current-node)'>
  nd<type>node-list<description>This node identifies the chunk we want the 
  children of.
<codebody>
(define (chunk-children #!optional (nd (current-node)))
  (node-list-filter-by-gi (select-by-class (children nd) 'element)
			  (chunk-element-list)))


<routine>
<routinename>html-contents
<purpose>Processing mode for html-contents lines
<description>The <funcname>section-reference</funcname> processing mode is not
quite appropriate for HTML contents lines, since we don't want all
the numbers in that case.  This mode is based on that mode however,
and passes control explicitly to that mode for those elements which
it processes suitably for our purposes.  For the others it lets
<funcname>make-html-contents-line</funcname> do the hard work.
<p>This mode should contain construction rules for all the elements 
in <funcname>section-element-list</funcname>.
<codebody>
(mode html-contents
  (element abstract
     (make-html-contents-line))
  (element sect
     (make-html-contents-line))
  (element subsect
     (make-html-contents-line))
  (element subsubsect
     (make-html-contents-line))
  (element subsubsubsect
     (make-html-contents-line))
  (element appendices
     (make-html-contents-line))
  ; routinelist and codecollection contents in slroutines.dsl
  (element routinelist
     (make-html-contents-line))
  ;(element codecollection
  ;   (make-html-contents-line))
  (element title
     (with-mode section-reference (process-node-list (current-node))))
  (element docbody
     (with-mode section-reference (process-node-list (current-node))))
  (element subhead
     (with-mode section-reference (process-node-list (current-node))))
  (element programcode
    (make element gi: "a"
	  attributes: `(("href" ,(href-to (current-node))))
	  (with-mode routine-ref-get-reference (process-node-list (current-node)))))
  (element codegroup
    (make element gi: "a"
	  attributes: `(("href" ,(href-to (current-node))))
	  (with-mode routine-ref-get-reference (process-node-list (current-node)))))
  (element routine
    (make element gi: "a"
	  attributes: `(("href" , (href-to (current-node))))
	  (with-mode routine-ref-get-reference (process-node-list (current-node)))))
  )


<routine>
<routinename>make-html-contents-line
<purpose>Generate a line for inclusion in the HTML table of contents.
<description>This generates a line for inclusion in the HTML table
of contents; it consists of the child-number of the section, formatted
as appropriate, followed by its title or something else which serves
as such.
<p>It operates on the current node.
<returnvalue type=sosofo>A sosofo suitable for use as the content of a li 
element.
<codebody>
(define (make-html-contents-line)
   (let* ((el-gi (gi (current-node))) 
          (seclev (cond 
                    ((equal? el-gi (normalize "subsubsubsect")) 3)
                    ((equal? el-gi (normalize "subsubsect")) 2)
                    ((equal? el-gi (normalize "subsect")) 1)
                    ((equal? el-gi (normalize "sect")) 0)
                    (else #f)))
          (fmt-type (cond 
                      (seclev (list-ref 
                                 (if (have-ancestor? (normalize "appendices"))
                                    %appendix-fmts
                                    %section-fmts)
                                  seclev))
                      ((equal? el-gi (normalize "routinelist")) #f)
                      ((equal? el-gi (normalize "codecollection")) "1")
                      (else #f))))
     (make element gi: "a" 
	   attributes: `(("href" ,(href-to (current-node))))
	   (if fmt-type
	       (literal 
		(format-number (child-number (current-node)) fmt-type) ". ")
	       (empty-sosofo))
	   (cond 
            (seclev (process-first-descendant (normalize "title")))
            ((equal? el-gi (normalize "appendices")) 
	     (literal "Appendices"))
            ((equal? el-gi (normalize "routinelist"))
	     (literal "Routine list"))
            ((equal? el-gi (normalize "abstract"))
	     (literal "Abstract"))
            ((equal? el-gi (normalize "codecollection"))
	     (with-mode routine-ref-get-reference
	       (process-codecollection 
		(attribute-string (normalize "doc")))))))))


<routine>
<routinename>make-contents
<description>
Make a table of contents of the node argument, down to the specified depth.
This works by listing children of the current node which are
members of <funcname>section-element-list</funcname>, and possibly recursing to
list their children.  It does not supply any header.
<returnvalue type=sosofo>TOC, currently formatted as a UL
<argumentlist>
<parameter optional default='(current-node)'>start-element
  <type>singleton-node-list
  <description>Node we want the contents of.  All the children of this
  node which are members of <funcname>section-element-list</funcname> will be
  listed.  If this is passed as false -- which can happen if this is
  called within the programcode DTD -- then do nothing, without error.
<parameter optional default=1>depth
  <type>integer
  <description>Maximum number of levels of TOC we want.  Zero means
  return immediately.
<parameter>include-backmatter-contents
  <type>boolean
  <description>If true, then include a TOC for the backmatter
<codebody>
(define (make-contents #!optional
		       (start-element (current-node))
		       (depth 1)
		       (include-backmatter-contents #f))
  (let ((subsects (if start-element
		      (node-list-filter-by-gi (select-by-class
					       (children start-element)
					       'element)
					      (section-element-list))
		      (empty-node-list))))
    (if (or (node-list-empty? subsects) (<= depth 0))
	(empty-sosofo)
	(make element gi: "ul"
	   (node-list-reduce
              subsects
              (lambda (last el)
                 (sosofo-append
                    last
                    (make element gi: "li"
                       (with-mode html-contents (process-node-list el))
                       (make-contents el (- depth 1)))))
              (empty-sosofo))
           (if (and include-backmatter-contents (hasbackmatter?))
               (make-contents-backmatter)
               (empty-sosofo))))))

<routine>
<routinename>has-contents?
<description>Finds out whether the contents which would be made by
the <funcname>make-contents</funcname> routine is non-empty.
<returnvalue type=boolean>#f if <funcname>make-contents</funcname> would 
return an empty sosofo, and #t otherwise.
<argumentlist>
<parameter optional default='(current-node)'>start-element
  <type>singleton-node-list
  <description>Node we want the contents of.  All the children of this
  node which are members of <funcname>section-element-list</funcname> will be
  listed.
<parameter optional default=1>depth
  <type>integer
  <description>Maximum number of levels of TOC we want.  Zero means
  return immediately.
<codebody>
(define (has-contents? #!optional
                       (start-element (current-node))
                       (depth 1))
   (let ((subsects (node-list-filter-by-gi (select-by-class
                                           (children start-element)
                                           'element)
                                          (section-element-list))))
      (not (or (node-list-empty? subsects) (<= depth 0)))))

<routine>
<routinename>has-chunk-contents?
<description>Finds out whether the given node contains elements 
which would be chunks.
<returnvalue type=boolean>#t if the given node has children which
are chunks, and #t otherwise.
<argumentlist>
<parameter optional default='(current-node)'>start-element
  <type>singleton-node-list
  <description>Node we want the contents of.
<codebody>
(define (has-chunk-contents? #!optional
                       (start-element (current-node)))
   (let ((subsects (node-list-filter-by-gi (select-by-class
                                           (children start-element)
                                           'element)
                                          (chunk-element-list))))
      (not (node-list-empty? subsects))))

<routine>
<routinename>navlink
<description>Generates a navigation hyperlink (i.e. HTML A element).
<argumentlist>
<parameter>target
   <type>singleton-node-list
   <description>The node to which the link is to be made.
<parameter>relation
   <type>string
   <description>The name of the relation which target bears to the current 
   node.  This is used as the text of a text link, or as the key for 
   finding an image using the <funcname>file-href</funcname> routine.
<parameter>link-type
   <type>string
   <description>Either "title" or "gif" according to the kind of link 
   you want.
<codebody>
(define (navlink target relation link-type)
   (if (node-list-empty? target)
       (case link-type 
          (("gif") 
              (make sequence 
                 (make empty-element gi: "img"
                             attributes: (list 
                       (list "src" (file-href (string-append relation ".off")))
                       (list "border" "0")
                       (list "alt" (string-append relation " (absent)"))))
                 (literal " ")))
          (("title") 
              (empty-sosofo)))
       (case link-type
          (("gif")
             (make sequence
                (make element gi: "a"
                      attributes: (list (list "href"  
                                              (href-to target reffrag: #f)))
                    (make empty-element gi: "img"
                                attributes: 
                            (list (list "src" (file-href relation))
                                  (list "border" "0")
                                  (list "alt" relation))))
                (literal " ")))
          (("title")
             (make sequence
                (make element gi: "b" 
                   (literal (string-append relation ": ")))
                (make element gi: "a"
                      attributes: (list (list "href" 
                                              (href-to target reffrag: #f)))
                   (with-mode section-reference (process-node-list target)))
                (make empty-element gi: "br"))))))

<routine>
<description>
Various functions to provide the links which navigate between the various
generated HTML documents.  
<codebody>


;; This forms a nodelist containing all the nodes which are chunks.
;; This list is used to find the forward and backward links relative
;; to the current one.  
;;
;; There would be (significant?) efficiency gains
;; in evaluating it once only rather than every time it is referenced
;; but I'm not immeidiately sure how to do this since it needs a
;; node in the document grove to find the document body node (i.e.
;; you can't evaluate (getdocbody) in the context of a top-level binding.
;(define (chunk-subtree nl)
;  (node-list-map 
;    (lambda (snl) (node-list snl (chunk-subtree (chunk-children snl))))
;    nl))
;(define (doc-chunk-sequence)
;  (chunk-subtree (getdocbody)))


(define (nav-up-element elemnode)
  (parent elemnode))
(define (nav-up? elemnode)
  (not (or (node-list-empty? (nav-up-element elemnode))
           (node-list=? (nav-up-element elemnode) (document-element)))))

;; Get the next chunk at the same level.  This relies on the majority
;; of the elements in chunk-element-list being direct children of one
;; another, so that (chunk-parent)=(parent).  There are a couple of
;; cases where this isn't true, such as with the abstract (I think),
;; and generally with the front-page processing, which are dealt with
;; separately and explicitly, so this should fail gracefully where
;; this assumption isn't true.
(define (nav-next-chunk nd)
  (let loop ((sibs (chunk-children (parent nd)))) ;all siblings
    (cond ((node-list-empty? sibs)
	   (empty-node-list))		;no siblings: chunk-parent != parent
	  ((node-list=? (node-list-first sibs) nd)
	   (node-list-first (node-list-rest sibs)))
	  (else
	   (loop (node-list-rest sibs))))))

;; Find the next element.  This is the first child, if any;
;; failing that, it is the nav-next-chunk; failing that, the
;; nav-forward-element of the parent, ignoring children.
(define (nav-forward-element nd
			     #!key (nokids #f))
  (let ((kids (if nokids (empty-node-list) (chunk-children nd))))
    (if (node-list-empty? kids)
	(let ((nchunk (nav-next-chunk nd)))
	  (if (node-list-empty? nchunk)
	      (let ((mum (parent nd)))	;no later siblings
		(if (node-list-empty? mum)
		    (empty-node-list)		;no next element anywhere
		    (nav-forward-element mum nokids: #t)))
	      nchunk))
	(node-list-first kids))))
;; Obsolete nav-forward-element, which relies on
;; doc-chunk-sequence (which is otherwise unused)
;(define (nav-forward-element elemnode)
;  (let loop ((rest (doc-chunk-sequence)))
;     (cond ((node-list-empty? rest)
;            (empty-node-list))
;           ((node-list=? (node-list-first rest) elemnode) 
;            (node-list-first (node-list-rest rest)))
;           (else
;            (loop (node-list-rest rest))))))
(define (nav-forward? elemnode)
  (not (node-list-empty? (nav-forward-element elemnode))))

(define (nav-last-chunk snl)
  (let loop ((last (empty-node-list))
	     (sibs (chunk-children (parent snl)))) ;all siblings
    (cond ((node-list-empty? sibs)
	   (empty-node-list))		;no siblings: chunk-parent != parent
	  ((node-list=? (node-list-first sibs) snl)
	   last)
	  (else
	   (loop (node-list-first sibs)
		 (node-list-rest sibs))))))
(define (nav-backward-element nd)
  (let ((nchunk (nav-last-chunk nd)))
    (if (node-list-empty? nchunk)
	(parent nd)			;nice and simple
	nchunk)))
;(define (nav-backward-element elemnode)
;  (let loop ((rest (node-list-reverse (doc-chunk-sequence))))
;     (cond ((node-list-empty? rest)
;            (empty-node-list))
;           ((node-list=? (node-list-first rest) elemnode)
;            (node-list-first (node-list-rest rest)))
;           (else
;            (loop (node-list-rest rest))))))
(define (nav-backward? elemnode)
   (not (node-list-empty? (nav-backward-element elemnode))))

(define (idindex-link elemnode)
   (let* ((in-sect (ancestor-member elemnode (list (normalize "sect"))))
          (sect-id (and in-sect (attribute-string (normalize "id") in-sect))))
      (make element gi: "a"
            attributes: `(("href" ,(string-append (idindex-sys-id)
                                                  "#"
                                                  (if sect-id
                                                      (string-append
                                                       idindex-frag-id
                                                       "_" sect-id)
                                                      idindex-frag-id))))
            (literal "[ID index]"))))

(define (keyword-index-link)
  (make element gi: "a"
        attributes: `(("href" ,(keywordindex-sys-id)))
        (literal "[Keyword index]")))

;; Generates a standard navigation bar.
(define (navbar nd #!key (uplink #f))
   (make sequence 
      (make empty-element gi: "hr")
      (navlink (nav-forward-element nd) "Next" "gif")
      (navlink (if uplink uplink (nav-up-element nd)) "Up" "gif")
      (navlink (nav-backward-element nd) "Previous" "gif")
      (navlink (document-element nd) "Contents" "gif")
      (make empty-element gi: "br")
      (navlink (nav-forward-element nd) "Next" "title")
      (navlink (if uplink uplink (nav-up-element nd)) "Up" "title")
      (navlink (nav-backward-element nd) "Previous" "title")
      (idindex-link nd)
      (keyword-index-link)
      (make empty-element gi: "hr")))


(define (header-navigation nd #!key (uplink #f))
    (if (and (node-list=? nd (document-element))
	     (not uplink))
        (root-header-navigation nd)
        (nav-header nd
		    uplink: uplink)))
(define (footer-navigation nd #!key (uplink #f)) 
    (if (and (node-list=? nd (document-element))
	     (not uplink))
        (root-footer-navigation nd)
        (nav-footer nd
		    uplink: uplink)))

(define (nav-header elemnode #!key (uplink #f))
   (navbar elemnode
	   uplink: uplink))

(define (nav-footer elemnode #!key (uplink #f))
   (make sequence 
      (if (has-chunk-contents?)
          (sosofo-append (make empty-element gi: "hr") (make-contents))
          (empty-sosofo))
      (navbar elemnode
	      uplink: uplink)
      ;; Make the footer only if the document has a title.  This is
      ;; really a test of whether we're in an instance of the General
      ;; DTD, or of the programcode DTD.
      (if (and (getdocinfo 'title)
	       (not uplink))
	  (make element gi: "address"
		(make element gi: "i"
		      (process-node-list (getdocinfo 'title))
		      (make empty-element gi: "br")
		      (literal (getdocnumber (current-node) 'asString))
		      (make empty-element gi: "br")
		      (getdocauthors)
		      (make empty-element gi: "br")
		      (literal (getdocdate))
		      (make empty-element gi: "br")))
	  (empty-sosofo))))
         

(define (root-header-navigation nd)
        (empty-sosofo))

;; We're producing the footer for the root element (SUN or MUD, or
;; whatever).  This doesn't have any element of (chunk-element-list)
;; as a child, so we have to give it some help.
;;
;; If we're not chunking, then we don't want to produce a table of
;; contents.  This is the only place we need to worry about this --
;; the section-footer-navigation functions will be invoked only if
;; we're chunking.

(define (root-footer-navigation elemnode)
  (if (chunking?)
      (if separate-toc
	  (let ((tocfname (string-append %toc-file-root% %html-ext%)))
	    (make sequence
	      (make element gi: "a"
		    attributes: `(("HREF" ,tocfname))
		    (literal "Table of Contents"))
	      (html-document
	       (literal "Table of Contents")
	       (make sequence
		 (make element gi: "h1"
		       (literal "Table of Contents"))
		 (make-contents (getdocbody) 4 #t))
	       system-id: tocfname
	       navbars?: #f		; This is essential.  If not
					; present, we get into an
					; infinite loop, since
					; html-document calls
					; footer-navigation if
					; navbars? is true 
	       )))
	  (make sequence
	    (make element gi: "h2"
		  (literal "Contents"))
	    (make-contents (getdocbody) 4 #t)))
    (empty-sosofo)))



