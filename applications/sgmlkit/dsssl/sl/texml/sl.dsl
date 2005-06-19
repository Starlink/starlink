<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [
<!ENTITY sldocs.dsl		SYSTEM "sldocs.dsl">
<!ENTITY slsect.dsl		SYSTEM "slsect.dsl">
<!ENTITY slmisc.dsl		SYSTEM "slmisc.dsl">
<!ENTITY slroutines.dsl		SYSTEM "slroutines.dsl">
<!ENTITY slmaths.dsl		SYSTEM "slmaths.dsl">
<!ENTITY sllinks.dsl		SYSTEM "sllinks.dsl">
<!ENTITY sltables.dsl		SYSTEM "sltables.dsl">

<!ENTITY commonparams.dsl	PUBLIC "-//Starlink//TEXT DSSSL Common Parameterisation//EN">
<!ENTITY slparams.dsl		PUBLIC "-//Starlink//TEXT DSSSL TeXML Parameterisation//EN">

<!ENTITY lib.dsl		SYSTEM "../lib/sllib.dsl" SUBDOC>
<!ENTITY common.dsl		SYSTEM "../common/slcommon.dsl" SUBDOC>
<!ENTITY slback.dsl		SYSTEM "slback.dsl" SUBDOC>
]>

<!-- $Id$ -->

<docblock>
<title>Starlink to TeXML stylesheet
<description>
This is the stylesheet for converting the Starlink General DTD to TeXML.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, 2004, Council for the Central Laboratory of the Research Councils


<codereference doc="lib.dsl" id="code.lib">
<title>Library code
<description>
<p>Some of this library code is from the standard, some from Norm
Walsh's stylesheet, other parts from me

<codereference doc="common.dsl" id="code.common">
<title>Common code
<description>
<p>Code which is common to both the HTML and print stylesheets.

<codereference doc="slback.dsl" id=code.back>
<title>Back-matter
<description>Handles notes, bibliography and indexing

<codegroup
  use="code.lib code.common code.back" id=texml>
<title>Conversion to TeXML

<routine>

<description>Declare the flow-object-classes which support the SGML
 transformation extensions of Jade.
<codebody>
(declare-flow-object-class element
  "UNREGISTERED::James Clark//Flow Object Class::element")
(declare-flow-object-class empty-element
  "UNREGISTERED::James Clark//Flow Object Class::empty-element")
(declare-flow-object-class document-type
  "UNREGISTERED::James Clark//Flow Object Class::document-type")
(declare-flow-object-class processing-instruction
  "UNREGISTERED::James Clark//Flow Object Class::processing-instruction")
(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-flow-object-class entity-ref
  "UNREGISTERED::James Clark//Flow Object Class::entity-ref")
(declare-flow-object-class formatting-instruction
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
(define debug
  (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))
(define all-element-number
  (external-procedure "UNREGISTERED::James Clark//Procedure::all-element-number"
))


;; define the functions which map the old LaTeX stylesheets, which used custom
;; flow-object-classes which were part of an extension Jade back-end,
;; into the classes of the usual Jade transformation back-end.


;; Extract any keyword-type objects from the front of REST, along with
;; the following argument (of any type), and put them as a cons-ed
;; pair at the front of the list KWS.  If the REST does not start
;; with a keyword, then return (KWS . REST)
(define (*reduce-args* kws rest)
  (if (or (null? rest)
          (not (keyword? (car rest))))
      (cons kws rest)
      (*reduce-args* (cons (cons (car rest) (cadr rest))
                           kws)
                     (cddr rest))))

;; Extracts the value of the keyword KW from the assoc list ALIST.  If the 
;; keyword is present, return its value; if not, return #f.  Yes, I
;; know this can't distinguish between a keyword not present, and a
;; keyword with value #f, but that doesn't matter in fact.
(define (*keyword-value* kw alist)
  (let ((p (assoc kw alist)))
    (if p
        (cdr p)
        #f)))

;; Process the parameters.  PARVAL is a list of parameter value
;; strings, or #f, indicating no parameters.  Each of the strings
;; starts with '!' for a required parameter, '?' for an optional one,
;; and anything else for a required parameter.
(define (*process-parameters* is-env parval)
  (let ((write-opt (if is-env
                       (lambda (s)
                         (literal (string-append "[" s "]")))
                       (lambda (s)
                         (make element gi: "opt" (literal s)))))
        (write-par (lambda (s)
                     (make element gi: (if is-env "group" "parm")
                           (literal s)))))
    (if parval
        (apply sosofo-append
               (map (lambda (par)
                      (cond
                       ((char=? (string-ref par 0) #\?)
                        (write-opt (substring par 1 (string-length par))))
                       ((char=? (string-ref par 0) #\!)
                        (write-par (substring par 1 (string-length par))))
                       (else
                        (write-par par))))
                    parval))
        (empty-sosofo))))


;; Helper for the two make-*-command functions.  KEYWORDS is a list
;; of the keywords which were given to the make-* command, possibly
;; with the magic keyword *BODY-SOSOFO-PARAMETER*, containing the body
;; of the command as a sosofo.
(define (*emit-cmd* keywords)
  (make element gi: "cmd"
        attributes: `(("name" ,(or (*keyword-value* name: keywords) "NAME")))

        (*process-parameters* #f (*keyword-value* parameters: keywords))
        (let ((body-param (*keyword-value* *body-sosofo-parameter*: keywords)))
          (if body-param
              (make element gi: "parm"
                    body-param)
              (empty-sosofo)))))

;; make-latex-command has two keyword arguments:
;;
;; "name" is a string that specifies the name
;; of the environment.
;;
;; "parameters" specifies the command's parameters as a list of
;; strings.  If the parameter string starts with a `?' character, the
;; parameter value is the remainder of the string, written as an
;; optional parameter.  If the parameter string starts with `!', it is
;; written as a required parameter, allowing you to start parameter
;; values with a `?' character.
(define (make-latex-command #!rest content)
  (let* ((kws-and-rest (*reduce-args* '() content))
         (kws (car kws-and-rest))
         (body-bit (cdr kws-and-rest)))
    (*emit-cmd* (cons (cons *body-sosofo-parameter*:
                            (if (null? body-bit)
                                (empty-sosofo)
                                (apply sosofo-append body-bit)))
                      kws))))

;; make-latex-empty-command is as for make-latex-command, except that
;; any body is ignored
(define (make-latex-empty-command #!rest content)
  (*emit-cmd* (car (*reduce-args* '() content)))) ; ignore any body

;; make-latex-environment
;;
;; "name" and "parameters" are as for make-latex-command
;;
;; "brackets"
;;   This allows you to specify explicitly what the beginning and end
;;   of the environment should be.  The characteristic's value is a list
;;   consisting of two strings, the first of which is inserted at the
;;   beginning, and the second at the end, of the environment.  For
;;   example, you might specify that an element type
;;   "equation" should be transformed into a LaTeX equation
;;   via a flow object constructor such as:
;;   (element mequation
;;     (make environment brackets: '("\\[" "\\]")
;;           (process-children)))
;;
;;   "recontrol"
;;   The LaTeX generated by the back-end has line-breaks inserted to
;;   avoid the lines becoming too long.  If these are inappropriate for
;;   some reason, you can override the line-breaking by giving a value to
;;   the "recontrol" characteristic.  Its value is a string
;;   with three characters, controlling the line-breaking before, in the
;;   middle of (ie, between the "\\begin" and the environment
;;   name), and after the opening and closing of the environment.  If the
;;   corresponding character is `/' a RE is inserted at the appropriate
;;   point; if it is `-', no RE is inserted.  For example, a
;;   "verbatim" element type might be supported via:
;;   (element verbatim
;;     (make environment
;;       name: "verbatim"
;;       recontrol: "/-/"
;;       (process-children)))
;;   Thus, the "\\begin{verbatim}" and
;;   "\\end{verbatim}" will be on lines by themselves, with no
;;   RE between the "\\begin" or "\\end" and the
;;   environment name.
;;
;; For the TeXML back-end, the middle character is redundant, and ignored.
(define (make-latex-environment #!rest content)
  (let* ((kws-and-rest (*reduce-args* '() content))
         (kws (car kws-and-rest))
         (body-bit (cdr kws-and-rest))
         (internal-newlines?
          (let ((recontrol (*keyword-value* recontrol: kws)))
            (and recontrol
                 (string=? recontrol "/-/"))))
         (brackets (*keyword-value* brackets: kws)))
    (cond
     ((and brackets
           (not (= (length brackets) 2)))
      (error "make-latex-environment: brackets characteristic not a two-element list"))

     (brackets
      (apply sosofo-append (append `(,(literal (car brackets)))
                                   body-bit
                                   `(,(literal (cadr brackets))))))

     ((not (*keyword-value* name: kws))
      (error "make-latex-environment: neither name nor brackets present"))

     (else
      (make element gi: "env"
            attributes: `(("name" ,(*keyword-value* name: kws)))
            (*process-parameters* #t (*keyword-value* parameters: kws))
            (if (null? body-bit)
                (empty-sosofo)
                (make sequence
                  (if internal-newlines?
                      (make fi data: "
")
                      (empty-sosofo))
                  (apply sosofo-append body-bit)
                  (if internal-newlines?
                      (make fi data: "
")
                      (empty-sosofo)))))))))

;; Convenience function to make a paragraph.  CONTENT is a sosofo
;; containing the content of the paragraph.
(define (make-latex-paragraph content)
  (sosofo-append content
                 (make-latex-empty-command name: "par")))

;; Convenience function -- produces a sosofo which results in "\\"
(define (latex-newline)
  (make empty-element gi: "ctrl" attributes: '(("ch" "\\"))))


;; incorporate the simple stylesheets directly

&sldocs.dsl;
&slsect.dsl;
&slmisc.dsl;
&slroutines.dsl;
&slmaths.dsl;
&sllinks.dsl;
&sltables.dsl;
&commonparams.dsl;
&slparams.dsl;

<routine>
<description>
The root rule.  Simply generate the LaTeX document at present.
<codebody>
(root
    (make sequence
      (process-children)
      (make-manifest)
      ))

<![ IGNORE [
<codegroup>
<title>The default rule
<description>There should be no default rule in the main group
(<code>style-specification</codebody> in the terms of the DSSSL architecture),
so that it doesn't take priority over mode-less rules in other process
specification parts.  See the DSSSL standard, sections 7.1 and 12.4.1.

<p>Put a sample default rule in here, just to keep it out of the way
(it's ignored).
<routine>
<description>The default rule
<codebody>
(default
  (process-children))
]]>

<routine>
<routinename>make-manifest
<description>Construct a list of the LaTeX files generated by the main
processing.  Done only if <code>suppress-manifest</code> is false and 
<code>%texml-manifest%</code> is true, giving the
name of a file to hold the manifest.  
<p>This is reasonably simple, since the manifest will typically consist
of no more than the main output file, plus whatever files are used
by the "figurecontent" element.
<argumentlist>
<parameter optional default='(current-node)'>nd<type>singleton-node-list
  <description>Node which identifies the grove to be processed.
<codebody>
(define (make-manifest #!optional (nd (current-node)))
  (if (and %texml-manifest% (not suppress-manifest))
      (let ((element-list (list (normalize "figure")))
	    (rde (document-element nd)))
	(make entity system-id: %texml-manifest%
	      (make fi
		data: (string-append (index-file-name) ".xml
")) ; see sldocs.dsl
	      (with-mode make-manifest-mode
		(process-node-list
		 (node-list rde		;include current file
			    (node-list-filter-by-gi
			     (select-by-class (descendants rde)
					      'element)
			     element-list))))))
      (empty-sosofo)))

(mode make-manifest-mode
  (default 
    (empty-sosofo))

  ;; The selection here should match the processing in slmisc.dsl
  (element figure
    (let ((content
           (figurecontent-to-notation-map
            (node-list (select-elements (children (current-node))
                                        (normalize "figurecontent"))))))
      (if content
          (process-node-list
           (apply node-list (map (lambda (p)
                                   (if (member (car p)
                                               '("eps" "pdf"))
                                       (cdr p)
                                       (empty-node-list)))
                                 content)))
          (empty-sosofo))))

  (element coverimage
    (let ((content
           (figurecontent-to-notation-map
            (node-list (select-elements (children (current-node))
                                        (normalize "figurecontent"))))))
      (if content
          (process-node-list
           (apply node-list (map (lambda (p)
                                   (if (member (car p)
                                               '("eps" "pdf"))
                                       (cdr p)
                                       (empty-node-list)))
                                 content)))
          (empty-sosofo))))
  
;  (element coverimage
;    (let* ((kids (children (current-node)))
;	   (content (get-best-figurecontent
;		     (select-elements kids (normalize "figurecontent"))
;		     '("eps" "pdf"))))
;      (if content
;	  (process-node-list content)
;	  (empty-sosofo))))
  ;; the figurecontent element writes out TWO fields in the manifest:
  ;; the first is the sysid of the figure as referred to by the
  ;; generated LaTeX, which will have no path, and the second is the sysid as
  ;; declared in the entity, which may well have a path.  Locations may
  ;; need some post-processing.
  (element figurecontent
    (let* ((image-ent (attribute-string (normalize "image")
					(current-node)))
	   (full-sysid (and image-ent
			    (entity-system-id image-ent)))
	   (base-sysid (and image-ent
			    (car (reverse
				  (tokenise-string
				   (entity-system-id image-ent)
				   boundary-char?: (lambda (c)
						     (char=? c #\/))))))))
      (if image-ent
	  (make fi data: (string-append base-sysid " " full-sysid "
"))
	  (empty-sosofo)))))



