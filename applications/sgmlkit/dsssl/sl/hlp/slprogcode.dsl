<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.6//EN" [

<![ IGNORE [ <!-- These are not currently used -->
   <!ENTITY lib.dsl     SYSTEM "sllib-jade-1.2.1.dsl" SUBDOC>
   <!ENTITY common.dsl  SYSTEM "slcommon.dsl" SUBDOC>
   <!ENTITY dblib.dsl   SYSTEM "dblib.dsl">
]]>

]>

<docblock>
<title>
   Starlink Programcode to Starlink HLP stylesheet

<description>
   This provides a downconverter from source code marked up with the 
   Starlink Programcode DTD to nroff source which can be processed to
   make Starlink .hlp files.

   <p>Note that the output from this stylesheet should receive some
   minor postprocessing prior to being submitted to tbl and nroff; all 
   leading and trailing whitespace on each line, and any line containing 
   only whitespace, should be removed.  The result which nroff generates
   also needs some postprocessing; multiple consecutive blank lines
   should be collapsed to a single one; additionally many blank lines 
   will be output follwing an end of text marker.

   <p>A suitable pipeline to apply to the output of this stylesheet 
   would therefore be:
   <verbatim>
        sed 's/^  *//;s/  *$//;/^$/d'
      | tbl 
      | nroff 
      | sed -n '/@@@@ END OF TEXT @@@@/q;N;/^\n$/!P;D'
   </verbatim>

<authorlist>
   <author id=mbt affiliation='Starlink'>Mark Taylor

<copyright>
   Copyright 2000 CLRC

</docblock>
<codegroup>
<docblock>
<title>HLP downconverter
</docblock>
<routine>
<description>
   Processing rules
<codebody>


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  External declarations.

      (declare-flow-object-class formatting-instruction 
         "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
      (define debug 
         (external-procedure "UNREGISTERED::James Clark//Procedure::debug"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Utility functions to cater to nroff's needs.

;  Output end of line character.
      (define (newline) 
         (literal (string #\&#RE)))

;  Output an nroff request, supplied as a string without the control character.
      (define (request rq)
         (make sequence
            (literal ".")
            (make formatting-instruction data: rq)
            (newline)))

;  Output a single line of text, supplied as a string.
      (define (line text)
         (make sequence
            (make formatting-instruction data: text)
            (newline)))


;  Output text of a node in normal output mode.  This ought to strip 
;  troublesome whitespace, which is all leading and trailing whitespace
;  on each line.  If this is not done, nroff will try to make something
;  of it, which is undesirable.  This processing should not be done
;  for environments in which whitespace in the source, other than 
;  perhaps linebreaks, is significant.  In fact this aspect of the 
;  processing is not perfect, and the output has to go through a sed
;  script anyway to pull out some extra whitespace.
;  Additionally, this function ensures that all CDATA characters are 
;  passed directly to the output (by using a formatting-instruction)
;  rather than getting translated to &#38;s etc, and it does some minor
;  escaping of characters to avoid inadvertently making roff commands.
      (define (process-children-normal)
         (let loop ((trim #t)
                    (in (node-property 'content (current-node) 
                                       null: (empty-node-list)))
                    (out (empty-sosofo)))
            (if (node-list-empty? in)
                out
                (let*
                   ((node (node-list-first in))
                    (class (node-property 'class-name node))
                    (datachar? (equal? class 'data-char))
                    (chr (if datachar? (data node) #f))
                    (space? (equal? chr " "))
                    (re? (equal? chr (string #\&#RE)))
                    (trimmed? (and trim (or space? re?))))
                   (loop
                      (or trimmed? re?)
                      (node-list-rest in)
                      (sosofo-append 
                         out
                         (if datachar?
                            (cond 
                               (trimmed? 
                                  (empty-sosofo))
                               ((equal? chr ".") 
                                  (make formatting-instruction
                                        data: (string #\\ #\& #\.)))
                               ((equal? chr "'") 
                                  (make formatting-instruction
                                        data: (string #\\ #\& #\')))
                               ((equal? chr (string #\\))
                                  (make formatting-instruction
                                        data: (string #\\ #\e)))
                               (else 
                                  (make formatting-instruction data: chr)))
                            (process-node-list node))))))))


;  Output text of a node in verbatim output mode.  Instead of allowing 
;  leading and trailing whitespace on a line to be stripped, it strips
;  a given number of whitespaces at the start of the line, and inserts
;  a zero-width space at the start of the line to indicate that the 
;  leading space is intentional.
      (define (process-children-verb verb-strip 
                                     #!optional (node (current-node)))
         (let loop ((trim verb-strip)
                    (in (node-property 'content node
                                       null: (empty-node-list)))
                    (out (empty-sosofo)))
            (if (node-list-empty? in)
                out
                (let*
                   ((node (node-list-first in))
                    (class (node-property 'class-name node))
                    (datachar? (equal? class 'data-char))
                    (chr (if datachar? (data node) #f))
                    (space? (equal? chr " "))
                    (re? (equal? chr (string #\&#RE)))
                    (trimmed? (and space? (> trim 0))))
                   (loop
                      (if re? 
                          verb-strip
                          (if trimmed? (- trim 1) 0))
                      (node-list-rest in)
                      (sosofo-append
                         out
                         (if datachar?
                            (make sequence
                               (if (equal? trim verb-strip)
                                   (make formatting-instruction
                                         data: (string #\\ #\&))
                                   (empty-sosofo))
                               (if trimmed?
                                   (empty-sosofo)
                                   (make formatting-instruction data: chr)))
                            (process-node-list node))))))))


;  Process blocks.  An element may have p content, but if the first
;  element is a p we do not wish to output the initial blank line, 
;  because its relationship to its parent has been dealt with in the
;  parent processing.  This function therefore processes the current 
;  node as normal, except that if the first child is a p, it merely 
;  processes its content and not itself.
      (define (process-pblock #!optional (node (current-node)))
         (let* 
            ((kids (children node))
             (firstborn (node-list-first kids))
             (others (node-list-rest kids)))
            (sosofo-append
               (if (or (equal? (gi firstborn) (normalize "p"))
                       (equal? (gi firstborn) (normalize "px")))
                   (process-node-list (node-property 'content firstborn))
                   (process-node-list firstborn))
               (process-node-list others))))


;  Replace spaces in a string by some other text.
      (define (space-replace replacement orig-string)
         (apply string-append
            (map (lambda (x) (if (equal? x #\space) replacement (string x)))
                 (string->list orig-string))))


;  Make a heading line for a HLP file at a given numeric level.
      (define (heading level head-text)
         (make sequence
            (newline)
            (request "ba 0")
            (request "LP")
            (line (number->string level))
            (line (space-replace "_" head-text))
            (request "LP")))


;  Indent a block by a standard amount.
      (define (indent-block sosofo-content)
         (make sequence
            (request "ba +3")
            (request "LP")
            sosofo-content
            (newline)
            (request "ba -3")
            (request "LP")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Some generic functions which are declared elsewhere in the downconverter
;  set; definitions are given here for convenience so that this stylesheet
;  can be freestanding.

;  Trim a string of spaces at both ends.
      (define (trim-string s)
         (apply string 
            (reverse 
               (trim-leading-space 
                  (reverse 
                     (trim-leading-space 
                        (string->list s)))))))
      (define (trim-leading-space clist)
         (let loop ((cl clist))
                   (if (null? cl)
                       cl
                       (if (or (equal? (car cl) #\space) 
                               (equal? (car cl) #\&#RE))
                           (loop (cdr cl))
                           cl))))

;  Normalize a string.
      (define (normalize s)
         (general-name-normalize s (current-node)))
              

;  String->list - DSSSL function not implemented in Jade 1.2.1.
      (define (string->list str)
         (let loop ((chars '())
                    (k (- (string-length str) 1)))
            (if (< k 0)
                chars
                (loop (cons (string-ref str k) chars) (- k 1)))))

;  Map - DSSSL function not implemented in Jade 1.2.1.
      (define (map f #!rest xs)
         (let ((map1 (lambda (f xs)
                        (let loop ((xs xs))
                           (if (null? xs)
                               '()
                               (cons (f (car xs))
                                     (loop (cdr xs))))))))
           (cond ((null? xs)
                   '())
                 ((null? (cdr xs))
                   (map1 f (car xs)))
                 (else
                   (let loop ((xs xs))
                      (if (null? (car xs))
                          '()
                          (cons (apply f (map1 car xs))
                                (loop (map1 cdr xs)))))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Rules for document processing.

;  Default rule.
      (default
         (process-children-normal))


;  Rules for the document elements.


;  Root element
      (root
         (make sequence
            (request "ll 72")        ; Page width
            (request "pl 9999")      ; Page length
            (request "de LP")        ; Define macro for normal line break
            (request "in \\\\n($iu") ;  - like an lp but without sp.
            (request "br")
            (line "..") 
            (request "de AB")        ; Define macro for end of input
            (request "ba 0")
            (request "LP")
            (line "@@@@ END OF TEXT @@@@")
            (line "..")
            (request "rm bp")        ; Remove macro for page eject
            (request "nr ii 3m")     ; Set default indent for ip request
            (request "em AB")        ; Trigger AB macro at end of input
            (request "m1 0")         ; Zero header and footer gaps
            (request "m2 0")
            (request "m3 0")
            (request "m4 0")
            (request "na")           ; No justification
            (request "nh")           ; No hyphenation
            (process-children)))


      (element routine
         (make sequence
            (with-mode active 
                (process-first-descendant 'routinename))
            (with-mode active 
                (process-first-descendant 'purpose))
            (with-mode active 
                (process-first-descendant 'usage))
            (with-mode active 
                (process-first-descendant (list 'routineprologue 'description)))
            (process-children)))
       


;  Elements which are to produce no output.
      (element argumentlist (empty-sosofo))
      (element codebody (empty-sosofo))
      (element codeopener (empty-sosofo))
      (element docblock (empty-sosofo))
      (element invocation (empty-sosofo))


;  Out of order elements.  These are processed at the start of the 
;  output, unlike all other elements which are processed in document order.
;  This is achieved by making them generate no output in the initial 
;  processing mode, but defining another mode, 'active', in which they 
;  generate the output which is required at the start.
      (element routinename (empty-sosofo))
      (element purpose (empty-sosofo))
      (element usage (empty-sosofo))
      (element (routineprologue description) (empty-sosofo))
      (element (blockquote attribution) (empty-sosofo))

      (mode active

         (element routinename
            (make sequence
               (request "ba 0")
               (request "LP")
               (line "1")
               (process-matching-children 'name)
               (newline)))

         (element purpose
            (make sequence
               (request "ba 0")
               (request "LP")
               (process-children-normal)
               (newline)))

         (element usage
            (make sequence
               (request "lp")
               (line "Usage:")
               (request "ba 3")
               (request "sp")
               (request "LP")
               (process-children)
               (newline)))

         (element (routineprologue description)
            (make sequence
               (request "ba 0")
               (request "lp")
               (line "Description:")
               (request "ba 3")
               (request "sp")
               (process-children-normal)
               (newline)))

         (element attribution
            (make sequence
               (request "LP")
               (request "ad r")
               (request "LP")
               (line "--")
               (process-children)
               (newline)
               (request "LP")
               (request "na")))
      )



;  Level 2 elements.  These are children of routineprologue which require
;  explicit special handling.
      (element examplelist
         (make sequence
            (heading 2 "Examples")
            (process-children-normal)))

      (element parameterlist
         (make sequence
            (heading 2 "Parameters")
            (request "LP")
            (line "For information on individual parameters")
            (line "select from the list below:")
            (process-children-normal)))

      (element authorlist
         (make sequence
            (heading 2 "Authors")
            (process-children-normal)))

      (element history
         (make sequence
            (heading 2 "History")
            (process-children-normal)))

      (element implementationstatus
         (make sequence
            (heading 2 "Implementation Status")
            (indent-block (process-children-normal))))
  
      (element bugs
         (make sequence
            (heading 2 "Bugs")
            (indent-block (process-children-normal))))

      (element diytopic
         (let*
            ((kids (children (current-node)))
             (title (node-list-first kids))
             (content (node-list-rest kids)))
            (make sequence
               (heading 2 (trim-string (data title)))
               (indent-block (process-node-list content)))))

      (element returnvalue
         (make sequence
            (heading 2 "Return value")
            (indent-block (process-children-normal))))


;  Generic elements.
      (element p
         (make sequence
            (request "LP")
            (process-children-normal)
            (if (node-list-empty? (follow (current-node)))
                (empty-sosofo)
                (request "sp"))))

      (element px
         (make sequence
            (request "LP")
            (process-children-normal)
            (if (node-list-empty? (follow (current-node)))
                (empty-sosofo)
                (request "sp"))))
      
      (element blockquote
         (make sequence
            (request "sp")
            (indent-block (process-children-normal))
            (with-mode active (process-first-descendant 'attribution))
            (request "sp")))

            
;  Programcode specific elements.
      (element verbatim
         (make sequence
            (request "(l M")
            (process-children-verb 5)
            (newline)
            (request ")l")))

      (element parameter
         (let*
            ((kids (children (current-node)))
             (name (select-elements kids (normalize "name")))
             (type (select-elements kids (normalize "type")))
             (description (select-elements kids (normalize "description")))
             (default (attribute-string (normalize "default")))
             (given? (attribute-string (normalize "given")))
             (returned? (attribute-string (normalize "returned"))))
            (make sequence
               (heading 3 (trim-string (data name)))
               (line (data name))
               (line "=")
               (line (data type))
               (line (cond 
                        ((and given? returned?) "(Read and Write)")
                        (returned? "(Write)")
                        (given? "(Read)")
                        (else "(Read)")))
               (request "ba 3")
               (request "LP")
               (process-node-list description)
               (if default
                   (sosofo-append (request "LP")
                                  (line (string-append "[" default "]")))
                   (empty-sosofo)))))

      (element example
         (make sequence
            (request "in 9")
            (request "ti -9")
            (process-children-normal)
            (request "in 0")
            (request "ba 0")))

      (element (examplelist description)
         (make sequence
            (indent-block (process-children-normal))
            (request "sp")))

      (element change
         (make sequence
            (request "LP")
            (line (string #\\ #\&))
            (line (string-append (attribute-string (normalize "date"))
                                 " ("
                                 (attribute-string (normalize "author"))
                                 "):"))
            (request "ba +4")
            (process-children-normal)
            (request "ba -4")
            (request "sp")))

      (element authorref
         (let*
            ((id (attribute-string (normalize "id")))
             (authel (element-with-id id))
             (name (data (node-list-first (children authel))))
             (affiliation (attribute-string (normalize "affiliation") authel)))
            (make sequence
               (request "LP")
               (line (string-append id ":"))
               (line name)
               (line (string-append "(" id ")"))
               (request "sp"))))

      (element author
         (let*
            ((id (attribute-string (normalize "id")))
             (name (data (node-list-first (children (current-node)))))
             (affiliation (attribute-string (normalize "affiliation"))))
            (make sequence
               (request "LP")
               (line (string-append id ":"))
               (line name)
               (line (string-append "(" id ")"))
               (request "sp"))))

      (element angle
         (let*
            ((angle (attribute-string (normalize "angle")))
             (unit (attribute-string (normalize "unit")))
             (minutes (attribute-string (normalize "minutes")))
             (seconds (attribute-string (normalize "seconds")))
             (fraction (attribute-string (normalize "fraction")))
             (deg? (not (equal? unit "hours"))))
            (make formatting-instruction data: 
               (string-append
                  (string #\\ #\&)
                  (if angle angle "")
                  (if (equal? unit "hours") "h" "d")
                  (if minutes (string-append " " minutes (if deg? "'" "m")) "")
                  (if seconds 
                      (string-append 
                         " " seconds 
                             (if fraction (string-append "." fraction) "")
                             (if deg? "''" "s")) 
                      "")))))

      (element cite
         (make sequence
            (make character char: #\")
            (process-children)
            (make character char: #\")))

      (element linespecific
         (indent-block (process-children-normal)))

      (element line
         (make sequence
            (process-children)
            (request "LP")))

      (element quote
         (make sequence
            (make character char: #\")
            (process-children)
            (make character char: #\")))

;  Note this is not doing a proper reference, but since references are 
;  likely to change dramatically I don't wish to work to hard getting 
;  it right in its current form.
      (element ref
         (let* ((text (attribute-string (normalize "text")))
                (id (attribute-string (normalize "id"))))
               (line (if text text id))))
                      
      (element url
         (line (string-append (string #\<)
                              (trim-string (data (current-node)))
                              (string #\>))))

      (element webref
         (make sequence
            (process-children)
            (line (string-append 
                     (string #\<)
                     (trim-string (attribute-string (normalize "url")))
                     (string #\>)))))



;  Math elements.  Processing of these currently just spits out the latex
;  source, which is not entirely satisfactory.

      (element m
         (process-children))
      (element mequation
         (indent-block (process-children-verb 5)))
      (element mlabel
         (empty-sosofo))
      (element meqnarray
         (indent-block (process-children)))
      (element mline
         (process-children-verb 5))


;  Descriptive list elements.
      (element dl
         (make sequence
            (indent-block (process-children-normal))
            (request "sp")))

      (element dt
         (make sequence
            (request "sp")
            (request "LP")
            (process-children-normal)
            (newline)))

      (element dd
         (indent-block (process-children-normal)))

;  Unordered list elements.
      (element ul
         (indent-block (process-children-normal)))

      (element (ul li)
         (make sequence
            (request "bu")
            (process-pblock)
            (if (attribute-string (normalize "compact")
                                             (parent (current-node)))
                (empty-sosofo)
                (request "sp"))
            (newline)))

;  Ordered list elements.
      (element ol
         (indent-block (process-children-normal)))

      (element (ol li)
         (make sequence
            (request (string-append 
                        "ip " 
                        (number->string (child-number (current-node)))
                        "." 
                        " 3"))
            (process-pblock)
            (newline)))


;  Tables.
;  This is a very primitive implementation.  Currently all columns are 
;  left-aligned, and most of the style-setting attributes of the various 
;  elements are ignored.  Spanned rows and columns are not implemented
;  either.
      (define (true x) (and x (not (= x 0))))

      (element tabular
         (indent-block (process-children-normal)))

      (element tgroup
         (let*
            ((cols (string->number (attribute-string (normalize "cols"))))
             (align (attribute-string (normalize "align")))
             (colsep? (true (attribute-string (normalize "colsep"))))
             (rowsep? (true (attribute-string (normalize "rowsep")))))
            (make sequence
               (request "TS")
               (line 
                  (let loop ((colnum cols)
                             (format ""))
                     (if (= colnum 0)
                         (string-append format ".")
                         (string-append "l " (loop (- colnum 1) format)))))
               (process-children-normal)
               (request "TE"))))

       (element colspec (empty-sosofo))

       (element thead
          (make sequence
             (process-children-normal)
             (line "=")))

       (element tbody
          (process-children-normal))

       (element row
          (process-children-normal))

       (element entry
          (make sequence
             (line "T{")
             (process-children-normal)
             (newline)
             (literal "T}")
             (make character
                char: (if (node-list-empty? (follow (current-node)))
                          #\&#RE
                          #\&#TAB))))
               

<!-- $Id$ -->
