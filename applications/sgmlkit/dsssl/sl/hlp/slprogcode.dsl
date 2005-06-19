<!DOCTYPE programcode PUBLIC "-//Starlink//DTD DSSSL Source Code 0.7//EN" [

<![ IGNORE [   -- These are not currently used --
   <!ENTITY lib.dsl       SYSTEM "../lib/sllib.dsl">
   <!ENTITY common.dsl    SYSTEM "../common/slcommon.dsl">
   <!ENTITY dblib.dsl     SYSTEM "../lib/dblib.dsl">
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

;  Output a non-breaking nroff request.
      (define (request-nobreak rq)
         (make sequence
            (literal "'")
            (make formatting-instruction data: rq)
            (newline)))

;  Output a single line of text, supplied as a string.
      (define (line text)
         (make sequence
            (make formatting-instruction data: text)
            (newline)))

;  Output a single literal character - if it might confuse nroff then 
;  escape it properly.
      (define (escape-chr chr)
         (case chr 
            ((".") (make formatting-instruction data: (string #\\ #\& #\.)))
            (("'") (make formatting-instruction data: (string #\\ #\& #\')))
            (("\\") (make formatting-instruction data: (string #\\ #\e)))
            (else (make formatting-instruction data: chr))))


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
;  rather than getting translated to &#38;s etc.
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
                            (if trimmed? (empty-sosofo)
                                         (escape-chr chr))
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
                                   (escape-chr chr)))
                            (process-children-verb verb-strip node))))))))


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
;
;  It would be better to use them from their existing homes in the various
;  library files, but I am unable to get the syntax right to reference
;  those definitions from this file :-(.

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

;  List->string - DSSSL function not implemented in Jade 1.2.1.
      (define (list->string chars)
        (let loop ((cl chars)
                   (str ""))
          (if (null? cl)
              str
              (loop (cdr cl)
                    (string-append str (string (car cl)))))))

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


;  Assoc - the value of a key from an associative list (or #f if key is
;  not present).
      (define (assoc obj alist)
        (let loop ((al alist))
          (if (null? al)
              #f
              (if (equal? obj (car (car al)))
                  (car al)
                  (loop (cdr al))))))

;  Isspace? - is character whitespace.
   (define (isspace? c)
      (or (not c)
          (char=? c #\space)
          (char=? c #\&#TAB)))


;  Tokenise-string - tokenise a string.
   (define (tokenise-string str
                            #!key
                            (boundary-char? isspace?)
                            (isbdy? (lambda (l)
                                      (if (boundary-char? (car l))
                                          (let loop ((rest l))
                                            (if (null? rest)
                                                '()
                                                (if (boundary-char? (car rest))
                                                    (loop (cdr rest))
                                                    rest)))
                                          #f)))
                            (max -1))
     (let loop ((charlist (string->list str))
                (wordlist '())
                (currword '())
                (splits max))
       (if (or (= splits 0)                ;reached max split
               (null? charlist))           ;nothing more to do
           (let ((cw (append currword charlist)))
             (if (null? cw)
                 wordlist
                 (append wordlist (list (list->string cw)))))
           (let ((nextword (isbdy? charlist)))
             (if nextword
                 (loop nextword            ;word just ended - add to list
                       (append wordlist (list (list->string currword)))
                       '()
                       (- splits 1))
                 (loop (cdr charlist)      ;within word
                       wordlist
                       (append currword (list (car charlist)))
                       splits))))))


;  Get-mediatypes - get value of the media attribute as associative list.
      (define (get-mediatypes #!optional (nd (current-node)))
         (let ((medstr (attribute-string (normalize "media") nd)))
            (if medstr
                (map (lambda (s)
                        (let ((l (tokenise-string (trim-string s) max: 1)))
                           (cons (car l) (if (> (length l) 1) 
                                             (car (cdr l)) 
                                             #f))))
                   (tokenise-string 
                      (normalize medstr)
                      boundary-char?: (lambda (c) (char=? c #\,))))
              #f)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;  Rules for document processing.

;  Default rule.
      (default
         (make sequence
            (process-children-normal)
            (newline)))


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
            (process-children)
            (newline)))


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
            (process-children)
            (newline)))
       


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
            (process-children-normal)
            (newline)))

      (element parameterlist
         (make sequence
            (heading 2 "Parameters")
            (request "LP")
            (line "For information on individual parameters")
            (line "select from the list below:")
            (process-children-normal)
            (newline)))

      (element authorlist
         (make sequence
            (heading 2 "Authors")
            (process-children-normal)
            (newline)))

      (element history
         (make sequence
            (heading 2 "History")
            (process-children-normal)
            (newline)))

      (element implementationstatus
         (make sequence
            (heading 2 "Implementation Status")
            (indent-block (process-children-normal))))

      (element moduletype
         (make sequence
            (heading 2 "Type of Module")
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
            (newline)
            (if (node-list-empty? (follow (current-node)))
                (empty-sosofo)
                (request "sp"))))

      (element px
         (make sequence
            (request "LP")
            (process-children-normal)
            (newline)
            (if (node-list-empty? (follow (current-node)))
                (empty-sosofo)
                (request "sp"))))
      
      (element blockquote
         (make sequence
            (request "sp")
            (indent-block (process-children-normal))
            (with-mode active (process-first-descendant 'attribution))
            (request "sp")))

      (element span
         (let ((mediatypes (get-mediatypes (current-node))))
            (if (or (not mediatypes)
                    (assoc "tty" mediatypes)
                    (assoc "all" mediatypes))
                (make sequence
                   (process-children-normal)
                   (newline))
                (empty-sosofo))))

            
;  Programcode specific elements.
      (element verbatim
         (make sequence
            (newline)
            (request "(l L")
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
            (newline)
            (request "in 0")
            (request "ba 0")))

      (element (examplenote)
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
            (newline)
            (request "ba -4")
            (request "sp")))

      (element author
         (let*
            ((id (attribute-string (normalize "id")))
             (name (data (node-list-first (children (current-node)))))
             (affiliation (attribute-string (normalize "affiliation"))))
            (make sequence
               (request "LP")
               (line (string-append id ":"))
               (line name)
               (line (string-append "(" affiliation ")"))
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
            (newline)
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
            (newline)
            (line (string-append 
                     (string #\<)
                     (trim-string (attribute-string (normalize "url")))
                     (string #\>)))))



;  Math elements.  Processing of these currently just spits out the latex
;  source, which is not entirely satisfactory.

      (element m
         (make sequence
            (process-children)
            (newline)))
      (element mequation
         (indent-block (process-children-verb 5)))
      (element mlabel
         (empty-sosofo))
      (element meqnarray
         (indent-block (process-children)))
      (element mline
         (make sequence 
            (process-children-verb 5)
            (newline)))


;  Lists.
      (element dl
         (make sequence
            (indent-block (process-children-normal))
            (request "sp")))

      (element ul
         (make sequence 
            (indent-block (process-children-normal))
            (request "sp")))

      (element ol
         (make sequence
            (indent-block (process-children-normal))
            (request "sp")))


;  Descriptive list elements.
      (element dt
         (make sequence
            (request "sp")
            (request "LP")
            (process-children-normal)
            (newline)
            (make formatting-instruction data: (string #\\ #\0))
            (newline)))

      (element dd
         (if (attribute-string (normalize "compact") (parent (current-node)))
             (make sequence 
                (request-nobreak "in +3")
                (request-nobreak "ba +3")
                (process-pblock)
                (newline)
                (request "ba -3")
                (request "LP"))
             (indent-block (process-children-normal))))
             

;  Unordered list elements.
      (element (ul li)
         (make sequence
            (request "bu")
            (process-pblock)
            (if (attribute-string (normalize "compact") (parent (current-node)))
                (empty-sosofo)
                (request "sp"))
            (newline)))

;  Ordered list elements.
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
               (newline)
               (request "TE"))))

       (element colspec (empty-sosofo))

       (element thead
          (make sequence
             (process-children-normal)
             (newline)
             (line "=")))

       (element tbody
          (make sequence 
             (process-children-normal)
             (newline)))

       (element row
          (make sequence
             (process-children-normal)
             (newline)))

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
