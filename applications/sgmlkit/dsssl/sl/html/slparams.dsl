<!DOCTYPE programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN">

<docblock>
<title>HTML stylesheet parameters

<codegroup id=code.params>
<title>HTML stylesheet parameters
<description>
<p>This file collects together parameters which control various
aspects of the generation of HTML.  All the things documented as
`functions' below are really parameters.

<p>Note that a feature of Jade is that if the argument <code/-V
variable/ is given on the command line, then that variable is set to
<code/#t/, overriding any setting within the stylesheet.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council


<func>
<routinename>%html-pubid%
<description>
<p>The public ID of the HTML this output is claimed to be conformant with</p>
<returnvalue type=string>
<argumentlist none>
<codebody>
(define %html-pubid%
  "-//W3C//DTD HTML 3.2 Final//EN")
</codebody></func>

<func>
<routinename>%body-attr%
<description><P>The attributes added to the HTML body, controlling
text colour, and the like.
<returnvalue type=list>
<argumentlist none>
<codebody>
(define %body-attr%
  (list (list "bgcolor" "#FFFFFF")
	(list "text" "#000000")))
</codebody>
</func>


<func>
<routinename>%html-manifest%
<description><p>If not '#f' then the list of HTML files created by the
stylesheet will be written to the file named by '%html-manifest%'.
<returnvalue type=string>Manifest filename
<argumentlist none>
<codebody>
(define %html-manifest%
	#f; "HTML.manifest"
	)
</codebody>
</func>

<func>
<routinename>nochunks
<description><p>
If true, the entire source document is formatted as a single HTML
document.
(This option can conveniently be set with <code/-V nochunks/ on the 
Jade command line).
<returnvalue type=boolean>True if chunking is to be turned <em/off/.
<argumentlist none>
<codebody>
(define nochunks #f)
</codebody>
</func>

<func>
<routinename>stream-output
<description>
<p>If true, then the output is sent to the standard output, rather
than to a selection of generated entities/files.  If this is set true,
then <code/nochunks/ is effectively true as well.

<p>The point of this is to be able to control the name of the
generated HTML file.  Arguably, the correct way to do this is to
root around in the grove and find the name of the source file,
then alter <funcname/root-file-name/ to use that information when it
generates the output file name.  That should be possible, and it
almost certainly involves the SosSequence property class (cf,
HyTime A.6.1, for example, or
<url>http://www.hytime.org/materials/sgmlpropset/classes/sosseq/index.htm</url>)

<p>Set this with the Jade option <code/-V stream-output/.
<returnvalue type=boolean>True if output is to be streamed
<argumentlist none>
<codebody>
(define stream-output #f)

<func>
<routinename>suppress-banner
<description>If true, then suppress the production of the banner, even when
  <code/%starlink-banner%/ is true.  Can be conveniently set using Jade, with
  the option <code/-V suppress-banner/.
<returnvalue>True if the banner is <em/not/ to be printed.
<argumentlist none>
<codebody>
(define suppress-banner #f)

<misccode>
<description><p>The rest of the parameters in this group are
miscellaneous tweaking parameters, which don't need much exrta
documentation
<codebody>
(define %html-ext%
  ;; extension for HTML files
  ".html")

(define %use-id-as-filename%
  #f)

(define %nav-header-table-attr%
  ;; The attributes added to the navigation header table
  (list ;(list "BGCOLOR" "#CCCCFF")
        (list "BGCOLOR" "#FFFF99")
	(list "WIDTH" "100%")
	(list "BORDER" "0")))

(define %nav-footer-table-attr%
  ;; The attributes added to the navigation footer table
  (list ;(list "BGCOLOR" "#CCCCFF")
	(list "BGCOLOR" "#FFFF99")
	(list "WIDTH" "100%")
	(list "BORDER" "0")))



(define %link-mailto-url%
  ;; REFENTRY htp-link-mailto-url
  ;; PURP Mailto URL for LINK REL=made
  ;; DESC
  ;; If not '#f', the '%link-mailto-url%' address will be used in a 
  ;; LINK REL=made element in the HTML HEAD.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

(define %html-header-tags% 
  ;; REFENTRY htp-html-header-tags
  ;; PURP What additional HEAD tags should be generated?
  ;; DESC
  ;; A list of the the HTML HEAD tags that should be generated.
  ;; The format is a list of lists, each interior list consists
  ;; of a tag name and a set of attribute/value pairs:
  ;; '(("META" ("NAME" "name") ("CONTENT" "content")))
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  '())

(define %stylesheet%
  ;; REFENTRY htp-stylesheet
  ;; PURP Name of the stylesheet to use
  ;; DESC
  ;; The name of the stylesheet to place in the HTML LINK TAG, or '#f' to
  ;; suppress the stylesheet LINK.
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  #f)

;;; Effectively always true!
;(define %gentext-nav-use-tables%
;  ;; REFENTRY htp-gentext-nav-use-tables
;  ;; PURP Use tables to build the navigation headers and footers?
;  ;; DESC
;  ;; If true, HTML TABLEs will be used to format the header and footer
;  ;; navigation information.
;  ;; /DESC
;  ;; AUTHOR N/A
;  ;; /REFENTRY
;  #t)

;; If so, how wide do you want them to be?
(define %gentext-nav-tblwidth% 
  ;; REFENTRY htp-gentext-nav-tblwidth
  ;; PURP If using tables for navigation, how wide should the tables be?
  ;; DESC
  ;; If tables are used for navigation (see '%gentext-nav-use-tables%'),
  ;; how wide should the tables be?
  ;; /DESC
  ;; AUTHOR N/A
  ;; /REFENTRY
  "100%")


;; Return a sosofo which produces the Starlink/CCLRC/RAL/PPARC banner,
;; or false if none is to be produced.  See also (suppress-banner).
(define %starlink-banner%
  (make sequence
    (make element gi: "A"
	  attributes: '(("HREF" "http://star-www.rl.ac.uk"))
	  (literal "Starlink Project"))
    (literal ": ")
    (make element gi: "A"
	  attributes: '(("HREF" "http://www.cclrc.ac.uk"))
	  (literal "CCLRC"))
    (literal " / ")
    (make element gi: "A"
	  attributes: '(("HREF" "http://www.cclrc.ac.uk/ral/"))
	  (literal "Rutherford Appleton Laboratory"))
    (literal " / ")
    (make element gi: "A"
	  attributes: '(("HREF" "http://www.pparc.ac.uk"))
	  (literal "PPARC"))
    ))

;; Return a string which locates the remote document server.  This is
;; prefixed to URLs generated by (href-to)
;(define %starlink-document-server%
;  "file:///star/docs/")
;(define %starlink-document-server%
;  "http://www.astro.gla.ac.uk/users/norman/star/testdocs/")
(define %starlink-document-server%
  "http://star-www.rl.ac.uk/star/docs/")
</codebody>
</misccode>
