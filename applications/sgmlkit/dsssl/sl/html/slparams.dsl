<!-- 
  $Id$


  This file collects together parameters which control various
  aspects of the generation of HTML.  All the things documented as
  `functions' below are really parameters.

  Note that a feature of Jade is that if the argument <code/-V
  variable/ is given on the command line, then that variable is set to
  <code/#t/, overriding any setting within the stylesheet.  The
  parameters which are described as `boolean' below can be set in this
  way.

  If you want to change any other parameters, then make a copy of this
  file called, say, params-mod.dsl, modify it to suit your needs, and
  create a catalogue file in the same directory which has an line like:

    system "sl-html-parameters" "params-mod.dsl"

  Then adjust your $SGML_CATALOG_FILES environment variable to put
  this catalogue file early in the path.
-->


<routine>
<routinename>%html-pubid%
<description>
The public ID of the HTML this output is claimed to be conformant with
<returnvalue type=string>Public ID
<codebody>
(define %html-pubid% "-//W3C//DTD HTML 3.2 Final//EN")


<routine>
<routinename>%body-attr%
<description>
The attributes added to the HTML body, controlling text colour, and the like.
<returnvalue type=list>List of lists of strings
<codebody>
(define %body-attr%
  '(("bgcolor" "#FFFFFF")
    ("text"    "#000000")))


<routine>
<routinename>%html-manifest%
<description>
If not <code/#f/ then the list of HTML files created by the
stylesheet will be written to the file named by <code/%html-manifest%/.
<returnvalue type=string>Manifest filename
<codebody>
(define %html-manifest%
	"HTML.manifest"
	)

<routine>
<routinename>suppress-manifest
<description>
If true, this will suppress the generation of a manifest, even if the variable 
<funcname/%html-manifest%/ is given.
(This option can conveniently be set with <code/-V suppress-manifest/ on the 
Jade command line).
<returnvalue type=boolean>True if the manifest is to be suppressed
<codebody>
(define suppress-manifest #f)

<routine>
<routinename>nochunks
<description>
If true, the entire source document is formatted as a single HTML
document.
(This option can conveniently be set with <code/-V nochunks/ on the 
Jade command line).
<returnvalue type=boolean>True if chunking is to be turned <em/off/.
<codebody>
(define nochunks #f)

<routine>
<routinename>stream-output
<description>
<p>If true, then the output is sent to the standard output, rather
than to a selection of generated entities/files.  If this is set true,
then <code/nochunks/ is effectively true as well.

<p>One point of this might be to control the name of the
generated HTML file.

<p>Set this with the Jade option <code/-V stream-output/.
<returnvalue type=boolean>True if output is to be streamed
<codebody>
(define stream-output #f)

<routine>
<routinename>%override-root-file-name%
<description>
If not <code/#f/, then this is the name of a file which is to become the 
root of the tree of files.  Note that this does <em/not/ affect the 
<funcname/root-file-name/ function.  See <funcname/html-file/.
<returnvalue type=string>File name, without extension
<codebody>
(define %override-root-file-name%
  #f ;"index"
  )

<routine>
<routinename>%html-ext%
<description>
Extension for HTML files
<returnvalue type=string>`.html', for example.
<codebody>
(define %html-ext% ".html")

<routine>
<routinename>%nav-header-table-attr%
<description>
The attributes added to the navigation header table
<returnvalue type='list of lists of strings'>List of attributes
<codebody>
(define %nav-header-table-attr%
  '(("BGCOLOR" "#FFFF99")
    ("WIDTH" "100%")
    ("BORDER" "0")))

<routine>
<routinename>%nav-footer-table-attr%
<description>
The attributes added to the navigation footer table
<returnvalue type='list of lists of strings'>List of attributes
<codebody>
(define %nav-footer-table-attr%
  '(("BGCOLOR" "#FFFF99")
    ("WIDTH" "100%")
    ("BORDER" "0")))
 
<routine>
<routinename>suppress-banner
<description>If true, then suppress the production of the banner, even when
  <code/%starlink-banner%/ is true (this option can be conveniently set
  using Jade, with the option <code/-V suppress-banner/).
<returnvalue type=boolean>True if the banner is <em/not/ to be printed.
<codebody>
(define suppress-banner #f)

<routine>
<routinename>%starlink-banner%
<description>
Return a sosofo which produces the Starlink/CCLRC/RAL/PPARC banner,
or false if none is to be produced.  See also (suppress-banner).
<returnvalue type=sosofo>Banner
<codebody>
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

<routine>
<routinename>%starlink-document-server%
<description>
Return a string which locates the remote document server.  This is
prefixed to URLs generated by (href-to)
<p>An alternative might be <code>"file:///star/docs/"</code>
<returnvalue type=string>Document server URL
<codebody>
(define %starlink-document-server%
  "http://star-www.rl.ac.uk/star/docs/")

<routine>
<routinename>show-element-ids
<description>
If true, then display exported IDs in section (etc) titles.  This is
useful for preparing a version of a document which you refer to while
working on it, or another which refers to it often.
<returnvalue type=boolean>True to show exported IDs
<codebody>
(define show-element-ids #f)

<routine>
<description>
The following attributes are not supported, and may disappear without
  warning.
<codebody>
(define %use-id-as-filename%
  #f)

<routine>
<routinename>%link-extension-list%
<description>
If not false, this specifies which output files are to have links generated
for them.
<returnvalue type='list of pairs of strings'>Each pair consists of a 
  file extension and a legend.  If <code/#f/, nothing is to be produced.
<codebody>
(define %link-extension-list%
  '(("tex" . "LaTeX") ("ps.gz" . "Postscript")))

<routine>
<routinename>suppress-printable
<description>If true, then suppress the production of the list of printable
  alternatives, even when
  <code/%link-extension-list%/ is true (this option can be conveniently set
  using Jade, with the option <code/-V suppress-printable/).
<returnvalue type=boolean>True if the list of printable alternatives
  is <em/not/ to be generated.
<codebody>
(define suppress-printable #f)

<routine>
<routinename>%short-crossrefs%
<description>If true, this will make the down-converter generate shorter 
link texts for cross-references.
<returnvalue type=boolean>True to make shorter crossrefs.
<codebody>
(define %short-crossrefs%
  #t)

