<!doctype programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN" [
<!entity sldocs.dsl		system "sldocs.dsl">
<!entity slsect.dsl		system "slsect.dsl">
<!entity slmisc.dsl		system "slmisc.dsl">
<!entity slroutines.dsl		system "slroutines.dsl">
<!entity slmaths.dsl		system "slmaths.dsl">
<!entity lib.dsl		system "../lib/sllib.dsl" subdoc>
<!entity common.dsl		system "../common/slcommon.dsl" subdoc>
<!entity slparams.dsl		system "slparams.dsl" subdoc>
<!entity sllinks.dsl		system "sllinks.dsl" subdoc>
<!entity sltables.dsl		system "sltables.dsl" subdoc>
]>

<![ ignore [ $Id$ ]]>

<docblock>
<title>Starlink to LaTeX stylesheet
<description>
This is the stylesheet for converting the Starlink General DTD to LaTeX.
<p>It requires a <em/modified/ version of Jade, which supports the
<code/-t latex/ back-end.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council


<codereference doc="lib.dsl" id="code.lib">
<title>Library code
<description>
<p>Some of this library code is from the standard, some from Norm
Walsh's stylesheet, other parts from me

<codereference doc="common.dsl" id="code.common">
<title>Common code
<description>
<p>Code which is common to both the HTML and print stylesheets.

<codereference doc="slparams.dsl" id="code.params">
<docblock>
<title>Stylesheet parameters
<description>
Code which parameterises the code in this stylesheet

<codereference doc="sllinks.dsl" id="code.links">
<title>Inter- and Intra-document links
<description>Supports the <code/ref/, <code/docxref/, <code/webref/ and
<code/url/ elements.

<codereference doc="sltables.dsl" id="code.tables">
<title>Table handling
<description>
This provides simple (ie, incomplete) support for the table model.

<codegroup
  use="code.lib code.common code.params code.links code.tables"
  id=latex>
<title>Conversion to LaTeX

<misccode>
<description>Declare the flow-object-classes to support the LaTeX back-end
of Jade, written by me.
<codebody>
(declare-flow-object-class command
  "UNREGISTERED::Norman Gray//Flow Object Class::command")
(declare-flow-object-class empty-command
  "UNREGISTERED::Norman Gray//Flow Object Class::empty-command")
(declare-flow-object-class environment
  "UNREGISTERED::Norman Gray//Flow Object Class::environment")
(declare-flow-object-class fi
  "UNREGISTERED::James Clark//Flow Object Class::formatting-instruction")
(declare-flow-object-class entity
  "UNREGISTERED::James Clark//Flow Object Class::entity")
(declare-characteristic escape-tex?
  "UNREGISTERED::Norman Gray//Characteristic::escape-tex?"
  #t)

(define %stylesheet-version%
  "Starlink LaTeX stylesheet, version 0.1")

;; incorporate the simple stylesheets directly

&sldocs.dsl;
&slsect.dsl;
&slmisc.dsl;
&slroutines.dsl;
&slmaths.dsl;

<misccode>
<description>
The root rule.  Simply generate the LaTeX document at present.
<codebody>
(root
    (process-children))


<codegroup>
<title>The default rule
<description>There should be no default rule in the main group
(<code/style-specification/ in the terms of the DSSSL architecture),
so that it doesn't take priority over mode-less rules in other process
specification parts.  See the DSSSL standard, sections 7.1 and 12.4.1.

<p>Put a sample default rule in here, just to keep it out of the way
(it's ignored).
<misccode>
<description>The default rule
<codebody>
(default
  (process-children))
; ;; Make text that comes from unimplemented elements easy to spot
; (default
;   (make sequence
;     (make command name: "typeout"
; 	  parameters: (list (string-append "Element " (gi)
; 					   " not implemented")))
;     (make environment name: "UNIMPLEMENTED"
; 	  parameters: (list (gi))
; 	  (process-children))))
