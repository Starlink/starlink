<!doctype programcode public "-//Starlink//DTD DSSSL Source Code 0.2//EN">

<![ ignore [ $Id$ ]]>

<docblock>
<title>LaTeX stylesheet configuration parameters

<codegroup id=code.params>
<title>LaTeX stylesheet parameters
<description>
This collects together parameters which control various aspects of the 
generation of LaTeX.  All the things documented as
`functions' below are really parameters.

<p>Note that a feature of Jade is that if the argument <code/-V
variable/ is given on the command line, then that variable is set to
<code/#t/, overriding any setting within the stylesheet.

<authorlist>
<author id=ng affiliation='Glasgow'>Norman Gray

<copyright>Copyright 1999, Particle Physics and Astronomy Research Council

<func>
<routinename>%latex-document-class%
<description>The type of document generated.
<returnvalue type='list of strings'>
<argumentlist none>
<codebody>
(define %latex-document-class%
  (list "?twoside,11pt,a4paper" "article"))

<func>
<routinename>%latex-document-preamble%
<description>Any definitions which are to be emitted as part of the LaTeX
preamble.
<p>The <code/\\url{}/ command in <code/url.sty/ sets URLs much better than
the following does, but we want to avoid requiring non-default packages in
the LaTeX we emit, so insert the following
crude (but effective enough) macro to allow `hyphenation' at the
dots and slashes in machine or file names, URLs, etc.  
The <code/\\urlfootnote/ command inserts a URL into a footnote.
<returnvalue type=string>
<argumentlist none>
<codebody>
(define %latex-document-preamble%
  "\\setcounter{secnumdepth}{0}
\\setcounter{tocdepth}{0}
\\makeatletter
\\def\\p@thdots{\\discretionary{.}{}{.}}
\\def\\p@thslash{\\hskip 0pt plus 0.5pt
	\\discretionary{/}{}{/}\\hskip 0pt plus 0.5pt\\relax }
{\\catcode`\\.=\\active \\catcode`\\/=\\active 
\\gdef\\p@thcats{%
  \\catcode`\\%=12 \\catcode`\\~=12 \\catcode`\\#=12 \\catcode`\\&=12
  \\catcode`\\_=12 \\catcode`\\.=\\active \\let.\\p@thdots
  \\catcode`\\/=\\active        \\let/\\p@thslash
  }}
\\def\\p@th#1{\\verbatim@font <\\nobreak #1\\nobreak>\\endgroup}
\\newcommand{\\url}{\\begingroup\\p@thcats\\p@th}
\\def\\verbatim@font{\\normalfont\\small\\ttfamily}
\\def\\urlfootnote{\\begingroup\\p@thcats\\@urlfootnote}
\\def\\@urlfootnote#1{\\footnote{\\raggedright\\verbatim@font #1}\\endgroup}
\\makeatother
")
