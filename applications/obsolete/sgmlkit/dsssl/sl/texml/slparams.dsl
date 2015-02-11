<!-- 
  $Id$


  This file collects together parameters which control various
  aspects of the generation of LaTeX.  All the things documented as
  `functions' below are really parameters.

  Note that a feature of Jade is that if the argument <code/-V
  variable/ is given on the command line, then that variable is set to
  <code>#t</code>, overriding any setting within the stylesheet.  The
  parameters which are described as `boolean' below can be set in this
  way.

  If you want to change any other parameters, then make a copy of this
  file called, say, params-mod.dsl, modify it to suit your needs, and
  create a catalogue file in the same directory which has a line like:

      PUBLIC "-//Starlink//TEXT DSSSL TeXML Parameterisation//EN" 
          ../dsssl/sl/texml/slparams.dsl

  (but all on a single line), giving the path to the parameterisation file,
  relative to the catalog.  Then adjust your $SGML_CATALOG_FILES
  environment variable to put this catalogue file early in the path.
-->

<routine>
<routinename>%latex-document-class%
<description>The type of document generated.
<returnvalue type='list of strings'>List of arguments to the
documentclass command, including a required argument for the document
class, preceded by an optional argument for the class options.
<codebody>
(define %latex-document-class%
  (list "?twoside,11pt,a4paper" "article"))

<routine>
<routinename>%texml-manifest%
<description>
If not '#f' then the list of files created by the
stylesheet will be written to the file named by '%texml-manifest%'.
<returnvalue type=string>Manifest filename
<codebody>
(define %texml-manifest%
	"texml.manifest"
	)

<routine>
<routinename>%latex-document-general-preamble%
<description>
<p>Any definitions which are to be emitted as part of the LaTeX
preamble.

<p>The <code>\\Url{}</code> command in <code>url.sty</code> sets URLs much better than
the following does, but we want to avoid requiring non-default packages in
the LaTeX we emit, so insert the following
crude (but effective enough) macro to allow `hyphenation' at the
dots and slashes in machine or file names, URLs, etc.  
The <code>\\UrlFootnote</code> command inserts a URL into a footnote.

<p>The stylesheet generates relatively high-level LaTeX, rather than emitting
detailed formatting codes.  This means that any LaTeX hackery that's required
can be confined to this `style file', and incorporated into the preamble 
rather than being scattered through the stylesheets.

<p>The stylesheet must put all of the following into a
<code>FrontMatter</code> environment.  Within that, it must 
call <code>\\setTitle</code> and <code>\\setAuthorlist</code>,
which contains a <code>fmtAuthorlist</code> environment containing a
sequence of <code>\\fmtAuthor</code> commands), <code>\\setDate</code>.  It should
also call <code>\\setDocCode</code> with the document code (eg, `SUN/123.4') and
<code>\\setDocRef</code> with the document description (eg, `Starlink User Note
123.4'), and set the copyright message with <code>\\setCopyright</code>.
After these, it must call <code>\\MakeTitle</code>, then create a
<code>VersoTitlepage</code> environment with any additional matter to go 
on the verso of the titlepage.  It may then call <code>\\TableOfContents</code>.

<returnvalue type=string>String with the LaTeX preamble in it.
<codebody>
(define %latex-document-general-preamble%
   "\\setcounter{secnumdepth}{0}
\\setcounter{tocdepth}{2}
\\pagestyle{myheadings}
\\setlength{\\textwidth}{160mm}
\\setlength{\\textheight}{230mm}
\\setlength{\\topmargin}{-2mm}
\\setlength{\\oddsidemargin}{0mm}
\\setlength{\\evensidemargin}{0mm}
\\setlength{\\parindent}{0mm}
\\setlength{\\parskip}{\\medskipamount}
\\setlength{\\unitlength}{1mm}
<!-- Are we using pdftex?  Use the ifpdf package, rather than attempting
     to roll our own: we can by now (2014) assume that this package will be
     present in any half-way reasonably up-to-date TeX installation.
     Rolling our own just causes breakage when some other package includes
     ifpdf, which hyper-cautiously bails out if it finds \ifpdf already defined. -->
\\usepackage{ifpdf}
<!-- Prepare to use @ in command names -->
\\makeatletter
<!-- Catcode all specials to other, and add discretionary hyphenation for
     dots and slashes within URLs and paths.  There's no need for any
     `namespace' prefix - we're writing a document here, not a package.
     However, for clarity, all the `user visible' commands here have at
     least one uppercase letter. -->
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
\\def\\Url{\\begingroup\\p@thcats\\p@th}
\\def\\verbatim@font{\\normalfont\\small\\ttfamily}
\\def\\UrlFootnote{\\begingroup\\p@thcats\\@urlfootnote}
\\def\\@urlfootnote#1{\\footnote{\\raggedright\\verbatim@font #1}\\endgroup}
<!-- % Formatting of individual elements -->
\\let\\Code\\texttt
\\let\\Kbd\\texttt
\\newcommand\\Quote[1]{`#1'}
\\let\\Strong\\textbf
\\let\\Cite\\textit
<!-- Verbatimlines environment.  This is based fairly distantly on the
     LaTeX raggedright environment, but somewhat simplified.  Note
     that the LaTeX text _must_ be generated in such a way that
     there's no newline between `\end' and `{Verbatimlines}'.  Also
     note that putting this inside, for example, {quote} stuffs things
     up, because {quote} is implemented as a trivlist, which plays
     merry hell with any indents inside {Verbatimlines}. 

     The Verbatimlines* environment differs by ignoring leading spaces.
     -->
<!-- \SpaceSpaceSkip skips by \fontdimen2 (interword space) in the
     current font, which will typically be \verbatim@font
     -->
\\def\\SpaceSpaceSkip{\\hskip \\fontdimen2\\font}
<!-- Reasonable default for active space -->
{\\obeyspaces\\global\\let =\\space}
\\def\\Verbatimlines{\\let\\SpaceSkip\\SpaceSpaceSkip\\Verbatim@lines}
\\@namedef{Verbatimlines*}{\\let\\SpaceSkip\\space\\Verbatim@lines}
<!-- Following is LaTeX {verbatim} env, with non-zero leftskip, 
     minus the \dospecials, and plus the Verbatim@space -->
{\\obeyspaces\\gdef\\Verbatim@space{\\let =\\SpaceSkip}}
\\def\\Verbatim@lines{\\trivlist \\item\\relax
  \\if@minipage\\else\\vskip\\parskip\\fi
  \\leftskip=4em
  \\rightskip\\z@skip
  \\parindent\\z@\\parfillskip\\@flushglue\\parskip\\z@skip
  \\@@par
  \\@tempswafalse
  \\def\\par{%
    \\if@tempswa
      \\leavevmode \\null \\@@par\\penalty\\interlinepenalty
    \\else
      \\@tempswatrue
      \\ifhmode\\@@par\\penalty\\interlinepenalty\\fi
    \\fi}%
  <!-- Omit the \@noligs command that's present in LaTeX's {verbatim}
       environment.  It makes things like ' active, which ends up
       swallowing following spaces.  The \tt fonts don't have (many?)
       ligatures anyway, so nothing's lost. -->
  \\obeylines \\verbatim@font
  \\everypar \\expandafter{\\the\\everypar \\unpenalty}%
  \\obeyspaces\\Verbatim@space%
}
\\def\\endVerbatimlines{\\if@newlist \\leavevmode\\fi\\endtrivlist}
\\@namedef{endVerbatimlines*}{\\endVerbatimlines}
<!-- I may make \DTitem more sophisticated about linebreaking -->
\\def\\DTitem#1{\\item[#1]}
<!-- Title page. 
     No error checking - assume the stylesheet takes care of this -->
\\let\\@Abstract\\@empty
\\long\\def\\setAbstract#1{\\def\\@Abstract{#1}}
\\let\\@Title\\@empty
\\def\\setTitle#1{\\def\\@Title{#1}}
\\let\\@SubTitle\\@empty
\\def\\setSubTitle#1{\\def\\@SubTitle{#1}}
\\let\\@DocCode\\@empty
\\def\\setDocCode#1{\\def\\@DocCode{#1}\\markboth{\\@DocCode}{\\@DocCode}}
\\let\\@DocRef\\@empty
\\def\\setDocRef#1{\\def\\@DocRef{#1}}
\\let\\@Authorlist\\@empty
\\def\\setAuthorlist#1{\\def\\@Authorlist{#1}}
\\let\\@Date\\@empty
\\def\\setDate#1{\\def\\@Date{#1}}
\\let\\@Copyright\\@empty
\\long\\def\\setCopyright#1{\\def\\@Copyright{#1}}
\\let\\@Coverimage\\@empty
%\\long\\def\\setCoverimage#1{\\def\\@Coverimage{#1}}
% \\setCoverimage* does not put the document in a minipage (use for images)
\\def\\setCoverimage{\\@ifnextchar*\\@tempswatrue\\@tempswafalse
    \\@dosetCoverimage}
\\long\\def\\@dosetCoverimage#1{\\if@tempswa \\def\\@Coverimage{#1}\\else
    \\def\\@Coverimage{\\@tempdima\\textwidth
        \\multiply\\@tempdima 9 \\divide\\@tempdima 10
        \\begin{minipage}{\\@tempdima}\\parskip=\\smallskipamount
           \\parindent=0pt #1\\end{minipage}}\\fi}
% Now create the title page
\\newenvironment{FrontMatter}{\\renewcommand\\thepage{\\roman{page}}}
  {\\cleardoublepage
    \\renewcommand\\thepage{\\arabic{page}}
    \\setcounter{page}{1}}
\\def\\MakeTitle{%
  \\thispagestyle{empty}
  \\noindent CCLRC / {\\scshape Rutherford Appleton Laboratory}
  \\ifx\\@DocCode\\@empty\\else		% optional
    \\hfill{\\bfseries \\@DocCode}\\fi
  \\\\
  {\\large Particle Physics \\& Astronomy Research Council}\\\\
  {\\large Starlink Project}
  \\ifx\\@DocRef\\@empty\\else   		% optional
    \\\\{\\large\\@DocRef}\\fi
  \\begin{flushright}
    \\@Authorlist
    \\@Date
  \\end{flushright}
  \\vspace{-4mm}\\rule{\\textwidth}{0.5mm}\\vspace{5mm}
  \\begin{center}\\Huge\\bfseries \\@Title
    \\ifx\\@SubTitle\\@empty\\else
        \\vspace{2ex}\\\\\\LARGE\\bfseries\\@SubTitle\\fi
    \\end{center}\\vspace{4ex}
  \\ifx\\@Abstract\\@empty\\else		% optional
    \\vspace{10mm}\\begin{center}\\Large\\bfseries Abstract\\end{center}
      \\begin{flushleft}\\@Abstract\\end{flushleft}\\fi
  \\ifx\\@Coverimage\\@empty\\else	% optional
    \\vbox to 0pt{\\vspace{10mm}	% cram onto page, not onto verso
        \\begin{center}\\fbox{\\@Coverimage}\\end{center}\\vss}\\fi
}
\\newenvironment{fmtAuthorlist}{\\def\\fmtAuthor##1{##1\\\\}}{}
\\newenvironment{fmtOtherAuthors}{Also: }{}
\\newenvironment{VersoTitlepage}{\\clearpage\\hbox{}\\vfill}{%
  \\ifx\\@Copyright\\@empty\\else
    \\par\\vspace{2ex}\\copyright \\@Copyright\\fi}
<!-- replace \\caption, and hence table/figure counter and assoc
     mechanisms.  Allow the caller to set the caption type, which is
     necessary if we set a tabular+caption outside of a (floating) {table}.
     -->
\\def\\SetCapType#1{\\def\\@captype{#1}}%
\\def\\Caption#1#2{\\expandafter\\def\\csname fnum@\\@captype\\endcsname{#1}%
  \\expandafter\\def\\csname the\\@captype\\endcsname{#2}%
  \\@dblarg{\\@caption\\@captype}}
<!-- Set equation number -->
\\newif\\if@SetEqnNum\\@SetEqnNumfalse
\\def\\SetEqnNum#1{\\global\\def\\Eqn@Number{#1}\\global\\@SetEqnNumtrue}
\\def\\@eqnnum{{\\normalfont \\normalcolor (\\Eqn@Number)}}
\\def\\equation{$$}
\\def\\endequation{\\if@SetEqnNum\\eqno \\hbox{\\@eqnnum}\\global\\@SetEqnNumfalse\\fi 
    $$\\@ignoretrue}
\\def\\@@eqncr{\\let\\reserved@a\\relax
    \\ifcase\\@eqcnt \\def\\reserved@a{& & &}\\or \\def\\reserved@a{& &}%
     \\or \\def\\reserved@a{&}\\else
       \\let\\reserved@a\\@empty
       \\@latex@error{Too many columns in eqnarray environment}\\@ehc\\fi
     \\reserved@a \\if@SetEqnNum\\@eqnnum\\global\\@SetEqnNumfalse\\fi
     \\global\\@eqcnt\\z@\\cr}
% Make \\nonumber a no-op, so it can't cause confusion
\\let\\nonumber\\relax
<!-- Other bits and bobs -->
\\def\\CharUnderscore{\\ifmmode _\\else\\texttt{\\char\"5F}\\fi}
\\def\\CharDquote{\\texttt{\\char\"22}}
\\def\\CharBackslash{\\texttt{\\char\"5C}}
\\def\\CharLangle{\\texttt{\\char\"3C}}
\\def\\CharRangle{\\texttt{\\char\"3E}}
\\def\\CharVbar{\\texttt{\\char\"7C}}
\\def\\Eqnref#1{Eqn.~(#1)}
<!-- \TabMod is a dimension used when formatting tables.  
     See latex/sltables.dsl -->
\\newdimen\\TabMod
<!-- Angle formatting macros.  See slmisc.dsl -->
\\def\\hmsangle#1{#1^{\\rm h}}
\\def\\hmsminutes#1{\\,#1^{\\rm m}}
\\def\\hmsseconds#1{\\,#1}
\\def\\hmsfraction#1{{}^{\\rm s}\\!\\!.#1}
\\def\\dmsangle#1{#1^{\\circ}}
\\def\\dmsminutes#1{\\,#1\\raisebox{-0.5ex}{$^{'}$}}
\\def\\dmsseconds#1{\\,#1}
\\def\\dmsfraction#1{\\hbox to 0pt{${}^{\\prime\\mskip-1.5mu \\prime}\\hss$}.#1}
<!-- index cross-references: see slback.dsl -->
\\def\\seealso#1#2{\\emph{See also:} #1}
")

(define %latex-ordinary-toc%
  "\\def\\TableOfContents{\\clearpage\\tableofcontents}
")

(define %latex-onepass-toc%
  "\\def\\@OpenTocFile#1{\\expandafter\\newwrite\\csname tf@#1\\endcsname
  \\immediate\\openout\\csname tf@#1\\endcsname \\jobname.#1 \\relax}
\\newcommand\\TableOfContents{\\cleardoublepage
  \\xdef\\@TocStartPage{\\the\\c@page}
  \\AtEndDocument\\WriteReadTableOfContents
  }
% Redefine @writefile to open output files on demand
\\long\\def\\@writefile#1#2{%
  \\@ifundefined{tf@#1}{\\@OpenTocFile{#1}}\\relax
  {\\@temptokena{#2}%
   \\immediate\\write\\csname tf@#1\\endcsname{\\the\\@temptokena}%
  }}
\\newcommand\\WriteReadTableOfContents{% append command to end of .aux file
  \\hbox{}%
<!-- Following write must not be immediate (or else out of sequence), 
     but it gets skipped if there's nothing on the page.  This can cause
     blank pages, though. -->
  \\write\\@mainaux{\\string\\ReadTableOfContents}}
\\newcommand\\ReadTableOfContents{%
  \\@ifundefined{@TocStartPage}\\relax  % this is beginning of a second pass
  {\\cleardoublepage
    \\renewcommand\\thepage{\\roman{page}}%
    \\setcounter{page}{\\@TocStartPage}%
    \\@ifundefined{tf@toc}\\relax
      {\\immediate\\closeout\\tf@toc
       \\section*{Table of Contents}
       \\@input{\\jobname.toc}}%
    \\@ifundefined{tf@lof}\\relax
      {\\immediate\\closeout\\tf@lof
       \\clearpage
       \\section*{List of Figures}
       \\@input{\\jobname.lof}}%
    \\@ifundefined{tf@lot}\\relax
      {\\immediate\\closeout\\tf@lot
       \\clearpage
       \\section*{List of Tables}
       \\@input{\\jobname.lot}}%
    \\clearpage  % flush pages
    }}
")

(define %latex-sst-preamble%
  ;; This is a modified version of the sst.tex file from the
  ;; now-defunct SUN/110 distribution.  It has been changed so that
  ;; most of the commands have become environments, which means that
  ;; it is now possible to have catcode-changing environments such as
  ;; {verbatim} in the arguments.

;;  Name:
;;     SST.TEX
;;  Purpose:
;;     Define LaTeX commands for laying out Starlink routine descriptions.
;;  Language:
;;     LaTeX
;;  Type of Module:
;;     LaTeX data file.
;;  Description:
;;     This file defines LaTeX commands which allow routine documentation
;;     produced by the SST application PROLAT to be processed by LaTeX and
;;     by LaTeX2html. The contents of this file should be included in the
;;     source prior to any statements that make of the sst commnds.
;;  Notes:
;;     The commands defined in the style file html.sty provided with LaTeX2html 
;;     are used. These should either be made available by using the appropriate
;;     sun.tex (with hypertext extensions) or by putting the file html.sty 
;;     on your TEXINPUTS path (and including the name as part of the  
;;     documentstyle declaration).
;;  Authors:
;;     RFWS: R.F. Warren-Smith (STARLINK)
;;     PDRAPER: P.W. Draper (Starlink - Durham University)
;;  History:
;;     10-SEP-1990 (RFWS):
;;        Original version.
;;     10-SEP-1990 (RFWS):
;;        Added the implementation status section.
;;     12-SEP-1990 (RFWS):
;;        Added support for the usage section and adjusted various spacings.
;;     8-DEC-1994 (PDRAPER):
;;        Added support for simplified formatting using LaTeX2html.
;;     {enter_further_changes_here}
;;  Bugs:
;;     {note_any_bugs_here}
;;
"<!-- Define length variables. -->
\\newlength{\\sstbannerlength}
\\newlength{\\sstcaptionlength}
\\newlength{\\sstexampleslength}
\\newlength{\\sstexampleswidth}
<!-- Define a \\tt font of the required size. -->
\\newfont{\\ssttt}{cmtt10 scaled 1095}
<!-- Define a command to produce a routine header, including its name,
     a purpose description and the rest of the routine's documentation.
     -->
\\def\\routinesubsectlevel{section}  % to be overridden by generated text
\\newenvironment{sstroutine}[2]{
   \\goodbreak
   \\rule{\\textwidth}{0.5mm}
   \\vspace{-7ex}
   \\newline
   \\settowidth{\\sstbannerlength}{{\\Large {\\bf #1}}}
   \\setlength{\\sstcaptionlength}{\\textwidth}
   \\setlength{\\sstexampleslength}{\\textwidth}
   \\addtolength{\\sstbannerlength}{0.5em}
   \\addtolength{\\sstcaptionlength}{-2.0\\sstbannerlength}
   \\addtolength{\\sstcaptionlength}{-5.0pt}
   \\settowidth{\\sstexampleswidth}{{\\bf Examples:}}
   \\addtolength{\\sstexampleslength}{-\\sstexampleswidth}
   \\parbox[t]{\\sstbannerlength}{\\flushleft{\\Large {\\bf #1}}}
   \\parbox[t]{\\sstcaptionlength}{\\center{\\Large #2\\par}}
   \\parbox[t]{\\sstbannerlength}{\\flushright{\\Large {\\bf #1}}}
   \\addcontentsline{toc}\\routinesubsectlevel{Routine #1}
   \\begin{description}}
  {\\end{description}}
<!-- Format the description section. -->
\\newenvironment{sstdescription}{\\item[Description:]}{}
<!-- Format the usage section. -->
\\newenvironment{sstusage}{\\item[Usage:] \\mbox{} \\\\[1.3ex] \\ssttt}{}
<!-- Format the invocation section. -->
\\newenvironment{sstinvocation}{\\item[Invocation:]\\hspace{0.4em}\\tt}{}
<!-- Format the arguments section. -->
\\newenvironment{sstarguments}{
   \\item[Arguments:] \\mbox{} \\\\
   \\vspace{-3.5ex}
   \\begin{description}}{\\end{description}}
<!-- Format the returned value section (for a function). -->
\\newenvironment{sstreturnedvalue}{
   \\item[Returned Value:] \\mbox{} \\\\
   \\vspace{-3.5ex}
   \\begin{description}}{\\end{description}}
<!-- Format the parameters section (for an application). -->
\\newenvironment{sstparameters}{
   \\item[Parameters:] \\mbox{} \\\\
   \\vspace{-3.5ex}
   \\begin{description}}{\\end{description}}
<!-- Format the examples section.  Modified SST examples container,
     which does NOT put its contents into a list environment, because
     that plays merry hell with the spacings in inner environments,
     specifically Verbatimlines. -->
\\newenvironment{sstexamples}{
    \\item[Examples:]\\mbox{}\\\\
    \\vskip -3ex \\@tempcnta 1}{}
<!-- Define the format of a subsection in a normal section. -->
\\newcommand{\\sstsubsection}[1]{ \\item[{#1}] \\mbox{} \\\\}
<!-- Define the format of a subsection in the examples section.
     The example numbering is rather redundant, but it looks better if
     there's something on the line before the (indented, tt-font)
     example. -->
\\newenvironment{sstexamplesubsection}{
    \\par Example \\the\\@tempcnta:\\par \\global\\advance\\@tempcnta 1}{}
<!-- Format the notes section. -->
\\newcommand{\\sstnotes}[1]{\\item[Notes:] \\mbox{} \\\\[1.3ex] #1}
<!-- Provide a general-purpose format for additional (DIY) sections. -->
\\newenvironment{sstdiytopic}[1]{\\item[{#1:}]\\mbox{}\\\\}{}
<!-- Format the implementation status section. -->
\\newenvironment{sstimplementationstatus}{
   \\item[{Implementation Status:}] \\mbox{} \\\\[1.3ex]}{}
<!-- Format the bugs section. -->
\\newenvironment{sstbugs}{\\item[Bugs:]}{}
<!-- Format a list of items while in paragraph mode. -->
\\newcommand{\\sstitemlist}[1]{
  \\mbox{} \\\\
  \\vspace{-3.5ex}
  \\begin{itemize}
     #1
  \\end{itemize}
}
<!-- Define the format of an item. -->
\\newcommand{\\sstitem}{\\item}
"
  )


<!-- If there turn out to be nasty spacing problems to do with
     paragraphs and blank lines, they might be solvable by removing
     the TeX feature that interprets a blank line as a \par.  We can
     do this with `\\catcode`\\^^M=10'. -->
(define %latex-end-preamble%
  "\\makeatother
")

<routine>
<routinename>section-samepage
<description>
If true, sections will not automatically start a new page (ie, the
default is that they will).  This can conveniently ve set with
<code>-V section-samepage</code> on the Jade command line. 
<returnvalue type=boolean>True if sections are to start a new page.
<codebody>
(define section-samepage
  #f)

<routine>
<routinename>appendix-samepage
<description>
If true, appendices will not automatically start a new page (ie, the
default is that they will).  This can conveniently ve set with
<code>-V appendix-samepage</code> on the Jade command line. 
<returnvalue type=boolean>True if sections are to start a new page.
<codebody>
(define appendix-samepage
  #f)

<routine>
<routinename>onepass-latex
<description>
If true, the generatex LaTeX will be such that it can be processed in
a single pass.  This has the side-effect that the table of contents
is printed last, even though it has the correct page-numbering.
This can conveniently ve set with
<code>-V onepass-latex</code> on the Jade command line. 
<returnvalue type=boolean>True if we are to generate one-pass LaTeX.
<codebody>
(define onepass-latex
  #f)

<routine>
<routinename>%latex-float-spec%
<description>The optional argument to the LaTeX floating
environments.
<returnvalue>String containing LaTeX float specification.  It must
include the leading `?' which indicates it is an optional argument.
<codebody>
(define %latex-float-spec%
  "?tph")

<routine>
<routinename>%passthrough-mediatype%
<description>Specifies the media type, chosen from the list specified
in the HTML4 spec at
<url>http://www.w3.org/TR/REC-html40/types.html#h-6.13</url>.  At
present, only `screen', `print', `tty' and `all' are documented as being
recognised, but there's no reason why further ones couldn't be added
ad lib., or fancy games played here.  For consistency, however, one
should probably stick to the list in the HTML 4 spec.
<returnvalue type=string>Media type string, chosen from the HTML4 list
of media types.
<codebody>
(define %passthrough-mediatype%
  "print")

