<!-- 
  $Id$


  This file collects together parameters which control various
  aspects of the generation of LaTeX.  All the things documented as
  `functions' below are really parameters.

  Note that a feature of Jade is that if the argument <code/-V
  variable/ is given on the command line, then that variable is set to
  <code/#t/, overriding any setting within the stylesheet.  The
  parameters which are described as `boolean' below can be set in this
  way.

  If you want to change any other parameters, then make a copy of this
  file called, say, params-mod.dsl, modify it to suit your needs, and
  create a catalogue file in the same directory which has an line like:

    system "sl-latex-parameters" "params-mod.dsl"

  Then adjust your $SGML_CATALOG_FILES environment variable to put
  this catalogue file early in the path.
-->

<func>
<routinename>%latex-document-class%
<description>The type of document generated.
<returnvalue type='list of strings'>
<argumentlist none>
<codebody>
(define %latex-document-class%
  (list "?twoside,11pt,a4paper" "article"))

<func>
<routinename>show-element-ids
<description>
If true, then display exported IDs in section (etc) titles.  This is
useful for preparing a version of a document which you refer to while
working on it, or another which refers to it often.
<returnvalue type=boolean>
<argumentlist none>
<codebody>
(define show-element-ids #f)

<func>
<routinename>%latex-document-preamble%
<description>
<p>Any definitions which are to be emitted as part of the LaTeX
preamble.

<p>The <code/\\Url{}/ command in <code/url.sty/ sets URLs much better than
the following does, but we want to avoid requiring non-default packages in
the LaTeX we emit, so insert the following
crude (but effective enough) macro to allow `hyphenation' at the
dots and slashes in machine or file names, URLs, etc.  
The <code/\\UrlFootnote/ command inserts a URL into a footnote.

<p>The stylesheet generates relatively high-level LaTeX, rather than emitting
detailed formatting codes.  This means that any LaTeX hackery that's required
can be confined to this `style file', and incorporated into the preamble 
rather than being scattered through the stylesheets.

<p>The stylesheet must put all of the following into a
<code/FrontMatter/ environment.  Within that, it must 
call <code/\\setTitle/ and <code/\\setAuthorlist/,
which contains a <code/fmtAuthorlist/ environment containing a
sequence of <code/\\fmtAuthor/ commands), <code/\\setDate/.  It should
also call <code/\\setDocCode/ with the document code (eg, `SUN/123.4') and
<code/\\setDocRef/ with the document description (eg, `Starlink User Note
123.4'), and set the copyright message with <code/\\setCopyright/.
After these, it must call <code/\\MakeTitle/, then create a
<code/VersoTitlepage/ environment with any additional matter to go 
on the verso of the titlepage.  It may then call <code/\\TableOfContents/.

<returnvalue type=string>
<argumentlist none>
<codebody>
(define %latex-document-preamble%
  "\\setcounter{secnumdepth}{0}
\\setcounter{tocdepth}{2}
%
\\makeatletter
% Catcode all specials to other, and add discretionary hyphenation for
% dots and slashes within URLs and paths.  There's no need for any
% `namespace' prefix - we're writing a document here, not a package.
% However, for clarity, all the `user visible' commands here have at
% least one uppercase letter.
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
% Formatting of individual elements
\\let\\Code\\texttt
\\let\\Kbd\\texttt
\\newcommand\\Quote[1]{`#1'}
\\let\\Strong\\textbf
\\let\\Cite\\textit
% Title page.  No error checking - assume the stylesheet takes care of this
\\let\\@Abstract\\@empty
\\long\\def\\setAbstract#1{\\def\\@Abstract{#1}}
\\let\\@Title\\@empty
\\def\\setTitle#1{\\def\\@Title{#1}}
\\let\\@DocCode\\@empty
\\def\\setDocCode#1{\\def\\@DocCode{#1}}
\\let\\@DocRef\\@empty
\\def\\setDocRef#1{\\def\\@DocRef{#1}}
\\let\\@Authorlist\\@empty
\\def\\setAuthorlist#1{\\def\\@Authorlist{#1}}
\\let\\@Date\\@empty
\\def\\setDate#1{\\def\\@Date{#1}}
\\let\\@Copyright\\@empty
\\long\\def\\setCopyright#1{\\def\\@Copyright{#1}}
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
  \\begin{center}\\Huge\\bfseries \\@Title\\end{center}\\vspace{4ex}
  \\ifx\\@Abstract\\@empty\\else		% optional
    \\vspace{10mm}\\begin{center}\\Large\\bfseries Abstract\\end{center}
      \\begin{flushleft}\\@Abstract\\end{flushleft}\\fi
}
\\newenvironment{fmtAuthorlist}{\\def\\fmtAuthor##1{##1\\\\}}{}
\\newenvironment{fmtOtherAuthors}{Also: }{}
\\newenvironment{VersoTitlepage}{\\clearpage\\hbox{}\\vfill}{%
  \\ifx\\@Copyright\\@empty\\else
    \\par\\vspace{2ex}\\copyright \\@Copyright\\fi}
%% Generate the TOC in a single LaTeX pass.
%% This should be generic enough that LOT and LOF should be similar
%%\\newcommand\\TableOfContents{\\clearpage\\tableofcontents}
%\\newcommand\\TableOfContents{\\cleardoublepage
%  \\newwrite\\tf@toc
%  \\immediate\\openout\\tf@toc \\jobname.toc \\relax
%  \\immediate\\write\\tf@toc{\\string\\renewcommand\\string\\thepage{\\string\\roman{page}}\\string\\setcounter{page}{\\arabic{page}}}
%  \\immediate\\write\\tf@toc{\\string\\section*{Table of Contents}}
%  \\AtEndDocument\\ReadTableOfContents
%  }
%% Redefine addtocontents, to avoid writing via aux file
%\\def\\addtocontents#1#2{\\write\\csname tf@#1\\endcsname{#2}}
%\\newcommand\\ReadTableOfContents{%
%  \\cleardoublepage
%  \\immediate\\closeout\\tf@toc
%  \\@input{\\jobname.toc}}
% Generate TOC+LOF+LOT in a single LaTeX pass.
% TOC etc appear at _end_ of document, with correct page numbers.
% Use standard aux-file mechanism.  Indirection isn't necessary here, but
% too complicated to replace.
\\def\\@OpenTocFile#1{\\expandafter\\newwrite\\csname tf@#1\\endcsname
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
  \\hbox{}% Following write must not be immediate (or else out of sequence), 
  % but it gets skipped if there's nothing on the page.  This can cause
  % blank pages, though.
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
% replace \\caption, and hence table/figure counter and assoc mechanisms
\\def\\Caption#1#2{\\expandafter\\def\\csname fnum@\\@captype\\endcsname{#1}%
  \\expandafter\\def\\csname the\\@captype\\endcsname{#2}%
  \\@dblarg{\\@caption\\@captype}}
%\\catcode`\\^^M=10 % make end-of-line a space
\\makeatother
")

<func>
<routinename>%latex-manifest%
<description>
If not '#f' then the list of LaTeX files created by the
stylesheet will be written to the file named by '%latex-manifest%'.
<returnvalue type=string>Manifest filename
<argumentlist none>
<codebody>
(define %latex-manifest%
	"LaTeX.manifest"
	)

<func>
<routinename>suppress-manifest
<description>
If true, this will suppress the generation of a manifest, even if the variable 
<funcname/%latex-manifest%/ is given.
(This option can conveniently be set with <code/-V suppress-manifest/ on the 
Jade command line).
<returnvalue type=boolean>True if the manifest is to be suppressed
<argumentlist none>
<codebody>
(define suppress-manifest #f)

