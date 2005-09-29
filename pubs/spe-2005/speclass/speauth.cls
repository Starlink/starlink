%---------------------------------------------------------------------------
%Please  be  aware that the use of  this LaTeX class file is governed by the
%following conditions:
%
% Copyright (c) 2000 John Wiley & Sons, Ltd, The Atrium, Southern Gate, Chichester,
% West Sussex, PO19 8SQ UK.   All rights reserved.
%
%Rules of Use
%
%This class file is made available for use by authors who wish to prepare an
%article for publication in the journal Software---Practice and Experience
%published by John Wiley & Sons Ltd.  The user may not exploit any part of
%the class file commercially.
%
%This class file is provided on an `as is'  basis, without warranties of any
%kind, either express or implied, including but not limited to warranties of
%title,   or  implied  warranties  of  merchantablility  or  fitness  for  a
%particular purpose.  There will be no duty on the author[s] of the software
%or  John Wiley & Sons Ltd to correct any errors or defects in the software.
%Any  statutory  rights you may have remain unaffected by your acceptance of
%these rules of use.
%---------------------------------------------------------------------------
%
% Created by Alistair Smith, Sunrise Setting Ltd, 31 January 2000
% Amendments 6 March 2000
% \oddhead{} corrected for updated TeX distributions 16 March 2001
% speauth.cls --- For Softw. Pract. Exper.

\def\update{2002/09/23 v2.2}

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{speauth}[\update\ Softw. Pract. Exper.]

\newcommand\hmmax{0}

\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions
\LoadClass{article}
\RequirePackage{graphicx}
\RequirePackage{pifont,latexsym,ifthen,theorem,rotating,alltt,calc}
\RequirePackage{amsfonts,amssymb,amsbsy,amsmath}
\RequirePackage{times}
%\RequirePackage[mtbold]{mathtime}
\RequirePackage[errorshow]{tracefnt}

\renewcommand{\normalsize}{\fontsize{10}{12pt}\selectfont}
\renewcommand{\small}{\fontsize{9}{10pt}\selectfont}
\renewcommand{\footnotesize}{\fontsize{8}{9pt}\selectfont}
\renewcommand{\scriptsize}{\fontsize{8}{9.5pt}\selectfont}
\renewcommand{\tiny}{\fontsize{6}{7pt}\selectfont}
\renewcommand{\large}{\fontsize{11}{12pt}\selectfont}
\renewcommand{\Large}{\fontsize{14}{18pt}\selectfont}
\renewcommand{\LARGE}{\fontsize{17}{22pt}\selectfont}
\renewcommand{\huge}{\fontsize{20}{25pt}\selectfont}
\renewcommand{\Huge}{\fontsize{25}{30pt}\selectfont}

\newcommand{\titlesize}{\fontsize{18}{20pt}\selectfont}

%Page and general

\@twosidetrue
\frenchspacing
\flushbottom
\hoffset -39pt
\parindent 1em
\textwidth 34pc
\textheight 528pt%44 lines
\topmargin 0pt
\headsep 27pt
\headheight 36pt
\footskip 12pt
\hyphenpenalty=1000
\columnsep 2pc
\pretolerance=8000
\tolerance=9500
\hbadness=8000
\vbadness=9000
\displaywidowpenalty=0
\clubpenalty=10000
\widowpenalty=10000

\setlength{\abovedisplayskip}{8\p@ plus2\p@ minus5\p@}
\setlength{\belowdisplayskip}{\abovedisplayskip}
\setlength{\abovedisplayshortskip}{\z@ plus3\p@}
\setlength{\belowdisplayshortskip}{6\p@ plus3\p@ minus3\p@}
\setcounter{topnumber}{5}
\def\topfraction{1}
\setcounter{bottomnumber}{5}
\def\bottomfraction{.8}
\setcounter{totalnumber}{10}
\def\textfraction{0}
\def\floatpagefraction{.8}

\def\normalfloatspace{\floatsep 6pt
\textfloatsep 20pt plus 14pt
\intextsep 12pt plus 14pt
}
\normalfloatspace

\theorembodyfont{\itshape}
\theoremheaderfont{\normalfont\bfseries}
\setlength{\theorempreskipamount}{6pt plus 2pt}
\setlength{\theorempostskipamount}{6pt plus 2pt}

\begingroup \makeatletter
\gdef\th@plain{\normalfont\itshape
  \def\@begintheorem##1##2{%
        \item[\hskip\labelsep \theorem@headerfont ##1\ {##2.}]}%
\def\@opargbegintheorem##1##2##3{%
   \item[\hskip\labelsep \theorem@headerfont ##1\ ##2\ (##3)]}}
\endgroup

\def\refname{references}
\def\figurename{Figure}
\def\tablename{Table}
\def\abstractname{SUMMARY}

% Title page

% print integer as a zero filled 2 digit field
\def\twodigits#1{\ifnum #1 < 10{0#1}\else{#1}\fi}

% print integer as a zero filled 4 digit field
\def\fourdigits#1{\ifnum #1 < 10{000#1}\else
  \ifnum #1 < 100{00#1}\else
    \ifnum #1 < 1000{0#1}\else{#1}\fi
  \fi
\fi}

%SPE{first page}{last page}{vol}{issue}{year}
\def\SPE#1#2#3#4#5{
\gdef\@mymainhead{\parbox[t]{\textwidth}{{\footnotesize SOFTWARE---PRACTICE AND EXPERIENCE\hfill}\\
{\footnotesize\it Softw. Pract. Exper.\ }\footnotesize #5; {\bf#3}:\ifnum #1 = #2 #2 \else #1--#2\fi\ %
\hfill\scriptsize\textit{Prepared using \textsf{speauth.cls} [Version: \update]}\\[3pt]
\rule{21pc}{0.5pt}\\
\hspace*{24.5pc}\raisebox{-49pt}{\smash{\resizebox{9.5pc}{!}{\includegraphics{spelogol}}}}
}}
\setcounter{page}{#1}
\newcounter{pageone}
\setcounter{pageone}{#1}
\newcounter{lastpage}
\setcounter{lastpage}{#2}
\newcounter{pages}
\setcounter{pages}{1}
\addtocounter{pages}{\thelastpage}
\addtocounter{pages}{-\thepageone}
%\gdef\mymainfoot{CCC 0038--0644/#5/%
%\twodigits{#4}\fourdigits{#1}--\twodigits{\thepages}\$17.50}
\gdef\mymainfoot{\phantom{1}}
\gdef\@mycopyright{Copyright \copyright\ #5 John Wiley \& Sons, Ltd.}
\def\thevolume{#3}
\def\theissue{#4}
\def\theyear{#5}
}

\gdef\received#1{\gdef\@myreceive{Received #1}}
\gdef\revised#1{\gdef\@myrevise{Revised #1}}
\gdef\accepted#1{\gdef\@myaccepted{Accepted #1}}
\gdef\noreceived#1{\gdef\@myreceive{#1}}
\gdef\norevised#1{\gdef\@myrevise{#1}}
\gdef\noaccepted#1{\gdef\@myaccepted{#1}}

\def\ps@myplain{\let\@mkboth\@gobbletwo
\def\@oddhead{\@mymainhead}
\def\@evenhead{}
\def\@oddfoot{\parbox[t]{\textwidth}{%
\rule{\textwidth}{0.5pt}\\[2pt]%
\small\mymainfoot\hfill\small\textit{\@myreceive}\\
\small\@mycopyright\hfill\small\textit{\@myrevise}\\
\small\phantom{1}\hfill\small\textit{\@myaccepted}
}}
\def\@evenfoot{}
}

\thispagestyle{myplain}

\def\runningheads#1#2{\markboth{\uppercase{#1}}{\uppercase{#2}}}

\def\ps@spe{
\let\@mkboth\@gobbletwo
\def\@evenhead{\parbox[t]{\textwidth}{\normalsize%
\thepage\hspace*{1.5pc}\footnotesize{\leftmark}\hfill%
\smash{\raisebox{-10.5pt}{\resizebox{3pc}{!}{\includegraphics*{spelogos}}}}\\[12pt]
\rule{\textwidth}{0.5pt}}}
\def\@oddhead{\parbox[t]{\textwidth}{\normalsize{\hspace{0pt}}%
\smash{\raisebox{-10.5pt}{\resizebox{3pc}{!}{\includegraphics*{spelogos}}}}%
\hfill\footnotesize{\rightmark}\hspace*{1.5pc}\normalsize\thepage\footnotesize\\[12pt]
\rule{\textwidth}{0.5pt}}}
\def\@evenfoot{\parbox[t]{\textwidth}{\rule{\textwidth}{0.5pt}\\[13pt]
{\footnotesize \@mycopyright}%
\hfill{\footnotesize\it Softw.\ Pract.\ %
Exper.\ }\footnotesize \theyear;
{\bf\thevolume}:\thepageone--\thelastpage\\
\scriptsize\textit{Prepared using \textsf{speauth.cls}}\hfill}}
\def\@oddfoot{\@evenfoot}
}

\pagestyle{spe}


%%%%%%%%%
%Title, author, etc

\def\title#1{\begin{flushleft}
\vspace*{5pt}%
\parbox{21pc}{\raggedright\titlesize{\bfseries #1}}\\%
\vspace*{16pt}}

\def\author#1{\parbox{21pc}{\raggedright\large{#1}}\\[15pt]}

\def\longauthor#1{\parbox{32pc}{\raggedright\large{#1}}\\[15pt]}

\def\address#1{\parbox{21pc}{\raggedright\small\textit{#1}}\\[6pt]%
\end{flushleft}%
\rule{\textwidth}{0.5pt}}

\def\longaddress#1{\parbox{32pc}{\raggedright\small\textit{#1}}\\[6pt]%
\end{flushleft}%
\rule{\textwidth}{0.5pt}}

\def\summary{\vspace*{26pt}\noindent\small\textbf{%
SUMMARY}\\[6pt]%
\small\begin{bfseries}\noindent\ignorespaces}%
\def\endsummary{\end{bfseries}\endlist\normalsize\par\noindent}

\def\keywords#1{\vspace{-3pt}\noindent{\small\textsc{key words:}%
\hspace*{1em}#1}\vskip 0.1pt \normalsize\par\noindent}

\def\abstract{\summary}
\def\endabstract{\endsummary}

\def\nosections{\vspace{30\p@ plus12\p@ minus12\p@}
\noindent\ignorespaces}


%define section heads etc for style

\setcounter{secnumdepth}{4}

\def\thesection{\arabic{section}}
\def\thesubsection{\thesection.\arabic{subsection}}
\def\thesubsubsection{\thesubsection.\arabic{subsubsection}}
\def\theparagraph{\thesubsubsection.\arabic{paragraph}}
%\def\thesubparagraph{\theparagraph.\arabic{subparagraph}}

\renewcommand{\section}{\makeatletter
\renewcommand{\@seccntformat}[1]{{\csname the##1\endcsname.}\hspace{1.0em}}
\makeatother%
\@startsection
{section}%           the name
{1}%                 the level
{\z@}%               the indent
{-24pt plus -4pt minus -2pt}%  the beforeskip
{+12pt plus -3pt minus -1pt}%     the afterskip
{\normalsize\bfseries}}% the style
%%
%%
\def\subsection{%
\@startsection
{subsection}%           the name
{2}%                 the level
{\z@}%               the indent
{-12pt plus -2pt minus -1pt}%  the beforeskip
{+12pt plus -2pt minus -1pt}%     the afterskip
{\normalsize\bfseries}}% the style

\def\subsubsection{%
\@startsection
{subsubsection}%           the name
{3}%                 the level
{\z@}%               the indent
{-12pt plus -2pt minus -1pt}%  the beforeskip
{+12pt plus -2pt minus -1pt}%     the afterskip
{\normalsize\itshape}}% the style

\def\paragraph{%
\@startsection
{paragraph}%           the name
{4}%                 the level
{\z@}%               the indent
{-12pt plus -2pt minus -1pt}%  the beforeskip
{-1em}%     the afterskip
{\normalsize\itshape}}% the style

%%%%%%%%%%%%%%%%
\def\enumerate{\ifnum \@enumdepth >3 \@toodeep\else
      \advance\@enumdepth \@ne
      \edef\@enumctr{enum\romannumeral\the\@enumdepth}\list
      {\csname label\@enumctr\endcsname}{\usecounter
        {\@enumctr}\itemsep 0pt\parsep 0pt
         \def\makelabel##1{\hss\llap{##1}}}\fi}

\let\endenumerate =\endlist

\def\itemize{\ifnum \@itemdepth >3 \@toodeep\else \advance\@itemdepth \@ne
\edef\@itemitem{labelitem\romannumeral\the\@itemdepth}%
\list{\csname\@itemitem\endcsname}{\itemsep 0pt\parsep 0pt
\def\makelabel##1{\hss\llap{##1}}}\fi}

\let\enditemize =\endlist

%%%%%%%%%%
\setlength{\skip\footins}{2.5pc}
\renewcommand{\footnoterule}{\noindent\rule{5pc}{0.25pt}\vspace*{6pt}}

\renewcommand{\thefootnote}{\fnsymbol{footnote}}
\long\def\@makefntext#1{\parindent 1em%
\noindent{$\m@th^{\@thefnmark}$}#1}

\def\comma{$^{\text{,}}$}

\def\corraddr#1{\footnotetext[1]{Correspondence to: #1}\stepcounter{footnote}}
\def\corrauth{\footnotemark[1]}

\def\cgsn#1#2{\footnotetext[0]{Contract/grant sponsor: #1; contract/grant
number: #2}}
\def\cgs#1{\footnotetext[0]{Contract/grant sponsor: #1}}

\def\affilnum#1{${}^{#1}$}
\def\affil#1{${}^{#1}$}

\newlength{\figcaptionwidth}
\newlength{\figcaphshift}

\renewenvironment{figure}{\@float{figure}\vspace*{24pt}}
 {\end@float}

\def\figcap#1{\setlength{\figcaptionwidth}{#1}}
\figcap{\textwidth}

\long\def\@makecaption#1#2{\vspace*{10pt}{%
\setlength{\figcaphshift}{(\textwidth-\figcaptionwidth)/2}
\noindent\hspace*{\figcaphshift}\parbox{\figcaptionwidth}{%
\setbox0=\vbox{\small\rm #1.\ #2}%
\setbox0=\vbox{\unvbox0 \setbox1=\lastbox%
\setbox1=\hbox to \figcaptionwidth{\hfill\unhbox1\hfill}\box1}%
\box0}}\par}

\def\en{\phantom{0}}

\newlength{\captionwidth}
\newlength{\tabcaphshift}

\def\tabcap#1{\setlength{\captionwidth}{#1}}

\long\def\@maketabcaption#1#2{{%
\setlength{\tabcaphshift}{(\textwidth-\captionwidth)/2}
\noindent\hspace*{\tabcaphshift}\parbox{\captionwidth}{%
\setbox0=\vbox{\small\rm #1.\ #2}%
\setbox0=\vbox{\unvbox0 \setbox1=\lastbox%
\setbox1=\hbox to \captionwidth{\hfill\unhbox1\hfill}\box1}%
\box0}}}

\renewcommand{\table}{\let\@makecaption\@maketabcaption\@float{table}\vspace*{24pt}}

\renewcommand{\thetable}{\Roman{table}}

\newcommand{\ms}{\noalign{\vspace{3\p@ plus2\p@ minus1\p@}}}
\newcommand{\boldarrayrulewidth}{0.5\p@}
\newcommand{\midarrayrulewidth}{0.25\p@}

\def\topline{\noalign{\ifnum0=`}\fi\hrule \@height
\boldarrayrulewidth \futurelet \@tempa\@xhline}
\def\@xhline{\ifx\@tempa\hline\vskip \doublerulesep\fi
\ifnum0=`{\fi}}

\def\midline{\noalign{\ifnum0=`}\fi\hrule \@height
\midarrayrulewidth \futurelet \@tempa\@xhline}
\def\@xhline{\ifx\@tempa\hline\vskip \doublerulesep\fi
\ifnum0=`{\fi}}


% Weighted rules for tables
\newcommand{\toprule}{\ms\topline\ms}
\newcommand{\midrule}{\ms\midline\ms}
\newcommand{\bottomrule}{\ms\topline}

%%%%%%%%%%%%%%%%%%

%Acks
\def\ack{\vspace{24pt plus4pt minus2pt}
\noindent{\footnotesize\bfseries{ACKNOWLEDGEMENT}}\\[12pt plus 3pt minus 1pt]\small\noindent}%

\def\acks{\vspace{24pt plus4pt minus2pt}
\noindent{\footnotesize\bfseries{ACKNOWLEDGEMENTS}}\\[12pt plus 3pt minus 1pt]\small\noindent}%


\def\appendix#1#2{%
\setcounter{figure}{0}
\setcounter{table}{0}
\setcounter{equation}{0}
\def\thefigure{#1\arabic{figure}}
\def\thetable{#1\Roman{table}}
\def\theequation{#1\arabic{equation}}
\section*{APPENDIX\ #1.\ #2}%
}

%references

\def\thebibliography#1{\vspace{24pt plus 4pt minus 2pt}
\noindent{\footnotesize\bfseries REFERENCES}\vspace{12pt plus 3pt minus 1pt}%
\list{{\arabic{enumi}}}{\def\makelabel##1{\hss{##1.}}\topsep=0\p@\parsep=0\p@
\partopsep=0\p@\itemsep=0\p@
\labelsep=1ex\itemindent=0\p@
\settowidth\labelwidth{\footnotesize[#1]}%
\leftmargin\labelwidth
\advance\leftmargin\labelsep
\advance\leftmargin -\itemindent
\usecounter{enumi}}\footnotesize
\def\newblock{\ }
\sloppy\clubpenalty4000\widowpenalty4000
\sfcode`\.=1000\relax}
\let\endthebibliography=\endlist

\def\proc#1{\medbreak\noindent{\it #1}\hspace{1ex}\ignorespaces}
\def\mb{\medbreak}
