% Latex2E class for writing starlink documents.
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{starlink}
%-----------------------------------------------------%
% .. Class options.

% With chapters...
\newif\ifwithchapters
\withchaptersfalse

% If twoside...
\newif\iftwoside
\twosidefalse

% If list of figures (lof)
\newif\ifwithlof
\withloftrue

% If no abstract
\newif\ifwithabs
\withabstrue

% If all one page (affects html output only)
\newif\ifmultipage
\multipagetrue

% Declare the options.
\DeclareOption{chapters}{\withchapterstrue}
\DeclareOption{twoside}{\twosidetrue}
\DeclareOption{nolof}{\withloffalse}
\DeclareOption{noabs}{\withabsfalse}
\DeclareOption{onepage}{\multipagefalse}

% Pass all options not defined above to the classes.
% (Must be done before process options)
\ifwithchapters
   \typeout{..... passing options to report .....}
   \DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}
\else
   \typeout{........passing options to article......}
   \DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\fi

% Process custom options.
\ProcessOptions\relax


\ifwithchapters
   \LoadClass{report}
   \typeout{........Report!...........}
\else
   \LoadClass{article}
   \typeout{..........Article!..........}
\fi


%--------------------------------------
% Packages required for all reports
% chek if in a pdf or not
\RequirePackage{ifpdf}


% Font types and encoding.
\RequirePackage[T1]{fontenc}
\RequirePackage[utf8]{inputenc}

% microtype For improved pdf typography (must come after loading class)
\RequirePackage{microtype}

%maths
\RequirePackage{amsmath}
\RequirePackage{mathpazo}

% units
\RequirePackage{siunitx}

% Titlesec.
\RequirePackage{titlesec}

% Package to allow graphics to be loaded (\includegraphics) and the
% default extensions it will look for (and their order).
\RequirePackage{graphicx}
\DeclareGraphicsExtensions{.pdf,.png,.jpg,.jpeg}

%.. Probably needed for something?
\RequirePackage{multirow}

% formatting of list enivornments
\RequirePackage{enumitem}

%.. Using color
\RequirePackage[usenames,dvipsnames,svgnames,table]{xcolor}

%.. Allow boxes with frames and backgrounds, over multiple pages
\RequirePackage[framemethod=TikZ]{mdframed}

%.. Allow tables on multiple pages
\RequirePackage{longtable}

%.. Allow sideways tables
\RequirePackage{rotating}

%.. Allow landscape pdf pages
\RequirePackage{pdflscape}

%.. Set up the page
\RequirePackage[text={160mm,230mm},centering]{geometry}

%.. title page formatting
\RequirePackage{titling}

%... Set up the headers.
\RequirePackage{fancyhdr}

%.... table of contents formatting
\ifpdf
\RequirePackage{tocloft}
\fi

%.. hyperref
\RequirePackage[pdfusetitle=true,backref,
                breaklinks=True,pdfdisplaydoctitle=true]{hyperref}

%... allow environments using verbatim
\RequirePackage{fancyvrb}

%... allow starlink docs to use indexes
\RequirePackage{makeidx}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starlink document identification commands
\newcommand{\stardoccategory}[1]{\def \@stardoccategory {#1}}
\newcommand{\stardocinitials}[1]{\def \@stardocinitials {#1}}
\newcommand{\stardoccopyright}[1]{\def \@stardoccopyright {#1}}
\newcommand{\stardocnumber}[1]{\def \@stardocnumber{#1}}

% Define a stardoctitle that takes an optional 'short' argument; this
% can then be used in various places. E.g. if you have a multi line
% full title, please ensure you also have a single line short title.
\def\stardoctitle{\@ifnextchar [{\@stardoctitletwo}{\@stardoctitleone}}
\def\@stardoctitletwo[#1]#2{\gdef\@stardoctitle{#2}\gdef\@shorttitle{#1}}
\def\@stardoctitleone#1{\gdef\@stardoctitle{#1}\gdef\@shorttitle{#1}}

\newcommand{\stardocversion}[1]{\def \@stardocversion{#1}}
\newcommand{\stardocmanual}[1]{\def \@stardocmanual{#1}}
\newcommand{\stardocabstract}[1]{\def \@stardocabstract{#1}}
\newcommand{\stardocauthors}[1]{\def\@stardocauthors{#1}}
\newcommand{\stardocdate}[1]{\def\@stardocdate{#1}}
\newcommand{\startitlepic}[1]{\def\@startitlepic{#1}}
\newcommand{\starfunders}[1]{\def\@starfunders{#1}}
\newcommand{\starproject}[1]{\def\@starproject{#1}}
\newcommand{\stardocsource}[1]{\def\@stardocsource{#1}}
\newcommand{\stardocname}[1]{\def\@stardocname{#1}}

% Defaults for current data?
\starfunders{}
\starproject{Starlink Project}

% initalise to nothing
\stardoccategory{}
\stardocinitials{}
\stardoccopyright{}
\stardocnumber{}
\stardoctitle{}
\stardocversion{}
\stardocmanual{}
\stardocabstract{}
\stardocauthors{}
\startitlepic{}
\stardocname{}
\stardocauthors{}
\stardocdate{}


% Provide \the... versions of these commands so you don't need to use @
% in latex.
\newcommand{\thestardoccategory}{\@stardoccategory}
\newcommand{\thestardocinitials}{\@stardocinitials}
\newcommand{\thestardoccopyright}{\@stardoccopyright}
\newcommand{\thestardocnumber}{\@stardocnumber}
\newcommand{\thestardoctitle}{\@stardoctitle}
\newcommand{\theshorttitle}{\@shorttitle}
\newcommand{\thestardocversion}{\@stardocversion}
\newcommand{\thestardocmanual}{\@stardocmanual}
\newcommand{\thestardocabstract}{\@stardocabstract}
\newcommand{\thestardocauthors}{\@stardocauthors}
\newcommand{\thestarfunders}{\@starfunders}
\newcommand{\thestarproject}{\@starproject}
\newcommand{\thestartitlepic}{\@startitlepic}
\newcommand{\thestardocsource}{\@stardocsource}
\newcommand{\thestardocdate}{\@stardocdate}

%  Ensure the stardoctitle etc is available as \thetitle.
\newcommand{\thetitle}{\@stardoctitle}
\title{\@stardoctitle}

% Ensure the author list is available as \theauthor.
\newcommand{\theauthor}{\@stardocauthors}
\author{\@stardocauthors}


%%--------------------------------------------------------

%% Various commands to setup the frontmatter of starlink docs.

%% This should consist of 1) the title, 2) the abstract, 3) the table
%% of contents, 4) the list of figures (unless class option nolof is given).

%.. Format the initial header.
\newcommand{\titleheader}{%
  \begin{flushright}
  \textbf{\thestardocinitials /\thestardocnumber}
  \end{flushright}
  \thestarproject\\
  \thestardoccategory\ \thestardocnumber\\
}

%... Format the main ttile
\newcommand{\titlemain}{%
\begin{center}
{\Huge\textbf{\thestardoctitle}}

{\Huge\textbf{\thestardocversion}}

{\Huge\textbf{\thestardocmanual}}
\end{center}
}


% Graphics for front page
\newcommand{\thestargraphics}{%
\centering
\thestartitlepic
\vspace{7.5mm}
\rule{\textwidth}{0.5mm}%
}

% .. Provide a command \startitle page that will produce a consistent
% starlink title page
\newcommand{\startitlepage}{%
  \null
  \vskip 2em%
\vspace*{-1\baselineskip}
\thispagestyle{empty}
\titleheader
%\vspace{-7mm}
\begin{flushright}
\par\thestardocauthors
\par\thestardocdate
\par\thestardoccopyright
\end{flushright}
%\vspace{-2mm}
\rule{\textwidth}{0.5mm}
\vspace{7.5mm}
\titlemain%\par
%\vspace{5mm}
\thestargraphics\\
}


%%..  command to print the abstract (with copyright at bottom of page)
\newcommand{\Abstract}{%
  \ifwithabs
    \ifwithchapters
        \chapter*{}
    \fi
    \section*{Abstract}
    \thispagestyle{fancy}
    \markboth{Abstract}{}
    \thestardocabstract{}
    \\
    \vspace*{\fill}
    \\
    {\small\thestardoccopyright{}}
    \clearpage
  \fi
}

%% General Front matter command -- title page, abstract, toc, lof This
%% command \scfrontmatter is what should be called
%% after \begin{document} in any starlink tex file.

\newcommand*{\scfrontmatter}{
% Use roman page numbers
\renewcommand{\thepage}{\roman{page}}
\setcounter{page}{1}

% Create the titlepage
\begin{titlepage}%
\startitlepage
\end{titlepage}

% Show the abstract (defined to do nothing if noabs is set)
\Abstract{}

\clearpage

% Table of contents (catcode stuff to prevent errors with _)
\begingroup \catcode`\_=12 \tableofcontents \endgroup
\clearpage

% unless the class option 'nolof' has been given, create a list of
% figures.
\ifwithlof
  \begingroup \catcode`\_=12 \listoffigures \endgroup
\fi
\clearpage

% Reset the page counting to arabic and start from 1.
\renewcommand{\thepage}{\arabic{page}}
\setcounter{page}{1}
}
%%%-------------------------------------
%% Back matter commands (references and index)
% Ensure index shows up in toc.
\let\oldprintindex\printindex
\renewcommand{\printindex}{%
  \phantomsection \addcontentsline{toc}{section}{Index}
  \oldprintindex}

%%------------------------------------

%% Various class specific macros

%% Starlink list enivornments

%% enumdesc: An enumerated description list
\newcounter{enumdescc}
\newcounter{enumdescci}
\newlist{enumdesc}{description}{2}
\setlist[enumdesc,1]{%
  before={\stepcounter{enumdescc}\setcounter{enumdescci}{0}},%
  style=nextline,leftmargin=0.5cm,labelindent=0.5cm,rightmargin=0.5cm,
  topsep=0.5\baselineskip, font={%
    \phantomsection\normalfont\normalsize\bfseries\refstepcounter{enumdescci}\theenumdescci~}
}



%% A description list which has the labels in a box on the left with
%% the length of the widest label, and the definitions aligned past
%% it. In HTML output, starstyle.4ht will format this as a table.

%% This uses the package eqparbox to get the box of width of the
%% widest label (takes 2 runs of pdflatex).
\usepackage{eqparbox}

\newcounter{desc}
\newcommand{\descriptionmakelabel}[1]{\eqparbox{descnb\romannumeral\value{desc}}{#1\hfill}}

\newlist{aligndesc}{description}{2}
\setlist[aligndesc]{before={\refstepcounter{desc}\renewcommand{\makelabel}{\descriptionmakelabel}},
  leftmargin=\dimexpr\eqboxwidth{descnb\romannumeral\numexpr\value{desc}+1\relax}+3em\relax,
  labelsep=1em, labelindent=2em, rightmargin=2em}

%%%--------------------------------------------------------------
%% Linking and referencing commands.

%%..  Starlink xref command
%% By default use the starlink.ac.uk; this will be fixed up at the
%% end of make world by a different program.
\newcommand{\xref}[3]{%
\href{http://www.starlink.ac.uk/cgi-bin/htxserver/#2.htx/#2.html?xref_#3}{#1}}
\newcommand{\xlabel}[1]{\label{\protect{xref_#1}}}
\newcommand{\cref}[3]{#1~\ref{#2}}
%problems with _ in labels (e.g. in xrefs)
\let\oldunderscore\_
\renewcommand{\_}{\ifmmode \oldunderscore \else \string_\fi}


%%---------------------------------------------------------------
%% Deprecated commands (for compatability only)

% % Graphics commands
\newcommand{\starfig}[6]{
  \begin{figure}#2
    \centering\includegraphics[#3]{#1}
    \typeout{#1 inserted on page \arabic{page}}
    \caption[#5]{\label{#4} #6}
  \end{figure}
}

% A starlink Hyperref (defined a bit differently to regular hyperref,
% and with a first argument that doesn't do anything. Deprecated; only
% provided for consistency with old documents.  Include the string, not
% just the cross reference number or letter in the hyperlink.
\newcommand{\slhyperref}[4]{\hyperref[#4]{#2\ref*{#4}#3}}

\newcommand{\latexhtml}[2]{#1}

% %.. Empty environment latex only.
\newenvironment{latexonly}{}{}

%%%% Command that doesn't do anything in latex
\newcommand{\html}[1]{}

%.. environments that don't do anything
\def\makeinnocent#1{\catcode`#1=12 }
 \def\csarg#1#2{\expandafter#1\csname#2\endcsname}

 \def\ThrowAwayComment#1{\begingroup
     \def\CurrentComment{#1}%
     \let\do\makeinnocent \dospecials
     \makeinnocent\^^L% and whatever other special cases
     \endlinechar`\^^M \catcode`\^^M=12 \xComment}
 {\catcode`\^^M=12 \endlinechar=-1 %
  \gdef\xComment#1^^M{\def\test{#1}
       \csarg\ifx{PlainEnd\CurrentComment Test}\test
           \let\html@next\endgroup
       \else \csarg\ifx{LaLaEnd\CurrentComment Test}\test
             \edef\html@next{\endgroup\noexpand\end{\CurrentComment}}
       \else \let\html@next\xComment
       \fi \fi \html@next}
 }
 \def\excludecomment
   #1{\expandafter\def\csname#1\endcsname{\ThrowAwayComment{#1}}%
      {\escapechar=-1\relax
       \csarg\xdef{PlainEnd#1Test}{\string\\end#1}%
       \csarg\xdef{LaLaEnd#1Test}{\string\\end\string\{#1\string\}}%
      }}

\excludecomment{htmlonly}

\newcommand{\latex}[1]{#1}

%------------------------------------------------------------------------
%..  Define additional colours.
\definecolor{mygray}{gray}{0.7}
\definecolor{MidnightBlue}{RGB}{25, 25, 112}
\definecolor{bblue}{RGB}{172,207,230}


%--------------------------------------------------
%.. Miscellanous commands
%.. Create a command to remove all space from input
\def\RemoveSpaces#1{\zap@space#1 \@empty}

% % Command for text that should be pushed to the right of the line (eg
% % following an hfill, on a single line of text
\newcommand*{\scpushright}[1]{\hfill #1}



% %.. verbatim environment for quoting terminal.
\DefineVerbatimEnvironment{terminalv}{Verbatim}{%
xleftmargin=.5in,formatcom=\color{MidnightBlue},fontsize=\small}



% command for a text box that floats around and pops out from the text (framed)
\mdfsetup{%
 backgroundcolor=white,%
    middlelinewidth=4pt,%
    middlelinecolor=bblue,%
    userdefinedwidth=0.8\textwidth,%
    roundcorner=10pt, %
    innertopmargin=\topskip}%

\newenvironment{sltextbox}[1]{%
\begin{figure*}[h]\begin{mdframed}[userdefinedwidth=0.8\textwidth,
                            align=center,%
                       frametitle=#1,%
        frametitlebackgroundcolor=bblue]%
\setlength{\parskip}{\medskipamount}%
}%
{\end{mdframed}\end{figure*}}


%Framed  boxes (obsolete).
\newsavebox{\fmbox}
\newenvironment{fmpage}[1]{\begin{lrbox}{\fmbox}\begin{minipage}{#1}}{\end{minipage}\end{lrbox}\fbox{\usebox{\fmbox}}}



%.. Tip box
\newenvironment{tip}%
{\begin{figure*}[h]\begin{mdframed}[userdefinedwidth=0.8\textwidth,align=center,frametitle=Tip,frametitlebackgroundcolor=bblue]%
\setlength{\parskip}{\medskipamount}%
}%
{\end{mdframed}\end{figure*}}

%.. starlink long table (used so that its easier to fix it up for html output)
\setlength{\LTcapwidth}{\textwidth}
\newenvironment{sllongtable}[2]{%
\begin{longtable}{#1}\caption{#2}\\}
{\end{longtable}}


%.........................................

\newcommand{\Acronyms}{%
  \ifwithchapters
  \chapter*{}
  \fi
  \section*{Acronyms}
  \markboth{Acronyms}{}
  \addcontentsline{toc}{section}{\protect\numberline{}Acronyms}
}


%--------------------------------------------------------------------
%% Load the reamining starlink specific classes.

%.. The remaining starlink specific definitions.
\RequirePackage{starabbrev}
\RequirePackage{starstyle}
\RequirePackage{sst}

%--------------------------------------------------------------------
