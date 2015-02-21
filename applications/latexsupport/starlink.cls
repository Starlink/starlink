% Latex2E class for writing starlink reports
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{starlink}

% Declare options

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



% Titlesec.
\RequirePackage{titlesec}

% Abstract?
\RequirePackage{abstract}

% Package to allow graphics to be loaded (\includegraphics)
\RequirePackage{graphicx}

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

\setlength{\parindent}{0mm}

%... allow environments using verbatim
\RequirePackage{fancyvrb}

%... allow starlink docs to use indexes
\RequirePackage{makeidx}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Starlink specific options
\newcommand{\stardoccategory}[1]{\def \@stardoccategory {#1}}
\newcommand{\stardocinitials}[1]{\def \@stardocinitials {#1}}
\newcommand{\stardoccopyright}[1]{\def \@stardoccopyright {#1}}
\newcommand{\stardocnumber}[1]{\def \@stardocnumber{#1}}
\newcommand{\stardoctitle}[1]{\def \@stardoctitle{#1}}
\newcommand{\stardocversion}[1]{\def \@stardocversion{#1}}
\newcommand{\stardocmanual}[1]{\def \@stardocmanual{#1}}
\newcommand{\stardocabstract}[1]{\def \@stardocabstract{#1}}
\newcommand{\stardocauthors}[1]{\def\@stardocauthors{#1}}
\newcommand{\stardocdate}[1]{\def\@stardocdate{#1}}
\newcommand{\startitlepic}[1]{\def\@startitlepic{#1}}

\newcommand{\starinstitute}[1]{\def\@starinstitute{#1}}
\newcommand{\starfunders}[1]{\def\@starfunders{#1}}
\newcommand{\starproject}[1]{\def\@starproject{#1}}

\newcommand{\stardocsource}[1]{\def\@stardocsource{#1}}
\newcommand{\stardocname}[1]{\def\@stardocname{#1}}

% Defaults for current data?
\starinstitute{}
\starfunders{}
\starproject{Starlink Project}

% initalise to nothing
\stardoccategory{}
\stardocinitials{}
\stardoccopyright{}
\stardocnumber{}
\stardoctitle{}
\stardocversion{\ }
\stardocmanual{\ }
\stardocabstract{}
\stardocauthors{}
\startitlepic{}
\stardocname{}
\stardocauthors{}
\stardocdate{}


% Provide the... versions of these commands so you don't need to use @
% in latex.
\newcommand{\thestardoccategory}{\@stardoccategory}
\newcommand{\thestardocinitials}{\@stardocinitials}
\newcommand{\thestardoccopyright}{\@stardoccopyright}
\newcommand{\thestardocnumber}{\@stardocnumber}
\newcommand{\thestardoctitle}{\@stardoctitle}
\newcommand{\thestardocversion}{\@stardocversion}
\newcommand{\thestardocmanual}{\@stardocmanual}
\newcommand{\thestardocabstract}{\@stardocabstract}
\newcommand{\thestardocauthors}{\@stardocauthors}
\newcommand{\thestarinstitute}{\@starinstitute}
\newcommand{\thestarfunders}{\@starfunders}
\newcommand{\thestarproject}{\@starproject}
\newcommand{\thestartitlepic}{\@startitlepic}
\newcommand{\thestardocsource}{\@stardocsource}


%  Ensure the title etc is available.
\newcommand{\thetitle}{\@stardoctitle}
\title{\@stardoctitle}

\newcommand{\thestardocdate}{\@stardocdate}
%\date{\thedate}
% Uncommeting the following line causes the \scfrontmatter command for
% pdf to break with a 'no line to end here fault...'
%\date{\@stardocdate}
\newcommand{\theauthor}{\@stardocauthors}
\author{\@stardocauthors}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%   SET UP THE TITLE PAGE 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\setlength{\droptitle}{0pt}
\addtolength{\droptitle}{-1\baselineskip}

%....set up initial header.
\newcommand{\titleheader}{%
  \textsc{\thestarinstitute} \scpushright{\textbf{\thestardocinitials /\thestardocnumber}}\\
  {\large \thestarfunders}\\
  {\large \thestarproject}\\
  {\large \thestardoccategory\ \thestardocnumber}%
}


% Graphics for front page
\newcommand{\thestargraphics}{%
\begin{center}
\thestartitlepic
\end{center}
\vspace{7.5mm}
\rule{\textwidth}{0.5mm}%
}

\newcommand{\sctitleformat}[1]{\Huge\bfseries #1}

% .. Provide a command \startitle page that will produce a consistent
% starlink title page
\newcommand{\startitlepage}{%
  \null
  \vskip 2em%
\vspace*{\droptitle}
\thispagestyle{empty}
\titleheader
\vspace{-7mm}
\begin{flushright}
  \thestardocauthors\\
  \thestardocdate\\
\end{flushright}
\vspace{-2mm}
\rule{\textwidth}{0.5mm}
\vspace*{7.5mm}
\begin{center}
  \sctitleformat{\thestardoctitle}\\
  \sctitleformat{\thestardocversion}\\
  \sctitleformat{\thestardocmanual}\\
  \end{center}
\vspace*{5mm}
\thestargraphics\\
}
%%%%%%%%%%%%%%%%%%%%%%%


%% Starlink list enivornments

% define some list-like environments



%% enumdesc: An enumerated description list
\newcounter{enumdescc}
\newcounter{enumdescci}
\newlist{enumdesc}{description}{2}
\setlist[enumdesc,1]{%
  before={\stepcounter{enumdescc}\setcounter{enumdescci}{0}},%
  style=nextline,leftmargin=0.5cm,labelindent=0.5cm,rightmargin=0.5cm,
  topsep=0.5\baselineskip, font={\phantomsection\normalfont\normalsize\bfseries\refstepcounter{enumdescci}\theenumdescci~}
}



% %% A description list with the label on the left in 15% of the space
% %% (e.g. for descriptions where the label has a pretty short section
% %% of the space.
% \newlist{sldes}{description}{2}
% \setlist[sldes]{%
%   style=sameline,leftmargin=*%
%   }

%% A description list which has the labels in a box on the left with
%% the length of the widest label, and the definitions aligned past
%% it. In HTML output, starstyle.4ht will format this as a table.

%% use eqparbox to get the box of width of the widest label (takes 2
%% runs of pdflatex).
\usepackage{eqparbox}
\newcounter{desc}
\newcommand{\descriptionmakelabel}[1]{\eqparbox{descnb\romannumeral\value{desc}}{#1\hfill}}

\newlist{aligndesc}{description}{2}
\setlist[aligndesc]{before={\refstepcounter{desc}\renewcommand{\makelabel}{\descriptionmakelabel}},
  leftmargin=\dimexpr\eqboxwidth{descnb\romannumeral\numexpr\value{desc}+1\relax}+3em\relax,
%  leftmargin=\dimexpr\labelwidth+2em\relax,
labelsep=1em, labelindent=2em, rightmargin=2em}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Starlink xref command

\newcommand{\xref}[3]{\href{http://starlink.jach.hawaii.edu/cgi-bin/htxserver/#2.htx/#2.html?xref_#3}{#1}}



%------------------------------------------------------------------------
%..  Define additional colours.
\definecolor{mygray}{gray}{0.7}
\definecolor{MidnightBlue}{RGB}{25, 25, 112}
\definecolor{bblue}{RGB}{172,207,230}

%.. Create a command to remove all space from input
\def\RemoveSpaces#1{\zap@space#1 \@empty}

%%%% Command that doesn't do anything in latex
\newcommand{\html}[1]{}


%.. The remaining starlink specific definitions.
\RequirePackage{starabbrev}
\RequirePackage{starstyle}
\RequirePackage{sst}


\newcommand{\checkfor}[1]{%
  \ifcsname#1\endcsname%
    ... command '#1' exists ...%
  \else%
    ... command '#1' does not exist ...%
  \fi%
}

% A starlink Hyperref.
\newcommand{\slhyperref}[4]{#2\ref{#4}#3}

%--------------------------------------------------------------------
%%%% Set up defaults for images

% maximum size of 'top area'
\renewcommand{\topfraction}{0.9}
% maximum size of 'bottom area'
\renewcommand{\bottomfraction}{0.9}
% minimum amount of text on a non-float page
\renewcommand{\textfraction}{0.1}
