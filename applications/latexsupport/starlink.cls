% Latex2E class for writing starlink reports
\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{starlink}
\RequirePackage[T1]{fontenc} 
\RequirePackage[utf8]{inputenc}


\newif\ifwithchapters
\withchaptersfalse

\DeclareOption{chapters}{\withchapterstrue}

\ProcessOptions\relax


\ifwithchapters
   \LoadClassWithOptions{report}
   \typeout{........Report!...........}
\else
   \LoadClassWithOptions{article}
   \typeout{..........Article!..........}
\fi


\RequirePackage{mathpazo}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Packages required for all reports
% chek if in a pdf or not
\RequirePackage{ifpdf}


% Load titlesec for the pdf files, but don't load it for tex4ht
%\ifpdf
\RequirePackage{titlesec}
%\RequirePackage{titletoc}
%\fi

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
\RequirePackage[backref,breaklinks=True,pdfdisplaydoctitle=true]{hyperref}

\setlength{\parindent}{0mm}

%... allow environments using verbatim
\RequirePackage{fancyvrb}


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
\newcommand{\stardocauthors}[1]{\def\@author{#1}}
\newcommand{\stardocdate}[1]{\def\@date{#1}}
\newcommand{\startitlepic}[1]{\def\@startitlepic{#1}}

\newcommand{\starinstitute}[1]{\def\@starinstitute{#1}}
\newcommand{\starfunders}[1]{\def\@starfunders{#1}}
\newcommand{\starproject}[1]{\def\@starproject{#1}}

\newcommand{\stardocsource}[1]{\def\@stardocsource{#1}}



% Defaults for current data?
\starinstitute{}%Joint Astronomy Centre}
\starfunders{}%Science \& Technology Facilities Council}
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

% provide the... versions of these commands so you don't need to use @
% in latex.

\newcommand{\thestardoccategory}{\@stardoccategory}
\newcommand{\thestardocinitials}{\@stardocinitials}
\newcommand{\thestardoccopyright}{\@stardoccopyright}
\newcommand{\thestardocnumber}{\@stardocnumber}
\newcommand{\thestardoctitle}{\@stardoctitle}
\newcommand{\thestardocversion}{\@stardocversion}
\newcommand{\thestardocmanual}{\@stardocmanual}
\newcommand{\thestardocabstract}{\@stardocabstract}
\newcommand{\thestardocauthors}{\@author}
\newcommand{\thestarinstitute}{\@starinstitute}
\newcommand{\thestarfunders}{\@starfunders}
\newcommand{\thestarproject}{\@starproject}
\newcommand{\thestartitlepic}{\@startitlepic}
\newcommand{\thestardocsource}{\@stardocsource}



%  Ensure the title is available at a command\thetitle
%\newcommand{\thetitle}{\@title}
\newcommand{\thedate}{\@date}

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
  \thedate\\
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



%% EDLONG: An enumerated description list
\newcounter{enumdesc}
\newcounter{enumdesci}
\newlist{edlong}{description}{2}
\setlist[edlong,1]{%
  before={\stepcounter{enumdesc}\setcounter{enumdesci}{0}},%
  style=nextline,leftmargin=0.5cm,labelindent=0.5cm,rightmargin=0.5cm,
  topsep=0.5\baselineskip, font={\phantomsection\normalfont\normalsize\bfseries\refstepcounter{enumdesci}\theenumdesci~}
}



%% A description list with the label on the left in 15% of the space
%% (e.g. for descriptions where the label has a pretty short section
%% of the space.
\newlist{sldes}{description}{2}

\setlist[sldes,1]{%
  style=nextline,leftmargin=0.15\textwidth%
}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Starlink xref command

%\newcommand{\xref}[3]{#1}
\newcommand{\xref}[3]{\href{http://starlink.jach.hawaii.edu/cgi-bin/htxserver/#2.htx/#2.html?xref_#3}{#1}}



%----------------------------------------------------------------------------
%..  Define additional colours.
\definecolor{mygray}{gray}{0.7}
\definecolor{MidnightBlue}{RGB}{25, 25, 112}
\definecolor{bblue}{RGB}{172,207,230}

%.. Create a command to remove all space from input
\def\RemoveSpaces#1{\zap@space#1 \@empty}

%%%% Command that doesn't do anything in latex

\newcommand{\html}{}


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