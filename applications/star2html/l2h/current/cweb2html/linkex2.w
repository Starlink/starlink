% $Id$

\documentclass{cweb}
\usepackage{html}
% This command also turns on the special treatment of refinement names
% such as writing them to the label file.
\HTCweblabels{dvi.obj/linkex1,dvi.obj/linkex2}{html.obj/linkex1,html.obj/linkex2}

\def\CWEB{{\tt CWEB\/}}

\begin{document}
\title{An Example of Linked \CWEB{} Documents}
\author{Jens Lippmann}
\date{22 Feb 98}
\maketitle



@* Example of linked \CWEB{} documents (2/2).

For the description of this example, see |@<linkex1.w@>|.

@c
@<linkex2 includes@>@/
@<linkex2 main()@>@/

@
See also the include files used in |@<linkex1 includes@>|.
@<linkex2 includes@>=
#include <stdio.h>

@
See also the main function as defined in |@<linkex1 main()@>|.
@<linkex2 main()@>=
int main()
{
    printf("%d*%d?\n",7,4);
    exit(0);
}


@
\end{document}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Log$
% Revision 1.1  2004/02/20 13:13:26  nxg
% Initial import
%
% Revision 1.1  1998/02/24 02:29:53  latex2html
% for 98.1
%
%
