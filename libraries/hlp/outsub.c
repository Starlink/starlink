#include <stdio.h>
int hlpOutsub ( char *string )
/*
**  - - - - - - - - - -
**   h l p O u t s u b
**  - - - - - - - - - -
**
**  A minimalist example of the user-supplied outsub routine, which is
**  called by the hlpHelp routine to output a line of HELP text.
**
**  This example outputs to stdio.
**
**  Given:
**     *string     char     HELP text string (without \nl)
**
**  Returned (function value):
**                 int      always 1
**
**  Last revision:   9 October 1995
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
   puts ( string );
   return 1;
}
