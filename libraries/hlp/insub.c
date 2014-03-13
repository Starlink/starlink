#include "help.h"

int hlpInsub ( char *string, char *prompt, int *l )
/*
**  - - - - - - - - -
**   h l p I n s u b
**  - - - - - - - - -
**
**  A minimalist example of the user-supplied insub routine, which is
**  called by the hlpHelp routine to obtain an interactive response.
**
**  Input is from stdin and output to stdout.
**
**  Returned:
**     *string     char     response string (up to 80 characters)
**
**  Given:
**     *prompt     char     prompt string
**
**  Returned (argument):
**     *l          int      length of string excluding trailing blanks
**
**  Returned (function value):
**                 int      always +1
**
**  Called:  hlpLength
**
**  Last revision:   30 July 2009
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int n, c;
   char *cs;


/* Output the prompt string */
   fputs ( prompt, stdout );

/* Read the input string, stopping at and eliminating any newline */
   cs = string;
   for ( n = 80 ; --n > 0 && ( c = getchar ( ) ) != EOF ; ) {
      if ( c != '\n' ) {
         *cs++ = (char) c;
      } else {
         break;
      };
    };

/* Terminate the string */
   *cs = '\0';

/* Determine the length excluding white space */
   *l = hlpLength ( string );

   return 1;
}
