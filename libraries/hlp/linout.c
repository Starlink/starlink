#include <string.h>
#include "help.h"

int hlpLinout ( int ( * outsub ) ( char* ),
                int lout, int indent, char *buffer )
/*
**  - - - - - - - - - -
**   h l p L i n o u t
**  - - - - - - - - - -
**
**  Output a line of HELP information, indented to the right.
**
**  Given:
**     outsub    func   user-supplied output routine
**     lout      int    maximum record length accepted by outsub
**     indent    int    number of spaces to indent by
**     *buffer   char   line before insertion of leading spaces
**
**  Returned (argument):
**     *buffer   char   line after insertion of leading spaces
**
**  Returned (function value):
**               int    status from outsub call: 1=OK
**
**  Notes:
**
**     1)  The calling sequence for outsub is j=outsub(string), where
**         string points to a character array of length lout or less.
**         The status j is +1 for success.
**
**     2)  If indent is less than (or equal to) zero, buffer is output
**         as it is.
**
**     3)  It is the caller's responsibility to ensure that the
**         character array buffer is big enough to hold both the
**         original string and the leading spaces, plus the null
**         terminator.
**
**  Called:  hlpTrim
**
**  Last revision:   11 February 2008
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int l, ilast, ito, ifrom;


/* Useful length of string. */
   l = (int) strlen ( hlpTrim ( buffer ) );

/* Highest buffer index to be used. */
   ilast = l + ( indent >= 0 ? indent : 0 );

/* Move the active part of the string rightwards and pad with spaces. */
   for ( ifrom = l, ito = ilast; ito >= 0; ifrom--, ito-- ) {
      if ( ito <= lout ) {
         buffer[ito] = ( ifrom >= 0 ) ? buffer[ifrom] : (char) ' ';
      }
   }

/* String terminator. */
   buffer[ilast+1] = (char) '\0';

/* Output the line. */
   return outsub ( buffer );
}
