#include <string.h>
#include "hlpsys.h"
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
**         original string and the leading spaces.
**
**  Called:  hlpTrim
**
**  Last revision:   2 January 2004
**
**  Copyright 2004 P.T.Wallace.  All rights reserved.
*/
{
   int l, ito, ifrom;


/* Useful length of string. */
   l = (int) strlen ( hlpTrim ( buffer ) );

/* Move the active part of the string to the right and pad with spaces. */
   for ( ifrom = l, ito = l + ( ( indent >= 0 ) ? indent : 0 );
         ito >= 0;
         ifrom--, ito-- ) {
      buffer [ ito ] = ( ifrom >= 0 ) ? buffer [ ifrom ] : (char) ' ';
   }

/* Output the line. */
   return outsub ( buffer );
}
