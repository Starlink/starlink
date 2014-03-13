#include <ctype.h>
#include "help.h"

int hlpLength ( char *string )
/*
**  - - - - - - - - - -
**   h l p L e n g t h
**  - - - - - - - - - -
**
**  Length of a string excluding any trailing whitespace.
**
**  Given:
**     *string      char     the string
**
**  Returned (function value):
**                  int      length excluding trailing spaces
**
**  The minimum length returned is 0.
**
**  Last revision:   30 July 2009
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i, l, c;


   i = l = 0;
   for ( ; ; ) {
      c = (int) string[i++];
      if ( ! c ) break;
      if ( ! isspace ( c ) ) l = i;
   }
   return l;
}
