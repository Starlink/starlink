#include <ctype.h>
#include "help.h"

char *hlpUpcase ( char *string )
/*
**  - - - - - - - - - -
**   h l p U p c a s e
**  - - - - - - - - - -
**
**  Convert a string to uppercase.
**
**  Given and returned:
**     *string     char     string to be converted
**
**  The string pointer is also returned as the function value.
**
**  Last revision:   1 September 2009
**
**  Copyright P.T.Wallace.  All Rights Reserved.
*/
{
   char *s;

   for ( s = string; *s; s++ ) {
      *s = (char) toupper ( (int) *s );
   }
   return string;
}
