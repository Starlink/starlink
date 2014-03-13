#include <string.h>
#include <ctype.h>
#include "help.h"

char *hlpTrim ( char *string )
/*
**  - - - - - - - -
**   h l p T r i m
**  - - - - - - - -
**
**  Strip from a string any trailing whitespace.
**
**  Given:
**     *string      char     the string with trailing whitespace
**
**  Returned:
**     *string      char     the string without trailing whitespace
**
**  Returned (function value):
**                  char*    the string without trailing whitespace
**
**  Last revision:   1 September 2009
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   int i;


   i = (int) strlen ( string ) - 1;
   while ( i >= 0 && isspace ( (char) string[i] ) ) {
      string[i--] = (char) '\0';
   }
   return string;
}
