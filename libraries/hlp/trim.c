#include <ctype.h>
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
**  Last revision:   7 January 1996
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
   int l, i, c;

   l = 0;
   for ( i = 0;
         ( c = (int) string [ i++ ] );
         l = isspace ( c ) ? l : i );
   string [ l ] = (char) '\0';
   return string;
}
