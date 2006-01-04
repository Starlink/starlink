#include <ctype.h>
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
**  Last revision:   6 November 1996
**
**  Copyright 1996 P.T.Wallace.  All Rights Reserved.
*/
{
   int i;

   for ( i = 0;
         ( string [ i ] = (char) toupper ( (int) string [ i ] ) );
         i++ );
   return string;
}
