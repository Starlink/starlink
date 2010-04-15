#include <ctype.h>
#include "hlpsys.h"
void hlpSplit ( char *string, int istart, int *ifrom, int *ito )
/*
**  - - - - - - - - -
**   h l p S p l i t
**  - - - - - - - - -
**
**  Split a space-separated substring from the contents of a string.
**
**  Given:
**     *string   char     string containing space-separated substrings
**     istart    int      where to start looking in string (0 = first)
**
**  Returned:
**     *ifrom    int      location of the start of the substring
**     *ito      int      location of the end of the substring
**
**  If no substring is found - i.e. if string[istart...] is all spaces
**  or if istart is past the end of the information in string - ifrom
**  is set to -1 and ito is left alone.  If istart is before the
**  beginning of string the search nonetheless starts at the beginning
**  of string.
**
**  Called: hlpLength
**
**  Last revision:   10 November 1998
**
**  Copyright 1998 P.T.Wallace.  All rights reserved.
*/
{
   int l, i, j;

/* Useful length of the string. */
   l = hlpLength ( string );

/* Search for the start of the substring. */
   for ( i = ( istart >= 0 ) ? istart : 0; i < l; i++ ) {
      if ( ! isspace ( ( int ) string [ i ] ) ) {

      /* Found it. */
         *ifrom = i;

      /* Now find the end of the substring. */
         for ( j = i + 1; j < l && ! isspace ( (int) string [ j ] ); j++ );

      /* Found it. */
         *ito = j - 1;

      /* All done: leave loop. */
         return;
      }
   }

/* There is no substring. */
   *ifrom = -1;
}
