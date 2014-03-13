#include "help.h"
#include <string.h>

char* hlpStrncp ( char* s, const char* ct, size_t n )
/*
**  - - - - - - - - - -
**   h l p S t r n c p
**  - - - - - - - - - -
**
**  A safe version of strncpy:  copy one string to another up to a
**  maximum number of characters.
**
**  Given:
**     *ct     char     source string
**     n       size_t   maximum number of characters to copy
**
**  Returned (both as an argument and as the function value):
**     *s      char     destination string
**
**  Notes:
**
**  1  The maximum number of characters to copy, n, includes the string
**     ct's terminating null if any.
**
**  2  This function replaces strncpy from the standard C library, which
**
**     o  potentially can leave the destination string unterminated,
**     o  wastefully pads the destination string with nulls, and
**     o  cannot be used with overlapping strings.
**
**  Last revision:   12 March 2014
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   size_t m;

   if ( n > 0 ) {
      m = strlen ( ct );
      if ( m > n-1 ) m = n-1;
      memmove ( s, ct, m );
      s[m] = (char) '\0';
   }
   return s;
}

