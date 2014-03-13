#include <stdio.h>
#include <string.h>
#include "help.h"

int hlpIndex ( char *string, char *pattern )
/*
**  - - - - - - - - -
**   h l p I n d e x
**  - - - - - - - - -
**
**  Find a substring within a string.
**
**  C interpretation of Fortran INDEX intrinsic function.
**
**  Given:
**    *string   char    string to search
**    *pattern  char    pattern to look for
**
**  Returned (function value):
**              int     offset of first occurrence of pattern
**                      within string (0 = string begins with
**                      pattern), or -1 if either the pattern
**                      can't be found or pattern is bigger
**                      than string.
**
**  Last revision:   14 January 2008
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   char *ptr;

   ptr = strstr ( string, pattern );
   return (int) ( ( ptr != NULL ) ? ptr - string : -1 );
}
