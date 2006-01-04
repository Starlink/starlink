#include <string.h>
char *hlpCopyn ( char *s, char *ct, int n )
/*
**  - - - - - - - - -
**   h l p C o p y n
**  - - - - - - - - -
**
**  Copy at most n characters from one string to another and then
**  append a null character.
**
**  Like the standard C function strncpy except that in addition to
**  the n characters you get a guaranteed null as well.
**
**  Given:
**     *s     char    destination string
**     *ct    char    source string
**     n      int     number of characters to copy not including the null
**
**  Returned (both as an argument and as the function value):
**     *s     char    resulting string
**
**  If ct is shorter than n characters, s is padded with nulls.
**
**  Last revision:   22 February 1996
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
   strncpy ( s, ct, n );
   s [ n ] = (char) '\0';
   return s;
}
