#include <string.h>
#include "hlpsys.h"
void hlpHtellx ( char *fname, long *iadrx, int *loglev )
/*
**  - - - - - - - - - -
**   h l p H t e l l x
**  - - - - - - - - - -
**
**  Inquire the HELP library's current name, index address and logical
**  level.
**
**  Given (in global data):
**     *hlnext   char    name of next HELP library to be accessed
**     nextx     long    index address for next hlpHreadx access
**     loffnu    int     logical level of next HELP library
**
**  Returned (arguments):
**     *fname    char    name of HELP library
**     *iadrx    long    index address within HELP library file (1st = 0)
**     *loglev   int     logical level of HELP library
**
**  Defined in #include:
**     LFNAME            maximum length of filenames
**
**  Note that no checks are made that the file is open, or that the iadrx
**  value points to the start of a record or lies within the file.  If
**  any of these conditions are violated, appropriate errors will be
**  reported when attempts to access the file are made based on the
**  result of calling the present routine.
**
**  Last revision:   22 February 1996
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
   strncpy ( fname, hlnext, LFNAME );
   *iadrx = nextx;
   *loglev = loffnu;
}
