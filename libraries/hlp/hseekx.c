#include "help.h"
#include "hlpsys.h"

void hlpHseekx ( char *fname, long iadrx, int loglev )
/*
**  - - - - - - - - - -
**   h l p H s e e k x
**  - - - - - - - - - -
**
**  Position HELP library index for a sequential access using the
**  hlpHreadx routine.
**
**  Given (arguments):
**     *fname    char    name of HELP library
**     iadrx     long    index address within HELP library file (1st = 0)
**     loglev    int     logical level of HELP library
**
**  Returned (in global data):
**     *hlnext   char    name of next HELP library to be accessed
**     nextx     long    address for next sequential access of index
**     loffnu    int     logical level of next HELP library
**
**  Defined in hlpsys.h:
**     LFNAME            maximum length of filenames
**
**  Note that no checks are made that the file is open, or that the iadrx
**  value points to the start of a record or lies within the file.  If
**  any of these conditions are violated, appropriate errors will be
**  reported when attempts to access the file are made.
**
**  Called:  hlpStrncp
**
**  Last revision:   12 March 2014
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   hlpStrncp ( hlnext, fname, LFNAME );
   nextx = iadrx;
   loffnu = loglev;
}
