#include <stdio.h>
#include <string.h>

#include "help.h"

#define extern
#include "hlpsys.h"
#undef extern

void hlpHinit ( char *fname )
/*
**  - - - - - - - - -
**   h l p H i n i t
**  - - - - - - - - -
**
**  Initialize HELP system.
**
**  Given (arguments):
**     *fname    char    HELP library file name
**
**  Returned (in global variables):
**     jhelp     int     state of HELP system:  -1 = closed
**     loffnu    int     logical level number for new HELP library
**     *hlopen   char    name of open HELP library (reset to null)
**     *hlnext   char    name of HELP library for next access
**
**  Defined in hlpsys.h:
**     LFNAME            maximum length of filenames
**
**  Last revision:   14 January 2008
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   *hlopen = '\0';
   strncpy ( hlnext, fname, LFNAME );
   loffnu = 0;
   jhelp = -1;
}
