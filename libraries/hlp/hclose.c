#include <string.h>
#include "hlpsys.h"
int hlpHclose ( void )
/*
**  - - - - - - - - - -
**   h l p H c l o s e
**  - - - - - - - - - -
**
**  Close the HELP library.
**
**  Given (in global data):
**     jhelp     int         state of HELP system: 1 or 2 = open
**     *fphl     FILE        file pointer for HELP library file
**
**  Returned (in global data):
**     jhelp     int         state of HELP system: -1=closed
**     *hlopen   char        name of open HELP library ("")
**     loffnu    int         level number for next HELP library (0)
**
**  Returned (function value):
**               int         status:  0 = OK
**                    hlp_ILLEGAL_STATE = HELP system in illegal state
**                      hlp_CLOSE_ERROR = close error
**
**  The above error codes are defined in #include file hlpsys.h.
**
**  Last revision:   2 September 1995
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
/* Abort if the system isn't in the right state. */
   if ( jhelp != 1 && jhelp != 2 ) return hlp_ILLEGAL_STATE;

/* Close the file. */
   if ( fclose ( fphl ) ) return hlp_CLOSE_ERROR;

/* Resets. */
   jhelp = -1;
   *hlopen = '\0';
   loffnu = 0;

/* Exit. */
   return 0;
}
