#include <string.h>
#include "help.h"
#include "hlpsys.h"

int hlpHdwrit ( char *string, long *iadr )
/*
**  - - - - - - - - - -
**   h l p H d w r i t
**  - - - - - - - - - -
**
**  Direct-access write to the HELP library file.
**
**  Given (in global data):
**     jhelp     int      state of HELP system: 1=open/write
**     fphl      int      file pointer for HELP library file
**     nchh      long     number of characters in HELP file
**
**  Given (arguments):
**     *string   char      string to write (see note)
**     *iadr     long      character address to write to (1st = 0)
**
**  Returned (arguments)
**     *iadr     long      points to next character address in sequence
**
**  Returned (function value):
**               int         status:  0 = OK
**                    hlp_ILLEGAL_STATE = HELP system in wrong state
**                       hlp_WRITE_WIDE = attempt to write outside file
**                      hlp_WRITE_ERROR = write error
**
**  The above error codes are defined in header file hlpsys.h.
**
**  Notes:
**
**  1)  If string is empty, the record which is output consists of a
**      single character, namely '\0'.  If not, the record is string
**      including the '\0'.
**
**  Last revision:   14 January 2008
**
**  Copyright P.T.Wallace.  All rights reserved.
*/
{
   long i;


/* Abort if the HELP system isn't in the right state. */
   if ( jhelp != 1 ) return hlp_ILLEGAL_STATE;

/* Abort if the specified address precedes the file. */
   if ( *iadr < 0 ) return hlp_WRITE_WIDE;

/* Address following the terminating '\0' of the new string. */
   i = *iadr + (long) strlen ( string ) + 1;

/* Abort if the new record will overflow the file. */
   if ( i > nchh ) return hlp_WRITE_WIDE;

/* Write the new record in the correct place in the file. */
   if ( ( fseek ( fphl, *iadr, 0 ) ) ||
        ( fputs ( string, fphl ) == EOF ) ) return hlp_WRITE_ERROR;

/* Update the address, and exit. */
   *iadr = i;
   return 0;
}
