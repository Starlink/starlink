#include <string.h>
#include "hlpsys.h"
int hlpHopenr ( int ( * nametr ) ( int, char*, int, char* ) )
/*
**  - - - - - - - - - -
**   h l p H o p e n r
**  - - - - - - - - - -
**
**  Open or re-open the HELP library file for reading.
**
**  Defined in #include file:
**     error codes (see below)
**
**  Given (argument):
**     nametr    func    user-supplied name translation routine (note 1)
**
**  Given (in global data):
**     jhelp     int     state of HELP system: -1=initialized
**     *hlnext   char    name of HELP library
**     loffnu    int     logical level for next HELP library
**
**  Returned (in global data):
**     jhelp     int     state of HELP system: 2=open/read
**     *hlopen   char    name of open HELP library
**     *fphl     FILE    file pointer for HELP library file
**     nextx     long    index address for 1st hlp_HREADX access
**     nextd     long    data address for 1st hlp_HREAD access
**     levoff    int     logical level for open HELP library
**     nchh      long    size of file (characters)
**
**  Returned (function value):
**               int      status:  0 = OK
**                 hlp_ILLEGAL_STATE = HELP system in illegal state
**                    hlp_OPEN_ERROR = open error (or bad header)
**                   hlp_WRITE_ERROR = write error
**
**  The above error codes are defined in #include file hlpsys.h.
**
**  Notes:
**
**  1)  The user-supplied name translation function nametr has
**      arguments command, string1, lstring2, string2 and returns
**      a status:
**
**      * command (given) is an integer which the HELP system always
**        sets to zero;  the application may use other values of
**        command, for example to perform initializations or enquiries;
**
**      * string1 (given) is, for command=0, the HELP library name to
**        be translated;  this name can be is the value of the file
**        argument to the present routine (identifying the root library)
**        or the name following the '@' symbol in a HELP source file
**        (identifying another library grafted onto the current
**        library);
**
**      * lstring2 (given) is, for command=0, the length of string2
**        (defined next);
**
**      * string2 (returned) is, for command=0, the translation of
**        string2 into a filename for use in fopen calls;
**
**      * the nametr function return is a status, zero indicating
**        success.
**
**  2)  Repeated calls to this routine are permitted during reading
**      without an error status, producing a "rewind" effect.
**
**  3)  The global variable nextx is initialized to point to the
**      first index record; nextd is initialized to point to the
**      final end marker.
**
**  Called:  hlpFopr, hlpDread, hlpDec
**
**  Last revision:   16 June 2000
**
**  Copyright 2000 P.T.Wallace.  All rights reserved.
*/

#define LBUF 500

{
   int nc, jstat, i;
   long iadr;
   char buffer [ LBUF ];


/* Abort if the system isn't in the right state. */
   if ( jhelp != -1 && jhelp != 2 ) return hlp_ILLEGAL_STATE;

/* Open (or re-open) the HELP library file. */
   if ( hlpFopr ( nametr, hlnext, &fphl ) )
      return hlp_OPEN_ERROR;

/* Note its name and logical level. */
   strcpy ( hlopen, hlnext );
   levoff = loffnu;

/* Set HELP state. */
   jhelp = 2;

/* Read the header record to get the file size. */
   iadr = 0l;
   nchh = (long) LBUF;
   jstat = hlpHdread ( LBUF, &iadr, buffer, &nc );
   if ( jstat || nc < 1 ) return hlp_OPEN_ERROR;
   i = 0;
   nchh = hlpDec ( buffer, &i );
   if ( nchh < 0l ) return hlp_OPEN_ERROR;

/* Store the initial index and data addresses. */
   nextx = iadr;
   nextd = nchh - 1l;

/* Set status and exit. */
   return 0;
}
