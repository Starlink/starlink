#include <string.h>
#include "hlpsys.h"
int hlpHleap ( int ( * nametr ) ( int, char*, int, char* ),
               int lstring, char *string, char *fname,
               long *iadr, int *logl )
/*
**  - - - - - - - - -
**   h l p H l e a p
**  - - - - - - - - -
**
**  Satisfy any pending switch to another HELP library.  If a switch is
**  made, return the context information for the first entry in the new
**  library and skip that entry.
**
**  Given (in global data):
**     *hlopen    char    name of open HELP library
**     *hlnext    char    name of next HELP library
**     *fphl      FILE    file pointer for HELP library file
**     nextx      long    index address for this sequential access
**     loffnu     int     logical level for the next HELP library
**
**  Given (arguments):
**     nametr     func     user-supplied name translation routine (note 1)
**     lstring    int      length of string (including '\0')
**
**  Returned (in global data):
**     nextx      long     index address for this sequential access
**     nextd      long     data address for next sequential access
**     levoff     int      logical level for the current HELP library
**
**  Returned (arguments)
**     *string    char     workspace to receive index entry
**     *fname     char     new HELP library name
**     *iadr      long     address for first index entry
**     *logl      int      logical level number for new library
**
**  Returned (function value):
**                int      status:  0 = OK
**                      hlp_BAD_INDEX = illegal index record
**                               else = status from called routines
**
**  Defined in #include file:
**     LFNAME     int      maximum length of file names (including '\0')
**     Error codes
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
**  2)  If no library switch occurs, string, fname, iadr and logl
**      are unchanged.
**
**  Called:  hlpHopenr, hlpHtellx, hlpHdread, hlpDec
**
**  Last revision:   7 January 1996
**
**  Copyright 1996 P.T.Wallace.  All rights reserved.
*/
{
   int jstat, nc, iptr;


/* Is a switch to another library pending? */
   if ( strcmp ( hlopen, hlnext ) ) {

   /* Yes: close the old HELP library file. */
      if ( fclose ( fphl ) ) return hlp_CLOSE_ERROR;

   /* Open the new HELP library file and set its logical level number. */
      if ( ( jstat = hlpHopenr ( nametr ) ) ) return jstat;
      levoff = loffnu;

   /* Return the position of the first index entry and read it. */
      hlpHtellx ( fname, iadr, logl );
      if ( ( jstat = hlpHdread ( lstring, &nextx, string, &nc ) ) )
         return jstat;
      if ( nc <= 0 ) return hlp_BAD_INDEX;

   /* Decode the "data" address. */
      iptr = 0;
      nextd = hlpDec ( string, &iptr );
   }

/* Exit. */
   return 0;
}
