#include <string.h>
#include "hlpsys.h"
int hlpHopenw ( int ( * nametr ) ( int, char*, int, char* ), long nchars )
/*
**  - - - - - - - - - -
**   h l p H o p e n w
**  - - - - - - - - - -
**
**  Open the HELP library file for writing.
**
**  Given (in global data):
**     jhelp     int         state of HELP system: -1=initialized
**     *hlnext   char        name of HELP library
**
**  Given (arguments):
**     nametr    func        user-supplied name translation routine (note 1)
**     nchars    long        size the file will be (characters)
**
**  Returned (in COMMON):
**     jhelp     int         state of HELP system: 1=open/write
**     nextx     long        index address for spurious sequential read
**     nextd     long        data address for 1st hlp_HDWRIT access
**     nchh      long        size of file (characters)
**     *hlopen   char        name of HELP library
**     *fphl     FILE        file pointer for HELP library file
**
**  Returned (function value):
**               int         status:  0 = OK
**                    hlp_ILLEGAL_STATE = HELP system in illegal state
**                       hlp_OPEN_ERROR = open error
**                      hlp_WRITE_ERROR = write error
**                                 else = errors from called routines
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
**  2)  The global variables nextx and nextd are initialized to zero
**      (just for tidiness in the case of nextx - a subsequent attempt
**      to perform a sequential read with hlpHreadx will be rejected
**      due to the system being in the wrong state).
**
**  3)  The library name is not used as a filename directly, but is
**      subjected to any required translation by means of the nametr
**      routine.
**
**  Called:  nametr (user-supplied)
**
**  Last revision:   16 June 2000
**
**  Copyright 2000 P.T.Wallace.  All rights reserved.
*/
{
   int j;
   long i;

/* Full filename (with prefix and suffix). */
   char file[200];


/* Abort if the system isn't in the right state. */
   if ( jhelp != -1 ) return hlp_ILLEGAL_STATE;

/* Translate the HELP library name and open the file. */
   if ( ( j = ( * nametr) ( 0, hlnext, 200, file ) ) ) return j;
   if ( ( fphl = fopen ( file, "w" ) ) == NULL ) return hlp_OPEN_ERROR;

/* Note its name. */
   strcpy ( hlopen, hlnext );

/* Fill the file with nulls */
   for ( i = 0l; i < nchars; i++ )
      if ( fputc ( '\0', fphl ) == EOF ) return hlp_WRITE_ERROR;

/* Initialize addresses for next sequential access. */
   nextx = 0l;
   nextd = 0l;

/* Set HELP state. */
   jhelp = 1;

/* Store the file size. */
   nchh = nchars;

/* Normal exit. */
   return 0;
}
