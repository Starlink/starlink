#include <stdio.h>
int hlpFopr ( int ( *nametr ) ( int, char*, int, char* ),
              char *file, FILE **fp )
/*
**  - - - - - - - -
**   h l p F o p r
**  - - - - - - - -
**
**  Open a file for reading.
**
**  Given:
**     *nametr   func    user-supplied name translation routine (note 1)
**     *file     char    file name (prior to translation by nametr)
**
**  Returned (argument):
**     **fp      FILE    file pointer
**
**  Returned (function value):
**               int     status:  0 = OK
**                               -1 = open error
**                             else = error from nametr function
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
**  2)  If the file is to be used in a random-access mode, the
**      housekeeping details, for example block and record lengths,
**      have to be handled outside the present routine.
**
**  3)  The HELP library name is not used directly as a filename, but is
**      subjected to any required translation by means of the nametr
**      routine specified in the call.
**
**  Last revision:   7 January 1996
**
**  Copyright 1996 P.T.Wallace.   All rights reserved.
*/
#define LFILETR 200  /* Maximum length of filename */
{
   char filetr [ LFILETR ];
   int j;

/* Translate the HELP library name into a filename. */
   if ( ( j = ( * nametr ) ( 0, file, LFILETR, filetr ) ) ) return j;

/* Open for reading. */
   return ( ( ( *fp = fopen ( filetr, "r" ) ) == NULL ) ? -1 : 0 );
}
