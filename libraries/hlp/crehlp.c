#include <string.h>
#include "hlpsys.h"
int main ( int argc, char *argv[] )
/*
**  - - - - - - -
**   c r e h l p
**  - - - - - - -
**
**  Create random access form of HELP library file.
**
**  This is a front-end for the hlpCreh function.  It obtains the
**  names of the source and library files, and opens the files.
**
**  I/O:
**     arg1     input: HELP source
**     arg2     output: HELP library
**     stdin    command input
**     stdout   prompts and error messages
**
**  If not specified through command-line arguments, the filenames
**  are prompted for.
**
**  Returned (function value):
**               int     status: 0 = OK
**                       other -ve = error reported by hlpCreh
**
**  The error codes are defined in the hlpsys.h #include file.
**
**  Called:  hlpNametr, hlpCreh, hlpErrmes, hlpTrim
**
**  Last revision:   16 June 2000
**
**  Copyright 2000 P.T.Wallace.  All rights reserved.
*/

/* Maximum length of filenames (including '\0'). */
#define LFN 100

{
   int jstat;

/* Names of source and library files. */
   char source [ LFN ], lib [ LFN ];


/* Get the source-file name. */
   if (argc >= 2 ) {
      strncpy ( source, argv [1], LFN );
   } else {
      puts ( "Name of help source file to be read?" );
      hlpTrim ( fgets ( source, LFN, stdin ) );
   }

/* Get the library-file name. */
   if (argc >= 3 ) {
      strncpy ( lib, argv [2], LFN );
   } else {
      puts ( "Name of help library file to be written?" );
      hlpTrim ( fgets ( lib, LFN, stdin ) );
   }

/* Create the HELP library. */
   if ( ( jstat = hlpCreh ( hlpNametr, source, lib ) ) )
      puts ( hlpErrmes ( jstat ) );
   return jstat;
}
