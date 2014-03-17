#include <string.h>
#include "hlpsys.h"
int main ( int argc, char *argv[] )
/*
**  - - - - - - -
**   l s t h l p
**  - - - - - - -
**
**  List a HELP library file for diagnostic purposes.
**
**  I/O:
**     arg1     input: HELP source
**     arg2     output: report
**     stdin    command input
**     stdout   prompts and error messages (also report via hlpOutsub)
**
**  If not specified through command-line arguments, the filenames
**  are prompted for.
**
**  Error codes defined in hlpsys.h #include file:
**      hlp_READ_WIDE
**      hlp_OPEN_ERROR
**
**  Returned (function value):
**               int     status: 0 = OK
**                  hlp_OPEN_ERROR = error opening report file
**                       other -ve = error reported by called routine
**
**  Called:  hlpHinit, hlpHopenr, hlpHdread, hlpErrmes, hlpTrim,
**           hlpNametr
**
**  Last revision:   16 June 2000
**
**  Copyright 2000 P.T.Wallace.  All rights reserved.
*/

/* Maximum length of filenames (including '\0'). */
#define LFN 100

/* Input buffer length. */
#define LBUF 200

{
   int jstat, nc;
   long iadr, iw;

/* Name of library and report files. */
   char lib [ LFN ], rep [ LFN ];

/* File pointer for report file. */
   FILE *fpr;

/* Input buffer. */
   char buf [ LBUF ];


/* Get the library-file name. */
   if (argc >= 2 ) {
      strncpy ( lib, argv [1], LFN );
   } else {
      puts ( "Name of help library to be read?" );
      hlpTrim ( fgets ( lib, LFN, stdin ) );
   }

/* Get the report-file name. */
   if (argc >= 3 ) {
      strncpy ( rep, argv [2], LFN );
   } else {
      puts ( "Name of report file to be written?" );
      hlpTrim ( fgets ( rep, LFN, stdin ) );
   }

/* Initialize the HELP system. */
   hlpHinit ( lib );

/* Open the HELP library file. */
   if ( ( jstat = hlpHopenr ( hlpNametr ) ) ) {
      puts ( hlpErrmes ( jstat ) );
      return jstat;
   }

/* Open the report file. */
   if ( ( fpr = fopen ( rep, "w+" ) ) == NULL ) {
      jstat = hlp_OPEN_ERROR;
      puts ( hlpErrmes ( jstat ) );
      return jstat;
   }

/* Initialize the starting address. */
   iadr = 0;

/* Loop while OK status. */
   while ( ! jstat ) {

   /* Save the current address for the report. */
      iw = iadr;

   /* Read a record. */
      jstat = hlpHdread ( LBUF, &iadr, buf, &nc );

   /* Report the record unless fatal error. */
      if ( ! jstat ) {
         fprintf ( fpr, "%6ld   %s\n", iw, buf );
      } else if ( jstat < 0 && ! ( jstat == hlp_READ_WIDE && ! nc )) {
         puts ( hlpErrmes ( jstat ) );
         return jstat;
      }
   }
   return 0;
}
