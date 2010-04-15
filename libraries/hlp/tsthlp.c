#include <string.h>
#include "hlpsys.h"
int hlpHelp ( int ( * ) ( char* ), int, char*, char*, int,
              int ( * ) ( char*, char*, int* ),
              int ( * ) ( int, char*, int, char* ) );
int main ( int argc, char *argv[] )
/*
**  - - - - - - -
**   t s t h l p
**  - - - - - - -
**
**  Testbed for hlpHelp routine.
**
**  The name of the HELP library file can be specified through a
**  command-line argument;  if not, the filename is prompted for.
**
**  I/O:
**     arg1     input: HELP library
**     stdin    command input (via hlpInsub)
**     stdout   prompts and error messages (via hlpOutsub)
**
**  Returned (function value):
**               int     status: 0 = OK
**                       other -ve = error reported by hlpHelp
**
**  The error codes are defined in the hlpsys.h #include file, also
**  the maximum length of filenames LFNAME.
**
**  Called:  hlpHelp, hlpErrmes and the user-replaceable
**           routines hlpInsub, hlpOutsub and hlpNametr.
**
**  Last revision:   16 June 2000
**
**  Copyright 2000 P.T.Wallace.  All rights reserved.
*/

/* Maximum length of HELP lines to be output. */
#define LOUT 79

{
/* Status. */
   int jstat;

/* Length of input response string. */
   int l;

/* Name of HELP library. */
   char lib [ LFNAME ];

/* Buffer to receive lines of HELP text. */
   char ipline [ LOUT ];


/* Get the library-file name. */
   if (argc >= 2 ) {
      strncpy ( lib, argv [1], LFNAME );
   } else {
      if ( ( jstat = hlpInsub ( lib, "Name of help library? ",
                                &l ) ) != 1 ) goto badinput;
   }

/* Make an announcement. */
   if ( ( jstat = hlpOutsub ( " " ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub (
       "This is the tsthlp program, which runs a help session using"
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub (
       "a nominated library.  To leave tsthlp, enter a period at"
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub ( "the colon prompt."
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub ( " " ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub (
       "Please note that tsthlp is merely a simple demonstration, not"
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub (
       "a fully-fledged help utility!  It lacks sophisticated,"
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub (
       "platform-specific features such as screen management, quick"
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub (
       "exits via control characters, and so on.  These capabilities"
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub (
       "are provided by the various application packages which use"
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub ( "the help system."
                                  ) ) != 1 ) goto badoutput;
   if ( ( jstat = hlpOutsub ( " " ) ) != 1 ) goto badoutput;

/* Initialize the command string. */
   ipline [ 0 ] = '\0';

/* Loop until terminate request. */
   for ( ; ; ) {

   /* Get a command line. */
      if ( ( jstat = hlpInsub ( ipline, ": ", &l ) ) != 1 ) goto badinput;

   /* Unless period, begin an interactive HELP session. */
      if ( ! strcmp ( ipline, "." ) ) {
         jstat = 0;
         goto exit;
      } else {
         if ( ( jstat = hlpHelp ( hlpOutsub, LOUT, ipline, lib, 1,
                                  hlpInsub, hlpNametr ) ) != 1 )
            goto abort;
      }

/* Next command line. */
   }

/* Bad status from hlpOutput. */
badoutput:
   jstat = hlp_LINE_OUTPUT_BAD;
   goto abort;

/* Bad status from hlpInput. */
badinput:
   jstat = hlp_LINE_INPUT_BAD;
   goto abort;

/* Abort. */
abort:
   hlpOutsub ( hlpErrmes ( jstat ) );
   goto exit;

/* Exit. */
exit:
   return jstat;
}
