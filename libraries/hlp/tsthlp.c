#include <string.h>
#include "help.h"
#include "hlpsys.h"

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
**  The error codes are defined in the hlpsys.h header file, also
**  the maximum length of filenames LFNAME.
**
**  Called:  hlpStrncp, hlpHelp, hlpErrmes and the user-replaceable
**           functions hlpInsub, hlpOutsub and hlpNametr.
**
**  Last revision:   11 March 2014
**
**  Copyright P.T.Wallace.  All rights reserved.
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
      hlpStrncp ( lib, argv [1], LFNAME );
   } else {
      jstat = hlpInsub ( lib, "Name of help library? ",  &l );
      if ( jstat != 1 ) goto badinput;
   }

/* Make an announcement. */
   jstat = hlpOutsub ( " " );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub (
       "This is the tsthlp program, which runs a help session using"
                     );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub (
       "a nominated library.  To leave tsthlp, enter a period at"
                     );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub ( "the colon prompt." );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub ( " " );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub (
       "Please note that tsthlp is merely a simple demonstration, not"
                     );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub (
       "a fully-fledged help utility!  It lacks sophisticated,"
                    );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub (
       "platform-specific features such as screen management, quick"
                     );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub (
       "exits via control characters, and so on.  These capabilities"
                     );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub (
       "are provided by the various application packages which use"
                     );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub ( "the help system." );
   if ( jstat != 1 ) goto badoutput;
   jstat = hlpOutsub ( " " );
   if ( jstat != 1 ) goto badoutput;


/* Initialize the command string. */
   ipline [ 0 ] = '\0';

/* Loop until terminate request. */
   for ( ; ; ) {

   /* Get a command line. */
      jstat = hlpInsub ( ipline, ": ", &l );
      if ( jstat != 1 ) goto badinput;

   /* Unless period, begin an interactive HELP session. */
      if ( ! strcmp ( ipline, "." ) ) {
         jstat = 0;
         goto exit;
      } else {
         jstat = hlpHelp ( hlpOutsub, LOUT, ipline, lib, 1,
                           hlpInsub, hlpNametr );
         if ( jstat != 1 ) goto abort;
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
