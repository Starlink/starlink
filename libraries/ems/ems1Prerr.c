/*
*+
*  Name:
*     EMS1PRERR

*  Purpose:
*     Deliver the text of an error message to the user.

*  Language:
*     Starlink ANSI C

*  Invocation:
*     ems1Prerr( text, status )

*  Description:
*     This uses the Fortran WRITE statement to send a message to the 
*     user. Trailing blanks are removed. 

*  Arguments:
*     text = char* (Given)
*        Text to be output.
*     status = int* (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     SLW: Sid Wright (UCL)
*     BDK: Dennis Kelly (ROE)
*     RFWS: R.F. Warren-Smith (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     AJC: A.J.Chipperfield (STARLINK)
*     RTP: R.T. Platon (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original FORTRAN version.
*     14-FEB-2001 (RTP):
*        Rewritten in C based on the Fortran routine EMS1_PRERR
*      6-MAR-2001 (AJC):
*        Initialise iposn to 0
*        Parameterise the output stream
*     26-SEP-2001 (AJC):
*        Allow for MAXTAB in continuation line length
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/*  Global Constants: */
#include <stdio.h>
#include "ems1.h"                    /* EMS_ Internal functions */
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */

/*  Global Variables: */
#include "ems_msgtb.h"               /* Error message table */

#define MAXTAB 6
#define CONTAB "!     "

void ems1Prerr( const char *text, int *status ) {
   int iostat;                    /* Fortran IOSTAT status */
   int iposn;                     /* Character position for text */
   int leng;                      /* Given string length */
   int oplen;                     /* Output string length */

   char line[ EMS__SZMSG + 1];    /* Output line of text */

   TRACE("ems1Prerr");

/*  Get length of text to send. */
   leng = strlen( text );

/*  if the text is not blank, then loop to send it. */
   if ( leng > 0 ) {

/*     Check the EMS tuning flag MSGSTM for output formatting. */
      if ( msgstm ) { 
/*        Output as much of the text as possible in one line. */
         iostat = fprintf(OP_STREAM, "%s\n", text );

      } else {
/*        Loop to split the line of text into sensible lengths for
 *        output, then write them to the default output stream. First,
 *        initialise the character pointer and Fortran I/O status. */
         iposn = 0;
         iostat = 0;

/*        Call ems1Rform to load the first output line and write the
 *        result. */
         ems1Rform( text, EMS__SZOUT, &iposn, line, &oplen );
         iostat = fprintf( OP_STREAM, "%s\n", line );

/*        DO WHILE loop. */
         while ( iposn != 0 && iostat >= 0 ) {

/*           Initialise the output line. */
            strcpy( line, CONTAB );
   
/*           Call ems1Rform to load the continuation line and write the
 *           result. */
            ems1Rform( text, EMS__SZOUT-MAXTAB, &iposn, &line[ MAXTAB ], &oplen );
            iostat = fprintf( OP_STREAM, "%s\n", line );
         }
      }
   } else {

/*     if there is no text, then send a blank message. */
       iostat = fprintf( OP_STREAM, "\n" );
   }

/*  Check I/O status and set STATUS if necessary. */
   if ( iostat < 0 ) *status = EMS__OPTER;

/* Ensure the message is displaye */
   (void) fflush( OP_STREAM );

   return;
}
