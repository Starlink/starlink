/*
 *+
 *  Name:
 *     ems1Perr

 *  Purpose:
 *     Deliver the text of an error message to the user.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Prerr( text, status )

 *  Description:
 *     Send a message to the user using standard C I/O facilities.
 *     Trailing blanks are removed.

 *  Arguments:
 *     text = char* (Given)
 *        Text to be output.
 *     status = int* (Given and Returned)
 *        The global status.

 *  Copyright:
 *     Copyright (C) 1983 Science & Engineering Research Council.
 *     Copyright (C) 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2008 Science and Technology Facilities Council.
 *     All Rights Reserved.

 *  Licence:
 *     This program is free software; you can redistribute it and/or
 *     modify it under the terms of the GNU General Public License as
 *     published by the Free Software Foundation; either version 2 of
 *     the License, or (at your option) any later version.
 *
 *     This program is distributed in the hope that it will be
 *     useful,but WITHOUT ANY WARRANTY; without even the implied
 *     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
 *     PURPOSE. See the GNU General Public License for more details.
 *
 *     You should have received a copy of the GNU General Public License
 *     along with this program; if not, write to the Free Software
 *     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
 *     02110-1301, USA

 *  Authors:
 *     JRG: Jack Giddings (UCL)
 *     SLW: Sid Wright (UCL)
 *     BDK: Dennis Kelly (ROE)
 *     RFWS: R.F. Warren-Smith (STARLINK)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
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
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     30-JUL-2008 (PWD):
 *        Back to using msgtab->msgwsz as length for a non-streamed
 *        line (lost in transistion to C).
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/*  Global Constants: */
#include <stdio.h>
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */
#include "ems.h"

#define MAXTAB 6
#define CONTAB "!     "

void ems1Prerr( const char *text, int *status )
{
    int iostat;                    /* Fortran IOSTAT status */
    int iposn;                     /* Character position for text */
    int leng;                      /* Given string length */
    int oplen;                     /* Output string length */

    char line[ EMS__SZMSG + 1];    /* Output line of text */

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "ems1Prerr" );

    /*  Get length of text to send. */
    leng = strlen( text );

    /*  If the text is not blank, then loop to send it. */
    if ( leng > 0 ) {

        /*  Check the EMS tuning flag MSGSTM for output formatting. */
        if ( msgtab->msgstm ) {
            /*  Output as much of the text as possible in one line. */
            iostat = fprintf( OP_STREAM, "%s\n", text );

        } else {
            /*  Loop to split the line of text into sensible lengths for
             *  output, then write them to the default output stream. First,
             *  initialise the character pointer and Fortran I/O status. */
            iposn = 0;
            iostat = 0;

            /*  Call ems1Rform to load the first output line and write the
             *  result. */
            ems1Rform( text, msgtab->msgwsz, &iposn, line, &oplen );
            iostat = fprintf( OP_STREAM, "%s\n", line );

            /*  DO WHILE loop. */
            while ( iposn != 0 && iostat >= 0 ) {

                /*  Initialise the output line. */
                strcpy( line, CONTAB );

                /*  Call ems1Rform to load the continuation line and write the
                 *  result. */
                ems1Rform( text, (msgtab->msgwsz)-MAXTAB, &iposn,
                           &line[ MAXTAB ], &oplen );
                iostat = fprintf( OP_STREAM, "%s\n", line );
            }
        }
    } else {

        /*  If there is no text, then send a blank message. */
        iostat = fprintf( OP_STREAM, "\n" );
    }

    /*  Check I/O status and set STATUS if necessary. */
    if ( iostat < 0 ) *status = EMS__OPTER;

    /*  Ensure the message is displayed */
    (void) fflush( OP_STREAM );

    return;
}
