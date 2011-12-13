/*+
 *  Name:
 *     emsEload

 *  Purpose:
 *     Return error messages from the current error context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsEload( param, parlen, opstr, oplen, status )

 *  Description:
 *     This function provides a C interface for the Error Message
 *     Service routine EMS_ELOAD (written in Fortran).

 *  Arguments:
 *     param = char * (Returned)
 *        The error message name.
 *     parlen = int *  (Returned)
 *        The length of the error message name.
 *     opstr = char * (Returned)
 *        The error message.
 *     oplen = int * (Returned)
 *        The length of the error message.
 *     status = int * (Given and Returned)
 *        The global status.

 *  Copyright:
 *     Copyright (C) 1990 Science & Engineering Research Council.
 *     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     AJC: A.J. Chipperfield (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     17-SEP-1990 (PCTR):
 *        C function code.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_eload_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_ELOAD
 *      5-MAR-2001 (AJC):
 *        Simplify
 *        Switch off REVEAL
 *     23-JUL-2001 (AJC):
 *        Correct message names length
 *     13-AUG-2001 (AJC):
 *        Remove unused variables
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     19-MAY-2008 (PWD):
 *        Use thread specific data to store table copy.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Include Statements: */
#include <string.h>                  /* String handling library functions */
#include "sae_par.h"
#include "ems_par.h"                 /* EMS_ public constant definitions */
#include "ems.h"                     /* EMS_ function prototypes */
#include "ems_sys.h"                 /* EMS_ private macro definitions */
#include "ems_err.h"
#include "ems1.h"                      /* EMS_ private functions prototypes */
#include "ems_defs.h"                  /* EMS_ message table */

/* Function Definitons: */
void emsEload( char *param, int *parlen, char *opstr, int *oplen,
               int *status )
{
    int *nerbuf;                   /* Number of error messages */
    int *old;                      /* Whether to start a new context. */
    int *opcnt;                    /* Message output counter */
    int i;                         /* Loop index   */
    int iend;                      /* Ending loop value */
    int istart;                    /* Starting loop value  */
    int istat;                     /* Local status */
    int leng;                      /* String length */
    int nerpnt;                    /* Error message pointer */
    short tmprvl;                  /* Temporary storage for MSGRVL */

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current active message table */
    ems_msgtab_t *contab = ems1Gmsgtab2(); /* Thread specific spare message table */

    TRACE( "emsEload" );

    /*  Use userdata part of table to carry additional context between calls
     *  when threading. These values are guaranteed to be 0 when the thread
     *  starts. */
    old = &contab->userdata[0];
    nerbuf = &contab->userdata[1];
    opcnt = &contab->userdata[2];

    /*  Is this a new error flush? */
    if ( ! *old  ) {

        /*  Unset the 'reload holding area' flag */
        *old = 1;

        /*  A new error flush, so initialise output counter. */
        *opcnt = 1;

        /*  Find the first message to flush. */
        if ( msgtab->msgmrk > EMS__BASE ) {
            istart = msgtab->msgcnt[ msgtab->msgmrk - 1 ] + 1;
        } else {
            istart = 1;
        }

        /*  Find the last message. */
        iend = msgtab->msgcnt[ msgtab->msgmrk ];

        /*  If there are messages to flush, loop to load error buffer. */
        if ( iend >= istart ) {

            /*  Set nerbuf (the number of messages in the context). */
            *nerbuf = iend - istart + 1;

            for ( i = 1; i<= *nerbuf; i++ ) {
                nerpnt = istart + i - 1;

                /*  Construct the output line. */
                strcpy( contab->msgpar[ i ], msgtab->msgpar[ nerpnt ] );
                contab->msgpln[ i ] = msgtab->msgpln[ nerpnt ];
                strcpy( contab->msgstr[ i ], msgtab->msgstr[ nerpnt ] );
                contab->msglen[ i ] = msgtab->msglen[ nerpnt ];
                contab->msgsta[ i ] = msgtab->msgsta[ nerpnt ];
            }

        } else {

            /*  If there are no messages to flush, load a warning. */
            *nerbuf = 1;
            nerpnt = 1;
            strcpy ( contab->msgpar[ nerpnt ], "EMS_ELOAD_NOMSG" );
            contab->msgpln[ nerpnt ] = strlen( contab->msgpar[ nerpnt ] );
            strcpy ( contab->msgstr[ nerpnt ],
                     "No error to report (improper use of EMS)." );
            contab->msglen[ nerpnt ] = strlen( contab->msgstr[ nerpnt ] );
            contab->msgsta[ nerpnt ] = EMS__NOMSG;
        }

        /*  Annul the error table at the current context. Switch off MSGRVL
         *  whilst doing it as messages have been handled. */
        tmprvl = msgtab->msgrvl;
        msgtab->msgrvl = FALSE;
        emsAnnul( &istat );
        msgtab->msgrvl = tmprvl;

    } else {

        /*  Increment output counter. */
        (*opcnt)++;
    }

    /*  Initialize the return values. These values also serve as the return
     *  values if there are no more messages.  */
    strcpy( param, " ");
    *parlen = 1;
    strcpy( opstr, " ");
    *oplen = 1;
    *status = SAI__OK;

    /*  Now, if there are any messages left to output - return the next one. */
    if ( *opcnt <= *nerbuf ) {

        /*  There are more messages - return the message name. */
        leng = contab->msgpln[ *opcnt ];
        if ( leng  > 0 ) {
            strcpy( param, contab->msgpar[ *opcnt ] );
            *parlen = leng;
        }

        /*  Return the message string. */
        leng = contab->msglen[ *opcnt ];
        if ( leng > 0 ) {
            strcpy( opstr, contab->msgstr[ *opcnt ] );
            *oplen = leng;
        }

        /*  Return the associated status */
        *status = contab->msgsta[ *opcnt ];

    } else {

        /*  There are no more messages, force the holding area to be updated
         *  again on the next call */
        *old = 0;
    }
    return;
}
