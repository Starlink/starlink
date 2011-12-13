/*
 *+
 *  Name:
 *     ems1Estor1

 *  Purpose:
 *     Store an error message in the current context of an error table.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Estor1( msgtab, param, plen, msg, mlen, status )

 *  Description:
 *     This routine stores an error message in the current context of a
 *     given error table. If there is no room in the error table, then the
 *     last reported error message is replaced by a fault message.

 *  Arguments:
 *     msgtab = ems_msgtb_t* (Given)
 *        A message table struct.
 *     param = char* (Given)
 *        The error message name.
 *     plen = char* (Given)
 *        The length of error message name.
 *     msg = char* (Given)
 *        The error message text.
 *     mlen = char* (Given)
 *        The length of error message text.
 *     status = int (Given and returned)
 *        The global status.

 *  Copyright:
 *     Copyright (C) 1990 Science & Engineering Research Council.
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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     2-MAR-1990 (PCTR):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_ESTOR
 *     13-AUG-2001 (AJC):
 *        #include ems1.h
 *     31-MAR-2008 (PWD):
 *        Do not copy more than EMS__SZPAR and EMS__SZMSG characters
 *        to the error tables. Always null terminate these strings
 *        (previously the null could be missing if the given strings
 *        had a length greater than could be stored, and they overran).
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     16-MAY-2008 (PWD):
 *        Copied from ems1Estor and adapted to work with a given table,
 *        rather than the current table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/*  Global Constants: */
#include "sae_par.h"
#include "ems_err.h"                 /* EMS_ error codes */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

void ems1Estor1( ems_msgtab_t *msgtab, const char *param, int plen,
                 const char *msg, int mlen, int *status )
{
    int index;                     /* Table index */
    int istat;                     /* Local status */

    TRACE( "ems1Estor1" );
    DEBUG( "ems1Estor1", "msglev = %d", msgtab->msglev );

    /*  If the message table is full, then replace the last reported error
     *  message with an EMS_ fault message. */
    if ( msgtab->msgcnt[ msgtab->msgmrk ] == EMS__MXMSG ) {
        strcpy( msgtab->msgstr[ EMS__MXMSG ],
                "Error stack overflow (EMS fault)." );
        msgtab->msglen[ EMS__MXMSG ] =
            (int) strlen( msgtab->msgstr[ EMS__MXMSG ] );
        strcpy( msgtab->msgpar[ EMS__MXMSG ], "ems_estor_ovflo" );
        msgtab->msgpln[ EMS__MXMSG ] =
            (int) strlen( msgtab->msgpar[ EMS__MXMSG ] );
        msgtab->msgsta[ EMS__MXMSG ] = EMS__EROVF;

    } else {
        /*  Increment the message count for the current context. */
        msgtab->msgcnt[ msgtab->msgmrk ]++;

        /*  Store the STATUS. */
        index = msgtab->msgcnt[ msgtab->msgmrk ];
        msgtab->msgsta[ index ] = *status;

        /*  Store the error message name in the error table. */
        msgtab->msgpln[ index ] = MIN( plen, EMS__SZPAR );
        strncpy( msgtab->msgpar[ index ], param, EMS__SZPAR );
        msgtab->msgpar[ index ][ EMS__SZPAR ] = '\0';

        /*  Store the error message text in the error table. */
        msgtab->msglen[ index ] = MIN( mlen, EMS__SZMSG );
        strncpy( msgtab->msgstr[ index ], msg, EMS__SZMSG );
        msgtab->msgstr[ index ][ EMS__SZMSG ] = '\0';
    }

    /*  Check the error context and flush the error table if it is at the base
     *  level (i.e. EMS__BASE). */
    if ( msgtab->msglev == EMS__BASE ) {

        /*  Flush the error table. */
        istat = *status;
        ems1Flush( &istat );

        /*  Store the last reported status. */
        msgtab->msglst = *status;

        /*  Set the returned status, STATUS, on output error. */
        if ( istat != SAI__OK ) *status = istat;
    }
    return;
}
