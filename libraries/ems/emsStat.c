/*+
 *  Name:
 *     emsStat

 *  Purpose:
 *     Return the last status set within the current error context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsStat( status )

 *  Description:
 *     This function provides the functionality of the Error Message
 *     Service routine EMS_STAT (written in Fortran).

 *  Arguments:
 *     status = int * (Returned)
 *        The global status value.

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
 *     8-NOV-1990 (PCTR):
 *        Original version.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_stat_c
 *     14-Feb-2001 (RTP):
 *        rewritten EMS_STAT in C
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include "sae_par.h"                   /* SAE_ public constant definitions */
#include "ems_par.h"                   /* EMS_ public constant definitions */
#include "ems_sys.h"                   /* EMS_ private macro definitions */
#include "ems_err.h"                   /* EMS_ error codes definitions */
#include "ems.h"                       /* EMS_ function prototypes */
#include "ems1.h"                      /* EMS_ private functions prototypes */
#include "ems_defs.h"                  /* EMS_ message table */

/* Function Definitions: */
void emsStat( int *status )
{
    int istart;
    int iend;
    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "emsStat" );

    /*  Check for context stack overflow. */
    if ( msgtab->msglev <= EMS__MXLEV ) {

        /*  Find the first message in the current context. */
        if ( msgtab->msgmrk == EMS__BASE ) {

            /*  The base context is flushed immediately, the last reported
             *  status value is stored in MSGLST. */
            *status = msgtab->msglst;
        } else {

            /*  Higher contexts use the error table. First find the position
             *  of the first error message in the current context. */
            istart = msgtab->msgcnt[ msgtab->msgmrk - 1 ] + 1;

            /*  Find the last message. */
            iend = msgtab->msgcnt[ msgtab->msgmrk ];

            /*  If there are any messages in the current context set STATUS to
             *  the last reported value, else return SAI__OK. */
            if ( iend >= istart ) {
                *status = msgtab->msgsta[ iend ];
            } else {
                *status = SAI__OK;
            }
        }
    } else {

        /* The error context stack has overflowed, so return the appropriate
         * error status. */
        *status = EMS__CXOVF;
    }

    return;
}
