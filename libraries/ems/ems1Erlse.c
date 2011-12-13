/*
 *+
 *  Name:
 *     ems1Erlse

 *  Purpose:
 *     Release a context in the error table.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Erlse()

 *  Description:
 *     If the error context level has been set, then the previous context
 *     is returned to. Any messages remaining at the current context
 *     level will be transferred to the previous level.

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
 *     SLW: Sid Wright (UCL)
 *     RFWS: R.F. Warren-Smith (STARLINK)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     17-Apr-1983 (SLW):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP):
 *        Rewritten in C based on the Fortran routine EMS1_ERLSE
 *     19-MAR-2001 (AJC):
 *        Correct 'msglev < EMS__MXLEV' to <=
 *     13-AUG-2001 (AJC):
 *        #include ems1.h
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

void ems1Erlse( void )
{
    int istat;                     /* Local status */
    int lstat;                     /* Local status */
    int newcnt;                    /* Updated MSGCNT( MSGMRK ) */

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "ems1Erlse" );
    DEBUG( "ems1Erlse", "BEFORE msglev = %d", msgtab->msglev );

    /*  If the context level is marked above EMS__MXLEV, lower the level mark
     *  only. If the context level within the allowed range: if there is more
     *  than one context marker, remove last one, transferring any messages
     *  associated with it to the next context down.
     */
    if ( msgtab->msglev > EMS__MXLEV ) {

        /*  Context stack overflow. */
        msgtab->msglev--;

    } else if ( msgtab->msglev <= EMS__MXLEV &&
                msgtab->msglev > msgtab->msgdef ) {

        /*  All marked context levels. */
        newcnt = msgtab->msgcnt[ msgtab->msgmrk ];
        msgtab->msglev--;
        msgtab->msgmrk--;
        msgtab->msgcnt[ msgtab->msgmrk ] = newcnt;

        /*  If the new error context is the base level, flush and pending
         *  error messages and update the last reported status (MSGLST).
         */
        if ( msgtab->msglev == EMS__BASE && newcnt > 0 ) {
            lstat = msgtab->msgsta[ newcnt ];
            istat = lstat;
            ems1Flush( &istat );
            msgtab->msglst = lstat;
        }
    } else {

        /*  Otherwise, do nothing. */
        msgtab->msglev = msgtab->msgdef;
        msgtab->msgmrk = msgtab->msgdef;
    }
    DEBUG( "ems1Erlse", "AFTER msglev = %d", msgtab->msglev );

    return;
}
