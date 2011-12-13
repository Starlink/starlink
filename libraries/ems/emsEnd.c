/*+
 *  Name:
 *     emsEnd

 *  Purpose:
 *     End the current error reporting environment.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsEnd( status )

 *  Description:
 *     This function provides a C interface for the Error Message
 *     Service routine EMS_END (written in Fortran).

 *  Arguments:
 *     status = int * (Returned)
 *        The global status value.

 *  Copyright:
 *     Copyright (C) 1990 Science & Engineering Research Council.
 *     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2008 Science and Technology Facility Council.
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
 *        Renamed from ems_end_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_END
 *     19-MAR-2001 (AJC):
 *        Substantial mods and comment.
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include "sae_par.h"
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems_err.h"
#include "ems1.h"                      /* EMS_ private functions prototypes */
#include "ems_defs.h"                  /* EMS_ message table */

/* Function Definitions: */
void emsEnd( int *status )
{
    int istat;
    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "emsEnd" );
    DEBUG( "emsEnd", "msglev = %d", msgtab->msglev );

    /*  First, check if there is a previous context and that the context stack
     *  has not overflowed. */
    if ( ( msgtab->msglev > msgtab->msgdef ) &&
         ( msgtab->msglev < EMS__MXLEV ) ) {

        /*  The current context level is not the default level, nor has the
         *  error context table overflowed: get the status value given in the
         *  nested call to EMS_BEGIN and act on it. */
        istat = msgtab->msgbgs[ msgtab->msglev - 1 ];

        /*  Check if the status value given to EMS_BEGIN in the previous error
         *  context is SAI__OK.*/
        if ( istat != SAI__OK ) {

            /*  The status value given to EMS_BEGIN in the previous error
             *  context is not SAI__OK, so annul the current context and
             *  return the status value given in the nested call to
             *  EMS_BEGIN. */
            *status = istat;
            emsAnnul ( &istat );

        } else {
            /*  Return the last reported status value. */
            emsStat( status );
        }

    } else if ( msgtab->msglev == msgtab->msgdef ) {

        /*  There is no previous active context, EMS_BEGIN and EMS_END calls
         *  must be badly nested. */
        *status = EMS__NSTER;

    } else if ( msgtab->msglev > EMS__MXLEV ) {

        /*  The context stack has overflowed, return the appropriate status
         *  value. */
        *status = EMS__CXOVF;
    }

    /*  Release the current error context. */
    emsRlse();

    return;
}
