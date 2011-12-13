/*+
 *  Name:
 *     emsLevel

 *  Purpose:
 *     Inquire the current error context level.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsLevel( level )

 *  Description:
 *     This function provides a C interface for the Error Message
 *     Service routine EMS_LEVEL (written in Fortran).

 *  Arguments:
 *     level = int * (Returned)
 *        The error context level.

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
 *        Renamed from ems_level_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_LEVEL
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

/* Include Statements: */
#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems1.h"                      /* EMS_ private functions prototypes */
#include "ems_defs.h"                  /* EMS_ message table */

/* Function Definitions: */
void emsLevel( int *level )
{
    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "emsLevel" );
    DEBUG( "emsLevel", "msglev = %d", msgtab->msglev );

    /*  Load the returned value of LEVEL. */
    *level = msgtab->msglev;

    return;
}
