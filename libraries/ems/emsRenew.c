/*+
 *  Name:
 *     emsRenew

 *  Purpose:
 *     Renew any annulled message tokens in the current context.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsRenew()

 *  Description:
 *     This function provides a C interface for the Error Message
 *     Service routine EMS_RENEW (written in Fortran).

 *  Copyright:
 *     Copyright (C) 1991 Science & Engineering Research Council.
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
 *     21-JUN-1991 (PCTR):
 *        Original version.
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_renew_c
 *     14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran routine EMS_RENEW
 *     14-MAY-2008 (PWD):
 *        Use struct to access token table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include "ems_par.h"                   /* EMS_ public constant definitions */
#include "ems.h"                       /* EMS_ function prototypes */
#include "ems_sys.h"                   /* EMS_ private macro definitions */
#include "ems1.h"                      /* EMS_ private function prototypes */
#include "ems_defs.h"                  /* EMS_ token table */

/* Function Definitons: */
void emsRenew( void )
{
    ems_toktab_t *toktab = ems1Gtoktab();  /* Current token table */

    TRACE( "emsRenew" );

    /*  Assign the token count to the token high water mark. */
    toktab->tokcnt[ toktab->tokmrk ] = toktab->tokhiw[ toktab->tokmrk ];

    return;
}
