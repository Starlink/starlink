/*
 *+
 *  Name:
 *     ems1Mpush

 *  Purpose:
 *     Push a new context for the token table.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Mpush

 *  Description:
 *     Set the token table indices for a new context.

 *  Copyright:
 *     Copyright (C) 1984 Science & Engineering Research Council.
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
 *     SLW: Sid Wright  (UCL)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     14-APR-1984 (SLW):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP):
 *        Rewritten in C based on the Fortran routine EMS1_MPUSH
 *     14-MAY-2008 (PWD):
 *        Use struct to access token table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/*  Global Constants: */
#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private function prototypes */
#include "ems_defs.h"                /* EMS_ token table */

void ems1Mpush( void )
{
    ems_toktab_t *toktab = ems1Gtoktab();  /* Current token table */

    TRACE( "ems1Mpush" );

    /*  Check for maximum number of message context levels. */
    if ( toktab->toklev < EMS__MXLEV ) {

        /*  OK to push context. */
        toktab->toklev++;
        toktab->tokmrk++;
        toktab->tokcnt[ toktab->tokmrk ] = toktab->tokhiw[ toktab->tokmrk-1 ];
        toktab->tokhiw[ toktab->tokmrk ] = toktab->tokhiw[ toktab->tokmrk-1 ];
    } else {

        /*  Context stack full, so increment TOKLEV only. */
        toktab->toklev++;
    }
    return;
}
