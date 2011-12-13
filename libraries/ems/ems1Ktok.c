/*
 *+
 *  Name:
 *     ems1Ktok

 *  Purpose:
 *     Clear the message token table.

 *  Language:
 *     Starlink ANSI C

 *  Invokation:
 *     ems1Ktok()

 *  Description:
 *     Clear all the message tokens at the current context level.

 *  Algorithm:
 *     -  Set the token count to that of previous context level.

 *  Copyright:
 *     Copyright (C) 1982 Science & Engineering Research Council.
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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     3-JAN-1982 (JRG):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP):
 *        Rewritten in C based on the Fortran routine EMS1_PFORM
 *      1-OCT-2001 (AJC):
 *        Remove setting high-water mark (it's already set by ems1Stok)
 *        - this allows emsRenew to work properly.
 *     14-MAY-2008 (PWD):
 *        Use struct to access token table.
 *     30-JUL-2008 (PWD):
 *        When in a mark only kill tokens to the last high water mark,
 *        not last token count. Without this token catenation can fail to
 *        spot that a token with the same name already exists.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private function prototypes */
#include "ems_defs.h"                /* EMS_ token table */

void ems1Ktok ( void )
{
    ems_toktab_t *toktab = ems1Gtoktab();  /* Current token table */

    TRACE( "ems1Ktok" );

    /*  Clear the token table at the current context. */
    if ( toktab->tokmrk > 1 ) {
        toktab->tokcnt[ toktab->tokmrk ] =
            toktab->tokhiw[ toktab->tokmrk - 1 ];
    } else {
        toktab->tokcnt[ toktab->tokmrk ] = 0;
    }
    return;
}
