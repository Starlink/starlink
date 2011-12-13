/*
 *+
 *  Name:
 *     ems1Iepnd

 *  Purpose:
 *     Return whether there are any error messages pending output.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     result = ems1Iepnd( )

 *  Description:
 *     The number of error messages at the current context in the error
 *     table is found. If there are any entries pending output then
 *     EMS1IEPND returns the value TRUE, otherwise the value FALSE
 *     is returned.

 *  Copyright:
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
 *     8-JAN-90 (PCTR):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_IEPND
 *     13-MAY-2008 (PWD):
 *        Use struct to access message table.
 *     {enter_any_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private functions prototypes */
#include "ems_defs.h"                /* EMS_ message table */

Logical ems1Iepnd ( void )
{
    int n;

    ems_msgtab_t *msgtab = ems1Gmsgtab();  /* Current message table */

    TRACE( "ems1Iepnd" );

    /*  Find the number of pending error messages at the current error
     *  context. */
    if ( msgtab->msgmrk > EMS__BASE ) {
        n = msgtab->msgcnt[ msgtab->msgmrk ] -
            msgtab->msgcnt[ msgtab->msgmrk - 1 ];
    } else {
        n = msgtab->msgcnt[ msgtab->msgmrk ];
    }

    return ( n > 0 );
}
