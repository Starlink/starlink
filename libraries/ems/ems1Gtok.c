/*
 *+
 *  Name:
 *     ems1Gtok

 *  Purpose:
 *     Get a token for the message parser, hiding the token escapes.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     result = ems1Gtok( namstr, tokval, tkvlen )

 *  Description:
 *     The message token table is searched for the given token name in the
 *     current context. if a valid token is found, { the token value and
 *     its length are returned. if no valid token is found, { the name
 *     string is returned using the syntax for an undefined message token.

 *  Arguments:
 *     namstr = char* (Given)
 *        The token name string.
 *     tokstr = char* (Returned)
 *        The token value string.
 *     toklen = int* (Returned)
 *        The length of the token value string.

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
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T. Platon (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     3-JAN-1982 (JRG):
 *        Original FORTRAN version.
 *     14-FEB-2001 (RTP)
 *        Rewritten in C based on the Fortran routine EMS1_GTOK
 *     14-MAY-2008 (PWD):
 *        Use struct to access token table.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include <strings.h>

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private function prototypes */
#include "ems_defs.h"                /* EMS_ token table */

Logical ems1Gtok( const char *namstr, char *tokval, int *tkvlen )
{
    int i;                         /* Loop index */
    int last;                      /* Search limit for token definition */

    ems_toktab_t *toktab = ems1Gtoktab();  /* Current token table */

    TRACE ( "ems1Gtok" );

    /*  Set the limits for the search of the message token table. */
    i = toktab->tokcnt[ toktab->tokmrk ];
    if ( toktab->tokmrk > EMS__BASE ) {
        last = toktab->tokcnt[ toktab->tokmrk - 1 ];
    } else {
        last = 0;
    }

    /*  Search the message token table for a match of in the token names.
     *  DO WHILE loop. */
    while ( i > last ) {
        /* Check if a match has been found. */
        if ( strcasecmp( namstr, toktab->toknam[ i ] ) == 0 ) {

            /*  A match has been found, so load the returned token string. */
            strcpy( tokval, toktab->tokstr[ i ] );
            *tkvlen = toktab->toklen[ i ];
            return TRUE;
        }
        i--;
    }

    /*  A match has not been found, so load the "undefined token" string. */
    return FALSE;
}
