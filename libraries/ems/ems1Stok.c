/*
 *+
 *  Name:
 *     ems1Stok

 *  Purpose:
 *     Set the value of a token with character string.

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     ems1Stok( token, string )

 *  Description:
 *     This subroutine assigns the supplied character string to the named
 *     message token. Errors arising here will lead to indicators in the
 *     expanded message text.

 *  Arguments:
 *     token = const char* (Given)
 *        The message token name.
 *     string = const char* (Given)
 *        The message token string.

 *  Algorithm:
 *     -  The token is looked up in the token table and inserted if it is not
 *     found. The supplied character string is then associated with the token.

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
 *     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
 *     02111-1307, USA

 *  Authors:
 *     JRG: Jack Giddings (UCL)
 *     SLW: Sid Wright (UCL)
 *     PCTR: P.C.T. Rees (STARLINK)
 *     RTP: R.T.Platon (STARLINK)
 *     AJC: A.J.Chipperfield (STARLINK)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *    3-JAN-1983 (JRG):
 *        Original FORTRAN version.
 *    14-FEB-2001 (RTP):
 *        Rewritten in C from Fortran Routine EMS1_STOK.
 *     6-MAR-2001 (AJC):
 *        Add maxlen argument for ems1Putc
 *        Correct loop looking for existing token (and use strcasecmp).
 *    14-MAY-2008 (PWD):
 *        Use struct to access token table.
 *    {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

#include <strings.h>

#include "ems_par.h"                 /* EMS_ public constants */
#include "ems_sys.h"                 /* EMS_ private constants */
#include "ems1.h"                    /* EMS_ private function prototypes */
#include "ems_defs.h"                /* EMS_ token table */

void ems1Stok( const char *token, const char *string )
{
    int i;                         /* Loop index */
    int last;                      /* Search limit for token definition */
    int lstat;                     /* Local status */
    int tlen;                      /* Length of the token name */
    int tokln;                     /* Pointer within token string */

    ems_toktab_t *toktab = ems1Gtoktab();  /* Current token table */

    TRACE( "ems1Stok" );
    if (!token) return;

    /*  Get the token length. */
    tlen = strlen( token );

    /*  Act only for token names which are non-zero length. */
    if ( tlen > 0 ) {

        /*  Find any existing table entry for the token, TOKEN. */
        i = toktab->tokcnt[ toktab->tokmrk ];

        if ( toktab->tokmrk > EMS__BASE ) {
            last = toktab->tokhiw[ toktab->tokmrk - 1 ];
        } else {
            last = 0;
        }

        /*  Search for the token name TOKEN in the token table. */
        while ( i > last ) {
            if ( strcasecmp( token, toktab->toknam[ i ] ) == 0 ) break;
            i--;
        }

        /*  Allocate a new table entry for this token if one does not exist. */
        if ( i > last ) {

            /*  Append the token string to the existing token string. */
            tokln = toktab->toklen[ i ] - 1;
            ems1Putc( string, EMS__SZTOK, toktab->tokstr[ i ], &tokln,
                      &lstat );
            toktab->toklen[ i ] = tokln + 1;

        } else {
            /*  This is a new token: first check that the message token table
             *  is not full.
             */
            if ( toktab->tokcnt[ toktab->tokmrk ] < EMS__MXTOK ) {

                /*  There is room in the message token table, so increment the
                 *  token count and add the new token.
                 */
                i = toktab->tokcnt[ toktab->tokmrk ] + 1;
                toktab->tokcnt[ toktab->tokmrk ] = i;
                toktab->tokhiw[ toktab->tokmrk ] = i;
                strcpy( toktab->toknam[ i ], token );
                toktab->toklen[ i ] = 0;

                /*  Assign the token string. */
                tokln = toktab->toklen[ i ] - 1;
                ems1Putc( string, EMS__SZTOK, toktab->tokstr[ i ], &tokln,
                          &lstat );
                toktab->toklen[ i ] = tokln + 1;

            } else {

                /*  There is no room in the message token table, so abort. */
                return;
            }
        }
    }

    return;
}
