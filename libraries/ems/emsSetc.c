/*+
 *  Name:
 *     emsSetc

 *  Purpose:
 *     Assign a CHARACTER value to a message token (concise).

 *  Language:
 *     Starlink ANSI C

 *  Invocation:
 *     emsSetc( token, cvalue )

 *  Description:
 *     This function sets the specified message token to the given string
 *     value, leading spaces are retained but trailing spaces removed.
 *     The string will be truncated to EMS__SZTOK characters if not null-
 *     terminated earlier.
 *     A blank string will be rendered as a token of one space. If the C
 *     pointer is NULL, the string <Null> will be inserted instead.
 *     A Fortran interface EMS_SETC is also provided (but it does not include
 *     the printf formatting).

 *  Arguments:
 *     token = const char * (Given)
 *        The message token name.
 *     cvalue = const char * (Given)
 *        The CHARACTER value to be assigned to the message token.
 *        A NULL pointer is converted to <Null>.

 *  Copyright:
 *     Copyright (C) 1990, 1991 Science & Engineering Research Council.
 *     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
 *     Copyright (C) 2007-2009 Science and Technology Facilities Council.
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
 *     TIMJ: Tim Jenness (JAC, Hawaii)
 *     PWD: Peter W. Draper (JAC, Durham University)
 *     {enter_new_authors_here}

 *  History:
 *     6-JUN-1990 (PCTR):
 *        Original version, coded as a C macro function.
 *     10-AUG-1990 (PCTR):
 *        C function code.
 *     21-JUN-1991 (PCTR):
 *        Made all given character strings type "const".
 *     13-MAY-1999 (AJC):
 *        Renamed from ems_setc_c
 *     14-FEB-2001 (RTP):
 *        Rewritten from the Fortran routine EMS_SETC
 *     13-MAR-2001 (AJC):
 *        Properly import strings and make maxlen argument optional.
 *     20-SEP-2001 (AJC):
 *        Restrict string length to EMS__SZTOK
 *     01-NOV-2007 (TIMJ):
 *        Trap null pointer.
 *     15-MAY-2008 (PWD):
 *        Remove unused variable.
 *     15-SEP-2008 (TIMJ)
 *        - Decide that a NULL pointer should print something rather than
 *          a space.
 *     16-SEP-2008 (TIMJ):
 *        Remove deprecated 3 arg interface.
 *     11-FEB-2009 (TIMJ):
 *        Allow ems1Putc to truncate the string consistently.
 *     {enter_further_changes_here}

 *  Bugs:
 *     {note_any_bugs_here}

 *-
 */

/* Include Statements: */
#include <stdio.h>                     /* sprintf */
#include <string.h>                    /* String handling library functions */


#include "ems_par.h"                   /* ems_ public constant definitions */
#include "ems_sys.h"                   /* ems_ private macro definitions */
#include "ems.h"                       /* ems_ function prototypes */
#include "ems1.h"                      /* ems_ internal function prototypes */

/* Function Definitons: */
void emsSetc( const char *token, const char *cvalue )
{
    int i;
    char valbuf[ EMS__SZTOK + 2 ]; /* +2 to allow ems1Putc to know when there is truncation */
    const char null[] = "<Null>";

    TRACE( "emsSetc" );
    DEBUG( "emsSetc", "emsSetc: '%s'", token );

    /*  Handle null pointer and printf formatting */
    valbuf[0] = '\0';
    if ( cvalue ) {
      strncpy( valbuf, cvalue, sizeof(valbuf) );
    } else {
      strncpy( valbuf, null, sizeof(valbuf) );
    }
    valbuf[ sizeof(valbuf)-1 ] = '\0';

    /*  Find the used length of the string */
    for ( i = strlen( valbuf ); i > 0 ; i-- ) {
        if ( valbuf[ i - 1 ] != ' ' ) break;
    }
    valbuf[ i ] = '\0';

    /*  Ensure minimum one space */
    if ( ! i ) {
        strcpy( valbuf, " " );
    }

    /*  Set the token value. */
    ems1Stok( token, valbuf );

    return;
}
