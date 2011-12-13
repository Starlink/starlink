#define _POSIX_SOURCE 1		 /* Declare POSIX source */
/*
*+
*  Name:
*     ndf1.c

*  Purpose:
*     Implement the private C interface to the NDF_ library.

*  Language:
*     ANSI C

*  Description:
*     This module implements C-callable wrappers for the internal
*     Fortran routines in the NDF_ library which are called from C.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils.

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
*     RFWS: R.F. Warren-Smith (STARLINK, RAL)
*     <{enter_new_authors_here}>

*  History:
*     5-OCT-1998 (RFWS):
*        Original version.
*     <{enter_further_changes_here}>
*-
*/

/* Macro definitions for this module. */
/* ================================== */
/* These are work-arounds for problems with "const" handling by
   CNF. They should be removed when these problems are fixed. */
#define fix_F77_EXPORT_CHARACTER(a,b,c) F77_EXPORT_CHARACTER(((char *)(a)),(b),(c));

/* Header files. */
/* ============= */
/* C run-time library header files. */
#include <string.h>              /* String handling */

/* External interface header files. */
#include "f77.h"                 /* C<-->Fortran interface macros */

/* Internal header files. */
#include "ndf1.h"                /* NDF_ library private interface */

/* Wrapper function implementations. */
/* ================================= */
F77_SUBROUTINE(ndf1_trace)( CHARACTER(routin),
                            INTEGER(status)
                            TRAIL(routin) );

void ndf1Trace( const char *routin,
                int *status ) {

DECLARE_CHARACTER_DYN(froutin);
DECLARE_INTEGER(fstatus);

   F77_CREATE_CHARACTER( froutin, strlen( routin ) );
   fix_F77_EXPORT_CHARACTER( routin, froutin, froutin_length );
   F77_EXPORT_INTEGER( *status, fstatus );

   F77_LOCK( F77_CALL(ndf1_trace)( CHARACTER_ARG(froutin),
                         INTEGER_ARG(&fstatus)
                         TRAIL_ARG(froutin) ); )

   F77_IMPORT_INTEGER( fstatus, *status );

   return;
}
