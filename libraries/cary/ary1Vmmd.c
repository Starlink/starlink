#include "sae_par.h"
#include "ary1.h"
#include "mers.h"
#include "ary_err.h"
#include "dat_par.h"
#include <string.h>

void ary1Vmmd( const char *mmod,  char mode[ARY__SZMOD+1],
               char inopt[ARY__SZIOP+1], int *status ) {
/*
*+
*  Name:
*     ary1Vmmd

*  Purpose:
*     Validate a mapping mode specification.

*  Synopsis:
*     void ary1Vmmd( const char *mmod,  char mode[ARY__SZMOD+1],
*                    char inopt[ARY__SZIOP+1], int *status )

*  Description:
*     The function checks a string containing a mapping mode
*     specification for validity. If it is valid, the mapping access
*     mode and the initialisation option string for write access are
*     returned. If the mapping mode specification is not valid, then an
*     error is reported.

*  Parameters:
*     mmod
*        The mapping mode string to be validated. Valid values are
*        "READ", "UPDATE" or "WRITE", with an initialisation option
*        "/ZERO" or "/BAD" optionally appended (case insensitive).
*     mode
*        Returned holding the mapping access mode (either "READ",
*        "UPDATE" or "WRITE", in upper case).
*     inopt
*        Returned holding the initialisation option (either "ZERO",
*        "BAD" or " ") in upper case.
*     status
*        The global status.

*  Copyright:
*      Copyright (C) 2017 East Asian Observatory
*      All rights reserved.

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
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     03-JUL-2017 (DSB):
*        Original version, based on equivalent Fortran routine by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Test the mapping mode string against each permitted value in turn,
   setting the returned arguments accordingly. */

/* ...Read access. */
   if( !strcasecmp( mmod, "READ" ) ){
      strcpy( mode, "READ" );
      strcpy( inopt, " " );

   } else if( !strcasecmp( mmod, "READ/ZERO" ) ){
      strcpy( mode,  "READ" );
      strcpy( inopt, "ZERO" );

   } else if( !strcasecmp( mmod, "READ/BAD" ) ){
      strcpy( mode,  "READ" );
      strcpy( inopt, "BAD" );

/* ...Update access. */
   } else if( !strcasecmp( mmod, "UPDATE" ) ){
      strcpy( mode,  "UPDATE" );
      strcpy( inopt, " " );

   } else if( !strcasecmp( mmod, "UPDATE/ZERO" ) ){
      strcpy( mode,  "UPDATE" );
      strcpy( inopt, "ZERO" );

   } else if( !strcasecmp( mmod, "UPDATE/BAD" ) ){
      strcpy( mode,  "UPDATE" );
      strcpy( inopt, "BAD" );

/* ...Write access. */
   } else if( !strcasecmp( mmod, "WRITE" ) ){
      strcpy( mode,  "WRITE" );
      strcpy( inopt, " " );

   } else if( !strcasecmp( mmod, "WRITE/ZERO" ) ){
      strcpy( mode,  "WRITE" );
      strcpy( inopt, "ZERO" );

   } else if( !strcasecmp( mmod, "WRITE/BAD" ) ){
      strcpy( mode,  "WRITE" );
      strcpy( inopt, "BAD" );

/* If the mapping mode string was not valid, then report an error. */
   } else {
      *status = ARY__MMDIN;
      msgSetc( "BADMMODE", mmod );
      errRep( " ", "Invalid array mapping mode '^BADMMODE' specified (possible"
              "programming error).", status );
   }

/* Call error tracing routine and exit. */
   if( *status != SAI__OK ) ary1Trace( "ary1Vmmd", status );

}
