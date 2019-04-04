#include <stdlib.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf_err.h"
#include "ndf1.h"
#include "ndf_ast.h"
#include "star/util.h"
#include "mers.h"

void ndf1Vmmd( const char *mmod, char *mode, size_t mode_length,
               char *inopt, size_t inopt_length, int *status ){
/*
*+
*  Name:
*     ndf1Vmmd

*  Purpose:
*     Validate a mapping mode specification.

*  Synopsis:
*     void ndf1Vmmd( const char *mmod, char *mode, size_t mode_length,
*                    char *inopt, size_t inopt_length, int *status )

*  Description:
*     This function checks a string containing a mapping mode specification
*     for validity. If it is valid, the mapping access mode and the
*     initialisation option string for write access are returned. If the
*     mapping mode specification is not valid, then an error is reported.

*  Parameters:
*     mmod
*        Pointer to a null terminated string holding the mapping mode
*        string to be validated. Valid values are "READ", "UPDATE" or
*        "WRITE" with either of the initialisation options "/ZERO" or
*        "/BAD" optionally appended (case insensitive).
*     mode
*        Pointer to an array in which to return a null terminated string
*        holding the mapping access mode (either "READ", "WRITE" or
*        "UPDATE") in upper case.
*     mode_length
*        The length of the supplied 'mode' array. This should include
*        room for the terminating null.
*     inopt
*        Pointer to an array in which to return a null terminated string
*        holding the initialisation option (either "ZERO", "BAD" or " ") in
*        upper case.
*     inopt_length
*        The length of the supplied 'inopt' array. This should include
*        room for the terminating null.
*     *status
*        The global status.

*  Copyright:
*     Copyright (C) 2018 East Asian Observatory
*     All rights reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or modify
*     it under the terms of the GNU General Public License as published by
*     the Free Software Foundation; either version 2 of the License, or (at
*     your option) any later version.
*
*     This program is distributed in the hope that it will be useful,but
*     WITHOUT ANY WARRANTY; without even the implied warranty of
*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
*     General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     DSB: David S. Berry (EAO)

*  History:
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Test the mapping mode string against each permitted value in turn,
   setting the returned arguments accordingly. */

/* ...Read access. */
   if( astChrMatch( mmod, "READ" ) ) {
      ndf1Ccpy( "READ", mode, mode_length, status );
      star_strlcpy( inopt, " ", inopt_length );

   } else if( astChrMatch( mmod, "READ/ZERO" ) ) {
      ndf1Ccpy( "READ", mode, mode_length, status );
      ndf1Ccpy( "ZERO", inopt, inopt_length, status );

   } else if( astChrMatch( mmod, "READ/BAD" ) ) {
      ndf1Ccpy( "READ", mode, mode_length, status );
      ndf1Ccpy( "BAD", inopt, inopt_length, status );

/* ...Write access. */
   } else if( astChrMatch( mmod, "WRITE" ) ) {
      ndf1Ccpy( "WRITE", mode, mode_length, status );
      star_strlcpy( inopt, " ", inopt_length );

   } else if( astChrMatch( mmod, "WRITE/ZERO" ) ) {
      ndf1Ccpy( "WRITE", mode, mode_length, status );
      ndf1Ccpy( "ZERO", inopt, inopt_length, status );

   } else if( astChrMatch( mmod, "WRITE/BAD" ) ) {
      ndf1Ccpy( "WRITE", mode, mode_length, status );
      ndf1Ccpy( "BAD", inopt, inopt_length, status );

/* ...Update access. */
   } else if( astChrMatch( mmod, "UPDATE" ) ) {
      ndf1Ccpy( "UPDATE", mode, mode_length, status );
      star_strlcpy( inopt, " ", inopt_length );

   } else if( astChrMatch( mmod, "UPDATE/ZERO" ) ) {
      ndf1Ccpy( "UPDATE", mode, mode_length, status );
      ndf1Ccpy( "ZERO", inopt, inopt_length, status );

   } else if( astChrMatch( mmod, "UPDATE/BAD" ) ) {
      ndf1Ccpy( "UPDATE", mode, mode_length, status );
      ndf1Ccpy( "BAD", inopt, inopt_length, status );

/* If the mapping mode string was not valid, then report an error. */
   } else {
      *status = NDF__MMDIN;
      msgSetc( "BADMMODE", mmod );
      errRep( " ", "Invalid mapping mode '^BADMMODE' specified (possible "
              "programming error).", status );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Vmmd", status );

}

