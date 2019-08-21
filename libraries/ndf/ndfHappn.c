#include <stdlib.h>
#include "sae_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "ndf_ast.h"
#include "mers.h"
#include "star/util.h"

void ndfHappn_( const char *appn, int *status ){
/*
*+
*  Name:
*     ndfHappn

*  Purpose:
*     Declare a new application name for NDF history recording.

*  Synopsis:
*     void ndfHappn( const char *appn, int *status )

*  Description:
*     This function declares a new application name to be used as the
*     default for subsequent recording of NDF history information. The name
*     supplied will subsequently be used when creating new history records
*     whenever a blank application name is passed to a function which
*     writes new history information. It will also be used as the
*     application name when recording default history information.
*
*     If this function is not called, then a system-supplied default name
*     will be used in its place.

*  Parameters:
*     appn
*        Pointer to a null terminated string holding the name of the new
*        application. If a blank value is supplied, then the name will
*        revert to the system-supplied default.
*     *status
*        The global status.

*  Notes:
*     -  This function should normally only be called to set up an
*     application name in cases where better information is available than
*     is provided by the default. For example, writers of environment-level
*     software may be able to include a software version number in the name
*     so that individual applications need not duplicate this in their own
*     calls to NDF history functions.
*     -  The maximum number of application name characters which can be
*     stored by this function is given by the constant NDF__SZAPP.  The
*     name supplied will be truncated without error if more then this
*     number of characters are supplied. The NDF__SZAPP constant is defined
*     in the header file "ndf.h".

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

/* Local Variables: */
   size_t f;             /* First non-blank character position */
   size_t l;             /* Last non-blank character position */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Find the first and last non-blank characters in the new application
   name and store the name, left-justified in the DCB. */
   astFandl( appn, 1, 0, &f, &l );
   if( f < l + 1 ) {
      star_strlcpy( Ndf_DCB_happn, appn + f, sizeof( Ndf_DCB_happn ));
      Ndf_DCB_happn[ l + 1 - f ] = 0;

/* Use a blank name if necessary. */
   } else {
      star_strlcpy( Ndf_DCB_happn, " ", sizeof( Ndf_DCB_happn ));
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfHappn: Error declaring a new application name for "
              "NDF history recording.", status );
      ndf1Trace( "ndfHappn", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

