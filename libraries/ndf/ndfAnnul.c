#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfAnnul_( int *indf, int *status ){
/*
*+
*  Name:
*     ndfAnnul

*  Purpose:
*     Annul an NDF identifier.

*  Synopsis:
*     void ndfAnnul( int *indf, int *status )

*  Description:
*     This function annuls the NDF identifier supplied so that it is no
*     longer recognised as a valid identifier by the NDF_ functions. Any
*     resources associated with it are released and made available for re-
*     use. If any NDF components are mapped for access, then they are
*     automatically unmapped by this function.

*  Parameters:
*     *indf
*        The NDF identifier to be annulled. A value of NDF__NOID is
*        returned (as defined in the header file "ndf.h").
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances. In particular, it will
*     fail if the identifier supplied is not initially valid, but this will
*     only be reported if "status" is set to SAI__OK on entry.
*     -  An error will result if an attempt is made to annul the last
*     remaining identifier associated with an NDF whose DATA component has
*     not been defined (unless it is a temporary NDF, in which case it will
*     be deleted at this point).

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
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Log any pending error message information for subsequent recording
   in NDF history records. */
   ndf1Hlerr( status );

/* Begin a new error reporting context. */
   errBegin( status );

/* Import the NDF identifier. */
   ndf1Impid( *indf, &acb, status );

/* Annul the associated ACB entry and reset the NDF identifier value. */
   if( *status == SAI__OK ) ndf1Anl( &acb, status );
   *indf = NDF__NOID;

/*If an error occurred, report context information and call the error
  tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAnnul: Error annulling an NDF identifier.", status );
      ndf1Trace( "ndfAnnul", status );
   }

/* End the error reporting context. */
   errEnd( status );

/* Restablish the original AST status pointer */
   NDF_FINAL

}

