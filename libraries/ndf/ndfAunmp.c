#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfAunmp_( int indf, const char *comp, int iaxis, int *status ){
/*
*+
*  Name:
*     ndfAunmp

*  Purpose:
*     Unmap an NDF axis array.

*  Synopsis:
*     void ndfAunmp( int indf, const char *comp, int iaxis, int *status )

*  Description:
*     This function unmaps an NDF axis array which has previously been
*     mapped for READ, UPDATE or WRITE access.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the axis
*        array component to be unmapped: "CENTRE", "VARIANCE", "WIDTH" or
*        "*". The last value acts as a wild card, causing all mapped axis
*        components to be unmapped.
*     iaxis
*        Number of the NDF axis whose array is to be unmapped.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  A comma-separated list of axis component names may also be given,
*     in which case each component will be unmapped in turn.
*     -  A value of zero may be supplied for the "iaxis" parameter, in
*     which case the function will unmap the specified component(s) for all
*     the NDF's axes.
*     -  An error will be reported if a component has not previously been
*     mapped for access, except in cases where a wild card unmapping
*     operation is specified (either with a component name of "*" or an
*     axis number of zero).

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   int tstat;            /* Temporary status variable */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Import the NDF identifier. */
   *status = SAI__OK;
   ndf1Impid( indf, &acb, status );

/* Unmap the NDF axis component(s). */
   ndf1Aump( iaxis, acb, comp, status );

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Report context information and call error tracing function if
   appropriate. */
      } else {
         errRep( " ", "ndfAunmp: Error unmapping an NDF axis array.", status );
         ndf1Trace( "ndfAunmp", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

