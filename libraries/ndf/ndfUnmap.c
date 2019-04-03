#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfUnmap_( int indf, const char *comp, int *status ){
/*
*+
*  Name:
*     ndfUnmap

*  Purpose:
*     Unmap an NDF or a mapped NDF array.

*  Synopsis:
*     void ndfUnmap( int indf, const char *comp, int *status )

*  Description:
*     This function unmaps an NDF, or an individual NDF array which has
*     previously been mapped for READ, UPDATE or WRITE access.

*  Parameters:
*     indf
*        NDF identifier.
*     comp
*        Pointer to a null terminated string holding the name of the NDF
*        component to be unmapped: "AXIS", "DATA", "QUALITY", "VARIANCE" or
*        "*". The last value acts as a wild card, causing all mapped arrays
*        to be unmapped.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.
*     -  A component name of "AXIS" will act as a partial wild card,
*     unmapping any axis arrays which are mapped, but leaving other
*     components unchanged. The function ndfAunmp may be used to unmap
*     individual axis arrays.
*     -  A comma-separated list of component names may also be given, in
*     which case each component will be unmapped in turn.
*     -  An error will be reported if a component has not previously been
*     mapped for access, except in the case where a value of "*" is given
*     for "comp", or where "AXIS" is used to unmap axis arrays.

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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   int tstat;            /* Temporary status variable */

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Import the NDF identifier and unmap its component(s). */
   *status = SAI__OK;
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) ndf1Ump( acb, comp, status );

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Report context information and call error tracing function if
   appropriate. */
      } else {
         errRep( " ", "ndfUnmap: Error unmapping an NDF or an array "
                 "component of an NDF.", status );
         ndf1Trace( "ndfUnmap", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

/* Restablish the original AST status pointer */
   NDF_FINAL

}

