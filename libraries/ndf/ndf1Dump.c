#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"

void ndf1Dump( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Dump

*  Purpose:
*     Unmap the data array component for an ACB entry.

*  Synopsis:
*     void ndf1Dump( NdfACB *acb, int *status )

*  Description:
*     This function unmaps the data array component for an ACB entry which
*     has previously been mapped for access. An error is reported if it has
*     not previously been mapped.

*  Parameters:
*     acb
*        Pointer to the ACB entry whose data array is to be unmapped.
*     *status
*        The global status.

*  Notes:
*     -  This function attempts to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

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

/*       "Ndf_DCB_ndmap"( NDF__MXDCB ) = INTEGER (Read and Write)
            Number of current mappings for the NDF's data array.
         "Ndf_DCB_nmap"( NDF__MXDCB ) = INTEGER (Read and Write)
            Total number of current mappings for the NDF. */

/*       "Ndf_ACB_did"( NDF__MXACB ) = INTEGER (Read)
            ARY_ system identifier for the NDF's data array.
         "Ndf_ACB_dmap"( NDF__MXACB ) = LOGICAL (Read and Write)
            Whether the NDF's data array is mapped for access.
         "Ndf_ACB_dmbad"( NDF__MXACB ) = LOGICAL (Read)
            Bad pixel flag for the mapped data values.
         "Ndf_ACB_dmbmd"( NDF__MXACB ) = LOGICAL (Read)
            Whether the "Ndf_ACB_dmbad" value has been modified.
         "Ndf_ACB_dmdpt"( NDF__MXACB ) = INTEGER (Write)
            Pointer to the mapped non-imaginary data values.
         "Ndf_ACB_dmipt"( NDF__MXACB ) = INTEGER (Write)
            Pointer to the mapped imaginary data values.
         "Ndf_ACB_dmtid"( NDF__MXACB ) = INTEGER (Read and Write)
            ARY_ system identifier for temporary array used when mapping
            the NDF's data component.
         "Ndf_ACB_idcb"( NDF__MXACB ) = INTEGER (Read)
            Index to data object entry in the DCB. */

/* Local Variables: */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int temp;             /* Whether a temporary array is mapped */
   int tstat;            /* Temporary status variable */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Obtain an index to the data object entry in the DCB. */
   *status = SAI__OK;
   dcb = acb->dcb;

/* Check that the data component is mapped for access and report an
   error if it is not. */
   if( !acb->dmap ) {
      *status = NDF__NTMAP;
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The data component in the NDF structure ^NDF is not "
              "mapped for access through the specified identifier "
              "(possible programming error).", status );

/* See if the temporary mapped array identifier in the ACB is valid. */
   } else {
      temp = aryValid( acb->dmtid, status );
      if( *status == SAI__OK ) {

/* If so, then a temporary array was mapped for read access and can
   simply be annulled. */
         if( temp ) {
            aryAnnul( &acb->dmtid, status );

/* If the bad pixel flag value for the mapped values has been modified,
   then set the new value for the data array before unmapping it. */
         } else {
            if( acb->dmbmd ) arySbad( acb->dmbad, acb->did, status );

/* Unmap the data array. */
            aryUnmap( acb->did, status );
         }
      }

/* If successful, then note the array is not mapped and decrement the
   DCB mapping counts for the NDF. */
      if( *status == SAI__OK ) {
         acb->dmap = 0;
         dcb->ndmap--;
         dcb->nmap--;

/* Clear the pointers to the mapped values. */
         acb->dmdpt = 0;
         acb->dmipt = 0;
      }
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Dump", status );
      }
   } else {
      *status = tstat;
   }

/* Release the error stack. */
   errRlse();

}

