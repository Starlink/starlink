#include <stdlib.h>
#include <string.h>
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "mers.h"
#include <string.h>

void ndf1Vump( NdfACB *acb, int *status ){
/*
*+
*  Name:
*     ndf1Vump

*  Purpose:
*     Unmap the variance component of an NDF.

*  Synopsis:
*     void ndf1Vump( NdfACB *acb, int *status )

*  Description:
*     This function unmaps the variance component of an NDF, which has
*     previously been mapped by ndf1Vmap. If write or update access is in
*     effect, then the mapped values are written back to the array (with
*     back-conversion from standard deviations to variance values if
*     necessary). The NDF is identified by its ACB entry.

*  Parameters:
*     acb
*        Pointer to the NDF's ACB entry.
*     *status
*        The global status.

*  Notes:
*     -  This function will attempt to execute even if "status" is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances.

*  Prior Requirements:
*     -  The variance array must previously have been mapped by a call to
*     ndf1Vmap. An error will be reported if this is not the case.

*  Side Effects:
*     -  The bad pixel flag for the array may be altered if data conversion
*     errors occur during the process of writing values back to the
*     variance array.

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
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   int bstat;            /* Setting bad pixel flag error status */
   int ddce;             /* Non-imaginary conversion error? */
   int icstat;           /* Data conversion error status */
   int idce;             /* Imaginary conversion error */
   int temp;             /* Whether a temporary array is mapped */
   int tstat;            /* Temporary status variable */
   size_t el;            /* Number of mapped values */

/* Save the "status" value and mark the error stack. */
   tstat = *status;
   errMark();

/* Obtain an index to the data object entry in the DCB. */
   *status = SAI__OK;
   dcb = acb->dcb;

/* If the variance array for the specified ACB entry is not mapped, then
   report an error. */
   if( !acb->vmap ) {
      *status = NDF__NTMAP;
      ndf1Amsg( "NDF", acb );
      errRep( " ", "The variance component in the NDF structure ^NDF is "
              "not mapped for access through the specified identifier "
              "(possible programming error).", status );

/* See if the temporary mapped array identifier in the ACB is valid. */
   } else {
      temp = aryValid( acb->vmtid, status );
      if( *status == SAI__OK ) {

/* If so, then a temporary array was mapped for read access and can
   simply be annulled. */
         if( temp ) {
            aryAnnul( &acb->vmtid, status );

/* If read access was in effect, but did not involve a temporary array,
   then simply unmap the ACB's variance array. */
         } else {
            if( !strcmp( acb->vmmod, "READ" ) ) {
               aryUnmap( acb->vid, status );

/* If the access mode requires values to be written back to the variance
   array, then see if conversion to standard deviations was specified
   when the array was mapped. */
            } else if( ( !strcmp( acb->vmmod, "WRITE" ) ) || ( !strcmp( acb->vmmod, "UPDATE" ) ) ) {
               if( acb->vmstd ) {

/* If so, then back-conversion is now required. Determine the number of
   array elements to convert from the size of the data array. */
                  ddce = 0;
                  idce = 0;
                  arySize( acb->did, &el, status );

/* Convert the array's non-imaginary values to variances, noting if any
   conversion errors occur. */
                  ndf1S2v( acb->vmbad, acb->vmtyp, el, acb->vmdpt, &ddce,
                           status );

/* If access to complex values is in effect, then similarly convert the
   imaginary values. Ensure that this second conversion executes even
   if "negative standard deviation" errors (NDF__NGSTD) occurred during
   the first conversion, so that the results returned to the array are
   still usable. */
                  if( acb->vmcpx ) {
                     if( ( *status == SAI__OK ) || ( *status == NDF__NGSTD ) ) {
                        icstat = SAI__OK;
                        ndf1S2v( acb->vmbad, acb->vmtyp, el, acb->vmipt,
                                 &idce, &icstat );

/* Update "status" if an error occurred during the second conversion. */
                        if( icstat != SAI__OK ) *status = icstat;
                     }
                  }

/* If data conversion errors occurred, then ensure that the bad pixel
   flag for the mapped values is set to non-zero and note it has been
   modified. */
                  if( ddce || idce ) {
                     acb->vmbad = 1;
                     acb->vmbmd = 1;
                  }
               }

/* If the bad pixel flag for the mapped values has been modified, then
   set the new value for the mapped array before it is unmapped,
   ensuring that this is done even if "status" is set to NDF__NGSTD. */
               if( ( *status == SAI__OK ) || ( *status == NDF__NGSTD ) ) {
                  if( acb->vmbmd ) {
                     bstat = SAI__OK;
                     arySbad( acb->vmbad, acb->vid, &bstat );

/* Update "status" if an error occurred while setting the bad pixel flag. */
                     if( bstat != SAI__OK ) *status = bstat;
                  }
               }

/* Unmap the array. */
               aryUnmap( acb->vid, status );
            }
         }
      }

/* If no error occurred, or the only error was a "negative standard
   deviation" error, then note that the array is no longer mapped and
   decrement the DCB mapping counts. */
      if( ( *status == SAI__OK ) || ( *status == NDF__NGSTD ) ) {
         acb->vmap = 0;
         dcb->nvmap--;
         dcb->nmap--;

/* Reset the pointers to the mapped values. */
         acb->vmdpt = 0;
         acb->vmipt = 0;
      }
   }

/* Annul any error if "status" was previously bad, otherwise let the new
   error report stand. */
   if( *status != SAI__OK ) {
      if( tstat != SAI__OK ) {
         errAnnul( status );
         *status = tstat;

/* Call the error tracing function if appropriate. */
      } else {
         ndf1Trace( "ndf1Vump", status );
      }
   } else {
      *status = tstat;
   }

/* Release error stack. */
   errRlse();

}

