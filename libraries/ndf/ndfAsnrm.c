#include "star/hds.h"
#include "star/cmp.h"
#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf.h"
#include "mers.h"

void ndfAsnrm_( int norm, int indf, int iaxis, int *status ){
/*
*+
*  Name:
*     ndfAsnrm

*  Purpose:
*     Set a new logical value for an NDF axis normalisation flag.

*  Synopsis:
*     void ndfAsnrm( int norm, int indf, int iaxis, int *status )

*  Description:
*     This function sets a new logical value for the normalisation flag
*     associated with an NDF axis.

*  Parameters:
*     norm
*        Normalisation flag value to be set.
*     indf
*        NDF identifier.
*     iaxis
*        Number of the NDF axis whose normalisation flag value is to be
*        set.
*     *status
*        The global status.

*  Notes:
*     -  A value of zero may be supplied for the "iaxis" component, in
*     which case the function will set the same normalisation flag value
*     for all the NDF's axes.
*     -  This function may only be used to set an axis normalisation flag
*     value for a base NDF. If an NDF section is supplied, then it will
*     return without action. No error will result.

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
   NdfACB *acb;          /* Pointer to the NDF entry in the ACB */
   NdfDCB *dcb;          /* Pointer to data object entry in the DCB */
   hdsbool_t there;      /* Whether component exists */
   int iax;              /* Loop counter for axes */
   int iax1;             /* First axis to process */
   int iax2;             /* Last axis to process */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );

/* Check the axis number for validity. */
   ndf1Van( acb, iaxis, 1, &iax1, &iax2, status );

/* Check that write access to the NDF is available. */
   ndf1Chacc( acb, "WRITE", status );
   if( *status == SAI__OK ) {

/* Check that the NDF is not a section. Return without action if it is. */
      if( !acb->cut ) {

/* Obtain an index to the data object entry in the DCB. */
         dcb = acb->dcb;

/* Loop to process each relevant axis. */
         for( iax = iax1; iax <= iax2; iax++ ){

/* Ensure that axis normalisation flag information is available in the
   DCB. */
            ndf1Dan( iax, dcb, status );
            if( *status == SAI__OK ) {

/* See if the new flag value differs from its current value. If not,
   then there is nothing to do. */
               if( norm != dcb->anrm[ iax ] ) {

/* If an axis structure exists, then see if it contains a NORMALISED
   component. */
                  if( dcb->aloc[ iax ] ) {
                     datThere( dcb->aloc[ iax ], "NORMALISED", &there, status );
                     if( *status == SAI__OK ) {

/* If the new normalisation value is non-zero, then create a NORMALISED
   component if it does not already exist and write a non-zero value to
   it. */
                        if( norm ) {
                           if( !there ) datNew0L( dcb->aloc[ iax ], "NORMALISED",
                                                  status );
                           cmpPut0L( dcb->aloc[ iax ], "NORMALISED", norm,
                                     status );

/* If the normalisation value is zero, then erase any NORMALISED
   component which may exist. */
                        } else {
                           if( there ) datErase( dcb->aloc[ iax ], "NORMALISED",
                                                 status );
                        }
                     }
                  }

/* Store the new normalisation value in the DCB and note whether this
   DCB value is now up to date. */
                  dcb->anrm[ iax ] = norm;
                  dcb->kan[ iax ] = ( *status == SAI__OK );
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfAsnrm: Error setting a new logical value for an NDF "
              "axis normalisation flag.", status );
      ndf1Trace( "ndfAsnrm", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

