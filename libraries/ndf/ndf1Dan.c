#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include <string.h>
#include "mers.h"

void ndf1Dan( int iax, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dan

*  Purpose:
*     Ensure that DCB information is available for an axis normalisation
*     flag.

*  Synopsis:
*     void ndf1Dan( int iax, NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information is available in the DCB for an
*     NDF's axis normalisation flag. If this information is already
*     available, then it returns without action. Otherwise, it inspects the
*     actual data object to obtain this information, which it then stores
*     in the DCB. Only those checks necessary to obtain and validate this
*     information are performed.

*  Parameters:
*     iax
*        Axis number for which the information is required.
*     dcb
*        Pointer to the data object entry in the DCB.
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
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   HDSLoc *loc = NULL;   /* Component locator */
   char type[ DAT__SZTYP + 1 ];    /* Component type */
   hdsbool_t there;      /* Whether component exists */
   hdsdim dim[ DAT__MXDIM ];       /* Component dimensions */
   int ndim;             /* Number of component dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See whether axis normalisation information is already available.
   There is nothing to do if it is. */
   if( !dcb->kan[ iax ] ) {

/* Set a default value of zero for the axis normalisation flag. */
      dcb->anrm[ iax ] = 0;

/* Ensure that axis structure information is available in the DCB. */
      ndf1Da( dcb, status );
      if( *status == SAI__OK ) {

/* If an axis structure exists, then see whether a NORMALISED component
   is present. */
         if( dcb->aloc[ iax ] ) {
            datThere( dcb->aloc[ iax ], "NORMALISED", &there, status );
            if( *status == SAI__OK ) {

/* If the component is present, then obtain a locator for it and
   determine its type and shape. */
               if( there ) {
                  datFind( dcb->aloc[ iax ], "NORMALISED", &loc, status );
                  datType( loc, type, status );
                  datShape( loc, DAT__MXDIM, dim, &ndim, status );
                  if( *status == SAI__OK ) {

/* Check that the type is _LOGICAL and report an error if it is not. */
                     if( strcmp( type, "_LOGICAL" ) ) {
                        *status = NDF__TYPIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        msgSetc( "BADTYPE", type );
                        errRep( " ", "The NORMALISED component in the NDF "
                                "axis structure ^AXIS has an invalid type "
                                "of '^BADTYPE'; it should be of type "
                                "'_LOGICAL'.", status );

/* Check that it is a scalar object and report an error if it is not. */
                     } else if( ndim != 0 ) {
                        *status = NDF__NDMIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        msgSeti( "BADNDIM", ndim );
                        errRep( " ", "The NORMALISED component in the NDF "
                                "axis structure ^AXIS is "
                                "^BADNDIM-dimensional; it should be "
                                "scalar.", status );
                     }
                  }

/* Obtain the logical value of the component. */
                  datGet0L( loc, &(dcb->anrm[ iax ]), status );

/* Annul the component locator. */
                  datAnnul( &loc, status );
               }
            }
         }
      }

/* Note whether axis normalisation information is now available. */
      dcb->kan[ iax ] = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dan", status );

}
