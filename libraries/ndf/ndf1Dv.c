#include "sae_par.h"
#include "star/util.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"
#include "star/util.h"

void ndf1Dv( NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dv

*  Purpose:
*     Ensure that variance information is available in the DCB.

*  Synopsis:
*     void ndf1Dv( NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information about a data object"s VARIANCE
*     component is available in the DCB. It does nothing if this
*     information is already available. Otherwise, it obtains this
*     information by inspecting the actual data object, performing
*     necessary validation checks in the process.

*  Parameters:
*     dcb
*        Pointer to the DCB entry for which variance information is
*        required.
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

/* Local Variables: */
   hdsbool_t there;      /* Whether variance component exists */
   hdsdim lbndd[ NDF__MXDIM ];     /* Data component lower bounds */
   hdsdim lbndv[ NDF__MXDIM ];     /* Variance component lower bounds */
   hdsdim ubndd[ NDF__MXDIM ];     /* Data component upper bounds */
   hdsdim ubndv[ NDF__MXDIM ];     /* Variance component upper bounds */
   int i;                /* Loop counter for dimensions */
   int ndimd;            /* Number of data component dimensions */
   int ndimv;            /* Number of variance dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* See if variance information is already available. There is nothing to
   do if it is. */
   if( !dcb->kv ) {

/* Ensure that information about the data array is available in the DCB. */
      ndf1Dd( dcb, status );

/* See if the VARIANCE component is present. If not, then signify this
   by storing the NULL value in the DCB. */
      datThere( dcb->loc, "VARIANCE", &there, status );
      if( *status == SAI__OK ) {
         if( !there ) {
            dcb->vid = NULL;

/* If it is present, then import the VARIANCE component into the ARY_
   system, storing the resulting identifier in the DCB. */
         } else {
            aryFind( dcb->loc, "VARIANCE", &dcb->vid, status );

/* Obtain the number of dimensions and the pixel index bounds of the
   NDF's data array and variance components. */
            aryBound( dcb->did, NDF__MXDIM, lbndd, ubndd, &ndimd, status );
            aryBound( dcb->vid, NDF__MXDIM, lbndv, ubndv, &ndimv, status );
            if( *status == SAI__OK ) {

/* Report an error if the number of variance dimensions does not match
   that of the data array. */
               if( ndimv != ndimd ) {
                  *status = NDF__NDMIN;
                  ndf1Dmsg( "NDF", dcb );
                  msgSeti( "BADNDIM", ndimv );
                  msgSeti( "NDIM", ndimd );
                  errRep( " ", "The VARIANCE array in the NDF structure "
                          "^NDF has an invalid number of dimensions "
                          "(^BADNDIM); it should be ^NDIM-dimensional.",
                          status );

/* Check that the variance pixel index bounds in each dimension match
   those of the data array. */
               } else {
                  for( i = 0; i < ndimd; i++ ){
                     if( ( lbndv[ i ] != lbndd[ i ] ) || ( ubndv[ i ] != ubndd[ i ] ) ) {

/* Report an error if a discrepancy is found. */
                        *status = NDF__BNDIN;
                        msgSeti( "DIM", i + 1 );
                        ndf1Dmsg( "NDF", dcb );
                        errRep( " ", "The pixel-index bounds of dimension "
                                "^DIM of the VARIANCE array in the NDF "
                                "structure ^NDF do not match those of the "
                                "NDF's DATA_ARRAY component.", status );
                        break;
                     }
                  }
               }
            }
         }

/* Set the default attributes of the variance component using the values
   initially derived from the data array component. */
         star_strlcpy( dcb->vtyp, dcb->detyp, sizeof( dcb->vtyp ) );
         dcb->vcpx = dcb->decpx;
         star_strlcpy( dcb->vfrm, dcb->defrm, sizeof( dcb->vfrm ) );

/* If the component is not suitable, then annul the associated ID. */
         if( *status != SAI__OK ) aryAnnul( &dcb->vid, status );
      }

/* Note whether variance information is now available in the DCB. */
      dcb->kv = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dv", status );

}

