#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"
#include "star/util.h"

void ndf1Daw( int iax, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Daw

*  Purpose:
*     Ensure that information about an NDF's axis width array is available
*     in the DCB.

*  Synopsis:
*     void ndf1Daw( int iax, NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information about an NDF's axis width
*     array is available in the DCB. If this information is already
*     available, then it returns without action. Otherwise, the actual data
*     object is examined to obtain this information, and an ARY_ system
*     identifier for the axis width array is stored in the DCB. Only those
*     checks necessary to obtain and validate this information are
*     performed.

*  Parameters:
*     iax
*        Zero-based index of the NDF axis for which information is
*        required.
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

/*       "Ndf_DCB_aloc"( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
         (Read)
            Locators to axis structure elements.
         "Ndf_DCB_awid"( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
            ARY_ system identifiers for axis width arrays.
         "Ndf_DCB_awfrm"( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
         ) (Write)
            Storage form of axis width arrays.
         "Ndf_DCB_awtyp"( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
         ) (Write)
            Numeric data type of axis width arrays.
         "Ndf_DCB_defrm"( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
            Default NDF array storage form.
         "Ndf_DCB_did"( NDF__MXDCB ) = INTEGER (Read)
            ARY_ system identifier for the NDF's data array.
         "Ndf_DCB_kaw"( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
            Whether information about axis width arrays is available. */

/* Local Variables: */
   hdsbool_t there;      /* Whether width array exists */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim lbndw[ NDF__MXDIM ];     /* Axis width array lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   hdsdim ubndw[ NDF__MXDIM ];     /* Axis width array upper bounds */
   int cmplxw;           /* Whether width array is complex */
   int ndim;             /* Number of NDF dimensions */
   int ndimw;            /* Number of width array dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check if information about the axis width array is already
   available.  There is nothing to do if it is. */
   if( !dcb->kaw[ iax ] ) {

/* Ensure that information about the NDF's axis structure is available. */
      ndf1Da( dcb, status );
      if( *status == SAI__OK ) {

/* Set an initial null ARY_ system identifier in the DCB for the axis
   width array. */
         dcb->awid[ iax ] = NULL;

/* Check that the required axis structure element locator in the DCB is
   not null. If it is, then there is no axis structure, so the width
   array identifier remains null. */
         if( dcb->aloc[ iax ] ) {

/* See if the axis width array component exists. If not, then its DCB
   identifier remains null. */
            datThere( dcb->aloc[ iax ], "WIDTH", &there, status );
            if( *status == SAI__OK ) {
               if( there ) {

/* Import the axis width array into the ARY_ system, storing the
   identifier in the DCB. */
                  aryFind( dcb->aloc[ iax ], "WIDTH", dcb->awid + iax, status );

/* Obtain the complex value flag and pixel index bounds of the axis
   width array and the pixel index bounds of the main NDF data array. */
                  aryCmplx( dcb->awid[ iax ], &cmplxw, status );
                  aryBound( dcb->awid[ iax ], NDF__MXDIM, lbndw, ubndw,
                            &ndimw, status );
                  aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
                  if( *status == SAI__OK ) {

/* Check that the axis width array does not hold complex values.
   Report an error if it does. */
                     if( cmplxw ) {
                        *status = NDF__TYPIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        errRep( " ", "The WIDTH array in the NDF axis "
                                "structure ^AXIS holds illegal complex "
                                "values.", status );

/* Check that the dimensionality of the axis width array is 1.  Report
   an error if it is not. */
                     } else if( ndimw != 1 ) {
                        *status = NDF__NDMIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        msgSeti( "BADNDIM", ndimw );
                        errRep( " ", "The WIDTH array in the NDF axis "
                                "structure ^AXIS is ^BADNDIM-dimensional; "
                                "it should be 1-dimensional.", status );

/* Check that the lower and upper pixel index bounds of the axis
   width array match the bounds of the corresponding NDF dimension.
   Report an error if they do not. */
                     } else if( ( lbndw[ 0 ] != lbnd[ iax ] ) || ( ubndw[ 0 ] != ubnd[ iax ] ) ) {
                        *status = NDF__BNDIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        msgSeti( "LBNDW", lbndw[ 0 ] );
                        msgSeti( "UBNDW", ubndw[ 0 ] );
                        msgSeti( "LBND", lbnd[ iax ] );
                        msgSeti( "UBND", ubnd[ iax ] );
                        errRep( " ", "The pixel-index bounds "
                                "(^LBNDW:^UBNDW) of the WIDTH array in the "
                                "NDF axis structure ^AXIS do not match the "
                                "bounds of the corresponding NDF dimension "
                                "(^LBND:^UBND).", status );
                     }
                  }

/* If an error occurred, then annul any ARY_ system identifier which
   may have been allocated. */
                  if( *status != SAI__OK ) aryAnnul( dcb->awid + iax, status );
               }
            }
         }
      }

/* Set the default axis width type to be _REAL and the default axis
   width storage form to match the NDF default storage form derived
   from the data array. */
      if( *status == SAI__OK ) {
         star_strlcpy( dcb->awtyp[ iax ], "_REAL", sizeof( dcb->awtyp[ iax ] ) );
         star_strlcpy( dcb->awfrm[ iax ], dcb->defrm, sizeof( dcb->awfrm[ iax ] ) );
      }

/* Note whether axis width array information is now available. */
      dcb->kaw[ iax ] = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Daw", status );

}

