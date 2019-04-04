#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ary.h"
#include "mers.h"
#include "star/util.h"

void ndf1Dad( int iax, NdfDCB *dcb, int *status ){
/*
*+
*  Name:
*     ndf1Dad

*  Purpose:
*     Ensure that information about an NDF's axis data array is available
*     in the DCB.

*  Synopsis:
*     void ndf1Dad( int iax, NdfDCB *dcb, int *status )

*  Description:
*     This function ensures that information about an NDF's axis data array
*     is available in the DCB. If this information is already available,
*     then it returns without action. Otherwise, the actual data object is
*     examined to obtain this information, and an ARY_ system identifier
*     for the axis data array is stored in the DCB. Only those checks
*     necessary to obtain and validate this information are performed.

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
*     3-APR-2019 (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/*       "Ndf_DCB_adfrm"( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM
         ) (Write)
            Storage form of axis data arrays.
         "Ndf_DCB_adid"( NDF__MXDIM, NDF__MXDCB ) = INTEGER (Write)
            ARY_ system identifiers for axis data arrays.
         "Ndf_DCB_aloc"( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( DAT__SZLOC )
         (Read)
            Locators to axis structure elements.
         "Ndf_DCB_adtyp"( NDF__MXDIM, NDF__MXDCB ) = CHARACTER * ( NDF__SZTYP
         ) (Write)
            Numeric data type of axis data arrays.
         "Ndf_DCB_defrm"( NDF__MXDCB ) = CHARACTER * ( NDF__SZFRM ) (Read)
            Default NDF array storage form.
         "Ndf_DCB_did"( NDF__MXDCB ) = INTEGER (Read)
            ARY_ system identifier for the NDF's data array.
         "Ndf_DCB_kad"( NDF__MXDIM, NDF__MXDCB ) = LOGICAL (Read and Write)
            Whether information about axis data arrays is available. */

/* Local Variables: */
   hdsbool_t there;      /* Whether axis data array exists */
   hdsdim lbnd[ NDF__MXDIM ];      /* NDF lower bounds */
   hdsdim lbndd[ NDF__MXDIM ];     /* Axis data array lower bounds */
   hdsdim ubnd[ NDF__MXDIM ];      /* NDF upper bounds */
   hdsdim ubndd[ NDF__MXDIM ];     /* Axis data array upper bounds */
   int cmplxd;           /* Whether axis data array is complex */
   int ndim;             /* Number of NDF dimensions */
   int ndimd;            /* Number of axis data array dimensions */

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Check if information about the axis data array is already available.
   There is nothing to do if it is. */
   if( !dcb->kad[ iax ] ) {

/* Ensure that information about the NDF's axis structure is available. */
      ndf1Da( dcb, status );
      if( *status == SAI__OK ) {

/* Set an initial null ARY_ system identifier in the DCB for the axis
   data array. */
         dcb->adid[ iax ] = NULL;

/* Check that the required axis structure element locator in the DCB is
   not null. If it is, then there is no axis structure, so the data
   array identifier remains null. */
         if( dcb->aloc[ iax ] ) {

/* See if the axis data array component exists. If not, then report an
   error. */
            datThere( dcb->aloc[ iax ], "DATA_ARRAY", &there, status );
            if( *status == SAI__OK ) {
               if( !there ) {
                  *status = NDF__NOAXD;
                  datMsg( "AXIS", dcb->aloc[ iax ] );
                  errRep( " ", "The DATA_ARRAY component in the NDF axis "
                          "structure ^AXIS is missing.", status );

/* Import the axis data array into the ARY_ system, storing the
   identifier in the DCB. */
               } else {
                  aryFind( dcb->aloc[ iax ], "DATA_ARRAY", dcb->adid + iax,
                           status );

/* Obtain the complex value flag and pixel index bounds of the axis
   data array and the pixel index bounds of the main NDF data array. */
                  aryCmplx( dcb->adid[ iax ], &cmplxd, status );
                  aryBound( dcb->adid[ iax ], NDF__MXDIM, lbndd, ubndd,
                            &ndimd, status );
                  aryBound( dcb->did, NDF__MXDIM, lbnd, ubnd, &ndim, status );
                  if( *status == SAI__OK ) {

/* Check that the axis data array does not hold complex values. Report
   an error if it does. */
                     if( cmplxd ) {
                        *status = NDF__TYPIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        errRep( " ", "The DATA_ARRAY array in the NDF axis "
                                "structure ^AXIS holds illegal complex "
                                "values.", status );

/* Check that the dimensionality of the axis data array is 1. Report an
   error if it is not. */
                     } else if( ndimd != 1 ) {
                        *status = NDF__NDMIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        msgSeti( "BADNDIM", ndimd );
                        errRep( " ", "The DATA_ARRAY array in the NDF axis "
                                "structure ^AXIS is ^BADNDIM-dimensional; "
                                "it should be 1-dimensional.", status );

/* Check that the lower and upper pixel index bounds of the axis data
   array match the bounds of the corresponding NDF dimension. Report an
   error if they do not. */
                     } else if( ( lbndd[ 0 ] != lbnd[ iax ] ) || ( ubndd[ 0 ] != ubnd[ iax ] ) ) {
                        *status = NDF__BNDIN;
                        datMsg( "AXIS", dcb->aloc[ iax ] );
                        msgSeti( "LBNDD", lbndd[ 0 ] );
                        msgSeti( "UBNDD", ubndd[ 0 ] );
                        msgSeti( "LBND", lbnd[ iax ] );
                        msgSeti( "UBND", ubnd[ iax ] );
                        errRep( " ", "The pixel-index bounds "
                                "(^LBNDD:^UBNDD) of the DATA_ARRAY array "
                                "in the NDF axis structure ^AXIS do not "
                                "match the bounds of the corresponding NDF "
                                "dimension (^LBND:^UBND).", status );
                     }
                  }

/* If an error occurred, then annul any ARY_ system identifier which may
   have been allocated. */
                  if( *status != SAI__OK ) aryAnnul( dcb->adid + iax, status );
               }
            }
         }
      }

/* Set the default axis data type to be _REAL and the default axis data
   storage form to match the NDF default storage form derived from the
   data array. */
      if( *status == SAI__OK ) {
         star_strlcpy( dcb->adtyp[ iax ], "_REAL", sizeof( dcb->adtyp[ iax ] ) );
         star_strlcpy( dcb->adfrm[ iax ], dcb->defrm, sizeof( dcb->adfrm[ iax ] ) );
      }

/* Note whether axis data array information is now available. */
      dcb->kad[ iax ] = ( *status == SAI__OK );
   }

/* Call error tracing function and exit. */
   if( *status != SAI__OK ) ndf1Trace( "ndf1Dad", status );

}

