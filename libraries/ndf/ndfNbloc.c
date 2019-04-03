#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfNbloc_( int indf, int ndim, const hdsdim mxdim[], int *nblock, int *status ){
/*
*+
*  Name:
*     ndfNbloc

*  Purpose:
*     Determine the number of blocks of adjacent pixels in an NDF.

*  Synopsis:
*     void ndfNbloc( int indf, int ndim, const hdsdim mxdim[], int *nblock,
*                    int *status )

*  Description:
*     This function determines the number of "blocks" (i.e. sections) of
*     adjacent pixels that can be obtained from an NDF, subject to the
*     constraint that no block should exceed a specified maximum number of
*     pixels in any dimension. More specifically, given the maximum size in
*     pixels of a block in each dimension ("mxdim"), this function returns
*     the maximum value which can be supplied for the IBLOCK parameter of
*     the function ndfBlock if a valid NDF identifier for a block of
*     adjacent pixels is to be returned.

*  Parameters:
*     indf
*        NDF identifier.
*     ndim
*        Number of maximum dimension sizes.
*     mxdim
*        Array specifying the maximum size of a block in pixels along each
*        dimension.
*     *nblock
*        Returned holding the number of blocks which can be obtained from
*        the NDF.
*     *status
*        The global status.

*  Notes:
*     -  This function is provided to calculate an upper bound on the
*     number of blocks for DO-loops which process NDFs by dividing them
*     into separate blocks by means of calls to the function ndfBlock.
*     -  If the number of maximum dimension sizes supplied ("ndim") is less
*     than the number of NDF dimensions, then a value of 1 will be used for
*     the extra dimension sizes. If the value of "ndim" is larger than this
*     number, then the excess dimension sizes will be ignored.
*     -  A value of zero will be returned for the "nblock" parameter if
*     this function is called with "status" set. The same value will also
*     be returned if the function should fail for any reason.

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
   NdfACB *acb;          /* Pointer to NDF in the ACB */
   hdsdim dim[ NDF__MXDIM ];       /* NDF dimension sizes */
   int dimx;             /* Maximum size of block dimension */
   int i;                /* Loop counter for NDF dimensions */
   int nblk;             /* Number of blocks within a dimension */
   int ndim1;            /* Number of NDF dimensions */

/* Set an initial default value for the "nblock" parameter. */
   *nblock = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Check that the number of block dimensions is valid. Report an error
   if it is not. */
      if( ( ndim < 1 ) || ( ndim > NDF__MXDIM ) ) {
         *status = NDF__NDMIN;
         msgSeti( "NDIM", ndim );
         msgSeti( "MXDIM", NDF__MXDIM );
         errRep( " ", "Invalid number of block dimensions (^NDIM) "
                 "specified; should be in the range 1 to ^MXDIM (possible "
                 "programming error).", status );

/* Check the maximum size specified for each block dimension. Report an
   error if these values are not all positive. */
      } else {
         for( i = 0; i < ndim; i++ ){
            if( mxdim[ i ] < 1 ) {
               *status = NDF__DIMIN;
               msgSeti( "I", i + 1 );
               msgSeti( "DIM", mxdim[ i ] );
               errRep( " ", "Maximum block size for dimension ^I has an "
                       "invalid value of ^DIM; its value should be "
                       "positive (possible programming error).", status );
               break;
            }
         }
      }

/* Obtain the bounds of the NDF from its main data array identifier. */
      if( *status == SAI__OK ) {
         aryDim( acb->did, NDF__MXDIM, dim, &ndim1, status );
         if( *status == SAI__OK ) {

/* Loop to determine how many blocks will fit into each dimension of
   the input NDF. */
            *nblock = 1;
            for( i = 0; i < ndim1; i++ ){

/* Obtain the maximum block size in this dimension, restricting it so
   as not to exceed the actual dimension size. Pad with 1's if the NDF
   has more dimensions than the requested block. */
               if( i < ndim ) {
                  dimx = NDF_MIN( mxdim[ i ], dim[ i ] );
               } else {
                  dimx = 1;
               }

/* Calculate how many blocks (or partial blocks) fit into this
   dimension. */
               nblk = ( dim[ i ] - 1 )/dimx + 1;

/* Accumulate the total number of blocks (or partial blocks) in the
   NDF. */
               *nblock *= nblk;
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfNbloc: Error determining the number of blocks of "
              "adjacent pixels in an NDF.", status );
      ndf1Trace( "ndfNbloc", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

