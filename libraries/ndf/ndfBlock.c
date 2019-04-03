#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfBlock_( int indf1, int ndim, const hdsdim mxdim[], int iblock,
                int *indf2, int *status ){
/*
*+
*  Name:
*     ndfBlock

*  Purpose:
*     Obtain an NDF section containing a block of adjacent pixels.

*  Synopsis:
*     void ndfBlock( int indf1, int ndim, const hdsdim mxdim[], int iblock,
*                    int *indf2, int *status )

*  Description:
*     This function returns an identifier for an NDF section describing a
*     "block" of adjacent pixels selected from an initial NDF. The function
*     divides the original NDF logically into a series of such blocks, each
*     of which does not exceed a specified maximum number of pixels in each
*     dimension. The function's "iblock" parameter allows one of these
*     blocks to be selected; an NDF section for it is then returned.

*  Parameters:
*     indf1
*        Identifier for the initial NDF.
*     ndim
*        Number of maximum dimension sizes.
*     mxdim
*        Array specifying the maximum size of a block in pixels along each
*        dimension.
*     iblock
*        Number of the block required (the first block is numbered 1).
*     *indf2
*        Returned holding the identifier for an NDF section describing the
*        block.
*     *status
*        The global status.

*  Notes:
*     -  This function is intended to allow NDFs to be processed in smaller
*     pieces by selecting successive blocks, each of which may then be
*     processed individually. Note that in general not all the blocks
*     selected from an NDF will have the same shape or size, although none
*     will exceed the specified maximum number of pixels in each dimension.
*     -  Corresponding blocks selected from different NDFs (or NDF
*     sections) with identical shapes will themselves have identical shapes
*     and will contain the same number of pixels.
*     -  All NDF sections obtained via this function have the same number
*     of dimensions as the input NDF. If the number of maximum dimension
*     sizes supplied ("ndim") is less than this number, then a value of 1
*     will be used for the extra dimension sizes. If the value of "ndim" is
*     larger than this number, then the excess dimension sizes will be
*     ignored.
*     -  If the number of the requested block ("iblock") exceeds the number
*     of blocks available in the NDF, then a value of NDF__NOID will be
*     returned for the "indf2" parameter (but no error will result). This
*     condition may be used to terminate a loop when all available blocks
*     have been processed. The ndfNbloc function may also be used to
*     determine the number of blocks available.
*     -  If this function is called with "status" set, then a value of
*     NDF__NOID will be returned for the "indf2" parameter, although no
*     further processing will occur. The same value will also be returned
*     if the function should fail for any reason.
*     -  The NDF__NOID constant is defined in the header file "ndf.h".

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
   NdfACB *acb1;         /* Pointer to input NDF in the ACB */
   NdfACB *acb2;         /* Pointer to output NDF in the ACB */
   hdsdim dim;           /* Input NDF dimension size */
   hdsdim lbnd1[ NDF__MXDIM ];     /* Lower bounds of input NDF */
   hdsdim lbnd2[ NDF__MXDIM ];     /* Lower bounds of output section */
   hdsdim ubnd1[ NDF__MXDIM ];     /* Upper bounds of input NDF */
   hdsdim ubnd2[ NDF__MXDIM ];     /* Upper bounds of output section */
   int blks;             /* Number of blocks in input NDF */
   int bstrid[ NDF__MXDIM ];       /* Dimension stride in blocks */
   int dimx[ NDF__MXDIM ];         /* Maximum size of each block dimension */
   int i;                /* Loop counter for NDF dimensions */
   int ib;               /* Block offset into dimension */
   int iblk;             /* Block index within a dimension */
   int nblk;             /* Number of blocks within a dimension */
   int ndim1;            /* Number of input NDF dimensions */

/* Set an initial null value for the "indf2" parameter. */
   *indf2 = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the input identifier. */
   ndf1Impid( indf1, &acb1, status );
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

/* Check that the block index value is valid. Report an error if it is
   not. */
      if( *status == SAI__OK ) {
         if( iblock < 1 ) {
            *status = NDF__IBLIN;
            msgSeti( "IBLOCK", iblock );
            errRep( " ", "Block index value (^IBLOCK) is invalid (possible "
                    "programming error).", status );
         }
      }

/* Obtain the bounds of the input NDF from its main data array
   identifier. */
      if( *status == SAI__OK ) {
         aryBound( acb1->did, NDF__MXDIM, lbnd1, ubnd1, &ndim1, status );
         if( *status == SAI__OK ) {

/* Loop to determine how many blocks will fit into each dimension of
   the input NDF. */
            blks = 1;
            for( i = 0; i < ndim1; i++ ){

/* Store the stride of each dimension expressed in blocks (this is the
   amount by which the block index must be incremented due to all the
   blocks lying in lower dimensions when the pixel index in this
   dimension is incremented by one). */
               bstrid[ i ] = blks;

/* Obtain the input dimension size. */
               dim = ubnd1[ i ] - lbnd1[ i ] + 1;

/* Obtain the maximum block size in this dimension, restricting it so
   as not to exceed the actual dimension size. Pad with 1's if the NDF
   has more dimensions than the requested block. */
               if( i < ndim ) {
                  dimx[ i ] = NDF_MIN( mxdim[ i ], dim );
               } else {
                  dimx[ i ] = 1;
               }

/* Calculate how many blocks (or partial blocks) fit into this
   dimension. */
               nblk = ( dim - 1 )/dimx[ i ] + 1;

/* Accumulate the total number of blocks (or partial blocks) in the
   input NDF. */
               blks *= nblk;
            }

/* If the block index does not exceed the number of blocks available,
   then calculate the required block bounds. */
            if( iblock <= blks ) {

/* Initialise the block offset for the current dimension and loop to
   handle each NDF dimension, starting with the highest. */
               ib = iblock;
               for( i = ndim1 - 1; i >= 0; i-- ){

/* Find the (zero based) block index in the current dimension by
   dividing the (vectorised) block offset by the dimension stride
   expressed in blocks. */
                  iblk = ( ib - 1 )/bstrid[ i ];

/* Convert the block index into dimension bounds, ensuring that they
   lie within the bounds of the input NDF. */
                  lbnd2[ i ] = lbnd1[ i ] + iblk*dimx[ i ];
                  ubnd2[ i ] = NDF_MIN( lbnd2[ i ] + dimx[ i ] - 1, ubnd1[ i ] );

/* Decrement the (vectorised) block offset to obtain the offset into
   the next lower dimension. */
                  ib -= iblk*bstrid[ i ];
               }

/* Cut the required section from the input NDF and export an identifier
   for it */
               ndf1Cut( acb1, ndim1, lbnd2, ubnd2, &acb2, status );
               *indf2 = ndf1Expid( ( NdfObject * ) acb2, status );
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfBlock: Error obtaining a block of adjacent pixels "
              "from an NDF.", status );
      ndf1Trace( "ndfBlock", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

