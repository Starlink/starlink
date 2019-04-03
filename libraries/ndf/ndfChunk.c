#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfChunk_( int indf1, int mxpix, int ichunk, int *indf2, int *status ){
/*
*+
*  Name:
*     ndfChunk

*  Purpose:
*     Obtain an NDF section containing a chunk of contiguous pixels.

*  Synopsis:
*     void ndfChunk( int indf1, int mxpix, int ichunk, int *indf2, int *status )

*  Description:
*     This function returns an identifier for an NDF section describing a
*     "chunk" of contiguous pixels selected from an initial NDF.  The
*     function divides the initial NDF logically into a series of such
*     chunks, each of which follows immediately on from the previous chunk,
*     and each of which contains no more than a specified maximum number
*     ("mxpix") of contiguous pixels. The function's "ichunk" parameter
*     allows one of these chunks to be selected; an NDF section for it is
*     then returned.

*  Parameters:
*     indf1
*        Identifier for the initial NDF.
*     mxpix
*        Maximum number of contiguous pixels required in each chunk.
*     ichunk
*        Number of the chunk required (the first chunk is numbered 1).
*     *indf2
*        Returned holding the identifier for an NDF section describing the
*        chunk.
*     *status
*        The global status.

*  Notes:
*     -  This function is intended to allow large NDFs to be processed in
*     smaller pieces by selecting successive chunks, each of which may then
*     be processed individually. Note that in general not all the chunks
*     selected from an NDF will have the same size, although none will
*     contain more than the specified maximum number of pixels.
*     -  Corresponding chunks selected from different NDFs (or NDF
*     sections) with identical shapes will themselves have identical shapes
*     and will contain the same number of pixels.
*     -  All NDF sections obtained via this function have the same number
*     of dimensions as the input NDF.
*     -  If the number of the requested chunk ("ichunk") exceeds the number
*     of chunks available in the NDF, then a value of NDF__NOID will be
*     returned for the "indf2" parameter (but no error will result). This
*     condition may be used to terminate a loop when all available chunks
*     have been processed. The ndfNchnk function may also be used to
*     determine the number of chunks available.
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
*     DSB: David "s". Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb1;         /* Pointer to initial NDF entry in the ACB */
   NdfACB *acb2;         /* Pointer to chunk entry in the ACB */
   hdsdim d;             /* Size of current dimension */
   hdsdim lbnd1[ NDF__MXDIM ];     /* Lower bounds of initial NDF */
   hdsdim lbnd2[ NDF__MXDIM ];     /* Lower bounds of chunk */
   hdsdim ubnd1[ NDF__MXDIM ];     /* Upper bounds of initial NDF */
   hdsdim ubnd2[ NDF__MXDIM ];     /* Upper bounds of chunk */
   int br;               /* First dimension to be broken */
   int dim[ NDF__MXDIM ];/* Size of each NDF dimension */
   int el;               /* Number of elements in NDF */
   int i;                /* Loop counter for dimensions */
   int idim;             /* Pixel index offset (higher dimension) */
   int iinc;             /* Increment offset in broken dimmension */
   int inc;              /* Broken dimension increment per chunk */
   int ischnk;           /* Whether the specified chunk exists */
   int ncycle;           /* Number of broken dimension cycles */
   int ndim;             /* Number of NDF dimensions */
   int ninc;             /* Number of increments/broken dimension */
   int offset;           /* Pixel offset from start of NDF */
   int s;                /* Stride of current dimension */
   int stride[ NDF__MXDIM ];       /* Stride of each dimension */

/* Set an initial value for the "indf2" parameter. */
   *indf2 = NDF__NOID;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf1, &acb1, status );
   if( *status == SAI__OK ) {

/* Check that the maximum number of pixels in the chunk is positive.
   Report an error if it is not. */
      if( mxpix < 1 ) {
         *status = NDF__MXPIN;
         msgSeti( "MXPIX", mxpix );
         errRep( " ", "Specified maximum number of contiguous pixels "
                 "(^MXPIX) is invalid (possible programming error).", status );

/* Check that the chunk index is positive. Report an error if it is
   not. */
      } else if( ichunk < 1 ) {
         *status = NDF__ICHIN;
         msgSeti( "ICHUNK", ichunk );
         errRep( " ", "Chunk index value (^ICHUNK) is invalid (possible "
                 "programming error).", status );

/* Obtain the pixel index bounds of the NDF from its data array. */
      } else {
         aryBound( acb1->did, NDF__MXDIM, lbnd1, ubnd1, &ndim, status );

/* Loop to identify the dimension which must be broken in order not to
   exceed the maximum number of contiguous pixels. */
         s = 1;
         br = 0;
         for( i = 0; i < ndim; i++ ){

/* Calculate the size of the current dimension and the stride of the
   next dimension (at this point "s" holds the stride of the current
   dimension). */
            d = ubnd1[ i ] - lbnd1[ i ] + 1;
            el = s*d;

/* If the stride of the next dimension does not exceed the contiguous
   pixel limit, then the current dimension need not be broken, so its
   bounds are unchanged. */
            if( el <= mxpix ) {
               lbnd2[ i ] = lbnd1[ i ];
               ubnd2[ i ] = ubnd1[ i ];

/* Note the first dimension to be broken, which is where the stride of
   the following dimension first exceeds the contiguous pixel limit.
   For this and subsequent dimensions, store the dimension size and
   stride for later use. */
            } else {
               if( br == 0 ) br = i + 1;
               dim[ i ] = d;
               stride[ i ] = s;
            }

/* Retain the stride for the next dimension. Note that on exit from
   this loop, "el" holds a count of the total number of pixels in the
   input NDF. */
            s = el;
         }

/* If no dimension has to be broken, then the entire NDF can be
   accommodated in a single chunk. The chunk index must therefore be 1
   if a chunk is to be returned. */
         if( br == 0 ) {
            ischnk = ( ichunk == 1 );

/* Otherwise, calculate by how many the pixel index of the broken
   dimension can be incremented to produce a chunk of contiguous pixels
   withput exceeding the contiguous pixel limit. */
         } else {
            inc = mxpix/stride[ br - 1 ];

/* Find how many of these pixel index increments fit into the size of
   the broken dimension. Allow for a partial increment at the end. */
            ninc = dim[ br - 1 ]/inc;
            if( ( ninc*inc ) < dim[ br - 1 ] ) ninc++;

/* As the offset in pixels from the start of the NDF increases, the
   pixel index of the broken dimension will cycle repeatedly between
   its lower and upper bounds. Determine how many complete times this
   dimension has to be cycled through in order to obtain chunk number
   "ichunk" by dividing the offset from the beginning of the NDF in
   chunks by the number of chunks (increments) per cycle, as determined
   above. */
            ncycle = ( ichunk - 1 )/ninc;

/* Find the remainder, which gives the number of chunks offset into the
   final (partial) cycle through the broken dimension. */
            iinc = ichunk - ninc*ncycle - 1;

/* Convert the number of chunks offset into a lower and upper bound in
   the broken dimension, checking that the resulting chunk's upper
   bound does not exceed the broken dimension's upper bound. */
            lbnd2[ br - 1 ] = lbnd1[ br - 1 ] + inc*iinc;
            ubnd2[ br - 1 ] = lbnd2[ br - 1 ] + inc - 1;
            if( ubnd2[ br - 1 ] > ubnd1[ br - 1 ] ) ubnd2[ br - 1 ] = ubnd1[ br - 1 ];

/* Calculate the number of pixels skipped over which must be taken up
   by incrementing the higher dimensions (above the broken one). */
            offset = ncycle*stride[ br - 1 ]*dim[ br - 1 ];

/* Check that this count is less than the total number of pixels in the
   NDF, otherwise there will be no pixels left for the chunk itself
   (i.e. the chunk index is too high). */
            ischnk = ( offset < el );
            if( ischnk ) {

/* Loop to calculate the bounds of the chunk in each higher dimension.
   In this case the lower and upper bounds are equal. */
               for( i = ndim - 1; i >= br; i-- ){

/* Divide the remaining pixel count by the dimension stride to get the
   dimension offset from its lower bound. Then calculate the required
   lower and upper bounds of the chunk in this dimension. */
                  idim = offset/stride[ i ];
                  lbnd2[ i ] = lbnd1[ i ] + idim;
                  ubnd2[ i ] = lbnd2[ i ];

/* Decrement the remaining pixel count and return to handle the next
   lower dimension. */
                  offset -= idim*stride[ i ];
               }
            }
         }

/* If the chunk index was valid, then cut an appropriate section from
   the NDF. */
         if( ischnk ) {
            ndf1Cut( acb1, ndim, lbnd2, ubnd2, &acb2, status );
            *indf2 = ndf1Expid( ( NdfObject * ) acb2, status );
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfChunk: Error obtaining a chunk of contiguous pixels "
              "from an NDF.", status );
      ndf1Trace( "ndfChunk", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

