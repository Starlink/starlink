#include "sae_par.h"
#include "dat_par.h"
#include "ndf1.h"
#include "ndf_err.h"
#include "ndf.h"
#include "mers.h"

void ndfNchnk_( int indf, int mxpix, int *nchunk, int *status ){
/*
*+
*  Name:
*     ndfNchnk

*  Purpose:
*     Determine the number of chunks of contiguous pixels in an NDF.

*  Synopsis:
*     void ndfNchnk( int indf, int mxpix, int *nchunk, int *status )

*  Description:
*     This function determines the number of "chunks" (i.e. sections) of
*     contiguous pixels that can be obtained from an NDF, subject to the
*     constraint that no chunk should contain more than a specified maximum
*     number of pixels. More specifically, given the maximum number of
*     pixels in a chunk ("mxpix"), this function returns the maximum value
*     which can be supplied for the ICHUNK parameter of the function
*     ndfChunk if a valid NDF identifier for a chunk of contiguous pixels
*     is to be returned.

*  Parameters:
*     indf
*        NDF identifier.
*     mxpix
*        Maximum number of contiguous pixels required in each chunk.
*     *nchunk
*        Returned holding the number of chunks which can be obtained from
*        the NDF.
*     *status
*        The global status.

*  Notes:
*     -  This function is provided to calculate an upper bound on the
*     number of chunks for DO-loops which process NDFs by dividing them
*     into separate chunks by means of calls to the function ndfChunk.
*     -  A value of zero will be returned for the "nchunk" parameter if
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
*     DSB: David "s". Berry (EAO)

*  History:
*     xxx (DSB):
*        Original version, based on equivalent Fortran function by RFWS.

*-
*/

/* Local Variables: */
   NdfACB *acb;          /* Pointer to NDF entry in the ACB */
   hdsdim dim[ NDF__MXDIM ];       /* NDF dimension sizes */
   int br;               /* First dimension to be broken */
   int el;               /* Stride of next dimension */
   int i;                /* Loop counter for dimensions */
   int inc;              /* Broken dimension increment per chunk */
   int ndim;             /* Number of NDF dimensions */
   int s;                /* Stride of current dimension */

/* Set an initial default value for the "nchunk" parameter. */
   *nchunk = 0;

/* Check inherited global status. */
   if( *status != SAI__OK ) return;

/* Ensure the NDF library has been initialised. */
   NDF_INIT( status );

/* Import the NDF identifier. */
   ndf1Impid( indf, &acb, status );
   if( *status == SAI__OK ) {

/* Check that the number of contiguous pixels specified is valid. Report
   an error if it is not. */
      if( mxpix < 1 ) {
         *status = NDF__MXPIN;
         msgSeti( "MXPIX", mxpix );
         errRep( " ", "Specified maximum number of contiguous pixels "
                 "(^MXPIX) is invalid (possible programming error).", status );

/* Determine the dimension sizes of the NDF from its data array. */
      } else {
         aryDim( acb->did, NDF__MXDIM, dim, &ndim, status );
         if( *status == SAI__OK ) {

/* Loop to identify the dimension which must be broken in order not to
   exceed the maximum number of contiguous pixels. */
            s = 1;
            br = 0;
            for( i = 0; i < ndim; i++ ){

/* Calculate the stride of the next dimension (at this point "s" holds
   the stride of the current dimension). */
               el = s*dim[ i ];

/* Note the first dimension to be broken, which is where the stride of
   the following dimension first exceeds the contiguous pixel limit.
   Quit the loop at this point. */
               if( el > mxpix ) {
                  br = i + 1;
                  break;
               }

/* Retain the stride for the next dimension. */
               s = el;
            }

/* If no dimension was broken, then all the NDF's pixels can be
   accommodated in a single chunk. */
            if( br == 0 ) {
               *nchunk = 1;

/* Otherwise, calculate by how many the pixel index of the broken
   dimension can be incremented to produce a chunk of contiguous pixels
   without exceeding the contiguous pixel limit. */
            } else {
               inc = mxpix/s;

/* Find how many of these pixel index increments fit into the size of
   the broken dimension. Allow for a partial increment at the end. */
               *nchunk = ( dim[ br - 1 ] - 1 )/inc + 1;

/* Loop to multiply the number of chunks per broken dimension by the
   number of times the broken dimension is cycled through in passing
   from start to finish through all the NDF's pixels. This latter
   number is given by the product of the sizes of all the dimensions
   higher than the broken dimension. */
               for( i = br; i < ndim; i++ ){
                  *nchunk *= dim[ i ];
               }
            }
         }
      }
   }

/* If an error occurred, then report context information and call the
   error tracing function. */
   if( *status != SAI__OK ) {
      errRep( " ", "ndfNchnk: Error determining how many chunks of "
              "contiguous pixels can be obtained from an NDF.", status );
      ndf1Trace( "ndfNchnk", status );
   }

/* Restablish the original AST status pointer */
   NDF_FINAL

}

