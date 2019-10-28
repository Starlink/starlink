#include "sae_par.h"
#include "prm_par.h"
#include "cupid.h"
#include <string.h>

typedef struct Pix {
   int ineb;
   size_t iv;
   hdsdim x[3];
   int edge;
} Pix;

int cupidCFErode( CupidPixelSet *ps, int *ipa, int ndim, hdsdim *dims,
                  size_t skip[3], int perspectrum, int naxis,
                  CupidPixelSet **clumps, int *status ){
/*
*+
*  Name:
*     cupidCFErode

*  Purpose:
*     Transfer all the pixels in one PixelSet into another, using the
*     algorithm of the Williams et al ClumpFind ApJ paper.

*  Language:
*     Starlink C

*  Synopsis:
*     int cupidCFErode( CupidPixelSet *ps, int *ipa, int ndim, hdsdim *dims,
*                       size_t skip[3], int perspectrum, int naxis,
*                       CupidPixelSet **clumps, int *status )

*  Description:
*     This function transfer all the pixels in PixelSet "ps" which are
*     adjacent to another PixelSet into the neighbouring PixelSet. The
*     Williams et al ApJ ClumpFInd paper describes such pixels as
*     "friends" of the neighbouring PixelSet, and the "friends-of-friends"
*     algorithm described in that paper is implemented by repeated calls
*     to this function.

*  Parameters:
*     ps
*        Pointer to the source PixelSet structure containing the pixels to
*        be moved.
*     ipa
*        Pointer to the start of the array holding the integer index
*        (if any) associated with each pixel in the data array. This
*        array should be the same shape and size as the data array.
*     ndim
*        The number of pixel axes in the data array.
*     dims
*        The number of pixels on each pixel axis of the data array. This
*        array should have 3 elements even if "ndim" is less than 3, and
*        the extra elements should be filled with 1's.
*     skip
*        The increment in 1D vector index required to move a distance of 1
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords.
*        Unused trailing elements should be filled with zero.
*     perspectrum
*        If non-zero, then each spectrum is processed independently of its
*        neighbours. A clump that extends across several spectra will be
*        split into multiple clumps, each restricted to a single spectrum.
*        The non-zero value supplied should be the 1-based axis index of
*        the spectral pixel axis. Should be supplied as zero if "ndim" is
*        not 3.
*     naxis
*        Determines which adjoining pixels are considered to be "neighbours"
*        of a specified central pixel. Should be in the range 1 to "ndim".
*        For a pixel to be considered to be a neighbour of another pixel,
*        the two pixels must be no more than 1 pixel away along no more than
*        "naxis" axes. The supplied value is ignored and a value of 1 is
*        used if "perspectrum" is non-zero.
*     clumps
*        Array holding pointers to all previously defined PixelSets, such
*        that a pointer to the PixelSet with index value "i" is stored at
*        element "i" of the "clumps" array.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     Non-zero if any pixels were transferred out of the source PixelSet.
*     Zero otherwise.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
*     Copyright (C) 2007 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     26-NOV-2005 (DSB):
*        Original version.
*     17-SEP-2007 (DSB):
*        Added "perspectrum" parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   Pix *pix;        /* Pointer to a Pix structure describing transferred pixel */
   Pix *xflist;     /* Pointer to list of Pix structures describing transferred pixels */
   hdsdim ix;       /* GRID index on 1st axis */
   hdsdim iy;       /* GRID index on 2nd axis */
   hdsdim iz;       /* GRID index on 3rd axis */
   hdsdim lbnd[ 3 ];/* New lower bounds for eroded PixelSet */
   hdsdim ubnd[ 3 ];/* New upper bounds for eroded PixelSet */
   hdsdim x[ 3 ];   /* GRID indices of current array element */
   int *v1;         /* Pointer to element at start of this row */
   int *v2;         /* Pointer to element at start of this plane */
   int *v;          /* Pointer to next array element */
   int edge;        /* Pixel at upper or lower bound on any axis? */
   int i1[27];      /* List of adjoining PixelSet indices at this level */
   int il1;         /* Lowest index of adjoining PixelSets at this level */
   int il2;         /* Index of adjoining PixelSets at higher level */
   int n1;          /* No. of adjoining PixelSets at this level */
   int n2;          /* No. of adjoining PixelSets at higher level */
   int old_index;   /* Original index value of the transferred pixels */
   int yedge;       /* Pixel at upper or lower bound on 2nd axis? */
   int zedge;       /* Pixel at upper or lower bound on 3rd axis? */
   size_t i;        /* Loop count */
   size_t iv;       /* 1D vector index */
   size_t nxf;      /* No. of pixels to transfer out of the source PixelSet */

/* Check inherited status. */
   if( *status != SAI__OK ) return 0;

/* Initialise the number of pixels to be transferred out of the source
   PixelSet. */
   nxf = 0;
   xflist = NULL;

/* Initialise the bounds of the eroded PixelSet. */
   lbnd[ 0 ] = VAL__MAXI;
   lbnd[ 1 ] = VAL__MAXI;
   lbnd[ 2 ] = VAL__MAXI;
   ubnd[ 0 ] = VAL__MINI;
   ubnd[ 1 ] = VAL__MINI;
   ubnd[ 2 ] = VAL__MINI;

/* Get the index value of the source PixelSet. */
   old_index = ps->index;

/* Get a pointer to the first pixel in the source PixelSet bounding box. If
   the data  has less than 3 axes, the unused upper and lower bounds will be
   set to [1,1] and so we can always pretend there are 3 axes. */
   v = ipa + ( ps->lbnd[ 0 ] - 1 ) + ( ps->lbnd[ 1 ] - 1 )*skip[ 1 ] +
             ( ps->lbnd[ 2 ] - 1 )*skip[ 2 ];

/* Loop round the pixels in the source PixelSet bounding box. */
   for( iz = ps->lbnd[ 2 ]; iz <= ps->ubnd[ 2 ]; iz++ ) {
      x[ 2 ] = iz;
      zedge = ( ndim > 2 && ( iz == 1 || iz == dims[ 2 ] ) );
      v2 = v;

      for( iy = ps->lbnd[ 1 ]; iy <= ps->ubnd[ 1 ]; iy++ ) {
         x[ 1 ] = iy;
         yedge = ( ndim > 1 && ( iy == 1 || iy == dims[ 1 ] ) );
         v1 = v;

         for( ix = ps->lbnd[ 0 ]; ix <= ps->ubnd[ 0 ]; ix++ ) {
            x[ 0 ] = ix;
            edge = yedge || zedge || ( ix == 1 || ix == dims[ 0 ] );

/* If the pixel was originally a member of the source PixelSet, then see
   if it is adjacent to any other PixelSets. */
            if( *v == old_index ) {
               iv = v - ipa;
               cupidCFNebs( ipa, iv, x, ndim, dims, skip, old_index, perspectrum,
                            naxis, &n1, &il1,  i1, &n2, &il2, clumps, status );

/* If this pixel adjoins another PixelSet, add its details to the end of the
   list of pixels to be transferred to that PixelSet. */
               if( il2 != CUPID__CFNULL ) {
                  xflist = astGrow( xflist, ++nxf, sizeof( Pix ) );
                  if( astOK ) {
                     pix = xflist + nxf - 1;
                     pix->ineb = il2;
                     pix->iv = iv;
                     pix->edge = edge;
                     pix->x[ 0 ] = x[ 0 ];
                     pix->x[ 1 ] = x[ 1 ];
                     pix->x[ 2 ] = x[ 2 ];
                  }

/* Of the pixel is not transfered out of the source PixelSet, update the
   bounds for the eroded source PixelSet. */
               } else {
                  if( ix < lbnd[ 0 ] ) lbnd[ 0 ] = ix;
                  if( iy < lbnd[ 1 ] ) lbnd[ 1 ] = iy;
                  if( iz < lbnd[ 2 ] ) lbnd[ 2 ] = iz;
                  if( ix > ubnd[ 0 ] ) ubnd[ 0 ] = ix;
                  if( iy > ubnd[ 1 ] ) ubnd[ 1 ] = iy;
                  if( iz > ubnd[ 2 ] ) ubnd[ 2 ] = iz;
               }
            }

/* Get the pointer to the next pixel in the source PixelSet bounding box. */
            v++;
         }
         v = v1 + skip[ 1 ];
      }
      v = v2 + skip[ 2 ];
   }

/* Loop round the pixels which are to be transferred out of the source
   PixelSet. */
   pix = xflist;
   for( i = 0; i < nxf; i++, pix++ ) {
      cupidCFAddPixel( ipa, clumps[ pix->ineb ], pix->iv, pix->x, VAL__MIND,
                       pix->edge, status );
   }

/* Reduce the population of the source PixelSet. */
   ps->pop -= nxf;

/* Store the modified bounds */
   ps->lbnd[ 0 ] = lbnd[ 0 ];
   ps->lbnd[ 1 ] = lbnd[ 1 ];
   ps->lbnd[ 2 ] = lbnd[ 2 ];
   ps->ubnd[ 0 ] = ubnd[ 0 ];
   ps->ubnd[ 1 ] = ubnd[ 1 ];
   ps->ubnd[ 2 ] = ubnd[ 2 ];

/* Free resources. */
   xflist = astFree( xflist );

/* Return a flag indicating if any pixels were transferred. */
   return ( nxf > 0 );

}
