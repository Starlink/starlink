#include "sae_par.h"
#include "prm_par.h"
#include "ast.h"
#include "cupid.h"
#include <string.h>
#include <math.h>

AstRegion *cupidPolygonDesc( double *ipd, int velax, double *peak,
                             int space_axes[ 2 ], int ndim, hdsdim *lbnd,
                             hdsdim *ubnd, AstMapping *wcsmap,
                             AstFrame *space_frm, AstMapping *space_map,
                             int *status ){
/*
*+
*  Name:
*     cupidPolygonDesc

*  Purpose:
*     Create an AST Polygon describing the spatial extent of a clump.

*  Language:
*     Starlink C

*  Synopsis:
*     AstRegion *cupidPolygonDesc( double *ipd, int velax, double *peak,
*                                  int space_axes[ 2 ], int ndim, hdsdim *lbnd,
*                                  hdsdim *ubnd, AstMapping *wcsmap,
*                                  AstFrame *space_frm, AstMapping *space_map,
*                                  int *status )

*  Description:
*     This function returns an Polygon describing the spatial extent of the
*     clump specified by the supplied masked clump data array.

*  Parameters:
*     ipd
*        Pointer to the 2D or 3D masked clump data array.
*     velax
*        The zero-based index of the velocity pixel axis. Should be -1 if
*        there is no velocity axis.
*     peak
*        Array holding pixel coords of clump peak value.
*     space_axes[ 2 ]
*        Zero based indices of the two spatial pixel axes.
*     ndim
*        Number of pixel axes.
*     lbnd
*        Point to array holding lower pixel indices of array "ipd".
*     ubnd
*        Point to array holding upper pixel indices of array "ipd".
*     wcsmap
*        If the returned Polygon is to be defined in pixel coordinates, then
*        a NULL pointer should be supplied for "wcsmap". Otherwise, a pointer
*        to a Mapping from the input PIXEL Frame to the WCS Frame should be
*        supplied.
*     space_frm
*        A pointer to the 2D spatial WCS Frame. Ignored if "wcsmap" is NULL.
*     space_map
*        A pointer to the 2D spatial pixel->WCS Mapping. Ignored if "wcsmap"
*        is NULL.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     A pointer to the Polygon, or NULL if no Polygon can be created.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     25-MAY-2009 (DSB):
*        Original version.
*     7-AUG-2009 (DSB):
*        For 3D data, use the largest pixel in the mask as the inside point,
*        not the peak (since the peak can get trimmed of if it is at the
*        edge of the clump, causing astOutline to fail).
*     22-OCT-2009 (DSB):
*        Correct conversion from 2D pixel coords to pixel indices.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   AstPolygon *polygon;     /* Returned Polygon */
   double *pd;              /* Point to next input mask value */
   double *px;              /* Pointer to next vertex X axis value */
   double *py;              /* Pointer to next vertex Y axis value */
   double *verts;           /* Pointer to memory holding vertex axis values */
   double pos[ 2 ];         /* Normalised vertex position */
   hdsdim inside[ 2 ];      /* Spatial pixel indices at clump peak */
   int *ipm;                /* Pointer to 2D mask array */
   int *pm;                 /* Pointer to next 2D mask element */
   int max;                 /* Maximum mask value */
   int nvert;               /* Number of vertices in Polygon */
   int v;                   /* Current mask value */
   size_t *iph;             /* Pointer to histogram array */
   size_t *pix;             /* Pointer to X spatial index in 3D data */
   size_t *piy;             /* Pointer to Y spatial index in 3D data */
   size_t dim[ 3 ];         /* Array pixel dimensions */
   size_t hi;               /* Highest no. of  spectral channels in 2D mask */
   size_t hist_size;        /* Length of histogram array */
   size_t i;                /* Pixel index on 1st pixel axis */
   size_t j;                /* Pixel index on 2nd pixel axis */
   size_t k;                /* Pixel index on 3rd pixel axis */
   size_t lo;               /* Lowest no. of  spectral channels in 2D mask */
   size_t nel;              /* Number of elements in 2D mask */
   size_t nx;               /* X dimension of 2D mask */
   size_t ny;               /* Y dimension of 2D mask */
   size_t target;           /* Threshold for no. of spectral channels */
   size_t tot;              /* Total no. of  spectral channels in 2D mask */

/* Abort if an error has already occurred, or if the data is
   one-dimensional. */
   if( *status != SAI__OK || ndim == 1 ) return NULL;

/* Initialisation. */
   polygon = NULL;

/* Get the pixel axis dimensions. */
   dim[ 0 ] = ubnd[ 0 ] - lbnd[ 0 ] + 1;
   dim[ 1 ] = ubnd[ 1 ] - lbnd[ 1 ] + 1;
   dim[ 2 ] = ubnd[ 2 ] - lbnd[ 2 ] + 1;

/* Allocate memory for a 2D integer mask array for the clump. */
   nx = dim[ space_axes[ 0 ] ];
   ny = dim[ space_axes[ 1 ] ];
   nel = nx*ny;
   ipm = astMalloc( nel*sizeof( *ipm ) );

/* Check the pointer can be used safely. */
   if( astOK ) {

/* Initialise the 2D mask to hold zero at every pixel. */
      memset( ipm, 0, nel*sizeof( *ipm )  );

/* If the data is 2D, each good data value in the supplied mask is set to
   1 in the 2D mask. */
      if( ndim == 2 ) {
         pd = ipd;
         pm = ipm;
         for( i = 0; i < nel; i++, pd++, pm++ ) {
            if( *pd != VAL__BADD ) *pm = 1;
         }

/* Store the pixel indices of the clump peak. */
         inside[ 0 ] = (hdsdim)floor( peak[ space_axes[ 0 ] ] + 1.0 );
         inside[ 1 ] = (hdsdim)floor( peak[ space_axes[ 1 ] ] + 1.0 );

/* If the data is 3D, we need to collapse the supplied 3D mask array along
   the spectral axis to get a 2D mask. */
      } else if( ndim == 3 ) {

/* Get pointers to the spatial pixel index variables. */
         if( velax == 0 ) {
            pix = &j;
            piy = &k;

         } else if( velax == 1 ) {
            pix = &i;
            piy = &k;

         } else {
            pix = &i;
            piy = &j;
         }

/* Loop round every element of the supplied 3D mask array. */
         max = -1;
         pd = ipd;
         for( k = 0; k < dim[ 2 ]; k++ ) {
            for( j = 0; j < dim[ 1 ]; j++ ) {
               for( i = 0; i < dim[ 0 ]; i++,pd++ ) {

/* If the 3D mask element is not bad, increment the count in the
   corresponding 2D mask. Also note the spatial indices of the largest
   mask value. */
                  if( *pd != VAL__BADD ) {
                     v = ++ipm[ *pix + nx*( *piy ) ];
                     if( v > max ) {
                        max = v;
                        inside[ 0 ] = *pix;
                        inside[ 1 ] = *piy;
                     }
                  }
               }
            }
         }

/* Adjust the pixel origin of the inside point */
         inside[ 0 ] += lbnd[ space_axes[ 0 ] ];
         inside[ 1 ] += lbnd[ space_axes[ 1 ] ];

/* We need to trim the 2D mask to exclude pixels around the edge of the
   clump that are only present in a very small number of spectral channels.
   We exclude the pixel that contain the bottom 10% of the spectral
   coverage. First, find the largest and smallest (non-zero) number of 3D
   pixels that contribute to each spatial pixel */
         pm = ipm;
         for( i = 0; i < nel; i++, pm++ ) {
            if( *pm > 0 ) break;
         }

         lo = *pm;
         hi = *pm;

         for( ; i < nel; i++, pd++, pm++ ) {
            if( *pm > hi ) {
               hi = *pm;
            } else if( *pm > 0 && *pm < lo ) {
               lo = *pm;
            }
         }

/* Create a histogram of 2D mask values between these two limits. */
         hist_size = hi - lo + 1;
         iph = astMalloc( hist_size*sizeof( *iph ) );
         memset( iph, 0, hist_size*sizeof( *iph )  );
         tot = 0;
         pm = ipm;
         for( i = 0; i < nel; i++, pm++ ) {
            if( *pm > 0 ) {
               iph[ *pm - lo ]++;
               tot++;
            }
         }

/* Find the 2D mask value that corresponds to the 10% point in this
   histogram. This is the minimum number of contributions that must be
   made to a 2D mask pixel in order for that pixel to be included in the 2D
   mask. */
         target = tot/10;
         i = 0;
         tot = iph[ 0 ];
         while( tot < target ) tot += iph[ ++i ];
         if( tot > target ) i--;

/* Set values below this minimum value to zero in the 2D mask. */
         target = lo + i;
         pm = ipm;
         for( i = 0; i < nel; i++, pm++ ) {
            if( *pm < target ) *pm = 0;
         }

/* Free resources */
         iph = astFree( iph );
      }

/* Create a Polygon with up to 15 vertices, enclosing the non-zero pixels
   around the clump peak. */
#if HDS_DIM_SIZEOF == 8
         polygon = astOutline8I( 0, AST__NE, ipm, lbnd, ubnd, 1.0, 15, inside, 1 );
#else
         polygon = astOutlineI( 0, AST__NE, ipm, lbnd, ubnd, 1.0, 15, inside, 1 );
#endif

/* If required, transform the polygon vertices into WCS. */
      if( wcsmap ) {

/* See how many vertices the Polygon has. */
         astGetRegionPoints( polygon, 0, 2, &nvert, NULL ) ;

/* Allocate memory to hold the vertex axis values */
         verts = astMalloc( 2*sizeof( *verts )*nvert );
         if( astOK ) {

/* Get the axis values at the vertices (in pixel coords). */
            astGetRegionPoints( polygon, nvert, 2, &nvert, verts );

/* Transform into WCS. */
            astTranN( space_map, nvert, 2, nvert, verts, 1, 2, nvert,
                      verts );

/* Normalise. */
            px = verts;
            py = verts + nvert;
            for( i = 0; i < nvert; i++ ) {
               pos[ 0 ] = *px;
               pos[ 1 ] = *py;
               astNorm( space_frm, pos );
               *(px++) = pos[ 0 ];
               *(py++) = pos[ 1 ];
            }

/* Create a new Polygon from these transformed and normalised positions.
   This polygon ins defined in the spatial coordinate Frame. */
            (void) astAnnul( polygon );
            polygon = astPolygon( space_frm, nvert, nvert, verts, NULL, " " );
         }

/* Free resources. */
         verts = astFree( verts );
      }
   }

/* Free resources */
   ipm = astFree( ipm );

/* Return the Region pointer. */
   return (AstRegion *) polygon;
}


