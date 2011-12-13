#include "sae_par.h"
#include "cupid.h"
#include <stdlib.h>
#include <limits.h>

int cupidRFill( int i0, int index, int *ipa, int nel, int skip[ 3 ],
         int *status ){
/*
*+
*  Name:
*     cupidRFill

*  Purpose:
*     Fill a clump of pixels with a given value.

*  Language:
*     Starlink C

*  Synopsis:
*     int cupidRFill( int i0, int index, int *ipa, int nel, int skip[ 3 ],
*        int *status )

*  Description:
*     This function identifies the clump edges surrounding a nominated pixel,
*     and fills the volume contained within those edges with a given integer
*     index value.
*
*     The filling process is done in steps in which a new layer of surface
*     pixels is added on to the existing filled volume. Initially, the
*     filled volume consists of just the nominated pixel, and the surface
*     layer consists of those pixel which touch the nominated pixel. Each
*     pixel in the surface layer is then checked in turn. If it is marked
*     as an edge pixel, it is ignored. If it has already been assigned to
*     another clump, or if it is marked as a pixel at the edge of the
*     entire data array, then the filling of the current clump is aborted as
*     it is unusable (see below for why). Otherwise, the given integer
*     index value is stored at the pixel, and the positions of the immediate
*     neighbours of the pixel are added to a list which will form the next
*     surface layer (so long as they are not already on the list). Once
*     all pixels in the current surface layer have been checked in this way,
*     checking recommences with the next surface layer (if it contains
*     any pixels). Thus the volume surrounding the specified pixel is
*     extended, layer by layer, in the style of a set of onion skins.
*     Pixels  which are flagged as edge pixels block this expansion, forcing
*     the new layers to find a way round which avoids such edge pixels.
*
*     If there are "holes" in the edges surrounding the nominated pixel,
*     then the clump value will "leak" out through the holes and fill
*     everything on the other side of the hole (which may well be the
*     entire background area). To prevent this form happening, clumps are
*     deemed unusable if they encounter pixels which have already been
*     assigned to another clump, or which are at the edge of the entire
*     data array.

*  Parameters:
*     i0
*        The 1D vector index of the nominated pixel from which the
*        filling process will originate.
*     index
*        The value to store at every pixel of "ipa" which is found to be
*        within the clump.
*     ipa
*        Pointer to an array which is the same shape and size as the data
*        array, and which holds a flag for every pixel. If the pixel is
*        an edge pixel this flag will be zero. If the pixel has been
*        assigned to a clump, this flag will equal the index of the clump
*        (all clump indices are integers greater than zero). If the pixel
*        has been added to the list of pixel forming the current or next
*        layer surrounding a clump, then the flag will be negative with
*        an absolute value equal to the index of the relevant clump. Note,
*        it is assumed that all pixels which touch the edge of the data
*        array (except for those which are are flagged as clump edges) are
*        flagged using INT_MAX.
*     nel
*        The total number of elements in "ipa".
*     skip
*        The increment in 1D vector index required to move a distance of 1
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords. This
*        array should have 3 elements even if there are less than 3 pixel
*        axes, and the extra elements should be filled with zero's.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The number of pixels added to the clump. This will be zero if the
*     filling process was aborted due to the clump encountering another
*     clump or the edges of the array. It will also be zero if an error
*     occurs.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     24-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   int *new_surface;    /*  1D indices of pixels in next surface layer */
   int *ptemp;          /* Temporary storage during swap */
   int *surface;        /* 1D indices of pixels in current surface layer */
   int flag;            /* The original pixel flag value */
   int inext;           /* Index of next item to add to the new surface list */
   int iv;              /* 1D vector index for the current pixel */
   int ivx;             /* 1D vector index at a particular column */
   int ivy;             /* 1D vector index at the start of a row */
   int ivz;             /* 1D vector index at the start of a plane */
   int ix;              /* Offset from central pixel along 1st pixel axis */
   int iy;              /* Offset from central pixel along 2nd pixel axis */
   int iz;              /* Offset from central pixel along 3rd pixel axis */
   int new_surface_len; /* Length of the "new_surface" array */
   int next_surface_el; /* Index of next element to use from "surface" */
   int ret;             /* The number of pixels added to the clump */
   int surface_len;     /* Length of the "surface" array */
   int ylim;            /* Number of neighbouring rows */
   int zlim;            /* Number of neighbouring planes */

/* Indicate we have not yet added any pixels to the clump. */
   ret = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Allocate memory to hold the indices of the pixels on the current surface
   layer, and initialise the list to hold just the nominated central pixel. */
   surface = astMalloc( sizeof(int)*30 );
   if( surface ) {
      surface[ 0 ] = i0;
      surface_len = 1;

/* Indicate that the list holding the indices of the pixels on the next
   surface layer is empty. */
      new_surface_len = 0;
      new_surface = NULL;

/* Indicate that we are about to check the first pixel (element zero) in
   the current surface layer. */
      next_surface_el = 0;

/* Loop round adding sucesive layers of pixels into the clump until the
   current surface layer is empty. This loop will exit early if the clump
   encounters another clump or the edge of the array. */
      while( surface_len > 0 ) {

/* Get the 1D vector index of the next pixel to be checked in the current
   surface layer, then increment the index. */
         iv = surface[ next_surface_el++ ];

/* Get the pixels flag value from "ipa" */
         flag = ipa[ iv ];

/* If this pixel is a clump edge pixel, skip it, */
         if( flag != 0 ) {

/* If this pixel is an array border pixel (i.e. is on the border of the
   entire data array), or has been assigned to a different clump, abort the
   current clump, erasing it from "ipa". */
            if( flag > 0 && flag != index ) {
               ret = 0;
               for( iv = 0; iv < nel; iv++ ) {
                  if( abs( ipa[ iv ] ) == index ) ipa[ iv ] = -INT_MAX;
               }
               break;

/* Otherwise, assign the clump index to the pixel, and increment the
   number of pixels added to the clump. */
            } else {
               ipa[ iv ] = index;
               ret++;

/* We now look at all the pixels which adjoin this pixel, adding them to
   the list of pixels within the next outer layer for this clump, so
   long as they have not already been included in the list. Get the 1D
   vector index of the first pixel in a 3x3x3 cube centred on the current
   pixel. Since the "ipa" array flags all pixels at the edge of the array
   as border pixels, we know that the central pixel in this cube cannot be
   at the border of the data array. This means we do not need to check the
   3D GRID indices against the bounds of the array. */
               ivz = iv - skip[ 2 ] - skip[ 1 ] - skip[ 0 ];

/* Loop round the 3D pixel offsets from the central pixel, noting the 1D
   vector index at the start of each plane and row. If there are less
   than 3 pixel axes, ensure the loops do not loop over non-existent axes. */
               zlim = ( skip[ 2 ] > 0 )  ? 3 : 1;
               ylim = ( skip[ 1 ] > 0 )  ? 3 : 1;
               for( iz = 0; iz < zlim; iz++ ) {
                  ivy = ivz;
                  for( iy = 0; iy < ylim; iy++ ) {
                     ivx = ivy;
                     for( ix = 0; ix < 3; ix++ ) {

/* If this neighbouring pixel has not already been assigned to the clump
   and has not already been added to the list of surface pixels, add it
   to the list now. */
                        if( abs( ipa[ ivx ] ) != index ) {
                           inext = new_surface_len++;
                           new_surface = astGrow( new_surface,
                                                  new_surface_len,
                                                  sizeof( int ) );
                           if( new_surface ) new_surface[ inext ] = ivx;

/* Flag that this pixel has been added to the list by setting its flag
   value to -(clump index). But never over-write a flag value indicating
   that the pixel is an edge pixel (zero flag) or border pixel or that the
   pixel has been added to a clump (+ve flags). This means that occasionally
   pixels will be included more than once in the list of surface pixels. */
                           if( ipa[ ivx ] < 0 ) ipa[ ivx ] = -index;
                        }

/* Increment the 1D vector index of the next neighbouring pixel. */
                        ivx += 1;
                     }
                     ivy += skip[ 1 ];
                  }
                  ivz += skip[ 2 ];
               }
            }
         }

/* If we have now exhausted the pixels in the current layer surrounding
   the clump peak, install the new layer as the current layer. We retain
   the memory allocated for the current surface layer to avoid the overhead
   of constantly allocating small arrays. */
         if( next_surface_el == surface_len ) {
            ptemp = surface;
            surface = new_surface;
            surface_len = new_surface_len;
            next_surface_el = 0;
            new_surface = ptemp;
            new_surface_len = 0;
         }
      }
   }

/* Return the number of pixels added to the clump. */
   return ret;

}
