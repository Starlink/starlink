#include "sae_par.h"
#include "cupid.h"
#include <stdlib.h>

int cupidRDrip( int ndrip, CupidRClump *clump, int *ipa, int skip[ 3 ] ){
/*
*  Name:
*     cupidRDrip

*  Purpose:
*     Drip a given number of pixels into a given clump.

*  Synopsis:
*     int cupidRDrip( int ndrip, CupidRClump *clump, int *ipa, 
*                     int skip[ 3 ] )

*  Description:
*     This function adds a given number of pixels into a specified clump.
*     Each clump structure contains a list of pixel indices which cover
*     the surface of the volume currently enclosed by the clump (but
*     which may not as yet be consdiered to be inside the volume). This
*     function checks each of these pixels in turn to find pixels which
*     are not edge pixels and which have not already been assigned to
*     another clump. Once "ndrip" such pixels have been found, they are
*     assigned to the clump and the function returns.
*
*     As each pixel is checked, a new surface is created which holds the
*     next layer of pixels which will be checked once the current layer
*     has been exhausted. Thus the volume surrounding the central peak
*     position is extended, layer by layer, in the style of a set of onion 
*     skins. Pixel which are flagged as edge pixels, or which have
*     already been assigned to another clump, block this expansion,
*     forcing the new layers to find a way round which avoids such edge
*     pixels or pre-assigned pixels.

*  Parameters:
*     ndrip
*        The number of pixels to add to the clump.
*     clump
*        A pointer to a structure containing a description of the clump
*        to which pixels are to be assigned.
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
*        array are flagged as edge pixels in this array.
*     skip
*        The increment in 1D vector index required to move a distance of 1 
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords. This
*        array should have 3 elements even if there are less than 3 pixel
*        axes, and the extra elements should be filled with zero's.

*  Returned Value:
*     The number of pixels added to the clump. This will be less than
*     "ndrip" (and may be zero) if fewer than "drip" pixels could be
*     found to assign to the clump. Zero will be returned if an error
*     occurs.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     24-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int *ptemp;          /* Temporary storage during swap */
   int ind;             /* The index associated with the clump */
   int inext;           /* Index of next item to add to the new surface list */
   int iv;              /* 1D vector index for the current pixel */
   int ivx;             /* 1D vector index at a particular column */
   int ivy;             /* 1D vector index at the start of a row */
   int ivz;             /* 1D vector index at the start of a plane */
   int ix;              /* Offset from central pixel along 1st pixel axis */
   int iy;              /* Offset from central pixel along 2nd pixel axis */
   int iz;              /* Offset from central pixel along 3rd pixel axis */
   int ret;             /* The number of pixels added to the clump */
   int ylim;            /* Number of neighbouring rows */
   int zlim;            /* Number of neighbouring planes */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return 0;

/* Save the clump index. */
   ind = clump->index;

/* Indicate we have not yet added any pixels to the clump. */
   ret = 0;

/* Loop until we have added the required number of pixels to the clump.
   This loop will exit early if there are insufficient data pixels left
   to be assigned to a clump . */
   while( ret < ndrip && clump->surface_len > 0 ) {

/* Get the 1D vector index of the next pixel to be checked in the current
   surface layer, then increment the index. */
      iv = clump->surface[ clump->next_surface_index++ ];

/* If this pixel is an edge pixel or has already been assigned to a clump, 
   skip it, incrementing the number of blocking pixels which surround the 
   clump. This value is only an upper limit on the actual number of
   blocking pixels because some blocking pixels may be included more than 
   once in the list of pixels forming the current surface. */
      if( ipa[ iv ] >= 0 ) {
         if( ipa[ iv ] != ind ) clump->nblock++;

/* Otherwise, assign the clump index to the pixel, and increment the
   number of pixels added to the clump. */
      } else {
         ipa[ iv ] = ind;
         ret++;

/* We now look at all the pixels which adjoin this pixel, adding them to
   the list of pixels within the next outer layer for this clump, so
   long as they have not already been included in the list. Get the 1D
   vector index of the first pixel in a 3x3x3 cube centred on the current
   pixel. Since the "ipa" array flags all pixels at the edge of the array
   as edge pixels, we know that the central pixel in this cube cannot be at
   the edge of the data array. This means we do not need to check the 3D
   GRID indices against the bounds of the array. */
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
                  if( abs( ipa[ ivx ] ) != ind ) {
                     inext = clump->new_surface_len++;
                     clump->new_surface = astGrow( clump->new_surface,
                                                   clump->new_surface_len, 
                                                   sizeof( int ) );
                     if( clump->new_surface ) clump->new_surface[ inext ] = ivx;

/* Flag that this pixel has been added to the list by setting its flag
   value to -(clump index). But never over-write a flag value indicating
   that the pixel is an edge pixel (zero flag) or that the pixel has been 
   added to a clump (+ve flags). This means that occasionally pixels will
   be included more than once in the list of surface pixels. */
                     if( ipa[ ivx ] < 0 ) ipa[ ivx ] = -ind;
                  }

/* Increment the 1D vector index of the next neighbouring pixel. */
                  ivx += 1;
               }
               ivy += skip[ 1 ];
            }
            ivz += skip[ 2 ];
         }
      }

/* If we have now exhausted the pixels in the current layer surrounding
   the clump peak, install the new layer as the current layer. We retain
   the memory allocated for the current surface layer to avoid the overhead
   of constantly allocating small arrays. */
      if( clump->next_surface_index == clump->surface_len ) {
         ptemp = clump->surface;
         clump->surface = clump->new_surface;
         clump->surface_len = clump->new_surface_len;
         clump->next_surface_index = 0;
         clump->new_surface = ptemp;
         clump->new_surface_len = 0;
      }
   }

/* Return the number of pixels added to the clump. */
   return ret;

}

