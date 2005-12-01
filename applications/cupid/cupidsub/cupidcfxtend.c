#include "sae_par.h"
#include "cupid.h"


/* Local Macros */


#define CHECK_NEIGHBOUR \
\
/* If the neighbouring pixel is still part of the source PixelSet add it \
   to the list of source pixels which adjoin the destination PixelSet. */ \
         if( ipa[ iv ] == old_index ) { \
            nj = new_sznbl++; \
            new_nbl = astGrow( new_nbl, new_sznbl, sizeof( int ) ); \
            if( new_nbl ) new_nbl[ nj ] = iv; \
         } 



#define CHECK_UPNEIGHBOUR(ii) \
\
/* Check the upper neighbour is not off the edge of the array. */ \
      if( x[ ii ] < dims[ ii ] ) { \
\
/* Modify the 1D vector index to represent the upper neighbour on axis \
   "ii". */ \
         iv += skip[ ii ]; \
\
/* Check the neighbour. */ \
         CHECK_NEIGHBOUR; \
\
/* Revert the 1D vector index and nD GRID coords to the original values. */ \
         iv -= skip[ ii ]; \
      }



#define CHECK_LONEIGHBOUR(ii) \
\
/* Check the lower neighbour is not off the edge of the array. */ \
      if( x[ ii ] > 1 ) { \
\
/* Modify the 1D vector index to represent the lower neighbour on axis \
   "ii". */ \
         iv -= skip[ ii ]; \
\
/* Check the neighbour. */ \
         CHECK_NEIGHBOUR; \
\
/* Revert the 1D vector index and nD GRID coords to the original values. */ \
         iv += skip[ ii ]; \
      }


#define CHECK_NEIGHBOURS(ii) \
   CHECK_LONEIGHBOUR(ii); \
   CHECK_UPNEIGHBOUR(ii);







int cupidCFXtend( CupidPixelSet *ps1, CupidPixelSet *ps2, int *ipa, 
                  int ndim, int *dims, int skip[3], int naxis ){
/*
*  Name:
*     cupidCFXtend

*  Purpose:
*     Extend a PixelSet by transfering pixels from a neighbouring PixelSet.

*  Synopsis:
*     int cupidCFXtend( CupidPixelSet *ps1, CupidPixelSet *ps2, int *ipa, 
*                       int ndim, int *dims, int skip[3], int naxis )

*  Description:
*     This function extends the destination PixelSet ("ps2") by transfering
*     pixels from the source PixelSet ("ps1") to the destination PixelSet. 
*     A source pixel is transfered to the destination PixelSet if it is a 
*     neighbour of any pixel in the destination PixelSet on entry.

*  Parameters:
*     ps1
*        Pointer to the source PixelSet structure containing the pixels to
*        be moved.
*     ps2
*        Pointer to the destination PixelSet structure to receive the pixels 
*        moved from "ps1". May be NULL.
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
*     naxis
*        Determines which adjoining pixels are considered to be "neighbours" 
*        of a specified central pixel. Should be in the range 1 to "ndim". 
*        For a pixel to be considered to be a neighbour of another pixel,
*        the two pixels must be no more than 1 pixel away along no more than 
*        "naxis" axes.

*  Returned Value:
*     Non-zero if any pixels were transferred into the destination
*     PixelSet. Zero otherwise.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     26-NOV-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int *nbl;        /* Pointer to list of neighbouring pixel indices */
   int *new_nbl;    /* Pointer to new list of neighbouring pixel indices */
   int i;           /* Loop count */
   int ips;         /* Index of destination clump within source PixelList */
   int iv;          /* 1D vector index of next neighbouring source pixel */
   int new_index;   /* Index value to assign to the transferred pixels */
   int new_sznbl;   /* Number of entries in the "new_nbl" array */
   int nj;          /* Index at which to store new neighbour index */
   int old_index;   /* Original index value of the transferred pixels */
   int rem;         /* Remaining count of pixels */
   int ret;         /* Returned value */
   int sznbl;       /* Number of entries in the "nbl" array */
   int x[ 3 ];      /* GRID axis values */
   int xx;          /* GRID axis value */

/* Check inherited status. Also check we have a destination PixelSet. */
   ret = 0;
   if( *status != SAI__OK || !ps2 ) return ret;

/* Get the index value of the source PixelSet. */
   old_index = ps1->index;

/* Get the index value of the destination PixelSet. */
   new_index = ps2->index;

/* Search through the list of clumps which adjoin the source PixelSet,
   looking for a clump with the same index as the destination PixelSet. */
   nbl = NULL;
   sznbl = 0;
   for( ips = 0; ips < ps1->nnb; ips++ ) {
      if( ps1->nb && ps1->nb[ ips ]->index == new_index ) {

/* When found, store a pointer to the array which holds the 1D vector
   indices of the pixels in the source PixelSet which adjoin the
   destination PixelSet. */
         nbl = ps1->nbl[ ips ];     

/* Also store the number of source pixels which adjoin the destination 
   PixelSet. */
         sznbl = ps1->sznbl[ ips ];
         break;
      }
   }

/* Check there are some pixels to transfer. */
   if( sznbl ) {

/* Once the source pixels which currently adjoin the destination clump 
   have been moved out of the source PixelSet, a new set of source pixels 
   will adjoin the destination clump. The following code identifies this
   "second row of seats" and stores the 1D vector indices of the new
   "front row" pixels in the "new_nbl" array. The size of this array is
   stored in "new_sznbl". Initialise these values to indicate that no such
   "second row" (or "once removed"?) pixels have yet been found. */
      new_sznbl = 0;
      new_nbl = NULL;

/* Change the index value stored in "ipa" for each of the neighbouring
   pixels. */
      for( i = 0; i < sznbl; i++ ) {
         iv = nbl[ i ];
         if( ipa[ iv ] == old_index ) {
            ipa[ iv ] = new_index;

/* Calculate the nD GRID coords of this neighbouring pixel and extend the
   bounding box of the destination PixelSet to include it. */
            rem = iv;
            if( ndim > 2 ) {
               xx = rem/skip[ 2 ] + 1;
               if( xx < ps2->lbnd[ 2 ] ) ps2->lbnd[ 2 ] = xx;
               if( xx > ps2->ubnd[ 2 ] ) ps2->ubnd[ 2 ] = xx;
               rem -= ( xx - 1 )*skip[ 2 ];
               x[ 2 ] = xx;
            } else {
               x[ 2 ] = 1;
            }
         
            if( ndim > 1 ) {
               xx = rem/skip[ 1 ] + 1;
               if( xx < ps2->lbnd[ 1 ] ) ps2->lbnd[ 1 ] = xx;
               if( xx > ps2->ubnd[ 1 ] ) ps2->ubnd[ 1 ] = xx;
               rem -= ( xx - 1 )*skip[ 1 ] + 1;
               x[ 1 ] = xx;
            } else {
               x[ 1 ] = 1;
            }
   
            xx = rem + 1;
            if( xx < ps2->lbnd[ 0 ] ) ps2->lbnd[ 0 ] = xx;
            if( xx > ps2->ubnd[ 0 ] ) ps2->ubnd[ 0 ] = xx;
            x[ 0 ] = xx;

/* Now add any neighbours of this pixel which are still in the source
   PixelSet to the list of new clump neighbours. First check the pixels 
   which are 1 pixel away along only one of the three axes. */
            for( i = 0; i < ndim; i++ ) {
               CHECK_NEIGHBOURS(i);
            }

/* If required, now check the pixels which are 1 pixel away along two axes. 
   For a naxis value of 2, "ndim" must be either 2 or 3 (it cannot be 1). */
            if( naxis >= 2 ) {

/* If not at the upper edge on axis 0, get the 1D and nD coords at the upper 
   neighbour on axis 0. */
               if( x[ 0 ] < dims[ 0 ] ) {
                  iv += skip[ 0 ];
                  x[ 0 ]++;

/* Check the neighbours on the 2nd axis. */
                  CHECK_NEIGHBOURS(1);

/* Revert the coords. */
                  iv -= skip[ 0 ];
                  x[ 0 ]--;
               }

/* Similarly, do the lower neighbour on axis 0. */
               if( x[ 0 ] > 1 ) {
                  iv -= skip[ 0 ];
                  x[ 0 ]--;
                  CHECK_NEIGHBOURS(1);
                  iv += skip[ 0 ];
                  x[ 0 ]++;
               }

/* For 3D arrays we need to find the new neighbours along the 3rd axis as
   well. */
               if( ndim == 3 ) {

/* If not at the upper edge on the 3rd axis, consider the upper neighbours */
                  if( x[ 2 ] < dims[ 2 ] ) {
                     iv += skip[ 2 ];
                     x[ 2 ]++;

/* Check the upper and lower neighbours on axis 0. */
                     CHECK_NEIGHBOURS(0);

/* Check the upper and lower neighbours on axis 1. */
                     CHECK_NEIGHBOURS(1);

/* Revert the coords on axis 2. */
                     iv -= skip[ 2 ];
                     x[ 2 ]--;
                  }

/* If not at the lower edge on the 3rd axis, consider the lower neighbours */
                  if( x[ 2 ] > 1 ) {
                     iv -= skip[ 2 ];
                     x[ 2 ]--;

                     CHECK_NEIGHBOURS(0);
                     CHECK_NEIGHBOURS(1);

                     iv += skip[ 2 ];
                     x[ 2 ]++;

                  }
               }

/* If required, now check the pixels which are 1 pixel away along three axes. 
   For a naxis value of 3, "ndim" must be 3 (it cannot be 1 or 2). */
               if( naxis == 3 ) {

/* If not at the upper edge on the 3rd axis, consider the upper neighbours */
                  if( x[ 2 ] < dims[ 2 ] ) {
                     iv += skip[ 2 ];
                     x[ 2 ]++;

/* If not at the upper edge on axis 0, get the 1D and nD coords at the upper 
   neighbour on axis 0. */
                     if( x[ 0 ] < dims[ 0 ] ) {
                        iv += skip[ 0 ];
                        x[ 0 ]++;

/* Extend the PixelSet to include the neighbours on the axis 1. */
                        CHECK_NEIGHBOURS(1);

/* Revert the coords. */
                        iv -= skip[ 0 ];
                        x[ 0 ]--;
                     }

/* Similarly, do the lower neighbour on axis 0. */
                     if( x[ 0 ] > 1 ) {
                        iv -= skip[ 0 ];
                        x[ 0 ]--;
                        CHECK_NEIGHBOURS(1);
                        iv += skip[ 0 ];
                        x[ 0 ]++;
                     }

/* Revert the coords on axis 2. */
                     iv -= skip[ 2 ];
                     x[ 2 ]--;
                  }

/* If not at the lower edge on the 3rd axis, consider the lower neighbours */
                  if( x[ 2 ] > 1 ) {
                     iv -= skip[ 2 ];
                     x[ 2 ]--;
         
                     if( x[ 0 ] < dims[ 0 ] ) {
                        iv += skip[ 0 ];
                        x[ 0 ]++;
                        CHECK_NEIGHBOURS(1);
                        iv -= skip[ 0 ];
                        x[ 0 ]--;
                     }
         
                     if( x[ 0 ] > 1 ) {
                        iv -= skip[ 0 ];
                        x[ 0 ]--;
                        CHECK_NEIGHBOURS(1);
                        iv += skip[ 0 ];
                        x[ 0 ]++;
                     }
         
                     iv += skip[ 2 ];
                     x[ 2 ]++;
         
                  }
               }  
            }
         }
      }

/* The old neighbours have now been moved from the source to the
   destination clump. So a new set of source pixels may now adjoin the
   (extended) destination clump. Replace the old list of neighbours 
   associated with the destination clump with the new list if any
   neighbours remain. */
      if( new_sznbl > 0 ) {
         astFree( ps1->nbl[ ips ] );
         ps1->nbl[ ips ] = new_nbl;
         ps1->sznbl[ ips ] = new_sznbl;

/* Set the returned value to indicate that the destination clump still has
   neighbours in the source clump. */
         ret = 1;

/* If no neighbours remain, remove the destination clump from the list of
   neighbours in the source clump. */
      } else {
         ps1->nb[ ips ] = NULL;
         ps1->nbl[ ips ] = astFree( ps1->nbl[ ips ] );
         ps1->sznbl[ ips ] = 0;
      }

   }

/* Return the result. */
   return ret;

}



#undef CHECK_NEIGHBOUR 
#undef CHECK_UPNEIGHBOUR 
#undef CHECK_LONEIGHBOUR 
#undef CHECK_NEIGHBOURS 


