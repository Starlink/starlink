#include "sae_par.h"
#include "cupid.h"

void cupidCFAddPixel( int *ipa, CupidPixelSet *ps, int iv, int x[3], double d, 
                      int edge, CupidPixelSet *psn ){
/*
*  Name:
*     cupidCFAddPixel

*  Purpose:
*     Add a pixel into a PixelSet.

*  Synopsis:
*     void cupidCFAddPixel( int *ipa, CupidPixelSet *ps, int iv, int x[3], 
*                           double d, int edge, CupidPixelSet *psn )

*  Description:
*     This function adds a specified pixel of the input data array into a
*     specified PixelSet.

*  Parameters:
*     ipa
*        Pointer to the start of the array holding the integer index
*        (if any) associated with each pixel in the data array. This
*        shows which clump each pixel belongs to (each clump is identified 
*        by a unique integer index). The array should be the same shape and 
*        size as the data array. Pixels which have not yet been assigned
*        to a clump are marked with the integer value CUPID__CFNULL.
*     ps
*        Pointer to the PixelSet which is to receive the pixel.
*     iv
*        The 1D vector index of the pixel within the data array and the
*        ipa array.
*     x
*        The GRID indices at the centre of the pixel.
*     d
*        The data value at the pixel.
*     edge
*        A boolean flag indicating if the pixel is adjacent to any edge of 
*        the data array.
*     psn
*        If this is not NULL,then it should be a pointer to a PixelSet 
*        defined at a higher contour level which is adjoined by the
*        specified pixel.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     11-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   int i;                /* Loop count */
   int j;                /* Index of new element in secondary array */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* If this is not the first pixel to be added to the PixelSet, extend the 
   bounding box so that it includes the supplied pixel. Increment the number 
   of pixels in the PixelSet at the same time. */
   if( ps->pop++ ) {
      for( i = 0; i < 3; i++ ) {
         if( x[ i ] < ps->lbnd[ i ] ) ps->lbnd[ i ] = x[ i ];
         if( x[ i ] > ps->ubnd[ i ] ) ps->ubnd[ i ] = x[ i ];
      }

/* If this is the first pixel to be added to the PixelSet, initialise the 
   bounding box so that it includes just the supplied pixel. */
   } else {
      ps->lbnd[ 2 ] = ps->ubnd[ 2 ] = x[ 2 ];
      ps->lbnd[ 1 ] = ps->ubnd[ 1 ] = x[ 1 ];
      ps->lbnd[ 0 ] = ps->ubnd[ 0 ] = x[ 0 ];
   }

/* If the pixel data value exceed the previous largest pixel value in the
   PixelSet, update the value and position of the peak pixel in the
   PixelSet. */
   if( d > ps->vpeak ) {
      ps->vpeak = d;
      ps->peak[ 0 ]= x[ 0 ];
      ps->peak[ 1 ]= x[ 1 ];
      ps->peak[ 2 ]= x[ 2 ];
   }

/* If the pixel is an edge pixel, indicate that the PixelSet touches the
   edge of the data array. */
   if( edge ) ps->edge = 1;

/* If the pixel touches a PixelSet identified at a higher contour level, 
   then we need to flag that the current pixel is a neighbour to that 
   PixelSet. "ps->nnb" is an int holding the number of existing PixelSets 
   known to adjoin "ps".  "ps->nb" is a pointer to an array of "ps->nnb" 
   elements, each of these element being a pointer to an existing PixelSet 
   which adjoins "ps". "ps->nbl" is a pointer to an array of "ps->nnb" 
   elements, each of these element being a pointer to an array of ints. The 
   secondary array of ints pointed to by ps->nbl[i] holds the 1D vector 
   indices of the pixels which are known to be adjacent to the PixelSet
   pointed to by "ps->nb[i]". The length of this secondary array of ints
   is stored in "ps->sznbl[i]" (that is, "ps->sznbl" is a pointer to an
   array of ints containing "ps->nnb" elements). */
   if( psn ) { 

/* Find the index of the element within the "ps->nb" array which refers to the
   adjoining PixelSet. */
      for( i = 0; i < ps->nnb; i++ ) {
         if( ps->nb[ i ] == psn ) break;
      }

/* If no element of "ps->nb" refers to the adjoining PixelSet, extend the
   "ps->nb" array (together with the other associated arrays) by 1 element, 
   and initialise it. */
      if( i == ps->nnb ) {
         ps->nnb++;
         ps->nb = astGrow( ps->nb, ps->nnb, sizeof( CupidPixelSet * ) );
         ps->nbl = astGrow( ps->nbl, ps->nnb, sizeof( int * ) );
         ps->sznbl = astGrow( ps->sznbl, ps->nnb, sizeof( int ) );
         if( ps->sznbl ) {
            ps->nb[ i ] = psn;
            ps->nbl[ i ] = NULL;
            ps->sznbl[ i ] = 0;
         }
      }

/* Extend the secondary array of ints and then insert the 1D vector index 
   of the pixel. */
      j = ( ps->sznbl[ i ] )++;
      ps->nbl[ i ] = astGrow( ps->nbl[ i ],
                              ps->sznbl[ i ],
                              sizeof( int ) );
      if( ps->nbl[ i ] ) ps->nbl[ i ][ j ] = iv;

/* Indicate that the pixel is contained within the PixelSet by storing
   a combination of the index value associated with the supplied PixelSet 
   and the index value associated with the adjoining PixelSet at the required 
   position in the pixel assignment array. */
      ipa[ iv ] = cupidMergeIndex( ps->index, psn->index );

/* If the pixel does not adjoin a PixelSet defined at a higher contour
   level, the value stored in the pixel assignment array is just the
   index associated with the supplied PixelSet. */
   } else {
      ipa[ iv ] = ps->index; 
   }

}
