#include "sae_par.h"
#include "cupid.h"
#include <limits.h>

void cupidRFillClumps( int *ipa, int nel, int ndim, int skip[ 3 ], 
                       int dims[ 3 ], int peakval ){
/*
*  Name:
*     cupidRFillClumps

*  Purpose:
*     Identify clumps by filling in the volume enclosed by the marked
*     edges.

*  Synopsis:
*     void cupidRFillClumps( int *ipa, int nel, int ndim, int skip[ 3 ], 
*                            int dims[ 3 ], int peakval )

*  Description:
*     This function is supplied with an array in which pixels marking the
*     edges of clumps are flagged using the value CUPID__KEDGE, and
*     pixels marking the peak value within a clump are marked by the
*     value "peakval". It assigns a unique integer index to each peak
*     and then finds the extent of the clump surrounding the peak. The
*     supplied array is modified so that all pixels assigned to a peaks clump
*     hold the integer index associated with the clump. Edge pixels are
*     left unchanged in the supplied array, and are not considered to be
*     part of any clump.
*
*     The process of identifying which pixels belong to a given clump is
*     iterative. Each clump has a notional "surface layer" of pixels.
*     Initially this surface layer consists only of the peak pixel itself.
*     On each iteration, the surface layer is extended outwards away from
*     the peak. When the surface layer meets an edge (or a pixel which
*     has already been assigned to another clump) the surface flows round
*     the blocking pixel(s). All clumps are processed simultaneouly in
*     this way in order to avoid giving unfair advantage in the fight for
*     pixels to those clumps which are done first.
*
*     If a peak is completely enclosed by surrounding edges, then the
*     volume enclosed by the edges will eventually be filled with the 
*     clumps index, at which point there will be nowhere for the clump to
*     expand to and so the clump will be excluded from the iterative 
*     process.
*
*     However, if there are breaks in the edges suppounding a peak, then
*     the filling process may "leak" through the holes in the edges and
*     potentially fill far more of the data array than is justified.

*  Parameters:
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
*     nel
*        The number of elements in "ipa".
*     ndim
*        The number of pixel axes in the data (this can be less than 3).
*     skip
*        The increment in 1D vector index required to move a distance of 1 
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords. This
*        array should have 3 elements even if there are less than 3 pixel
*        axes, and the extra elements should be filled with zero's.
*     dims
*        The no. of pixels along each pixel axis. This array should have 3 
*        elements even if there are less than 3 pixel axes, and the extra 
*        elements should be filled with one's.
*     peakval
*        The "ipa" value used to flag peaks.

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
   CupidRClump **clumps; /* List of Clump pointers */
   CupidRClump *newclump;/* Pointer to a clump description */
   double mx;            /* Mean of peak X coords */
   double my;            /* Mean of peak Y coords */
   double mz;            /* Mean of peak Z coords */
   double ndrip;         /* No. of pixels to drip into a clump */
   double sx2;           /* Sum of peak squared X coords */
   double sx;            /* Sum of peak X coords */
   double sy2;           /* Sum of peak squared Y coords */
   double sy;            /* Sum of peak Y coords */
   double sz2;           /* Sum of peak squared Z coords */
   double sz;            /* Sum of peak Z coords */
   int *pa;              /* Pointer to next "ipa" element */
   int i;                /* Index of next "ipa" element */
   int iclump;           /* Index of next clump */
   int ix;               /* The X Grid coord of the current pixel */
   int iy;               /* The Y Grid coord of the current pixel */
   int iz;               /* The Z Grid coord of the current pixel */
   int more;             /* Do we need to attempt more dripping? */
   int nclump;           /* Number of clumps being produced */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* So far we have no clumps */
   nclump = 0;
   clumps = NULL;

/* First scan the supplied "ipa" array, which contains a flag for every
   data pixel, looking for peaks. A new Clump structure is created and
   initialised for each peak found. At the same time we change the values
   in the "ipa" array so that they are suitable for use with cupidRDrip. We
   also get some statistics on the spread of peak positions. */
   sx = 0.0;
   sy = 0.0;
   sz = 0.0;
   sx2 = 0.0;
   sy2 = 0.0;
   sz2 = 0.0;

   pa = ipa;
   i = 0;

   for( iz = 1; iz <= dims[ 2 ]; iz++ ) {
      for( iy = 1; iy <= dims[ 1 ]; iy++ ) {
         for( ix = 1; ix <= dims[ 0 ]; ix++, i++, pa++ ) {

/* If this is an edge pixel, then set it to zero, as required by cupidRdrip. */
            if( ix == 1 || ix == dims[ 0 ] ) {
               *pa = 0;

            } else if( ndim > 1 && ( iy == 1 || iy == dims[ 1 ] ) ) {
               *pa = 0;

            } else if( ndim > 2 && ( iz == 1 || iz == dims[ 2 ] ) ) {
               *pa = 0;

/* Otherwise, if this is a peak, convert it to -INT_MAX (cupidRDrip requires 
   non-edge pixels to be marked by -INT_MAX), and create a new Clump structure,
   initialising it so that the current surface layer holds the single peak
   pixel. */
            } else if( *pa == peakval ) {
               *pa = -INT_MAX;

               clumps = astGrow( clumps, ++nclump, sizeof( CupidRClump * ) );
               if( clumps ) {
                  newclump = astMalloc( sizeof( CupidRClump ) );
                  clumps[ nclump - 1 ] = newclump;            
                  if( newclump ) {         
                     newclump->index = nclump;
                     newclump->new_surface_len = 0;
                     newclump->new_surface = NULL;
                     newclump->nblock = 0;
                     newclump->next_surface_index = 0;
                     newclump->surface_len = 0;
                     newclump->surface = astMalloc( sizeof(int)*30 );
                     if( newclump->surface ) {
                        newclump->surface[ 0 ] = i;
                        newclump->surface_len = 1;
                     }
                  }
               }
         
               sx += ix;
               sy += iy;
               sz += iz;
               sx2 += ix*ix;
               sy2 += iy*iy;
               sz2 += iz*iz;

/* cupidRDrip requires edges to be marked by zeros. */
            } else if( *pa == CUPID__KEDGE ) {
               *pa = 0;
         
/* cupidRDrip requires non-edge pixels to be marked by -INT_MAX. */
            } else {
               *pa = -INT_MAX;
            }  
         }
      }
   }

/* Find a typical distance between peaks. Then find the area (in pixels)
   in square of this size. The use a fraction of this area as the number
   of pixels to drip into each clump on eac iteration.  */
   if( nclump > 0 ) {
      mx = sx/nclump;
      my = sy/nclump;
      mz = sz/nclump;
      ndrip = 0.1*( sx2/nclump - mx*mx +
                    sy2/nclump - my*my +
                    sz2/nclump - mz*mz );
      if( ndrip < 2 ) ndrip = 2;

/* Loop until no more pixels can be dripped into any clump. */
      more = 1;
      while( more ) {

/* Loop round attempting ot extend all clumps, noting if at least one clump 
   is succesfully extended. */
         more = 0;
         for( iclump = 0; iclump < nclump; iclump++ ) {
            if( cupidRDrip( ndrip, clumps[ iclump ], ipa, skip ) ) more = 1;
         }
      }
   }

/* Release resources */
   for( iclump = 0; iclump < nclump; iclump++ ) {
      newclump = clumps[ nclump - 1 ];
      if( newclump ) {         
         newclump->new_surface = astFree( newclump->new_surface );
         newclump->surface = astFree( newclump->new_surface );
      }
      clumps[ nclump - 1 ] = astFree( newclump );
   }
   clumps = astFree( clumps );

}
