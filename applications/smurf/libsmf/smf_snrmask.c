/*
*+
*  Name:
*     smf_snrmask

*  Purpose:
*     Return a 2D mask identifying the high SNR regions in a map.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_snrmask( ThrWorkForce *wf,  int abssnr, unsigned char *oldmask,
*                       const double *map, const double *mapvar,
*                       const dim_t *dims, double snr_hi, double snr_lo,
*                       unsigned char *mask, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads (can be NULL)
*     abssnr = int (Given)
*        Should be supplied non-zero if sources can be negative as well
*        as positive (e.g. when making maps of Q or U Stokes parameters).
*        If non-zero, large negative SNR values are treated as source rather
*        than background.
*     oldmask = unsigned char * (Given)
*        May be NULL. If supplied, any source pixels in the old mask (i.e.
*        zero values) are copied unchanged into the returned new mask. Only
*        background pixels in the old mask are allowed to be changed.
*        Thus source pixels are accumulated form itgeration to iteration.
*        In other words, "once a source pixel, always a source pixel".
*     map = const double * (Read)
*        The 2D map containing the data values, or SNR values if "mapvar"
*        is NULL.
*     mapvar = const double * (Read)
*        The 2D map containing the variance values. If this is NULL, then
*        the "map" array is assumed to contain SNR values rather than data
*        values.
*     dims = const dim_t * (Read)
*        The dimensions of the map.
*     snr_hi = double (Read)
*        The higher SNR value at which to do the initial cut.
*     snr_lo = double (Read)
*        The lower SNR value to which the mask defined by the initial cut
*        should be extended.
*     mask =  unsigned char * (Returned)
*        The array to recieve the mask - mask pixels are zero in the
*        "source" (i.e. high SNR) regions and zero in "background" regions.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function creates a mask based on SNR values that goes down
*     to low SNR values without including lots of isolated noise spikes.
*     It does this by forming an initial mask by thresholding the supplied
*     map at the high SNR value given by "snr_hi". Map pixels above this
*     threshold ("source" pixels) are set to one in the mask, and those
*     below the threshold ("Background" pixels) are set to zero. The
*     areas of source pixels within the mask are then extended down to
*     the lower SNR value given by "snr_lo". Finally, edges of the source
*     regions are smoothed using a cellular automaton that replaces the
*     central pixel in each 3x3 box of mask pixels by the most commonly
*     occuring value in the box (with a slight bias towards extending the
*     masked area). This smoothing is done three times.

*  Authors:
*     David S Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-MAR-2012 (DSB):
*        Original version.
*     14-MAR-2013 (DSB):
*        - Fix bug in checks for neighbours being off the edge of the map.
*        - When flagging adjoining clumps as source clumps, work down the
*        tree of neighbouring clumps as well as up the tree.
*        - Reduce the number of cleanings from 3 to 2.
*     15-MAR-2013 (DSB):
*        - Prevent segfault caused by erroneously using the
*        uninitialised first value of the "table" arrays.
*        - Further fixes to merging of adjoining clumps.
*     10-MAR-2014 (DSB):
*        Added argument oldmask.
*     2-APR-2014 (DSB):
*        Allow SNR values to be supplied, rather than separate data and
*        variance values.
*     28-AUG-2015 (DSB):
*        Added argument abssnr.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Starlink includes */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

/* The number of cleaning iterations to perform on the returned mask. */
#define NCLEAN 2

/* Data types */
typedef struct smfSnrMaskJobData {
   int operation;
   int *cindex;
   dim_t jlo;
   dim_t jhi;
   dim_t rowlen;
   unsigned char *mask;
   unsigned char *maskold;
   int *table;
} smfSnrMaskJobData;

/* Prototypes for local functions */
static void smf1_snrmask_job( void *job_data, int *status );






void smf_snrmask( ThrWorkForce *wf, int abssnr, unsigned char *oldmask,
                  const double *map, const double *mapvar,
                  const dim_t *dims, double snr_hi, double snr_lo,
                  unsigned char *mask, int *status ){

/* Local Variables: */
   const double *pm = NULL;
   const double *pv = NULL;
   dim_t i;
   dim_t j;
   double snr;
   int *cindex = NULL;
   int *ps = NULL;
   int *psn = NULL;
   int *table = NULL;
   int iass;
   int iclean;
   int iclump;
   int ineb;
   int itemp;
   int itop1;
   int itop2;
   int iworker;
   int neb_offset[ 4 ];
   int nworker;
   int ok;
   int rowstep;
   int top;
   smfSnrMaskJobData *job_data = NULL;
   smfSnrMaskJobData *pdata = NULL;
   unsigned char *maskold = NULL;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Save a copy of the old mask, if supplied. Doing it now, means that the
   old and new mask pointers can be the same. */
   if( oldmask ) maskold = astStore( NULL, oldmask,
                                     sizeof(*oldmask)*dims[0]*dims[1] );

/* Allocate an array to hold a clump index for every map pixel. Initialise
   it to hold zeros. */
   cindex = astCalloc( dims[ 0 ]*dims[ 1 ], sizeof( *cindex ) );

/* Initialise the index to assign to the next clump of pixels found above
   the lower SNR limit. Note, no clump is given an index of zero. */
   top = 1;

/* Initialise the pointer to the table holding associated clump indices.
   The first element is unused, so set it to a safe value of zero (i.e.
   "no clump"). */
   table = astCalloc( top, sizeof( *table ) );

/* Set up the vector offsets to the three neighbouring pixels in the lower
   row, and the left hand neighbour in the current row. */
   neb_offset[ 0 ] = -1;              /* Left neighbour in current row */
   neb_offset[ 1 ] = -dims[ 0 ] - 1;  /* Left neighbour in lower row */
   neb_offset[ 2 ] = -dims[ 0 ];      /* Central neighbour in lower row */
   neb_offset[ 3 ] = -dims[ 0 ] + 1;  /* Right neighbour in lower row */

/* Loop round the map, looking for pixels that are above the lower SNR
   limit. Within this loop we store a positive clump index for each pixel that
   is above the lower SNR limit. Each clump of contiguous pixel above the limit
   has a separate clump index. If two clumps touch each other, we associate
   their indices together using a table to indicate that they are part of the
   same physical clump. */
   pm = map;
   pv = mapvar;
   ps = cindex;
   for( j = 0; j < dims[ 1 ] && *status == SAI__OK; j++ ) {
      for( i = 0; i < dims[ 0 ]; i++, pm++, pv++, ps++ ) {

/* Get the SNR value. */
         if( mapvar ) {
            if( *pm != VAL__BADD && *pv != VAL__BADD && *pv > 0.0 ){
               snr = *pm / sqrt( *pv );
            } else {
               snr = VAL__BADD;
            }
         } else {
            snr = *pm;
         }

/* If source can be negative as well as positive, use the absolute SNR in
   the following check. */
         if( abssnr && snr != VAL__BADD ) snr = fabs( snr );

/* Check the SNR is good and above the lower limit. */
         if( snr != VAL__BADD && snr > snr_lo ){

/* The three neighbouring pixels on row (j-1), and the left hand
   neighbouring pixel on row j, have already been checked on earlier
   passes round this loop. Check each of these four pixels in turn to see
   if they were flagged as being above the lower SNR limit. */
            itop1 = 0;
            for( ineb = 0; ineb < 4; ineb++ ) {

/* Get a pointer to the neighbouring clump index value, checking it is not off
   the edge of the array. */
               if( ineb == 0 ) {
                  ok = ( i > 0 );
               } else if( ineb == 1 ) {
                  ok = ( i > 0 && j > 0 );
               } else if( ineb == 2 ) {
                  ok = ( j > 0 );
               } else {
                  ok = ( i < dims[ 0 ] - 1 && j > 0 );
               }
               if( ok ) {
                  psn = ps + neb_offset[ ineb ];

/* If this neighbour is flagged as above the lower SNR limit (i.e. has a
   positive clump index), and the current pixel has not yet been assigned to
   an existing clump, assign the neighbour's clump index to the current pixel. */
                  if( *psn > 0 ) {
                     if( *ps == 0 ) {
                        *ps = *psn;

/* Find the clump index at the top of the tree containing the neighbouring pixel. */
                        itop1 = *psn;
                        while( table[ itop1 ] ) itop1 = table[ itop1 ];

/* If this neighbour is flagged as above the lower SNR limit, but the
   current pixel has already been assigned to an existing clump, the current
   pixel is adjacent to both clumps and so joins them into one. So record that
   this neighbours clump index should be associated with the clump index of
   the current pixel. */
                     } else {

/* We need to check first that the two clump indices are not already part
   of the same tree of associated clumps. Without this we could produce
   loops in the tree. Find the clump indices at the top of the tree
   containing the neighbouring pixel. */
                        itop2 = *psn;
                        while( table[ itop2 ] ) itop2 = table[ itop2 ];

/* If the two clumps are not in the same tree, indicate that the pixel
   index at the top of the tree for the neighbouring pixels clump index is
   associated with the central pixel's clump index. */
                        if( itop1 != itop2 ) table[ itop2 ] = *ps;
                     }
                  }
               }
            }

/* If the current pixel has no neighbours that are above the lower SNR
   limit, we start a new clump for the current pixel. */
            if( *ps == 0 ) {

/* Assign the next clump index to the current pixel, and then increment
   the next clump index. Report an error if we have reached the max
   allowable clump index value. */
               if( top == INT_MAX ) {
                  *status = SAI__ERROR;
                  errRep( "", "smf_snrmask: Too many low-SNR clumps found.",
                          status );
                  break;
               }
               *ps = top++;

/* Extend the table that holds the associations between clumps. This
   table has one element for each clump index (plus an unused element at the
   start for the unused clump index "0"). The value stored in this table
   for a given clump index is the index of another clump with which the
   first clump should be associated. If two clumps are associated it
   indicates that they are part of the same physical clump. Associations
   form a tree structure. A value of zero in this table indicates that
   the clump is either unassociated with any other clump, or is at the head
   of a tree of associated clumps. */
               table = astGrow( table, top, sizeof( *table ) );
               if( *status != SAI__OK ) break;
               table[ *ps ] = 0;
            }
         }
      }
   }

/* We now loop round the map again, this time looking for pixels that are
   above the higher SNR limit. */
   pm = map;
   pv = mapvar;
   ps = cindex;
   for( j = 0; j < dims[ 1 ]; j++ ) {
      for( i = 0; i < dims[ 0 ]; i++, pm++, pv++, ps++ ) {

/* Get the SNR value. */
         if( mapvar ) {
            if( *pm != VAL__BADD && *pv != VAL__BADD && *pv > 0.0 ){
               snr = *pm / sqrt( *pv );
            } else {
               snr = VAL__BADD;
            }
         } else {
            snr = *pm;
         }

/* If source can be negative as well as positive, use the absolute SNR. */
         if( abssnr && snr != VAL__BADD ) snr = fabs( snr );

/* Check the SNR is good and above the upper limit. */
         if( snr != VAL__BADD && snr > snr_hi ){

/* Since this pixel is above the higher SNR limit, it must also be above
   the lower SNR Limit, and so will have a non-zero clump index. We flag that
   this clump contains "source" pixels by storing a value of -1 for it in the
   clump association table. First record the original value for later use. */
            iass = table[ *ps ];
            table[ *ps ] = -1;

/* If this clump index is associated with another clump (i.e. had a non-zero
   value in the clump association table), the two clumps adjoins each other.
   So indicate that the second clump also contains "source" pixels by
   changing its table value to -1. Enter a loop to do this all the way up
   to the top of the association tree. Note, this is not necessarily all
   adjoining clumps, since we have only gone "up" the tree - there may be
   other adjoining clumps lower down the tree. */
            while( iass > 0 ) {
               itemp =  table[ iass ];
               table[ iass ] = -1;
               iass = itemp;
            }
         }
      }
   }

/* Now check all cumps to see if they adjoin a "source" clump. Note, no
   clumps are given the index zero, so we skip the first element of the
   table. */
   for( iclump = 1; iclump < top; iclump++ ) {
      iass = table[ iclump ];

/* Work up the tree of neighbouring clumps until we find a clump that has
   an index of 0 or -1. If 0, it means that we have reached the top of
   the tree without finding a "source" clump. If -1 it means we have
   reached a source clump. */
      while( iass > 0 ) {
         iass =  table[ iass ];
      }

/* If we have found a source clump, then all clumps above it in the tree
   should already be set to -1. We now walk up the tree from the current
   clump until we reach the source clump, marking all intermediate clumps
   as source clumps by setting them to -1 in the table. */
      if( iass < 0 ) {
         iass = iclump;
         while( iass > 0 ) {
            itemp =  table[ iass ];
            table[ iass ] = -1;
            iass = itemp;
         }

/* If no source clump was found, mark all intermediate clumps as
   non-source by setting theem to zero in the table. This may give us a
   little extra speed (maybe) since subsequent walks will terminate
   sooner. */
      } else {
         iass = iclump;
         while( iass > 0 ) {
            itemp =  table[ iass ];
            table[ iass ] = 0;
            iass = itemp;
         }
      }
   }

/* One last pass, to store the final mask values. We can multi-thread
   this bit. Create structures used to pass information to the worker
   threads. If we have more threads than rows, we will process one row
   in each thread and so we can reduce the number of threads used to
   equal the number of rows. */
   nworker = wf ? wf->nworker : 1;
   if( nworker > (int) dims[ 1 ] ) nworker = dims[ 1 ];
   job_data = astMalloc( nworker*sizeof( *job_data ) );

/* Check we can de-reference the job data pointer safely. */
   if( *status == SAI__OK ) {

/* Decide how many rows to process in each thread. */
      rowstep = dims[ 1 ]/nworker;
      if( rowstep == 0 ) rowstep = 1;

/* Set up the information needed by each thread, */
      for( iworker = 0; iworker < nworker; iworker++ ) {
         pdata = job_data + iworker;
         pdata->operation = 1;
         pdata->cindex = cindex;
         pdata->jlo = iworker*rowstep;
         if( iworker == nworker - 1 ) {
            pdata->jhi = dims[ 1 ] - 1;
         } else {
            pdata->jhi = pdata->jlo + rowstep - 1;
         }
         pdata->rowlen = dims[ 0 ];
         pdata->mask = mask;
         pdata->table = table;

/* Pass the job to the workforce for execution. */
         thrAddJob( wf, 0, pdata, smf1_snrmask_job, 0, NULL, status );
      }

/* Wait for the workforce to complete all jobs. */
      thrWait( wf, status );

/* Now clean up the very crinkly edges of the mask. Also, the mask may
   contain small holes which need to be cleaned. Clean it NCLEAN times. */
      for( iclean = 0; iclean < NCLEAN; iclean++ ) {

/* Clean the mask, putting the cleaned mask into "cindex" array. We
   exclude pixels in the first and last rows since they do not have a
   complete set of neighbours (each worker thread also ignores the first
   and last pixel in each row for the same reason). Decide how many rows
   to process in each thread. */
         rowstep = ( dims[ 1 ] - 2 )/nworker;
         if( rowstep == 0 ) rowstep = 1;

/* Modify the information needed by each thread, */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;
            pdata->operation = 2;
            pdata->jlo = iworker*rowstep + 1;
            if( iworker == nworker - 1 ) {
               pdata->jhi = dims[ 1 ] - 2;
            } else {
               pdata->jhi = pdata->jlo + rowstep - 1;
            }

/* Pass the job to the workforce for execution. */
            thrAddJob( wf, 0, pdata, smf1_snrmask_job, 0, NULL, status );
         }

/* Wait for the workforce to complete all jobs. */
         thrWait( wf, status );

/* Transfer the new mask from the "cindex" array back to the "mask" array.
   Add in any source pixels from the old mask if required. */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;
            pdata->maskold = maskold;
            pdata->operation = 3;
            thrAddJob( wf, 0, pdata, smf1_snrmask_job, 0, NULL, status );
         }
         thrWait( wf, status );

/* If an old mask was supplied, ensure any source pixels in the old mask
   are also source pixels in the new mask. */
         if( oldmask ) {
            for( iworker = 0; iworker < nworker; iworker++ ) {
               pdata = job_data + iworker;
               pdata->maskold = maskold;
               pdata->operation = 4;
               thrAddJob( wf, 0, pdata, smf1_snrmask_job, 0, NULL, status );
            }
            thrWait( wf, status );
         }
      }
   }

/* Free resources. */
   job_data = astFree( job_data );
   maskold = astFree( maskold );
   table = astFree( table );
   cindex = astFree( cindex );
}




static void smf1_snrmask_job( void *job_data, int *status ) {
/*
*  Name:
*     smf1_snrmask_job

*  Purpose:
*     Perform various parallel operations for snf_snrmask.

*  Invocation:
*     void smf1_snrmask_job( void *job_data, int *status )

*  Arguments:
*     job_data = void * (Given)
*        Pointer to the data needed by the job. Should be a pointer to a
*        smfSnrMaskJobData structure.
*     status = int * (Given and Returned)
*        Pointer to global status.

*/

/* Local Variables: */
   dim_t i;
   dim_t j;
   int *ps;
   int bgcount;
   int ineb;
   int neb_offset[ 8 ];
   smfSnrMaskJobData *pdata;
   unsigned char *pk;
   unsigned char *po;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer to the job data. */
   pdata = (smfSnrMaskJobData *) job_data;

/* Store mask values based on the clump index association table. */
   if( pdata->operation == 1  ) {

/* Loop round all pixels in the rows being processed by this thread. */
      ps = pdata->cindex + pdata->jlo*pdata->rowlen;
      pk = pdata->mask + pdata->jlo*pdata->rowlen;
      for( j = pdata->jlo; j <= pdata->jhi; j++ ) {
         for( i = 0; i < pdata->rowlen; i++,pk++,ps++ ) {

/* If the current pixel has a non-zero clump index in the mask, it means
   it is in a clump of pixels with map values above the lower SNR limit. */
            if( *ps > 0 ) {

/* If the clump index has a value of -1 in the clump association table,
   it means the clump containing the current pixel touches an area of the
   map that is above the higher SNR limit. So we set the final mask value
   to 0 for the current pixel, indicating that the pixel is within the
   "source" area of the map. Otherwise we set the mask value to one. */
               *pk = ( (pdata->table)[ *ps ] == -1 ) ? 0 : 1;

            } else {
               *pk = 1;
            }
         }
      }

/* Use a cellular automaton to clean up rows of the mask. Read from
   "mask" and write the cleaned rows to "cindex". */
   } else if( pdata->operation == 2  ) {

/* Set up the vector offsets to all eight neighbouring pixels. */
      neb_offset[ 0 ] = -1;                /* Left neighbour in current row */
      neb_offset[ 1 ] = -pdata->rowlen - 1;/* Left neighbour in lower row */
      neb_offset[ 2 ] = -pdata->rowlen;    /* Central neighbour in lower row */
      neb_offset[ 3 ] = -pdata->rowlen + 1;/* Right neighbour in lower row */
      neb_offset[ 4 ] = 1;                 /* Right neighbour in current row */
      neb_offset[ 5 ] = pdata->rowlen - 1; /* Left neighbour in upper row */
      neb_offset[ 6 ] = pdata->rowlen;     /* Central neighbour in upper row */
      neb_offset[ 7 ] = pdata->rowlen + 1; /* Right neighbour in upper row */

/* Loop round the rows of the mask to be processed by this thread. We
   exclude the first and last pixel in each row since they do not have a
   complete set of neighbours. The following pointers are initialised to
   point to the second pixel in the first row being processed by this
   thread. */
      ps = pdata->cindex + pdata->rowlen*pdata->jlo + 1;
      pk = pdata->mask + pdata->rowlen*pdata->jlo + 1;
      for( j = pdata->jlo; j <= pdata->jhi; j++ ) {
         for( i = 2; i < pdata->rowlen; i++,pk++,ps++ ) {

/* Count the number of "background" pixels in a square centred on the current
   pixel (background pixels have a value of +1 in the mask, source pixels
   have a value of zero). */
            bgcount = *pk;
            for( ineb =0; ineb < 8; ineb++ ) {
               bgcount += *( pk + neb_offset[ ineb ] );
            }

/* If more than 5 of the 9 pixels are background pixels, the central pixel is
   considered to be background. Otherwise, the central pixel is considered to
   be a source pixel in future. Note, this gives a small bias in favour
   of expanding the source regions. */
            *ps = ( bgcount > 5 );
         }

/* Skip the last pixel in the current ro, and the first pixel in the next
   row. */
         pk += 2;
         ps += 2;
      }

/* Transfer rows of the cleaned mask from the "cindex" array back to the
   "mask" array. */
   } else if( pdata->operation == 3 ) {

/* Loop round the rows of the mask to be processed by this thread. We
   exclude the first and last pixel in each row since we know their mask
   values have not changed (since they were excluded in the above
   "operation=2" code. The following pointers are initialised to point to
   the second pixel in the first row being processed by this thread. */
      ps = pdata->cindex + pdata->rowlen*pdata->jlo + 1;
      pk = pdata->mask + pdata->rowlen*pdata->jlo + 1;
      for( j = pdata->jlo; j <= pdata->jhi; j++ ) {
         for( i = 2; i < pdata->rowlen; i++,pk++,ps++ ) {
            *pk = ( *ps > 0 );
         }
         pk += 2;
         ps += 2;
      }

/* Add in any source pixels in the supplied old mask. */
   } else {
      pk = pdata->mask + pdata->rowlen*pdata->jlo;
      po = pdata->maskold + pdata->rowlen*pdata->jlo;
      for( j = pdata->jlo; j <= pdata->jhi; j++ ) {
         for( i = 0; i < pdata->rowlen; i++,pk++,po++ ) {
            if( *po == 0 ) *pk = 0;
         }
      }
   }
}

