/*+
*  Name:
*     smf_flag_rings

*  Purpose:
*     Flag samples that show strong filter ringing.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     int smf_flag_rings( double *data, size_t stride, int nel, int box1,
*                         int box2, double nsigma, int wing, int minsize,
*                         smf_qual_t *qual, smf_qual_t qmask,
*                         const unsigned char *mask, const int *lut,
*                         int *status )

*  Arguments:
*     data = double * (Given and Returned)
*        Pointer to the 1D array to be checked for ringing.
*     stride = (Given)
*        Stride between elements in the data array.
*     nel = int (Given)
*        Number of points in the input data.
*     box1 = int (Given)
*        The size of the first filter (in array elements). This should
*        normally correspond to the FLT filter width.
*     box2 = int (Given)
*        The size of the second filter (in array elements). This should
*        be several times the value of "box1".
*     nsigma = double (Given)
*        The number of standard deviations at which to flag samples.
*     wing = int (Given)
*        The number of extra samples to flag on either side of a section
*        flagged as oscillatory.
*     minsize = int (Given)
*        The minimum number of samples in a section of contiguous ringing
*        samples that will be flagged. Section shorter than this size
*        will never be flagged.
*     qual = smf_qual_t * (Given and Returned)
*        The QUALITY array associated with the data array. On entry, it
*        is assumed that no samples are flagged with SMF__Q_RING. On exit,
*        samples that are flagged as ringing samples by this function are
*        assigned the quality SMF__Q_RING
*     qmask = smf_qual_t (Given)
*        Use with qual to define which samples should be included in the
*        filtered values.
*     mask = const unsigned char * (Given)
*        NULL, or a pointer to a 2D mask which is zero in source regions,
*        and non-zero for background regions. The mask should have the same
*        pixel bounds as the output map. If supplied, samples that fall
*        within source regions are never flagged by this function (i.e.
*        only background pixels are flagged).
*     lut = const int * (Given)
*        Only used if "mask" is not NULL. It should be a pointer to the
*        look-up-tyable that gives the map pixel index for each sample index.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     The number of samples to which the SMF__Q_RING quality were assigned.

*  Description:
*     This routine locates sections of the supplied data array that appear
*     to oscillate about zero with a period of "box1" or less, and
*     assigns the flag SM__Q_RING to them in the supplied quality array.

*  Authors:
*     David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     17-OCT-2013 (DSB):
*        Initial version.
*     11-DEC-2015 (DSB):
*        - Rename old argument "mask" as "qmask".
*        - Add new arguments "mask" and "lut".
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2013 Science & Technology Facilities Council.

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
#include "sae_par.h"
#include "ast.h"

/* SMURF includes */
#include "smf.h"

int smf_flag_rings( double *data, size_t stride, int nel, int box1,
                    int box2, double nsigma, int wing, int minsize,
                    smf_qual_t *qual, smf_qual_t qmask,
                    const unsigned char *mask, const int *lut, int *status ){

/* Local variables */
   const int *pl;
   double *p1;
   double *p2;
   double *work1;
   double *work2;
   double mean;
   double stddev;
   double thresh;
   int end;
   int endl;
   int i;
   int j;
   int result;
   int start;
   int state;
   smf_qual_t *pq;

/* Initialise returned value. */
   result = 0;

/* Check the inherited status. */
   if (*status != SAI__OK) return result;

/* Take a copy of the supplied data array so that they are left unchanged. */
   work1 = astMalloc( nel*sizeof( *work1 ) );
   p1 = work1;
   p2 = data;
   for( i = 0; i < nel; i++ ) {
      *(p1++) = *data;
      data += stride;
   }

/* Smooth the data stream using a filter of width "box1" samples. This
   gives the low frequency background to the data array. The smoothed
   values are stored back in "work1". */
   smf_boxcar1D( work1, nel, 1, box1, qual, qmask, 1, NULL, status );

/* We are trying to identify sections of the above array that oscillate
   about zero - i.e. have a mean value close to zero over some larger
   period. So we smooth it again using a filter of width "box2" samples
   (which should be much larger than "box1"), and then subtract the
   smoothed version from the original. This ensures we have a mean of
   zero in "work1". We then square these values as the next step will
   be to find the RMS amplitude of any oscillations present in "work1". */
   work2 = astStore( NULL, work1, nel*sizeof( *work2 ) );
   smf_boxcar1D( work2, nel, 1, box2, qual, qmask, 1, NULL, status );

   if( *status == SAI__OK ) {
      p1 = work1;
      p2 = work2;
      for( i = 0; i < nel; i++,p1++,p2++ ) {
         if( *p1 != VAL__BADD && *p2 != VAL__BADD ) {
            *p1 -= *p2;
            *p1 *= *p1;
         } else {
            *p1 = VAL__BADD;
         }
      }
   }

/* Now we seek the RMS amplitude of any oscillations, so smooth the square
   of the zero-mean low frequency background, using a large filter. */
   smf_boxcar1D( work1, nel, 1, box2, qual, qmask, 1, NULL, status );

/* Find the clipped mean and standard deviation of these mean squared
   amplitudes. */
   mean = smf_sigmaclipD( nel, work1, NULL, 3.0, 3, &stddev, status );

/* Now flag samples that have a mean squared amplitude greater than the
   mean plus "nsigma" standard deviations. Also flag extra samples at
   each end of each contiguous section of flagged samples. */
   thresh = mean + nsigma*stddev;

   if( *status == SAI__OK ) {
      endl = 0;
      p1 = work1;
      state = 0;
      for( i = 0; i < nel; i++,p1++ ) {
         if( *p1 != VAL__BADD ) {
            if( state == 0 ) {
               if( *p1 > thresh ) {
                  start = i;
                  state = 1;
               }
            } else if( state == 1 ) {
               if( *p1 < thresh ) {
                  end = i;

                  if( end - start + 1 > minsize ) {
                     start -= wing;
                     if( start < 0 ) start = 0;

                     end += wing;
                     if( end > nel ) end = nel;

                     for( j = start; j < end; j++ ) {
                        qual[ j ] |= SMF__Q_RING;
                     }

                     if( start < endl ) {
                        result += end - endl;
                     } else {
                        result += end - start + 1;
                     }
                     endl = end;
                  }

                  state = 0;
               }
            }
         }
      }
   }

/* If a mask was supplied, ensure that no samples that correspond to source
   regions within the supplied mask are flagged by SMF__Q_RING. */
   if( mask ) {
      pq = qual;
      pl = lut;
      for( i = 0; i < nel; i++ ) {
         if( *pl != VAL__BADI && mask[ *pl ] == 0 ) {
            *pq &= ~SMF__Q_RING;
         }
         pq += stride;
         pl += stride;
      }
   }

/* Free resources. */
   work1 = astFree( work1 );
   work2 = astFree( work2 );

/* Return the number of flagged samples. */
   return result;
}
