#include "sae_par.h"
#include "prm_par.h"
#include "cupid.h"

/* Local Constants: */
/* =============== */
/* The number of pixels used to estimate the gradient.· */
#define GRADSTEP 3

void cupidREdges( size_t nel, double *dval, size_t *dpos, int *mask, int minpix,
                  double thresh, double noise, double rms, double flatslope,
                  int *status ){
/*
*+
*  Name:
*     cupidREdges

*  Purpose:
*     Identify peaks and peak edges in a 1D line of data.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidREdges( size_t nel, double *dval, size_t *dpos, int *mask,
*                       int minpix, double thresh, double noise, double rms,
*                       double flatslope, int *status )

*  Description:
*     This function finds the highest data value in the supplied 1D line
*     of data. It then works outwards on both sides of this peak position
*     looking for the edges of the peak. When found, the pixels
*     corresponding to the edge are flagged as edge pixels in the
*     supplied 3D mask array so long as they encompass more than a given
*     number of pixels, and the peak position is flagged as a peak in the
*     mask. The 1D line of data is then rescanned looking for the next
*     highest peak, and the edges of this peak are also flagged in the mask.
*     This procedure is repeated until all peaks above the given threshhold
*     value have been found.

*  Parameters:
*     nel
*        The number of elements in the "dval" and "dpos" arrays.
*     dval
*        Pointer to an array holding the data values at each point in the
*        1D line of data.
*     dpos
*        Pointer to an array holding the 1D vector index within the "mask"
*        array corresponding to each data value in the 1D line of data.
*        On exit, all pixels contained within any peak are set to -1.
*     mask
*        Pointer to the mask array. Edge pixels are set to 1 on exit.
*     minpix
*        The minimum number of pixels (including the edge pixels) which must
*        be spanned by a peak in order for its edges to be marked in the
*        returned mask.
*     thresh
*        The smallest significant peak height. The edges of peaks which are
*        below this limit are not included in the returned mask.
*     noise
*        Defines the data value below which pixels are considered to be in
*        the noise. A peak is considered to end when the peak value dips
*        below the "noise" value.
*     rms
*        The RMS noise in the data
*     flatslope
*        The minimum significant slope along a peak, in units of change
*        in data value per pixel.
*     status
*        Pointer to the inherited status value.

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
*     17-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   double *pd;        /* Pointer to next data value */
   double deltav;     /* Minimum significant change in data value */
   double maxval;     /* Maximum data value found so far */
   double minval;     /* Minimum data value found so far */
   double v;          /* Current data value */
   double vlast;      /* Previous data value */
   double vlim;       /* Data value marking start of a rise in data value */
   int hslok;         /* Significant slope found on upper slope? */
   int lo_ok;         /* Was "ilo" found before the start of the line? */
   int lslok;         /* Significant slope found on lower slope? */
   int up_ok;         /* Was "iup" found before the end of the line? */
   size_t *pp;        /* Pointer to next vector index value */
   size_t i;          /* Index within "dlow" */
   size_t ilo;        /* Index within "dlow" of lower edge */
   size_t iup;        /* Index within "dlow" of upper edge */
   size_t maxpos;     /* Index within "dlow" of maximum data value */
   size_t minpos;     /* Index within "dlow" of minimum data value */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Initialise the change in data value which is considered to be
   significant. */
   deltav = GRADSTEP*flatslope;

/* Enter a loop in which we look for a new peak in the 1D data line. */
   while( 1 ) {

/* Initialise the maximum value found so for in the remaining data values. */
      maxval = VAL__MIND;
      maxpos = 0;

/* Find the position and value at the highest remaining data value.
   Pixels which have already been included in a peak are flagged by the fact
   that the "dpos" array holds -1 (all valid 1D vector indices are larger
   than or equal to zero). */
      pd = dval;
      pp = dpos;
      for( i = 0; i < nel; i++, pd++, pp++ ) {
         if( *pd != VAL__BADD && *pp != -1 ) {
            if( *pd > maxval ) {
               maxval = *pd;
               maxpos = i;
            }
         }
      }

/* We have finished if there are no good pixels left or if the highest
   pixel value is below the noise level. */
      if( maxval < noise ) break;

/* Indicate we have not yet found any section of the curve which has a
   slope greater than flatslope. */
      hslok = 0;

/* Initialise the lowest value found so far in the peaks profile. */
      minval = maxval - deltav;
      minpos = maxpos;
      vlast = VAL__MIND;
      vlim = VAL__MAXD;

/* Loop round extending the peak to larger radii until the first significant
   minimum is found in the line of data. "i" is left holding the index of
   the first pixel which is not included in the current peak. */
      up_ok = 0;
      i = maxpos + 1;
      while( i < nel ) {

/* If this pixel has been included in an adjacent peak, we have reached
   the edge of the current peak. */
         if( dpos[ i ] == -1 ) {
            up_ok = 1;
            break;
         }

/* Get the pixel value. */
         v = dval[ i ];

/* Ignore bad pixels */
         if( v != VAL__BADD ) {

/* If both this pixel value and the previous pixel value are below the
   noise threshold, assume we have reached the noise background. */
            if( v < noise && vlast < noise ) {
               up_ok = 1;
               break;

/* Otherwise, if this value is lower by a significant amount than the
   previous lowest value, remember it. */
            } else if( v < minval ) {
               hslok = hslok || ( minval + deltav - v )/( i - minpos ) > flatslope;
               minval = v - deltav;
               minpos = i;
               vlim = v + 2*rms;

/* Otherwise, if this value is more than 2*RMS noise higher than the
   lowest value found so far, we assume that the lowest value found so far
   is the minimum point in the profile. */
            } else if( v > vlim ){
               i = minpos + 1;
               up_ok = 1;
               break;

/* Otherwise, if the lowest pixel has not changed for the past GRADSTEP
   pixels, assume we have reached the flat bit of the profile, so long as
   we have also had a steep bit (i.e. distinguish between the flat bit at
   the top of a peak and the flat background surrounding the peak). */
            } else if( i - minpos > GRADSTEP && hslok ) {
               i = minpos + 1;
               up_ok = 1;
               break;
            }

/* Save this pixel value for use on the next pass. */
            vlast = v;

         }

/* Increment the index of the current pixel. */
         i++;
      }

/* Store the index of the upper edge pixel. Note if no upper edge was
   found within the line. */
      iup = i - 1;

/* Indicate we have not yet found any section of the curve which has a
   slope greater than flatslope. */
      lslok = 0;

/* We now search for the lower edge pixel. Initialise the lowest value found
   so far in the peaks profile. */
      minval = maxval - deltav;
      minpos = maxpos;
      vlast = VAL__MIND;
      vlim = VAL__MAXD;

/* Loop round extending the peak to smaller radii until the first significant
   minimum is found in the line of data. "i" is left holding the index of
   the first pixel which is not included in the current peak. */
      lo_ok = 0;
      i = maxpos - 1;
      while( i >= 0  ) {

/* If this pixel has been included in an adjacent peak, we have reached
   the edge of the current peak. */
         if( dpos[ i ] == -1 ) {
            lo_ok = 1;
            break;
         }

/* Get the pixel value. */
         v = dval[ i ];

/* Ignore bad pixels */
         if( v != VAL__BADD ) {

/* If both this pixel value and the previous pixel value are below the
   noise threshold, assume we have reached the noise background. */
            if( v < noise && vlast < noise ) {
               lo_ok = 1;
               break;

/* Otherwise, if this value is lower by a significant amount than the previous
   lowest value, remember it. */
            } else if( v < minval ) {
               lslok = lslok || ( minval + deltav - v )/( minpos - i ) > flatslope;
               minval = v - deltav;
               minpos = i;
               vlim = v + 2*rms;

/* Otherwise, if this value is more than the 2*RMS noise higher than the
   lowest value found so far, we assume that the lowest value found so far
   is the minimum point in the profile. */
            } else if( v > vlim ){
               i = minpos - 1;
               lo_ok = 1;
               break;

/* Otherwise, if the lowest pixel has not changed for the past GRADSTEP
   pixels, assume we have reached the flat bit of the profile, so long as
   we have also had a steep bit. */
            } else if( minpos - i > GRADSTEP && lslok ) {
               i = minpos - 1;
               lo_ok = 1;
               break;
            }

/* Save this pixel value for use on the next pass. */
            vlast = v;

         }

/* Decrement the index of the current pixel. */
         i--;
      }

/* Store the index of the lower edge pixel. */
      ilo = i + 1;

/* Flag the relevant mask pixels as edge pixels, if they span sufficient
   pixels, and do not cross an edge of the data. */
      if( iup != maxpos && ilo != maxpos && iup - ilo >= minpix &&
          up_ok && lo_ok ) {
         mask[ dpos[ ilo ] ] = CUPID__KEDGE;
         mask[ dpos[ iup ] ] = CUPID__KEDGE;

/* If a sufficiently high gradient was achieved on either side of the peak,
   and if the peak position has not previously been flagged as an edge,
   increment the mask value by CUPID__KPEAK. */
         if( hslok && lslok && mask[ dpos[ maxpos ] ] != CUPID__KEDGE &&
             maxval >= thresh ) {
            mask[ dpos[ maxpos ] ] += CUPID__KPEAK;

         }
      }

/* Indicate that the intervening pixels have been allocated to a peak. */
      for( i = ilo; i <= iup; i++ ) dpos[ i ] = -1;
   }
}
