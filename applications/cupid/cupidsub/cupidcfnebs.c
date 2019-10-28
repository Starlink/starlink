#include <limits.h>
#include "sae_par.h"
#include "cupid.h"
#include "prm_par.h"
#include <stdio.h>

/* Local Macros */


/* This macro checks the pixel with index "iv" (a neighbour of the central
   pixel) to see if it belongs to a PixelSet. If it does, the index of the
   PixelSet is either returned in "*il2" (if the PixelSet was defined at a
   higher contour level and the peak of this PixelSet is closer than any
   other neighbouring PixelSet defined at a higher contour level), or is
   appended to the end of the list of indices returned in "il" (if the
   PixelSet was defined at the same contour level as the central pixel).
   In this second case, the index is also returned in "*il1" if it is the
   lowest such index yet encountered. */
#define CHECK_NEIGHBOUR \
         if( ipa[ iv ] != CUPID__CFNULL ) { \
            clump_index = ipa[ iv ]; \
            if( clump_index < hindex ) { \
               (*n2)++; \
\
               if( *il2 == CUPID__CFNULL ) { \
                  *il2 = clump_index; \
                  d1 = VAL__BADD; \
\
               } else { \
                   if( d1 == VAL__BADD ) { \
                      p = clumps[ *il2 ]->peak; \
                      dx = p[ 0 ] - x[ 0 ]; \
                      dy = p[ 1 ] - x[ 1 ]; \
                      dz = p[ 2 ] - x[ 2 ]; \
                      d1 = dx*dx + dy*dy + dz*dz; \
                   } \
\
                   p = clumps[ clump_index ]->peak; \
                   dx = p[ 0 ] - x[ 0 ]; \
                   dy = p[ 1 ] - x[ 1 ]; \
                   dz = p[ 2 ] - x[ 2 ]; \
                   d2 = dx*dx + dy*dy + dz*dz; \
\
                   if( d2 < d1 ) { \
                      *il2 = clump_index; \
                      d1 = d2; \
                   } \
               } \
            } else { \
               i1[ (*n1)++ ] = clump_index; \
               if( clump_index < *il1 ) *il1 = clump_index; \
            } \
         }

#define CHECK_UPNEIGHBOUR(ii) \
\
/* Check the upper neighbour is not off the edge of the array. */ \
      if( x[ ii ] < dims[ ii ] ) { \
\
/* Modify the 1D vector index and nD GRID coords to represent the coords \
   of the upper neighbour on axis "ii". */ \
         iv += skip[ ii ]; \
         x[ ii ]++; \
\
/* Check the neighbour. */ \
         CHECK_NEIGHBOUR; \
\
/* Revert the 1D vector index and nD GRID coords to the original values. */ \
         iv -= skip[ ii ]; \
         x[ ii ]--; \
      }



#define CHECK_LONEIGHBOUR(ii) \
\
/* Check the lower neighbour is not off the edge of the array. */ \
      if( x[ ii ] > 1 ) { \
\
/* Modify the 1D vector index and nD GRID coords to represent the coords \
   of the lower neighbour on axis "ii". */ \
         iv -= skip[ ii ]; \
         x[ ii ]--; \
\
/* Check the neighbour. */ \
         CHECK_NEIGHBOUR; \
\
/* Revert the 1D vector index and nD GRID coords to the original values. */ \
         iv += skip[ ii ]; \
         x[ ii ]++; \
      }


#define CHECK_NEIGHBOURS(ii) \
   CHECK_LONEIGHBOUR(ii); \
   CHECK_UPNEIGHBOUR(ii);




void cupidCFNebs( int *ipa, hdsdim iv, hdsdim x[3], int ndim, hdsdim dims[3],
                  hdsdim skip[3], hdsdim hindex, int perspectrum, int naxis,
                  int *n1, int *il1, int i1[27], int *n2, int *il2,
                  CupidPixelSet **clumps, int *status ){
/*
*+
*  Name:
*     cupidCFNebs

*  Purpose:
*     Check if the neighbours of a specified pixel are already in a PixelSet.

*  Language:
*     Starlink C

*  Synopsis:
*     void cupidCFNebs( int *ipa, hdsdim iv, hdsdim x[3], int ndim, hdsdim dims[3],
*                       hdsdim skip[3], hdsdim hindex, int perspectrum, int naxis,
*                       int *n1, int *il1, int i1[27], int *n2, int *il2,
*                       CupidPixelSet **clumps, int *status )

*  Description:
*     This function returns information about which, if any, PixelSets
*     adjoin a given pixel.

*  Parameters:
*     ipa
*        Pointer to the start of the array holding the integer index
*        (if any) associated with each pixel in the data array. This
*        shows which clump each pixel belongs to (each clump is identified
*        by a unique integer index). The array should be the same shape and
*        size as the data array. Pixels which have not yet been assigned
*        to a clump are marked with the integer value CUPID__CFNULL.
*     iv
*        The 1D vector index of the pixel within the data array and the
*        ipa array.
*     x
*        The GRID indices at the centre of the pixel.
*     ndim
*        The number of significant pixel axes in the data array.
*     dims
*        The number of pixels along each pixel axis in the data array.
*        This is padded with trailing 1's if "ndim" is less than 3.
*     skip
*        The increment in 1D vector index required to move a distance of 1
*        pixel along each axis. This allows conversion between indexing
*        the array using a single 1D vector index and using nD coords. This
*        array should have 3 elements even if "ndim" is less than 3, and
*        the extra elements should be filled with zero's.
*     hindex
*        If a PixelSet has an index greater than or equal to "hindex"
*        then the PixelSet is defined at the contour level containing the
*        pixel specified by "iv". Otherwise, the PixelSet is defined at a
*        higher contour level.
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
*     n1
*        Location at which to return the number of PixelSets defined at
*        the current contour level which adjoin the pixel specified by "iv".
*     i11
*        Location at which to return the lowest index of any PixelSets
*        defined at the current contour level which adjoin the pixel
*        specified by "iv". Returned equal to CUPID__CFNULL if "*n1" is
*        zero.
*     i1
*        The first "*n1" elements of this array will be returned holding
*        the indices of all PixelSets defined at the current contour level
*        which adjoin the pixel specified by "iv".
*     n2
*        Location at which to return the number of PixelSets defined at
*        a higher contour level which adjoin the pixel specified by "iv".
*     i12
*        Location at which to return the index of a PixelSet defined at a
*        higher contour level which adjoins the pixel specified by "iv".
*        Returned equal to CUPID__CFNULL if no such PixelSets adjoin the
*        pixel. If there is more than one such PixelSet, the returned
*        PixelSet is the one which has the closest peak to the pixel
*        being tested.
*     clumps
*        Array holding a list of pointers to the currently existing
*        PixelSets. The pixelSet identified by index "id" should be held
*        in "clumps[id]".
*     status
*        Pointer to the inherited status value.

*  Copyright:
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     11-JAN-2006 (DSB):
*        Original version.
*     17-SEP-2007 (DSB):
*        Added "perspectrum" parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   double d1;           /* Distance from pixel to nearest PixelSet peak */
   double d2;           /* Distance from pixel to other PixelSet peak */
   double dx;           /* X offset between pixel and PixelSet peak */
   double dy;           /* Y offset between pixel and PixelSet peak */
   double dz;           /* Z offset between pixel and PixelSet peak */
   hdsdim *p;           /* Pointer to array holding peak position */
   hdsdim i0;           /* 1D vector index of central pixel */
   int clump_index;     /* Index of neighbouring clump */
   int i;               /* Axis index */
   int velax;           /* Zero based velocity axis index */

/* Initialise */
   *n1 = 0;
   *n2 = 0;
   *il1 = INT_MAX;
   *il2 = CUPID__CFNULL;
   d1 = VAL__BADD;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

/* Store the zero based velocity axis index, if needed. */
   velax = perspectrum - 1;

/* Save the 1D vector index of the point whose neighbours are being tested. */
   i0 = iv;

/* We now check each immediate neighbour of the supplied pixel. First
   check the pixels which are 1 pixel away along only one of the three axes.
   This uses local macros defined at the start of this file. If we are
   processing spectra independently, only check for neighbours on the
   spectral axis. */
   if( !perspectrum ) {
      for( i = 0; i < ndim; i++ ) {
         CHECK_NEIGHBOURS(i);
      }
   } else {
      CHECK_NEIGHBOURS(velax);
   }

/* If required, now check the pixels which are 1 pixel away along two axes.
   For a naxis value of 2, "ndim" must be either 2 or 3 (it cannot be 1). */
   if( naxis >= 2 && !perspectrum ) {

/* If not at the upper edge on axis 0, get the 1D and nD coords at the upper
   neighbour on axis 0. */
      if( x[ 0 ] < dims[ 0 ] ) {
         iv += skip[ 0 ];
         x[ 0 ]++;

/* Extend the PixelSet to include the neighbours on the 2nd axis. */
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

/* Return CUPID__CFNULL for "il1" if no PixelSets found. */
   if( !*n1 ) *il1 = CUPID__CFNULL;

}

#undef CHECK_NEIGHBOUR
#undef CHECK_UPNEIGHBOUR
#undef CHECK_LONEIGHBOUR
#undef CHECK_NEIGHBOURS
