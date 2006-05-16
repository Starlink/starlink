#include "sae_par.h"
#include "cupid.h"
#include "ndf.h"
#include "prm_par.h"
#include <math.h>

double *cupidClumpDesc( int indf, double beamcorr[ 3 ], double *cpars, 
                        const char ***names, int *ncpar, int *ok ){
/*
*+
*  Name:
*     cupidClumpDesc

*  Purpose:
*     Get the parameters describing a single clump.

*  Language:
*     Starlink C

*  Synopsis:
*     double *cupidClumpDesc( int indf, double beamcorr[ 3 ], double *cpars, 
*                             const char ***names, int *ncpar, int *ok )

*  Description:
*     This function calculates the parameters describing a single clump,
*     on the basis of the clump data values in the supplied NDF. If the
*     NDF has "n" pixel axes, the parameters are returned in the following 
*     order:
*
*        0  - (n-1) : The pixel coords of the clump peak value
*        n  - (2n-1): The pixel coords of the clump centroid
*        2n - (3n-1): The clump size (in pixels) on each pixel axis. This is 
*                     the standard deviation of the pixel axis value about the 
*                     centroid position, weighted by the pixel values, then 
*                     corrected to remove the effect of the instrumental 
*                     smoothing specified in "beamcorr".
*        3n         : The total data value in the clump
*        3n + 1     : The peak value in the clump. This will be larger than 
*                     the peak data value by a factor determined by the
*                     "beamcorr" values, to take account of the lowering
*                     of the peak value caused by the instrumental smoothing.
*        3n + 2     : The total number of pixels within the clump.

*  Parameters:
*     indf
*        Identifier for an NDF holding the data values associated with
*        the clump. Any pixels which are not part of the clump should be
*        set bad. 
*     beamcorr
*        An array holding the FWHM (in pixels) describing the instrumental 
*        smoothing along each pixel axis. The clump widths stored in the 
*        output catalogue are reduced to correct for this smoothing.
*     cpars
*        Pointer to an array in which to store the clump parameters. If
*        this is NULL, a new dynamic array is allocated and a pointer to
*        it is returned as the function value. This array should have at
*        least "*ncpar" elements.
*     names
*        Pointer to a location at which to return a pointer to an array
*        of constant character strings. The number of pointers returned in 
*        this array will be returned in "*ncpar". Each string is the name
*        associated with the corresponding parameter value returned in "cpars".
*     ncpar
*        Pointer to an int in which to return the number of parameters
*        describing the clump.
*     ok
*        Pointer to an int in which to return a flag indicating if the
*        clump can be used or not. This will be set to zero if the clump
*        size is zero after correction for the effect of beam smoothing.

*  Returned Value:
*     A pointer to the array holding the returned clump parameters. This
*     will be the same as "cpars" if cpars is not NULL, or will be a
*     pointer to a newly allocated dynamic array otherwise (the array
*     should be freed using astFree when no longer needed).

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-DEC-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   double *ipd;             /* Pointer to start of data array */
   double *pd;              /* Pointer to next element of data array */
   double *ret;             /* Returned list of parameters */
   double d;                /* Height above background */
   double dmax;             /* Max value in data array */
   double dmin;             /* Min value in data array */
   double peakfactor;       /* Factor by which to increase the peak value */
   double s;                /* Sum of weights */
   double sx2;              /* Sum of weighted squared X pixel indices */
   double sx;               /* Sum of weighted X pixel indices */
   double sy2;              /* Sum of weighted squared X pixel indices */
   double sy;               /* Sum of weighted Y pixel indices */
   double sz2;              /* Sum of weighted squared X pixel indices */
   double sz;               /* Sum of weighted Z pixel indices */
   double v0;               /* Variance before corr'n for instrumental blurring */
   double v;                /* Variance after corr'n for instrumental blurring */
   int i;                   /* Pixel index on 1st pixel axis */
   int j;                   /* Pixel index on 2nd pixel axis */
   int k;                   /* Pixel index on 3rd pixel axis */
   int lbnd[ 3 ];           /* Lower NDF pixel bounds */   
   int n;                   /* Number of good pixels indices */
   int ndim;                /* Number of pixel axes */   
   int nel;                 /* Number of elements in mapped array */
   int px;                  /* X pixel index at peak value */
   int py;                  /* Y pixel index at peak value */
   int pz;                  /* Z pixel index at peak value */
   int ubnd[ 3 ];           /* Upper NDF pixel bounds */   

   static const char *pnames[ 10 ];/* Parameter names to return */

/* Initialise. */
   ret = cpars;
   *ok = 0;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Get the bounds of the NDF. */
   ndfBound(  indf, 3, lbnd, ubnd, &ndim, status );

/* If no pointer was supplied, do some initialisation. */
   if( !ret ) {

/* Determine the number of parameters needed to describe the clump. */
      *ncpar = ndim*3 + 3; 

/* Allocate memory for this number of parameters. */
      ret = astMalloc( sizeof( double )*( *ncpar ) );

/* Store the parameter names. */ 
      pnames[ 0 ] = "PeakX";
      pnames[ ndim ] = "CenX";
      pnames[ 2*ndim ] = "SizeX";

      if( ndim > 1 ) {
         pnames[ 1 ] = "PeakY";
         pnames[ 1 + ndim ] = "CenY";
         pnames[ 1 + 2*ndim ] = "SizeY";

         if( ndim > 2 ) {
            pnames[ 2 ] = "PeakZ";
            pnames[ 2 + ndim ] = "CenZ";
            pnames[ 2 + 2*ndim ] = "SizeZ";
         }
      }
       
      pnames[ 3*ndim ] = "Sum";
      pnames[ 1 + 3*ndim ] = "Peak";
      pnames[ 2 + 3*ndim ] = "Area";

      *names = pnames;
   }

/* Map the NDF data array */
   ndfMap(  indf, "Data", "_DOUBLE", "READ", (void *) &ipd, &nel, status );
   if( ipd ) {

/* Find the minimum and maximum pixel value, and the position of the
   peak. We can treat the NDF as if it were 3-dimensional even if it is
   actually 1 or 2 dimension, since ndfBound fills in unused trailing
   elements of "lbnd" and "ubnd" with the value 1. */
      dmin = VAL__BADD;
      dmax = VAL__BADD;

      px = 0;
      py = 0;
      pz = 0;

      pd = ipd;
      for( k = lbnd[ 2 ]; k <= ubnd[ 2 ]; k++ ) {
         for( j = lbnd[ 1 ]; j <= ubnd[ 1 ]; j++ ) {
            for( i = lbnd[ 0 ]; i <= ubnd[ 0 ]; i++, pd++ ) {
               if( *pd != VAL__BADD ) {
                  if( dmin == VAL__BADD ) {
                     dmin = *pd;
                     dmax = *pd;
                     px = i;
                     py = j;
                     pz = k;

                  } else if( *pd < dmin ) {
                     dmin = *pd;

                  } else if( *pd > dmax ) {
                     dmax = *pd;
                     px = i;
                     py = j;
                     pz = k;

                  }                      
               }            
            }
         }
      }

/* Find the other required statistics of the data values. */
      sx = 0;
      sy = 0;
      sz = 0;
      sx2 = 0;
      sy2 = 0;
      sz2 = 0;
      s = 0;
      n = 0;
      pd = ipd;
      for( k = lbnd[ 2 ]; k <= ubnd[ 2 ]; k++ ) {
         for( j = lbnd[ 1 ]; j <= ubnd[ 1 ]; j++ ) {
            for( i = lbnd[ 0 ]; i <= ubnd[ 0 ]; i++, pd++ ) {
               if( *pd != VAL__BADD ) {
                  d = *pd - dmin;

                  sx += d*i;
                  sy += d*j;
                  sz += d*k;

                  sx2 += d*i*i;
                  sy2 += d*j*j;
                  sz2 += d*k*k;

                  s += d;

                  n++;
               }            
            }
         }
      }

/* Calculate and store the clump parameters */
      if( s != 0 ) {
         ret[ 0 ] = px - 0.5;
         ret[ ndim ] = sx/s;

         v0 = sx2/s - ret[ ndim ]*ret[ ndim ];
         if( v0 <= 0.0 ) v0 = 0.25;

         v = v0 - beamcorr[ 0 ]*beamcorr[ 0 ]/5.5451774;
         peakfactor = v0/v;
         *ok = ( v > 0 );
         ret[ 2*ndim ] = ( *ok ) ? sqrt( v ) : 0.0;

         if( ndim > 1 ) {
            ret[ 1 ] = py - 0.5;
            ret[ 1 + ndim ] = sy/s;

            v0 = sy2/s - ret[ 1 + ndim ]*ret[ 1 + ndim ];
            if( v0 <= 0.0 ) v0 = 0.25;

            v = v0 - beamcorr[ 1 ]*beamcorr[ 1 ]/5.5451774;
            peakfactor *= v0/v;
            if( v > 0 ) {
               ret[ 1 + 2*ndim ] = sqrt( v );
            } else {
               ret[ 1 + 2*ndim ] = 0.0;
               *ok = 0;
            }

            if( ndim > 2 ) {
               ret[ 2 ] = pz - 0.5; 
               ret[ 2 + ndim ] = sz/s;

               v0 = sz2/s - ret[ 2 + ndim ]*ret[ 2 + ndim ];
               if( v0 <= 0.0 ) v0 = 0.25;

               v = v0 - beamcorr[ 2 ]*beamcorr[ 2 ]/5.5451774;
               peakfactor *= v0/v;

               if( v > 0 ) {
                  ret[ 2 + 2*ndim ] = sqrt( v );
               } else {
                  ret[ 2 + 2*ndim ] = 0.0;
                  *ok = 0;
               }
            }
         }

      } else {
         peakfactor = 1.0;
         ret[ 0 ] = VAL__BADD;
         ret[ ndim ] = VAL__BADD;
         ret[ 2*ndim ] = VAL__BADD;

         if( ndim > 1 ) {
            ret[ 1 ] = VAL__BADD;
            ret[ 1 + ndim ] = VAL__BADD;
            ret[ 1 + 2*ndim ] = VAL__BADD;

            if( ndim > 2 ) {
               ret[ 2 ] = VAL__BADD;
               ret[ 2 + ndim ] = VAL__BADD;
               ret[ 2 + 2*ndim ] = VAL__BADD;
            }
         }
      }

      ret[ 3*ndim ] = s + n*dmin;
      ret[ 3*ndim + 1 ] = dmax*( (peakfactor > 0.0) ? sqrt( peakfactor ) : 1.0 );
      ret[ 3*ndim + 2 ] = n;

   }

/* Unmap the NDF data array */
   ndfUnmap(  indf, "Data", status );

/* Return the array of clump parameters. */
   return ret;

}
