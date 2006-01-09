#include "sae_par.h"
#include "cupid.h"
#include "ndf.h"
#include "prm_par.h"
#include <math.h>

double *cupidClumpDesc( int indf, double *cpars, const char ***names,
                        int *ncpar ){
/*
*  Name:
*     cupidClumpDesc

*  Purpose:
*     Get the parameters describing a single clump.

*  Synopsis:
*     double *cupidClumpDesc( int indf, double *cpars, const char ***names,
*                             int *ncpar )

*  Description:
*     This function calculates the parameters describing a single clump,
*     on the basis of the clump data values in the supplied NDF. If the
*     NDF has "n" pixel axes, the parameters are returned in the following 
*     order:
*
*        0  - (n-1) : The pixel coords of the clump centroid
*        n  - (2n-1): The pixel indices of the clump peak value
*        2n - (3n-1): The clump size (in pixels) on each pixel axis. This is 
*                     the standard deviation of the pixel axis value about the 
*                     centroid position, weighted by the pixel values.
*        3n         : The total data value in the clump
*        3n + 1     : The peak data value in the clump
*        3n + 2     : The total number of pixels within the clump.

*  Parameters:
*     indf
*        Identifier for an NDF holding the data values associated with
*        the clump. Any pixels which are not part of the clump should be
*        set bad. 
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

*  Returned Value:
*     A pointer to the array holding the returned clump parameters. This
*     will be the same as "cpars" if cpars is not NULL, or will be a
*     pointer to a newly allocated dynamic array otherwise (the array
*     should be freed using astFree when no longer needed).

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     5-DEC-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   double *ipd;             /* Pointer to start of data array */
   double *pd;              /* Pointer to next element of data array */
   double *ret;             /* Returned list of parameters */
   double d;                /* Height above background */
   double dmax;             /* Max value in data array */
   double dmin;             /* Min value in data array */
   double s;                /* Sum of weights */
   double sx2;              /* Sum of weighted squared X pixel indices */
   double sx;               /* Sum of weighted X pixel indices */
   double sy2;              /* Sum of weighted squared X pixel indices */
   double sy;               /* Sum of weighted Y pixel indices */
   double sz2;              /* Sum of weighted squared X pixel indices */
   double sz;               /* Sum of weighted Z pixel indices */
   double v;                /* Temp value */
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
      pnames[ 0 ] = "CenX";
      pnames[ ndim ] = "PeakX";
      pnames[ 2*ndim ] = "SizeX";

      if( ndim > 1 ) {
         pnames[ 1 ] = "CenY";
         pnames[ 1 + ndim ] = "PeakY";
         pnames[ 1 + 2*ndim ] = "SizeY";

         if( ndim > 2 ) {
            pnames[ 2 ] = "CenZ";
            pnames[ 2 + ndim ] = "PeakZ";
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
         ret[ 0 ] = sx/s;
         ret[ ndim ] = px;

         v = sx2/s - ret[ 0 ]*ret[ 0 ];
         ret[ 2*ndim ] = ( v > 0 ) ? sqrt( v ) : 0.0;

         if( ndim > 1 ) {
            ret[ 1 ] = sy/s;
            ret[ 1 + ndim ] = py;
            v = sy2/s - ret[ 1 ]*ret[ 1 ];
            ret[ 1 + 2*ndim ] = ( v > 0 ) ? sqrt( v ) : 0.0;

            if( ndim > 2 ) {
               ret[ 2 ] = sz/s;
               ret[ 2 + ndim ] = pz;
               v = sz2/s - ret[ 2 ]*ret[ 2 ];
               ret[ 2 + 2*ndim ] = ( v > 0 ) ? sqrt( v ) : 0.0;
            }
         }

      } else {
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
      ret[ 3*ndim + 1 ] = dmax;
      ret[ 3*ndim + 2 ] = n;

   }

/* Unmap the NDF data array */
   ndfUnmap(  indf, "Data", status );

/* Return the array of clump parameters. */
   return ret;

}

