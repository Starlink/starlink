#include "sae_par.h"
#include "mers.h"
#include "cupid.h"
#include "ast.h"
#include "prm_par.h"
#include "ndf.h"
#include "star/hds.h"
#include <math.h>

HDSLoc *cupidReinhold( int type, int ndim, int *slbnd, int *subnd, void *ipd,
                     double *ipv, double rms, AstKeyMap *config, int velax,
                     int ilevel, double beamcorr[ 3 ] ){
/*
*  Name:
*     cupidReinhold

*  Purpose:
*     Identify clumps of emission within a 1, 2 or 3 dimensional NDF using
*     the REINHOLD algorithm.

*  Synopsis:
*     HDSLoc *cupidReinhold( int type, int ndim, int *slbnd, int *subnd, 
*                          void *ipd, double *ipv, double rms, 
*                          AstKeyMap *config, int velax, int ilevel,
*                          double beamcorr[ 3 ] )

*  Description:
*     This function identifies clumps within a 1, 2 or 3 dimensional data
*     array using the REINHOLD algorithm, developed by Kim Reinhold at
*     JAC. This algorithm identifies the boundaries between clumps by
*     looking for minima in 1D sections through the data. No a priori clump 
*     profile is assumed. In this algorithm, clumps never overlap.

*  Parameters:
*     type
*        An integer identifying the data type of the array values pointed to 
*        by "ipd". Must be either CUPID__DOUBLE or CUPID__FLOAT (defined in
*        cupid.h).
*     ndim
*        The number of dimensions in the data array. Must be 2 or 3.
*     slbnd
*        Pointer to an array holding the lower pixel index bound of the
*        data array on each axis.
*     subnd
*        Pointer to an array holding the upper pixel index bound of the
*        data array on each axis.
*     ipd
*        Pointer to the data array. The elements should be stored in
*        Fortran order. The data type of this array is given by "itype".
*     ipv
*        Pointer to the input Variance array, or NULL if there is no Variance
*        array. The elements should be stored in Fortran order. The data 
*        type of this array is "double".
*     rms
*        The default value for the global RMS error in the data array.
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.
*     velax
*        The index of the velocity axis in the data array (if any). Only
*        used if "ndim" is 3. 
*     ilevel
*        Amount of screen information to display (in range zero to 6).
*     beamcorr
*        An array in which is returned the FWHM (in pixels) describing the
*        instrumental smoothing along each pixel axis. The clump widths
*        stored in the output catalogue are reduced to correct for this
*        smoothing.

*  Retured Value:
*     A locator for a new HDS object which is an array of NDF structures.
*     Each NDF will hold the data values associated with a single clump 
*     and will be the smallest possible NDF that completely contains the 
*     corresponding clump. Pixels not in the clump will be set bad. The 
*     pixel origin is set to the same value as the supplied NDF.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     16-JAN-2006 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   AstKeyMap *rconfig;  /* Configuration parameters for this algorithm */
   HDSLoc *ret;         /* Locator for the returned array of NDFs */
   double *pd;          /* Pointer to next element of data array */
   double *peakvals;    /* Pointer to array holding clump peak values */
   double cathresh;     /* Threshold for second cellular automata */
   double flatslope;    /* Minimum significant slope at edge of a peak */
   double noise;        /* Noise level */
   double pv;           /* Pixel value */
   double thresh;       /* Minimum peak value to be considered */
   float *pf;           /* Pointer to next element of data array */
   int *clbnd;          /* Array holding lower axis bounds of all clumps */
   int *cubnd;          /* Array holding upper axis bounds of all clumps */
   int *igood;          /* Pointer to array holding usable clump indices */
   int *m1;             /* Pointer to mask array */
   int *m2;             /* Pointer to mask array */
   int *m3;             /* Pointer to mask array */
   int *mask2;          /* Pointer to array marking out edge pixels */
   int *mask;           /* Pointer to array marking out edge pixels */
   int *nrem;           /* Pointer to array marking out edge pixels */
   int *pa;             /* Pointer to next element in the pixel assignment array */
   int caiter;          /* The number of CA iterations to perform */
   int dims[3];         /* Pointer to array of array dimensions */
   int el;              /* Number of elements in array */
   int fixiter;         /* The number of CA iterations to perform */
   int i;               /* Loop count */
   int ii;              /* Loop count */
   int ix;              /* Grid index on 1st axis */
   int iy;              /* Grid index on 2nd axis */
   int iz;              /* Grid index on 3rd axis */
   int j;               /* Loop count */
   int maxid;           /* Largest id for any peak (smallest is zero) */
   int minlen;          /* Minimum size of a clump in pixels along one dimension*/
   int minpix;          /* Minimum total size of a clump in pixels */
   int more;            /* Continue looping?*/
   int ngood;           /* Number of good clumps */
   int nsmall;          /* Number of clumps that are too small */
   int nthin;           /* Number of clumps that span only a single pixel */
   int peakval;         /* Minimum value used to flag peaks */
   int skip[3];         /* Pointer to array of axis skips */

/* Initialise */
   ret = NULL;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Say which method is being used. */
   if( ilevel > 0 ) {
      msgBlank( status );
      msgOut( "", "Reinhold:", status );
   }

/* Get the AST KeyMap holding the configuration parameters for this
   algorithm. */
   if( !astMapGet0A( config, "REINHOLD", &rconfig ) ) {     
      rconfig = astKeyMap( "" );
      astMapPut0A( config, "REINHOLD", rconfig, "" );
   }

/* The configuration file can optionally omit the algorithm name. In this
   case the "config" KeyMap may contain values which should really be in
   the "rconfig" KeyMap. Add a copy of the "config" KeyMap into "rconfig" 
   so that it can be searched for any value which cannot be found in the
   "rconfig" KeyMap. */
   astMapPut0A( rconfig, CUPID__CONFIG, astCopy( config ), NULL );

/* Return the instrumental smoothing FWHMs */
   beamcorr[ 0 ] = cupidConfigD( rconfig, "FWHMBEAM", 2.0 );
   beamcorr[ 1 ] = beamcorr[ 0 ];
   if( ndim == 3 ) {
      beamcorr[ 2 ] = beamcorr[ 0 ];
      beamcorr[ velax ]= cupidConfigD( rconfig, "VELORES", 2.0 );
   }

/* Find the size of each dimension of the data array, and the total number
   of elements in the array, and the skip in 1D vector index needed to
   move by pixel along an axis. We use the memory management functions of the 
   AST library since they provide greater security and functionality than 
   direct use of malloc, etc. */
   el = 1;
   for( i = 0; i < ndim; i++ ) {
      dims[ i ] = subnd[ i ] - slbnd[ i ] + 1;
      el *= dims[ i ];
      skip[ i ] = ( i == 0 ) ? 1 : skip[ i - 1 ]*dims[ i - 1 ];
   }
   for( ; i < 3; i++ ) {
      dims[ i ] = 1;
      skip[ i ] = 0;
   }

/* Get various configuration parameters. */
   rms = cupidConfigD( rconfig, "RMS", rms );
   minlen = cupidConfigI( rconfig, "MINLEN", 4 );
   minpix = cupidConfigI( rconfig, "MINPIX", 16 );
   noise = cupidConfigD( rconfig, "NOISE", 2*rms );
   thresh = cupidConfigD( rconfig, "THRESH", noise + 2*rms );
   flatslope = cupidConfigD( rconfig, "FLATSLOPE", rms );
   cathresh = pow( 3, ndim ) - 1.0;
   cathresh = cupidConfigI( rconfig, "CATHRESH", (int) cathresh );
   caiter = cupidConfigI( rconfig, "CAITERATIONS", 1 );
   fixiter = cupidConfigI( rconfig, "FIXCLUMPSITERATIONS", 1 );

/* Convert CATHRESH from a number of pixels to a fraction. */
   cathresh = cathresh/pow( 3, ndim );
   if( cathresh > 0.98 ) cathresh = 0.98;
   if( cathresh < 0 ) cathresh = 0.0;
   cathresh += 0.01;

/* Get a mask which is the same size and shape as the data array and which 
   holds CUPID__KEDGE at every pixel thought to be on the edge of a clump. 
   This is done by scanning the data cube using sets of parallel lines in
   different directions. Peaks are searched for in each line, and then the 
   edges found by following the curve down from each peak until the
   gradient becomes zero or positive, or until the data value drops below a
   threshold value. Pixels which correspond to peaks in the data cube
   are flagged with the value greater than or equal to the returned 
   "*peakval" value. All other pixels are set to some other value (which 
   will usually be CUPID__KBACK but will be something else at positions of 
   peaks which were not peaks in all scan directions). */
   mask = cupidRInitEdges( type, ipd, el, ndim, dims, skip, minlen, thresh, 
                           noise, rms, flatslope, &peakval );

/* Dilate the edge regions using a cellular automata. This creates a new
   mask array in which a pixel is marked as an edge pixel if any of its
   neighbours are marked as edge pixels in the mask array created above. */
   mask2 = cupidRCA( mask, NULL, el, dims, skip, 0.0, peakval, CUPID__KEDGE, 
                     CUPID__KBACK, 0 );

/* Erode the edge regions using a second cellular automata. This over-writes
   the original mask array so that a pixel is marked as an edge pixel if a
   fraction greater than "cathresh" of neighbouring pixels are marked as edge 
   pixels in "mask2". We loop doing this "CAiteration" times. */
   m1 = mask;
   m2 = mask2;
   for( i = 0; i < caiter; i++ ) {
      m1 = cupidRCA( m2, m1, el, dims, skip, cathresh, peakval, CUPID__KEDGE, 
                     CUPID__KBACK, 0 );
      m3 = m1;
      m1 = m2;
      m2 = m3;
   }

/* Fill the volume around each peak with integer values which indicate
   which peak they are close to. All the pixels around one peak form one
   clump. */
   maxid = cupidRFillClumps( m2, m1, el, ndim, skip, dims, peakval );

/* Abort if no clumps found. */
   if( maxid < 0 ) {
      if( ilevel > 0 ) {
         msgOut( "", "No usable clumps found", status );
         msgBlank( status );
      }
      goto L10;
   }

/* Smooth the boundaries between the clumps. This cellular automata replaces 
   each output pixel by the most commonly occuring value within a 3x3x3 
   cube of input pixels centred on the output pixel. Put the smoothed
   results back into the supplied "m1" array. */
   for( i = 0; i < fixiter; i++ ) {
      m2 = cupidRCA2( m1, m2, el, dims, skip );
      m3 = m2;
      m2 = m1;
      m1 = m3;
   }

/* Allocate an array used to store the number of pixels remaining in each 
   clump. */
   nrem = astMalloc( sizeof( int )*( maxid + 1 ) );

/* Allocate an array used to store the peak value in every clump. */
   peakvals = astMalloc( sizeof( double )*( maxid + 1 ) );

/* Determine the bounding box of every clump. First allocate memory to
   hold the bounding boxes. */
   clbnd = astMalloc( sizeof( int )*( maxid + 1 )*3 );
   cubnd = astMalloc( sizeof( int )*( maxid + 1 )*3 );
   igood = astMalloc( sizeof( int )*( maxid + 1 ) );
   if( igood ) {

/* Initialise a list to hold zero for every clump id. These values are
   used to count the number of pixels remaining in each clump. Also
   initialise the peak values to a very negative value. */
      for( i = 0; i <= maxid; i++ ) {
         nrem[ i ] = 0;
         peakvals[ i ] = VAL__MIND;
      }

/* Initialise the bounding boxes. */
      for( i = 0; i < 3*( maxid + 1 ); i++ ) {
         clbnd[ i ] = VAL__MAXI;
         cubnd[ i ] = VAL__MINI;
      }

/* Loop round every pixel in the final pixel assignment array. */
      if( type == CUPID__DOUBLE ) {
         pd = (double *) ipd;
      } else {
         pf = (float *) ipd;
      }
      pa = m1;
      for( iz = 1; iz <= dims[ 2 ]; iz++ ){
         for( iy = 1; iy <= dims[ 1 ]; iy++ ){
            for( ix = 1; ix <= dims[ 0 ]; ix++, pa++ ){

/* Get the data value at this pixel */
               if( type == CUPID__DOUBLE ) {
                  pv = *(pd++);
               } else {
                  pv = (double) *(pf++);
               }

/* Skip pixels which are not in any clump. */
               if( *pa >= 0 ) {

/* Increment the number of pixels in this clump. */
                  ++( nrem[ *pa ] );

/* If this pixel value is larger than the current peak value for this
   clump, record it. */
                  if( pv > (double) peakvals[ *pa ] ) peakvals[ *pa ] = pv;

/* Get the index within the clbnd and cubnd arrays of the current bounds
   on the x axis for this clump. */
                  i = 3*( *pa );

/* Update the bounds for the x axis, then increment to get the index of the y
   axis bounds. */
                  if( ix < clbnd[ i ] ) clbnd[ i ] = ix;
                  if( ix > cubnd[ i ] ) cubnd[ i ] = ix;
                  i++;

/* Update the bounds for the y axis, then increment to get the index of the z
   axis bounds. */
                  if( iy < clbnd[ i ] ) clbnd[ i ] = iy;
                  if( iy > cubnd[ i ] ) cubnd[ i ] = iy;
                  i++;

/* Update the bounds for the z axis. */
                  if( iz < clbnd[ i ] ) clbnd[ i ] = iz;
                  if( iz > cubnd[ i ] ) cubnd[ i ] = iz;
               }
            }
         }
      }

/* Loop round counting the clumps which are too small or too low. Put the
   indices of usable clumps into another array. */
      nsmall = 0;
      ngood = 0;
      nthin = 0;
      for( i = 0; i <= maxid; i++ ) {
         j = 3*i;

         if( nrem[ i ] <= minpix ) {
            nsmall++;

         } else if( clbnd[ j ] == cubnd[ j ] || 
                  ( clbnd[ j + 1 ] == cubnd[ j + 1 ] && ndim > 1 ) || 
                  ( clbnd[ j + 2 ] == cubnd[ j + 2 ] && ndim > 2 ) ) {
            nthin++;           

         } else {
            igood[ ngood++ ] = i;
         }
      }

      if( ilevel > 0 ) {
         if( ngood == 0 ) {
            msgOut( "", "No usable clumps found", status );
         } else if( ngood == 1 ){
            msgOut( "", "One usable clump found", status );
         } else {
            msgSeti( "N", ngood );
            msgOut( "", "^N usable clumps found", status );
         }
         if( ilevel > 1 ) {
            if( nsmall == 1 ){
               msgOut( "", "One clump rejected because it contains too few pixels", status );
            } else {
               msgSeti( "N", nsmall );
               msgOut( "", "^N clumps rejected because they contain too few pixels", status );
            }
            if( nthin == 1 ) {
               msgOut( "", "1 clump rejected because it spans only a single "
                       "pixel along one or more axes", status );

            } else if( nthin > 1 ) {
               msgSeti( "N", nthin );
               msgOut( "", "^N clumps rejected because they spans only a single "
                       "pixel along one or more axes", status );
            }
         }
         msgBlank( status );
      }         

/* Sort the clump indices into descending order of peak value. */
      j = ngood;
      more = 1;
      while( more ) {
         j--;
         more = 0;
         for( i = 0; i < j; i++ ) {
            if( peakvals[ igood[ i ] ] < peakvals[ igood[ i + 1 ] ] ) {
               ii = igood[ i + 1 ];
               igood[ i + 1 ] = igood[ i ];
               igood[ i ] = ii;
               more = 1;
            }
         }
      }

/* Loop round creating an NDF describing each usable clump. */
      for( j = 0; j < ngood; j++ ) {
         i = igood[ j ];
         ret = cupidNdfClump( type, ipd, m1, el, ndim, dims, skip, slbnd, 
                              i, clbnd + 3*i, cubnd + 3*i, NULL, ret );
      }

/* Free resources */
      clbnd = astFree( clbnd );
      cubnd = astFree( cubnd );
      igood = astFree( igood );
   }

   peakvals = astFree( peakvals );
   nrem = astFree( nrem );

L10:;

/* Remove the secondary KeyMap added to the KeyMap containing configuration 
   parameters for this algorithm. This prevents the values in the secondary 
   KeyMap being written out to the CUPID extension when cupidStoreConfig is 
   called. */
   astMapRemove( rconfig, CUPID__CONFIG );

/* Free resources */
   rconfig = astAnnul( rconfig );
   mask = astFree( mask );
   mask2 = astFree( mask2 );

/* Return the list of clump NDFs. */
   return ret;

}

