#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"
#include "ast.h"
#include "ndf.h"
#include "cupid.h"
#include "star/hds.h"
#include <math.h>
#include <stdio.h>
#include <signal.h>
#include <setjmp.h>

void cupidGCHandler( int );
jmp_buf CupidGCHere;


/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm
   used to communicate with the service functions cupidGCcalcf and
   cupidGCcalcg called by the PDA minimisation function. The contents of
   this structure are initialised in cupidSetInit. */
CupidGC cupidGC;

HDSLoc *cupidGaussClumps( int type, int ndim, hdsdim *slbnd, hdsdim *subnd, void *ipd,
                          double *ipv, double rms, AstKeyMap *config, int velax,
                          double beamcorr[ 3 ], int *status ){
/*
*+
*  Name:
*     cupidGaussClumps

*  Purpose:
*     Identify clumps of emission within a 1, 2 or 3 dimensional NDF using
*     the GAUSSCLUMPS algorithm.

*  Language:
*     Starlink C

*  Synopsis:
*     HDSLoc *cupidGaussClumps( int type, int ndim, hdsdim *slbnd, hdsdim *subnd,
*                               void *ipd, double *ipv, double rms,
*                               AstKeyMap *config, int velax,
*                               double beamcorr[ 3 ], int *status )

*  Description:
*     This function identifies clumps within a 1, 2 or 3 dimensional data
*     array using the GAUSSCLUMPS algorithm, described by Stutski & Gusten
*     (1990, ApJ 356, 513). This algorithm proceeds by fitting a Gaussian
*     profile to the brightest peak in the data. It then subtracts the fit
*     from the data and iterates, fitting a new ellipse to the brightest peak
*     in the residuals. This continues until a termination criterion is
*     reached. The main termination criterion in this implementation is
*     not quite the same as in the Stutski & Gusten paper. They had two main
*     termination criteria; 1) the total data sum of the fitted gaussians
*     is close to the total data sum of the original data, and 2) the peak
*     residual is less than a given multiple of the RMS noise in the data.
*     However, 1) is very sensitive to errors in the estimation of the
*     background level in the data, and 2) may never be achieved because
*     the expected residuals depend not only on the RMS noise in the data
*     but also on how accurately gaussian the clumps are, which is not
*     known. Therefore, this implementation instead terminates when the
*     peak amplitude of the fitted clumps falls below a given fraction of
*     the first (i.e. largest) fitted peak.
*
*     Two additional termination criteria are used; 1) If there are many
*     failed attempts to fit a clump to the peak residual or if 2) a
*     specified maximum number of clumps are found, then the process
*     terminates early.

*  Parameters:
*     type
*        An integer identifying the data type of the array values pointed to
*        by "ipd". Must be either CUPID__DOUBLE or CUPID__FLOAT (defined in
*        cupid.h).
*     ndim
*        The number of dimensions in the data array. Must be 1, 2 or 3.
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
*     beamcorr
*        An array in which is returned the FWHM (in pixels) describing the
*        instrumental smoothing along each pixel axis. The clump widths
*        stored in the output catalogue are reduced to correct for this
*        smoothing.
*     status
*        Pointer to the inherited status value.

*  Notes:
*     - The specific form of algorithm used here is informed by a Fortran
*     implementation of GaussClumps obtained on 27/9/05 from
*     ftp.astro.uni-bonn.de/pub/heith/gaussclumps.
*     - Most of the "cupid..." functions used in this file which start
*     with a "type" parameter (e.g. cupidFindMax, cupidUpdateArrays, etc) are
*     actually not functions at all, but macros defined in cupid.h. These
*     macros are wrappers which invoke a type-specific function (e.g.
*     cupidFindMaxD, cupidFindMaxF) appropriate to the specific data type
*     being used (as indicated by the "type" parameter). Macros are used in
*     order to simplify the code here, and thus make the flow of the
*     algorithm clearer. The source code for the type-specific functions
*     are generated automatically at build time from equivalent files which
*     have file type ".cupid". For instance, the files cupidfindmaxD.c and
*     cupidfindmaxF.c are generated automatically from cupidfindmax.cupid.
*     Also, the rlevant macros definitions and prototypes within cupid.h
*     are generated automatically at build time from these ".cupid" files.

*  Returned Value:
*     A locator for a new HDS object which is an array of NDF structures.
*     Each NDF will hold the data values associated with a single clump and
*     will be the smallest possible NDF that completely contains the
*     corresponding clump. Pixels not in the clump will be set bad. The
*     pixel origin is set to the same value as the supplied NDF.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     7-MAR-2007 (DSB):
*        Use VELORES instead of FWHMBEAM if the data is 1D.
*     7-MAR-2007 (DSB):
*        Guard against segvio caused by use of null pointers that are
*        returned by astMalloc if an error has occurred.
*     14-JAN-2009 (TIMJ):
*        Use MERS for message filtering.
*     20-NOV-2013 (DSB):
*        Supplied config KeyMap now holds the method parameters directly,
*        rather than holding them in a sub-KeyMap.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   HDSLoc *ret;         /* Locator for the returned array of NDFs */
   char buf[30];        /* File name buffer */
   double *peaks;       /* Holds the "npeak" most recently fitted peak values */
   double chisq;        /* Chi-squared value of most recently fitted Gaussian */
   double maxbad;       /* Max fraction of bad pixels allowed in a clump */
   double mean_peak;    /* The mean of the values within "peaks" */
   double mlim;         /* Truncation level for Gaussians */
   double new_peak;     /* The value most recently added to "peaks" */
   double nsig;         /* No.of standard deviations at which to reject peaks */
   double old_peak;     /* The oldest value within "peaks" */
   double peak_thresh;  /* The lower threshold for clump peak values */
   double sigma_peak;   /* The standard deviation of the values within "peaks" */
   double sum_peak2;    /* Sum of the squares of the values in "peaks" */
   double sum_peak;     /* Sum of the values in "peaks" */
   double sumclumps;    /* Sum of the values in all the used clumps so far */
   double sumdata;      /* Sum of the supplied data values */
   double x[ CUPID__GCNP3 ]; /* Parameters describing new Gaussian clump */
   hdsdim *dims;        /* Pointer to array of array dimensions */
   int allbad;          /* Are all the residuals bad? */
   int excols;          /* Are extra output columns required? */
   int iter;            /* Continue finding more clumps? */
   int maxskip;         /* Max no. of failed fits between good fits */
   int niter;           /* Iterations performed so far */
   int npad;            /* No. of peaks below threshold for temination */
   int nskip;           /* No. of failed fits since last good fit */
   int peaks_below;     /* Count of consecutive peaks below the threshold */
   size_t area;         /* Number of pixels contributing to the clump */
   size_t area_below;   /* Count of consecutive clump areas below the threshold */
   size_t area_thresh;  /* The lower threshold for clump areas */
   size_t el;           /* Number of elements in array */
   size_t i;            /* Loop count */
   size_t iclump;       /* Number of clumps found so far */
   size_t imax;         /* Index of element with largest residual */
   size_t ipeak;        /* Index within "peaks" at which to store the new peak */
   size_t maxclump;     /* Max no. of clumps */
   size_t nclump;       /* Number of usable clumps */
   size_t npeak;        /* The number of elements in the "peaks" array. */
   void *res;           /* Pointer to residuals array */

/* Initialise */
   ret = NULL;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Initialise things to avoid compiler warnings. */
   mean_peak = 0.0;
   sigma_peak = 0.0;
   new_peak = 0.0;

/* Say which method is being used. */
   msgBlankif( MSG__NORM, status );
   msgOutif( MSG__NORM,  "", "GaussClumps:", status );
   msgBlankif( MSG__VERB, status );

/* Return the instrumental smoothing FWHMs. For 1D data, we assume the
   axis is spectral and so use VELORES instead of FWHMBEAM.  */
   if( ndim == 1 ) {
      beamcorr[ 0 ] = cupidConfigD( config, "VELORES", 2.0, status );
   } else {
      beamcorr[ 0 ]= cupidConfigD( config, "FWHMBEAM", 2.0, status );
      beamcorr[ 1 ] = beamcorr[ 0 ];
      if( ndim == 3 ) {
         beamcorr[ 2 ] = beamcorr[ 0 ];
         beamcorr[ velax ]= cupidConfigD( config, "VELORES", 2.0, status );
      }
   }

/* See if extra diagnostic info is required. */
   excols = cupidConfigI( config, "EXTRACOLS", 0, status );

/* Get the maximum allowed number of failed fits between succesful fits. */
   maxskip = cupidConfigI( config, "MAXSKIP", 10, status );

/* Get the maximum allowed number of failed fits between succesful fits. */
   maxclump = cupidConfigI( config, "MAXCLUMPS", VAL__MAXI, status );

/* The iterative process ends when "npad" consecutive clumps all had peak
   values below "peak_thresh" or all had areas below "area_thresh". */
   npad = cupidConfigI( config, "NPAD", 10, status );

/* Get the RMS noise level to use. */
   rms = cupidConfigD( config, "RMS", rms, status );

/* Find the size of each dimension of the data array, and the total number
   of elements in the array. We use the memory management functions of the
   AST library since they provide greater security and functionality than
   direct use of malloc, etc. */
   dims = astMalloc( sizeof( *dims )*(size_t) ndim );
   el = 1;
   if( dims ) {
     for( i = 0; i < (size_t)ndim; i++ ) {
         dims[ i ] = subnd[ i ] - slbnd[ i ] + 1;
         el *= dims[ i ];
      }
   }

/* Copy the supplied data array into a work array which will hold the
   residuals remaining after subtraction of the fitted Gaussians. The
   cupidStore macro is a wrapper around the astStore function. */
   res = cupidStore( NULL, ipd, el, type, "cupidGaussClumps" );
   if( res ) {

/* Set the lower threshold for clump peaks to a user-specified multiple
   of the RMS noise. */
      peak_thresh = cupidConfigD( config, "THRESH", 2.0, status );

/* Set the lower threshold for clump area to a user-specified number of
   pixels. */
      area_thresh = cupidConfigI( config, "MINPIX", 3, status );

/* Get the lowest value (normalised to the RMS noise level) at which
   model Gaussians should be evaluated. */
      mlim = cupidConfigD( config, "MODELLIM", 0.5, status );

/* Get the max allowed fraction of bad pixels in a clump. */
      maxbad = cupidConfigD( config, "MAXBAD", 0.05, status );

/* Initialise the number of clumps found so far. */
      iclump = 0;

/* Indicate that no peaks have been found below the lower threshold for clump
   peak values, or below the lower area threshold. */
      peaks_below = 0;
      area_below = 0;

/* Initialise the variables used to keep track of the mean and standard
   deviation of the most recent "npeak" fitted peak values. */
      nsig = cupidConfigD( config, "NSIGMA", 3.0, status );
      npeak = cupidConfigI( config, "NPEAK", 9, status );
      ipeak = 0;
      sum_peak = 0.0;
      sum_peak2 = 0.0;
      iter = 1;
      niter = 0;
      nskip = 0;
      sumclumps = 0.0;
      sumdata = VAL__BADD;
      peaks = astMalloc( sizeof( *peaks )*npeak );
      if( peaks ) {
         for( i = 0; i < npeak; i++ ) peaks[ i ] = 0.0;


/* Use the setjmp function to define here to be the place to which the
   signal handling function will jump when a signal is detected. Zero is
   returned on the first invocation of setjmp. If a signal is detected,
   a jump is made into setjmp which then returns a positive signal
   identifier. */
         if( setjmp( CupidGCHere ) ) {
            iter = 0;
            msgBlankif( MSG__QUIET, status );
            msgOutif( MSG__QUIET, "",
                      "Interupt detected. Clumps found so far will be saved",
                      status );
            msgBlankif( MSG__QUIET, status );
         }
      }

/* Set up a signal handler for the SIGINT (interupt) signal. If this
   signal occurs, the function "cupidGCHandler" will be called. */
      signal( SIGINT, cupidGCHandler );

/* Loop round fitting a gaussian to the largest remaining peak in the
   residuals array. */
      while( iter && *status == SAI__OK ) {

/* Report the iteration number to the user if required. */
         ++niter;
         msgBlankif( MSG__DEBUG1, status );
         msgOutiff( MSG__DEBUG1, "", "Iteration %d:", status, niter );

/* Find the 1D vector index of the elements with the largest value in the
   residuals array. */
         allbad = cupidGCFindMax( type, res, el, &imax, &sumdata, status );

/* Finish iterating if all the residuals are bad, or if too many iterations
   have been performed since the last succesfully fitted clump. */
         if( allbad ) {
            iter = 0;
            niter--;
            msgBlankif( MSG__DEBUG, status );
            msgOutif( MSG__DEBUG1, "",
                      "There are no good pixels left to be fitted.",
                      status );
            msgBlankif( MSG__DEBUG1, status );
         } else if( nskip > maxskip ){
            iter = 0;
            niter--;
            msgBlankif( MSG__DEBUG, status );
            msgOutiff( MSG__DEBUG1, "",
                       "The previous %d fits were unusable.", status, maxskip );
            msgBlankif( MSG__DEBUG1, status );
         }

/* If not, make an initial guess at the Gaussian clump parameters centred
   on the current peak. */
         if( iter ) {
            cupidGCSetInit( type, res, ipv, ndim, dims, imax, rms, config,
                            ( niter == 1 ), velax, x, slbnd, status );

/* Find the best fitting parameters, starting from the above initial guess.
   This returns a function value of zero if no fit could be performed. */
            if( cupidGCFit( type, res, imax, x, &chisq, status ) ) {

/* Skip this fit if we have an estimate of the standard deviation of the
   "npeak" most recent clump peak values, and the peak value of the clump
   just fitted is a long way (more than NSIGMA standard deviations) from the
   peak value of the previously fitted clump. Also skip it if the peak
   value is less than the "mlim" value. */
               if( ( npeak == 0 || iclump < npeak ||
                     fabs( x[ 0 ] - new_peak ) < nsig*sigma_peak ) &&
                    x[ 0 ] > mlim ) {

/* Record the new peak value for use with the next peak, and update the
   standard deviation of the "npeak" most recent peaks. These values are
   stored cyclically in the "peaks" array. */
                  if( npeak > 0 ) {
                     new_peak = x[ 0 ];
                     old_peak = peaks[ ipeak ];
                     peaks[ ipeak ] = new_peak;
                     if( ++ipeak == npeak ) ipeak = 0;
                     sum_peak += new_peak - old_peak;
                     sum_peak2 += new_peak*new_peak - old_peak*old_peak;
                     if( sum_peak2 < 0.0 ) sum_peak2 = 0.0;
                     mean_peak = sum_peak/npeak;
                     sigma_peak = sqrt( sum_peak2/npeak - mean_peak*mean_peak );
                  }

/* Increment the number of peaks found. */
                  iclump++;

/* Reset the number of failed fits since the last good fit. */
                  nskip = 0;

/* Remove the model fit (excluding the background) from the residuals
   array. This also creates an NDF containing the data values associated
   with the clump. This NDF is stored in the HDS array of NDFs in the
   returned HDS object. The standard deviation of the new residuals is
   returned. */
                  cupidGCUpdateArrays( type, res, ipd, el, ndim, dims,
                                       x, rms, mlim, imax, peak_thresh, slbnd,
                                       &ret, iclump, excols, mean_peak,
                                       maxbad, &area, &sumclumps, status );

/* Dump the modified residuals if required. */
                  sprintf( buf, "residuals%lu", iclump );
                  cupidGCDump( type, MSG__DEBUG3, res, ndim, dims, buf,
                               status );

/* Display the clump parameters on the screen if required. */
                  cupidGCListClump( iclump, ndim, x, chisq, slbnd,
                                    rms, status );

/* If this clump has a peak value which is below the threshold, increment
   the count of consecutive clumps with peak value below the threshold.
   Otherwise, reset this count to zero. */
                  if( x[ 0 ] < peak_thresh ) {
                     peaks_below++;
                  } else {
                     peaks_below = 0;
                  }

/* If this clump has an area which is below the threshold, increment
   the count of consecutive clumps with area below the threshold.
   Otherwise, reset this count to zero. */
                  if( area < area_thresh ) {
                     area_below++;
                  } else {
                     area_below = 0;
                  }

/* If the maximum number of clumps have now been found, exit.*/
                  if( iclump == maxclump ) {
                     iter = 0;

                     msgBlankif( MSG__DEBUG, status );
                     msgOutiff( MSG__DEBUG1, "",
                                "The specified maximum number of "
                                "clumps (%lu) have been found.", status,
                                maxclump );
                     msgBlankif( MSG__DEBUG1, status );

/* If the integrated data sum in the fitted gaussians exceeds or equals
   the integrated data sum in th einput, exit. */
                  } else if( sumclumps >= sumdata ) {
                     iter = 0;

                     msgBlankif( MSG__DEBUG, status );
                     msgOutiff( MSG__DEBUG1,"",
                                "The total data sum of the fitted "
                                "Gaussians (%g) has reached the total "
                                "data sum in the supplied data (%g).",
                                status, (float)sumclumps, (float)sumdata );
                     msgBlankif( MSG__DEBUG1, status );

/* If the count of consecutive peaks below the threshold has reached
   "Npad", terminate. */
                  } else if( peaks_below == npad ) {
                     iter = 0;

                     msgBlankif( MSG__DEBUG, status );
                     msgOutiff( MSG__DEBUG1, "",
                                "The previous %d clumps all had peak "
                                "values below the threshold.", status,
                                npad );
                     msgBlankif( MSG__DEBUG1, status );

/* If the count of consecutive clumps with area below the threshold has reached
   "Npad", terminate. */
                  } else if( area_below == npad ) {
                     iter = 0;

                     msgBlankif( MSG__DEBUG, status );
                     msgOutiff( MSG__DEBUG1, "",
                                "The previous %d clumps all had areas "
                                "below the threshold.", status, npad );
                     msgBlankif( MSG__DEBUG1, status );
                  }

/* If the peak value fitted is very different from the previous fitted peak
   value, set the residuals array element bad in order to prevent the
   algorithm from trying to fit a peak to the same pixel again. */
               } else {
                  if( type == CUPID__DOUBLE ) {
                     ((double *)res)[ imax ] = VAL__BADD;
                  } else {
                     ((float *)res)[ imax ] = VAL__BADR;
                  }

                  new_peak = 0.5*( new_peak + x[ 0 ] );

                  nskip++;
                  msgOutif( MSG__DEBUG1, "", "   Clump rejected due to "
                            "aberrant peak value.", status );
               }

/* Tell the user if no clump could be fitted around the current peak
   pixel value */
            } else {
               nskip++;
               msgOutif( MSG__DEBUG1, "", "   No clump fitted.", status );

/* Set the specified element of the residuals array bad if no fit was
   performed. This prevents the any subsequent attempt to fit a Gaussian
   to the same peak value.*/
               if( type == CUPID__DOUBLE ) {
                  ((double *)res)[ imax ] = VAL__BADD;
               } else {
                  ((float *)res)[ imax ] = VAL__BADR;
               }
            }

/* Tell the user if one of the trmination criteria has ben met. */
         } else {
           msgOutif( MSG__DEBUG1, "",
                     "   At least one termination criterion has been reached.",
                     status );
           msgBlankif( MSG__DEBUG1, status );
         }
      }

/* Tell the user how clumps are being returned. */
      if( ret ) {
        datSize( ret, &nclump, status );
      } else {
        nclump = 0;
      }

      if( nclump == 0 ) msgOutif( MSG__NORM, "", "No usable clumps found.", status );

      if( iclump - nclump == 1 ) {
        msgOutif( MSG__NORM, "",
                  "1 clump rejected because it touches an edge of "
                  "the data array or was below the threshold.", status );
      } else if( iclump - nclump > 1 ) {
        msgOutiff( MSG__NORM, "",
                   "%d clumps rejected because they touch an edge of "
                   "the data array or were below the threshold.", status,
                   (int)( iclump - nclump ) );
      }

/* Tell the user how many iterations have been performed (i.e. how many
   attempts there have been to fit a Gaussian peak). */
      if( niter == 1 ){
        msgOutif( MSG__DEBUG1, "", "No fit attempted.", status );
      } else {
        msgOutiff( MSG__DEBUG1, "",
                   "Fits attempted for %d candidate clumps (%d failed).",
                   status, (int)( niter - iclump ), niter );
      }

/* Free resources */
      peaks = astFree( peaks );

   }

/* Free resources */
   res = astFree( res );
   dims = astFree( dims );

   cupidGC.data = astFree( cupidGC.data );
   cupidGC.weight = astFree( cupidGC.weight );
   cupidGC.res = astFree( cupidGC.res );
   cupidGC.resu = astFree( cupidGC.resu );
   cupidGC.initmodel = astFree( cupidGC.initmodel );
   cupidGC.model = astFree( cupidGC.model );
   cupidGC.resids = astFree( cupidGC.resids );

/* Return the list of clump NDFs. */
   return ret;

}

void cupidGCHandler( int sig ){
/*
*  Name:
*     cupidGCHandler

*  Purpose:
*     Called when an interupt occurs within GaussClumps.

*  Description:
*     This function is called when an interupt signal is detected during
*     execution of the GaussClumps algorithm. It just jumps back to the
*     location defined by the global variable "CupidGCHere", returning the
*     signal value.
*/
   longjmp( CupidGCHere, sig );
}


