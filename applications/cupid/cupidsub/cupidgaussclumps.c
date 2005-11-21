#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"
#include "ast.h"
#include "cupid.h"
#include "star/hds.h"
#include <math.h>


/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm 
   used to communicate with the service functions cupidGCcalcf and
   cupidGCcalcg called by the PDA minimisation function. The contents of
   this structure are initialised in cupidSetInit. */
CupidGC cupidGC;

char *cupidGaussClumps( int type, int ndim, int *slbnd, int *subnd, void *ipd, 
                        double *ipv, double rms, AstKeyMap *config, int velax, 
                        int ilevel, int *nclump, double *bg ){
/*
*  Name:
*     cupidGaussClumps

*  Purpose:
*     Identify clumps of emission within a 2 or 3 dimensional NDF using
*     the GAUSSCLUMPS algorithm.

*  Synopsis:
*     char *cupidGaussClumps( int type, int ndim, int *slbnd, int *subnd, 
*                             void *ipd, double *ipv, double rms, 
*                             AstKeyMap *config, int velax, int *nclump,
*                             double *bg )

*  Description:
*     This function identifies clumps within a 2 or 3 dimensional data
*     array using the GAUSSCLUMPS algorithm, described by Stutski & Gusten 
*     (1990, ApJ 356, 513). This algorithm proceeds by fitting a Gaussian 
*     profile to the brightest peak in the data. It then subtracts the fit 
*     from the data and iterates, fitting a new ellipse to the brightest peak 
*     in the residuals. This continues until a termination criterion is
*     reached. The main termination criterion in this implementation is
*     not quote the same as in the Stutski & Gusten paper. They had two main
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
*        The global RMS error in the data array.
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.
*     velax
*        The index of the velocity axis in the data array (if any). Only
*        used if "ndim" is 3. 
*     ilevel
*        Amount of screen information to display (in range zero to 3).
*     nclump
*        Pointer to an int to receive the number of clumps found.
*     bg
*        Pointer to an double to receive the mean background level.

*  Retured Value:
*     A pointer to a dynamically allocated character string, which should
*     be freed using astFree when no longer needed. It will contain a
*     list of HDS locators. The number of locators in the list is given
*     by the value returned in "*nclump". Each locator will occupy
*     (DAT__SZLOC+1) elements of the character array, and will locate a
*     "Clump" structure describing a single clump.

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

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     29-SEP-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   AstKeyMap *gcconfig; /* Configuration parameters for this algorithm */
   char *clist;         /* Pointer to list of returned HDS locators */
   double *peaks;       /* Holds the "npeak" most recently fitted peak values */
   double chisq;        /* Chi-squared value of most recently fitted Gaussian */
   double mean_peak;    /* The mean of the values within "peaks" */
   double mlim;         /* Truncation level for Gaussians */
   double new_peak;     /* The value most recently added to "peaks" */
   double nsig;         /* No.of standard deviations at which to reject peaks */
   double old_peak;     /* The oldest value within "peaks" */
   double peak_thresh;  /* The lower threshold for clump peak values */
   double sigma_peak;   /* The standard deviation of the values within "peaks" */
   double sum_peak2;    /* Sum of the squares of the values in "peaks" */
   double sum_peak;     /* Sum of the values in "peaks" */
   double sumbg;        /* Sum of background estimates */
   double swbg;         /* Sum of background estimate weights */
   double urms;         /* User-supplied RMS noise level */
   double wbg;          /* Background estimate weight */
   double x[ CUPID__GCNP3 ]; /* Parameters describing new Gaussian clump */
   int *dims;           /* Pointer to array of array dimensions */
   int allbad;          /* Are all the residuals bad? */
   int diag;            /* Is extra diagnostic information required? */
   int el;              /* Number of elements in array */
   int i;               /* Loop count */
   int iclump;          /* Number of clumps found so far */
   int imax;            /* Index of element with largest residual */
   int ipeak;           /* Index within "peaks" at which to store the new peak */
   int iter;            /* Continue finding more clumps? */
   int maxclump;        /* Max no. of clumps */
   int maxskip;         /* Max no. of failed fits between good fits */
   int niter;           /* Iterations performed so far */
   int npad;            /* No. of peaks below threshold for temination */
   int npeak;           /* The number of elements in the "peaks" array. */
   int nskip;           /* No. of failed fits since last good fit */
   int peaks_below;     /* Count of consecutive peaks below the threshold */
   void *res;           /* Pointer to residuals array */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return NULL;

/* Get the AST KeyMap holding the configuration parameters for this
   algorithm. */
   if( !astMapGet0A( config, "GAUSSCLUMPS", &gcconfig ) ) {     
      gcconfig = astKeyMap( "" );
      astMapPut0A( config, "GAUSSCLUMPS", gcconfig, "" );
   }

/* See if extra diagnostic info is required. */
   diag = cupidConfigI( gcconfig, "DIAG", 0 );

/* Get the maximum allowed number of failed fits between succesful fits. */
   maxskip = cupidConfigI( gcconfig, "MAXSKIP", 10 );

/* Get the maximum allowed number of failed fits between succesful fits. */
   maxclump = cupidConfigI( gcconfig, "MAXCLUMPS", VAL__MAXI );

/* The iterative process ends when "npad" consecutive clumps all had peak
   values below "peak_thresh". */
   npad = cupidConfigI( gcconfig, "NPAD", 10 );

/* Find the size of each dimension of the data array, and the total number
   of elements in the array. We use the memory management functions of the 
   AST library since they provide greater security and functionality than 
   direct use of malloc, etc. */
   dims = astMalloc( sizeof( int )*(size_t) ndim );
   el = 1;
   if( dims ) {
      for( i = 0; i < ndim; i++ ) {
         dims[ i ] = subnd[ i ] - slbnd[ i ] + 1;
         el *= dims[ i ];
      }
   }

/* Copy the supplied data array into a work array which will hold the
   residuals remaining after subtraction of the fitted Gaussians. The
   cupidStore macro is a wrapper around the astStore function. */
   res = cupidStore( NULL, ipd, el, type, "cupidGaussClumps" );
   if( res ) {

/* Store the information level in the cupidGC structure, from where it
   can be accessed from the service routines which evaluate the objective
   function for the fitting routine. */
      cupidGC.ilevel = ilevel;

/* Allow the user to override the supplied RMS error value. */
      urms = cupidConfigD( gcconfig, "RMS", VAL__BADD );
      if( urms != VAL__BADD ) {
         rms = urms;
         if( ilevel > 1 ) {
            msgSetd( "N", rms );
            msgOut( "", "User-supplied RMS noise estimate: ^N", status );
         }

/* If the user did not supply an RMS value, access it again, this time
   suppling the default RMS value. This is done to ensure that the
   default RMS value is stored in the CUPID NDF extension when the
   program exits. */
      } else {
         (void) cupidConfigD( gcconfig, "RMS", rms );
      }

/* Tell the user what RMS value is being used. */
      if( ilevel > 1 ) {
         msgSetd( "N", rms );
         msgOut( "", "RMS noise level actually used: ^N", status );

      } else if( ilevel > 0 ) {
         msgSetd( "N", rms );
         msgOut( "", "RMS noise level used: ^N", status );
      }

/* Set the lower threshold for clump peaks to a user-specified multiple
   of the RMS noise. */
      peak_thresh = cupidConfigD( gcconfig, "THRESH", 20.0 );

/* Get the lowest value (normalised to the RMS noise level) at which
   model Gaussians should be evaluated. */
      mlim = cupidConfigD( gcconfig, "MODELLIM", 0.5 );

/* Initialise the number of clumps found so far. */
      *nclump = 0;
      iclump = 0;
      clist = NULL;

/* Initialise the sum of the background estimates, and the number of such
   estimates summed. */
      sumbg = 0.0;
      swbg = 0.0;

/* Indicate that no peaks have been found below the lower threshold for clump 
   peak values. */
      peaks_below = 0;

/* Initialise the variables used to keep track of the mean and standard
   deviation of the most recent "npeak" fitted peak values. */
      nsig = cupidConfigD( gcconfig, "NSIGMA", 3.0 );
      npeak = cupidConfigI( gcconfig, "NPEAK", 9 );
      ipeak = 0;
      sum_peak = 0.0;
      sum_peak2 = 0.0;
      peaks = astMalloc( sizeof( double )*npeak );
      for( i = 0; i < npeak; i++ ) peaks[ i ] = 0.0;

/* Loop round fitting a gaussian to the largest remaining peak in the
   residuals array. */
      iter = 1;
      niter = 0;
      nskip = 0;
      while( iter && *status == SAI__OK ) {

/* Report the iteration number to the user if required. */
         ++niter;         
         if( ilevel > 3 ) {
            msgBlank( status );
            msgSetd( "N", niter );
            msgOut( "", "Iteration ^N:", status );
         }

/* Find the 1D vector index of the elements with the largest value in the 
   residuals array. This returns a flag indicating if all residuals are
   bad. */
         allbad = cupidGCFindMax( type, res, el, &imax );

/* Finish iterating if all the residuals are bad, or if too many iterations 
   have been performed since the last succesfully fitted clump. */
         if( allbad ) {    
            iter = 0;
            niter--;
            if( ilevel > 1 ) msgBlank( status );
            if( ilevel > 3 ) {
               msgOut( "", "There are no good pixels left to be fitted.", 
                       status );
               msgBlank( status );
            }
         } else if( nskip > maxskip ){
            iter = 0;
            niter--;
            if( ilevel > 1 ) msgBlank( status );
            if( ilevel > 3 ) {
               msgSeti( "N", maxskip );
               msgOut( "", "The previous ^N fits were unusable.", status );
               msgBlank( status );
            }
         }

/* If not, make an initial guess at the Gaussian clump parameters centred
   on the current peak. */
         if( iter ) {
            cupidGCSetInit( type, res, ipv, ndim, dims, imax, rms, gcconfig,
                            iclump, velax, x, slbnd );

/* Find the best fitting parameters, starting from the above initial guess. 
   This returns a function value of zero if no fit could be performed. */
            if( cupidGCFit( type, res, imax, x, &chisq ) ) {

/* Skip this fit if we have an estimate of the standard deviation of the
   "npeak" most recent clump peak values, and the peak value of the clump
   just fitted is a long way (more than NSIGMA standard deviations) from the 
   peak value of the previously fitted clump. */
               if( npeak == 0 || iclump < npeak || 
                   fabs( x[ 0 ] - new_peak ) < nsig*sigma_peak ) {

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

/* Increment the sum of the background estimates. Background estimates
   are weights to give greater weight to estimates formed form larger
   peaks and from closer fits. */
                  wbg = ( chisq > 0.0 ) ? x[ 0 ]/chisq : 1.0;
                  sumbg += x[ 1 ]*wbg*rms;
                  swbg += wbg;

/* Extend the returned array of HDS Clump structures to include room for
   the new one. This list is actually a long character string containing
   room for "iclump" HDS locators. */
                  clist = astGrow( clist, iclump, DAT__SZLOC + 1 );

/* Remove the model fit (excluding the background) from the residuals
   array. This also creates a HDS "Clump" structure containing information 
   about the clump. An HDS locator for this new Clump structure is added 
   into the "clist" ring. The standard deviation of the new residuals is 
   returned. */
                  cupidGCUpdateArrays( type, res, ipd, el, ndim, dims,
                                       x, rms, mlim, imax, ilevel, slbnd,    
                                       clist + ( iclump - 1 )*( DAT__SZLOC + 1 ),
                                       iclump, sumbg, swbg, diag, mean_peak );

/* Display the clump parameters on the screen if required. */
                  cupidGCListClump( iclump, ndim, x, chisq, slbnd, ilevel,
                                    rms );

/* If this clump has a peak value which is below the threshold, increment
   the count of consecutive clumps with peak value below the threshold.
   Otherwise, reset this count to zero. */
                  if( x[ 0 ] < peak_thresh ) {
                     peaks_below++;
                  } else {
                     peaks_below = 0;
                  }

/* If the maximum number of clumps have now been found, exit.*/
                  if( iclump == maxclump ) {
                     iter = 0;

                     if( ilevel > 1 ) msgBlank( status );
                     if( ilevel > 3 ) {
                        msgSeti( "M", maxclump );
                        msgOut( "", "The specified maximum number of "
                                "clumps (^M) have been found.", status );
                        msgBlank( status );
                     }

/* If the count of consecutive peaks below the threshold has reached
   "Npad", terminate. */
                  } else if( peaks_below == npad ) {
                     iter = 0;
 
                     if( ilevel > 1 ) msgBlank( status );
                     if( ilevel > 3 ) {
                        msgSeti( "N", npad );
                        msgOut( "", "The previous ^N clumps all had peak "
                                "values below the threshold.", status );
                        msgBlank( status );
                     }
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
                  if( ilevel > 3 ) msgOut( "", "   Clump rejected due to "
                                           "aberrant peak value.", status );
               }

/* Tell the user if no clump could be fitted around the current peak
   pixel value */
            } else {
               nskip++;
               if( ilevel > 3 ) msgOut( "", "   No clump fitted.", status );
            }

/* Tell the user if one of the trmination criteria has ben met. */
         } else if( ilevel > 3 ) {
            msgOut( "", "   At least one termination criterion has been reached.", status );
            msgBlank( status );
         }
      }
   }

/* Tell the user the mean background level, and return it. */
   if( swbg > 0 ) {
      *bg = sumbg/swbg;
      if( ilevel > 2) {
         msgSetd( "BG", *bg );
         msgOut( "", "Estimated background level = ^BG", status );
      }
   } else {
      *bg = 0.0;
   }

/* Tell the user how clumps are being returned. */
   *nclump = iclump;
   if( ilevel > 0 ) {
      if( *nclump == 0 ) {
         msgOut( "", "No clumps found", status );
      } else if( *nclump == 1 ){
         msgOut( "", "One clump found", status );
      } else {
         msgSeti( "N", *nclump );
         msgOut( "", "^N clumps found", status );
      }
   }

/* Tell the user how many iterations have been performed (i.e. how many
   attempts there have been to fit a Gaussian peak). */
   if( ilevel > 3 ) {
      if( niter == 1 ){
         msgOut( "", "No fit attempted", status );
      } else {
         msgSeti( "M", niter - *nclump );
         msgSeti( "N", niter );
         msgOut( "", "Fits attempted for ^N candidate clumps (^M failed)", status );
      }
   }

   if( ilevel > 0 ) msgBlank( status );

/* Free resources */
   res = astFree( res );
   dims = astFree( dims );
   peaks = astFree( peaks );

   cupidGC.data = astFree( cupidGC.data );
   cupidGC.weight = astFree( cupidGC.weight );
   cupidGC.res = astFree( cupidGC.res );
   cupidGC.resu = astFree( cupidGC.resu );
   cupidGC.initmodel = astFree( cupidGC.initmodel );
   cupidGC.model = astFree( cupidGC.model );
   cupidGC.resids = astFree( cupidGC.resids );

   gcconfig = astAnnul( gcconfig );

/* Return the list of clump structure locators. */
   return clist;

}

