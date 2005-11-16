#include "sae_par.h"
#include "mers.h"
#include "prm_par.h"
#include "ast.h"
#include "cupid.h"
#include "star/hds.h"


/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm 
   used to communicate with the service functions cupidGCcalcf and
   cupidGCcalcg called by the PDA minimisation function. The contents of
   this structure are initialised in cupidSetInit. */
CupidGC cupidGC;


char *cupidGaussClumps( int type, int ndim, int *slbnd, int *subnd, void *ipd, 
                        double *ipv, float *rmask, double rms, 
                        AstKeyMap *config, int velax, int ilevel, void *ipo,
                        int *nclump ){
/*
*  Name:
*     cupidGaussClumps

*  Purpose:
*     Identify clumps of emission within a 2 or 3 dimensional NDF using
*     the GAUSSCLUMPS algorithm.

*  Synopsis:
*     char *cupidGaussClumps( int type, int ndim, int *slbnd, int *subnd, 
*                             void *ipd, double *ipv, float *rmask, 
*                             double rms, AstKeyMap *config, int velax,
*                             void *ipo, int *nclump )

*  Description:
*     This function identifies clumps within a 2 or 3 dimensional data
*     array using the GAUSSCLUMPS algorithm, described by Stutski & Gusten 
*     (1990, ApJ 356, 513). This algorithm proceeds by fitting a Gaussian 
*     profile to the brightest peak in the data. It then subtracts the fit 
*     from the data and iterates, fitting a new ellipse to the brightest peak 
*     in the residuals. This continues until the total value in the fitted 
*     ellipses equals the total value in the original data. Each fitted 
*     ellipse is taken to be a single clump and is added to the output 
*     catalogue. In this algorithm, clumps may overlap.

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
*     rmask
*        Pointer to a mask array. The elements should be stored in
*        Fortran order. If this is not NULL, then pixels which fall
*        within any clump are set to 1.0 (all other pixels are left 
*        unchanged).
*     rms
*        The global RMS error in the data array.
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.
*     velax
*        The index of the velocity axis in the data array (if any). Only
*        used if "ndim" is 3. 
*     ilevel
*        Amount of screen information to display (in range zero to 3).
*     ipo
*        Pointer to the output data array, or NULL. The elements should be 
*        stored in Fortran order. The data type of this array is given by 
*        "itype". If supplied, it is assumed that the array has been
*        initialised to hold zero at every pixel.
*     nclump
*        Pointer to an int to receive the number of clumps found.

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
*     with a "type" parameter (e.g. cupidFindMax, cupidIterate, etc) are
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
   double *outd;        /* Pointer to double output array */
   double chisq;        /* Chi-squared value of most recently fitted Gaussian */
   double mlim;         /* Truncation level for Gaussians */
   double sum;          /* Sum of all residuals */
   double sumbg;        /* Sum of background estimates */
   double urms;         /* User-supplied RMS noise level */
   double x[ CUPID__GCNP3 ]; /* Parameters describing new Gaussian clump */
   float *outf;         /* Pointer to float output array */
   int *dims;           /* Pointer to array of array dimensions */
   int diag;            /* Is extra diagnostic information required? */
   int el;              /* Number of elements in array */
   int i;               /* Loop count */
   int iclump;          /* Number of clumps found so far */
   int imax;            /* Index of element with largest residual */
   int iter;            /* Continue finding more clumps? */
   int nbg;             /* Number of background estimates summed in sumbg */
   int ngood;           /* Number of good pixels in residuals array */
   int niter;           /* Iterations performed so far */
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

/* Allow the user to override the supplied RMS error value. */
      urms = cupidConfigD( gcconfig, "RMS", VAL__BADD );
      if( urms != VAL__BADD ) {
         rms = urms;
         if( ilevel > 2 ) {
            msgSetd( "N", rms );
            msgOut( "", "User-supplied RMS noise: ^N", status );
         }
/* If the user did not supply an RMS value, access it again, this time
   suppling the default RMS value. This is done to esnure that the
   default RMS value is stored in the CUPID NDF extension when the
   program exits. */
      } else {
         (void) cupidConfigD( gcconfig, "RMS", rms );
      }

      if( ilevel > 2 ) {
         msgSetd( "N", rms );
         msgOut( "", "RMS noise level actually used: ^N", status );

      } else if( ilevel > 1 ) {
         msgSetd( "N", rms );
         msgOut( "", "RMS noise level used: ^N", status );
      }

/* Get the lowest value (normalised to the RMS noise level) at which
   model Gaussian should be evaluated. */
      mlim = cupidConfigD( gcconfig, "MODELLIM", 0.5 );

/* Initialise the number of clumps found so far. */
      iclump = 0;
      clist = NULL;

/* Initialise the sum of the background estimates, and the number of such
   estimates summed. */
      sumbg = 0.0;
      nbg = 0;

/* Loop round fitting a gaussian to the largest remaining peak in the
   residuals array. */
      iter = 1;
      niter = 0;
      while( iter ) {

/* Report the iteration number to the user if required. */
         ++niter;         
         if( ilevel > 2 ) {
            msgBlank( status );
            msgSetd( "N", niter );
            msgOut( "", "Iteration ^N:", status );
         }

/* Find the 1D vector index of the elements with the largest value in the 
   residuals array. Also find the total data sum in the residuals array. */
         cupidGCFindMax( type, res, el, &imax, &sum, &ngood );

/* Determine if a gaussian clump should be fitted to the peak around the 
   pixel found above. */
         cupidGCIterate( type, res, imax, sum, iclump, rms, gcconfig,
                         sumbg, nbg, niter, ngood, &iter, ilevel );

/* If so, make an initial guess at the Gaussian clump parameters centred
   on the current peak. */
         if( iter ) {
            cupidGCSetInit( type, res, ipv, ndim, dims, imax, rms, gcconfig,
                            iclump, velax, x, slbnd );

/* Find the best fitting parameters, starting from the above initial
   guess. If succesful, increment the number of clumps found. */
            if( cupidGCFit( type, res, imax, x, &chisq ) ) {
               iclump++;

/* Increment the sum of the background estimates. */
               sumbg += x[ 1 ]*rms;
               nbg++;

/* Display the clump parameters on the screen if required. */
               cupidGCListClump( iclump, ndim, x, chisq, slbnd, ilevel, rms );

/* Extend the returned array of HDS Clump structures to include room for
   the new one. This list is actually a long character string containing
   room for "iclump" HDS locators. */
               clist = astGrow( clist, iclump, DAT__SZLOC + 1 );

/* Remove the fit (excluding the background) from the residuals array, and 
   add it onto the total fit array. This also updates any output array and 
   mask, and creates a HDS "Clump" structure containing information about 
   the clump. An HDS locator for this new Clump structure is added into the 
   "clist" ring. */
               cupidGCUpdateArrays( type, res, el, ndim, dims, x, rms,
                                    mlim, imax, ipo, ilevel, rmask, slbnd,    
                                    clist + ( iclump - 1 )*( DAT__SZLOC + 1 ),
                                    iclump, sumbg, nbg, diag );

/* Tell the user if no clump could be fitted around the current peak
   pixel value */
            } else if( ilevel > 2 ) {
               msgOut( "", "   No clump fitted.", status );
            }

/* Tell the user if one of the trmination criteria has ben met. */
         } else if( ilevel > 2 ) {
            msgOut( "", "   At least one termination criterion has been reached.", status );
            msgBlank( status );
         }
      }
   }

/* Tell the user the mean background level, and add this value onto any
   output array. */
   if( nbg > 0 ) {
      sumbg /= nbg;
      if( ipo ) {
         if( type == CUPID__FLOAT ) {
            outf = (float *) ipo;
            for( i = 0; i < el; i++ ) outf[ i ] += sumbg;
         } else {
            outd = (double *) ipo;
            for( i = 0; i < el; i++ ) outd[ i ] += sumbg;
         }
      }

      if( ilevel > 2 ) {
         msgSetd( "BG", sumbg );
         msgOut( "", "Estimated background level = ^BG", status );
      }
   }

/* Tell the user how many iterations have been performed (i.e. how many
   attempts there have been to fit a Gaussian peak). */
   if( ilevel > 1 ) {
      if( niter == 1 ){
         msgOut( "", "No fit attempted", status );
      } else {
         msgSeti( "M", niter - 1 - iclump );
         msgSeti( "N", niter - 1 );
         msgOut( "", "Fits attempted for ^N candidate clumps (^M failed)", status );
      }
   }

/* Tell the user how many of the iterations resulted in a succesful fit
   to a peak. */
   if( ilevel > 0 ) {
      if( iclump == 0 ) {
         msgOut( "", "No clumps found", status );
      } else if( iclump == 1 ){
         msgOut( "", "One clump found", status );
      } else {
         msgSeti( "N", iclump );
         msgOut( "", "^N clumps found", status );
      }
      msgBlank( status );
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

   gcconfig =astAnnul( gcconfig );

/* Save the number of clumps found. */
   *nclump = iclump;

/* Return the list of clump structure locators. */
   return clist;

}

