#include "sae_par.h"
#include "mers.h"
#include "ast.h"
#include "cupid.h"

void cupidGaussClumps( int type, int ndim, int *slbnd, int *subnd, void *ipd, 
                       unsigned char *ipq, double rms, AstKeyMap *config  ){
/*
*  Name:
*     cupidGaussClumps

*  Purpose:
*     Identify clumps of emission within a 2 or 3 dimensional NDF using
*     the GAUSSCLUMPS algorithm.

*  Synopsis:
*     void cupidGaussClumps( int type, int ndim, int *slbnd, int *subnd, 
*                           void *ipd, unsigned char *ipq, double rms,
*                           AstKeyMap *config )

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
*     ipq
*        Pointer to the Quality array. The elements should be stored in
*        Fortran order. If this is not NULL, a mask is written to the
*        array identifying which clump each pixel belongs to. A value of 
*        zero indicates that the pixel is not contained within any clump.
*        A non-zero value indicates that the pixel is part of the clump
*        which has the same non-zero index within the returned catalogue.
*        Only the first 511 clumps can be identified in this way. Any
*        subsequent clumps are not included in the mask. If a pixel
*        contributes to more than one clump, then the quality value 
*        associated with the pixel will be the index of the last clump
*        found.
*     rms
*        The global RMS error in the data array.
*     config
*        An AST KeyMap holding tuning parameters for the algorithm.

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
   int *dims;           /* Pointer to array of array dimensions */
   int el;              /* Number of elements in array */
   int i;               /* Loop count */
   int iclump;          /* Number of clumps found so far */
   int iter;            /* Continue finding more clumps? */
   void *res;           /* Pointer to residuals array */
   int imax;            /* Index of element with largest residual */
   double sum;          /* Sum of all residuals */

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return;

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
      rms = cupidConfigD( config, "RMS", rms );

/* Initialise the number of clumps found so far. */
      iclump = 0;

/* Loop round fitting a gaussian to the largest remaining peak in the
   residuals array. */
      iter = 1;
      while( iter ) {

/* Find the element with the largest value in the residuals array. Also 
   find the total data sum in the residuals array. */
         cupidFindMax( type, res, el, &imax, &sum );

/* Determine if a gaussian clump should be fitted to the peak around the 
   pixel found above.*/
         cupidIterate( type, res, imax, sum, iclump, rms, config, &iter );
         if( iter ) {

/* If so, make an initial guess at the Gaussian clump parameters. */
            cupidSetInit( type );

/* Find the best fitting parameters, starting from the above initial
   guess. If succesful, increment the number of clumps found. */
            if( cupidCubFit( type ) ) {
               iclump++;

/* Add the clump to the output list. */
               cupidListClump( );

/* Remove the fit from the residuals array, and add it onto the total fit
   array. */
               cupidUpdateArrays( type );

            }
         }
      }
   }

/* Free resources */
   res = astFree( res );
   dims = astFree( dims );

}

