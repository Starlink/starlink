#include "sae_par.h"
#include "prm_par.h"
#include "cupid.h"

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm 
   needed by this function. These are set by function cupidGaussClumps. */
extern CupidGC cupidGC;


double cupidGCChiSq( int ndim, double *par, int what, int newp ){
/*
*  Name:
*     cupidGCChiSq

*  Purpose:
*     The merit function to be minimised by the GaussClumps algorithm.

*  Synopsis:
*     double cupidGCChiSq( int ndim, double *par, int what, int newp )

*  Description:
*     This function evaluates the modified chi squared used to estimate
*     the goodness of fit between a given Gaussian clump model and the
*     residual data array, or the rate of change of the modified
*     chi-squared with respect to one of the model parameters.

*  Parameters:
*     ndim
*        The number of axes in the data array being fitted.
*     par
*        Pointer to an array holding the parameters which define the
*        model to be measured against the data. How many of these are used
*        depends on the value of "ndim": if "ndim" is 1 only elements 0 to 
*        3 are used, if "ndim" is 2 only elements 0 to 6 are used, if "ndim" 
*        is 3 all elements are used. All axis values are represented in GRID 
*        pixels: 
*
*           par[0]: Peak intensity of clump ("a0" in Stutski & Gusten)
*           par[1]: Constant intensity offset ("b0" in Stutski & Gusten)
*           par[2]: Model centre on axis 0 ("x1_0" in Stutski & Gusten)
*           par[3]: Intrinsic FWHM on axis 0 ("D_xi_1" in Stutski & Gusten)
*           par[4]: Model centre on axis 1 ("x2_0" in Stutski & Gusten)
*           par[5]: Intrinsic FWHM on axis 1 ("D_xi_2" in Stutski & Gusten)
*           par[6]: Spatial orientation angle ("phi" in Stutski & Gusten)
*                   In rads, positive from +ve GRID1 axis to +ve GRID2 axis.
*           par[7]: Model centre on velocity axis ("v_0" in Stutski & Gusten)
*           par[8]: Intrinsic FWHM on velocity axis ("D_xi_v" in Stutski & 
*                                                     Gusten)
*           par[9]: Axis 0 of internal velocity gradient vector ("alpha_0" 
*                   in Stutski & Gusten), in vel. pixels per spatial pixel.
*           par[10]: Axis 1 of internal velocity gradient vector ("alpha_1" 
*                   in Stutski & Gusten), in vel. pixels per spatial pixel.
*     what
*        If negative, then the chi-squared value is returned. Otherwise, the 
*        partial derivative of the chi-squared value with respect to the 
*        parameter "par[what]" is returned.
*     newp
*        If zero, it is assumed that "par" is the same as on the previous 
*        invocation of this function. This causes cached intermediate values 
*        to be re-used, thus speeding things up. A non-zero value should
*        be supplied if "par" is not the same as on the previous invocation.

*  Returned Value:
*     The chi-squared value or gradient.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-OCT-2005 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*/      

/* Local Variables: */
   double *pim;            /* Pointer for next initial model value */
   double *pm;             /* Pointer for storing next model value */
   double *pr;             /* Pointer for storing next scaled residual */
   double *prs;            /* Pointer for storing next absolute residual */
   double *pw;             /* Pointer to next weight value to use */
   double *py;             /* Pointer to next data value to use */
   double g;               /* Rat eof change of model value */
   double m;               /* Model value */
   double res;             /* Difference between data and model value */
   double ret;             /* Returned value */
   double rr;              /* A factor for the residual to suppress -ve residuals */
   double t;               /* Temporary storage */
   double x[ 3 ];          /* Next pixel position at which to get model value */ 
   int i;                  /* Parameter index */
   int iax;                /* Axis index */
   int iel;                /* Index of pixel within section currently being fitted */ 
 
   static double chisq;    /* Total modified chi squared */  
   static double pdiff;    /* Difference between model and data peak values */
   static double v_off;    /* Offset on vel axis from data to model peak */
   static double x0_off;   /* Offset on axis 0 from data to model peak */
   static double x1_off;   /* Offset on axis 1 from data to model peak */

/* Initialise */
   ret = VAL__BADD;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If neccessary, re-calculate cached intermediate values */
   if( newp ) {

/* The difference between the model peak value and the data peak value. */
      pdiff = par[ 0 ] + par[ 1 ] - cupidGC.ymax;

/* The offset from the model centre to the data peak */
      x0_off = par[ 2 ] - cupidGC.x_max[ 0 ];
      if( ndim > 1 ) x1_off = par[ 4 ] - cupidGC.x_max[ 1 ];
      if( ndim > 2 ) v_off = par[ 7 ] - cupidGC.x_max[ 2 ];

/* Initialise the total chi squared value */
      chisq = 0.0;

/* Initialise pointers to the next element to be used in the arrays
   defining the data to be fitted. Note, the elements in these arays have
   fortran ordering (i.e. axis 0 varies most rapidly). */
      py = cupidGC.data;
      pw = cupidGC.weight;
      pr = cupidGC.res;
      pm = cupidGC.model;
      prs = cupidGC.resids;

      for( iax = 0; iax < ndim; iax++ ) x[ iax ] = cupidGC.lbnd[ iax ];

/* Loop round every element in the section of the data array which is
   currently being fitted. */
      for( iel = 0; iel < cupidGC.nel; iel++ ){

/* Get the Gaussian model value at the centre of the current pixel. Store 
   the residual between the Gaussian model at the centre of the current
   pixel and the current pixel's data value. */
         m = cupidGCModel( ndim, x, par, -1, 1, ( iel == 0 ) );
         *pm = m;
         res = *py - m;
         *prs = res;

/* Determine a scale factor which encourages the fitted intensity to stay
   below the observed intensity. This does the same job as the 
   "s0.exp( Yi_fit - Yi )" term in the chi-squared expression given in
   the Stutski & Gusten paper. The form used here was inherited from the 
   implementation of GaussClumps (obtained from 
   ftp.astro.uni-bonn.de/pub/heith/gaussclumps on 27/9/05) upon which this 
   implementation was based. */
         rr = ( res > 0.0 ) ? 1.0 : cupidGC.s0p1;

/* Increment the running sum of chi-squared. We save the scaled residuals
   in a work array (pr) so that we do not need to calculate them again if 
   this function is called subsequently to find the gradient for the same
   set of parameer values. */
         *pr = *pw*res*rr;         
         chisq += *pr*res;

/* Move the pointers on to the next pixel in the section of the data
   array being fitted. */
         py++;
         pw++;
         pr++;
         pm++;
         prs++;

/* Get the grid coords (within the full size original data array) of the
   next pixel in the section currently being fitted. This assumes fortran
   ordering of the elements in the arrays.*/
         iax = 0;
         x[ iax ] += 1.0;
         while( x[ iax ] > cupidGC.ubnd[ iax ] ) {
            x[ iax ] = cupidGC.lbnd[ iax ];
            if( ++iax == ndim ) break;
            x[ iax ] += 1.0;
         }

      }

/* Divide by the number of degrees of freedom (the sum of the weights
   minus the number of parameters being fitted). */
      chisq /= cupidGC.ndf;

/* Modify this basic chi-squared value as described in the Stutski &
   Gusten paper. */
      if( ndim == 1 ) {
         t = ( cupidGC.beam_sq > 0.0 ) ? x0_off*x0_off/cupidGC.beam_sq : 0.0;
      } else { 
         t = ( cupidGC.beam_sq > 0.0 ) ? 
               ( x0_off*x0_off + x1_off*x1_off )/cupidGC.beam_sq : 0.0;
         if( ndim == 3 && cupidGC.velres_sq > 0.0 ) t += v_off*v_off/cupidGC.velres_sq;
      }
      chisq += cupidGC.sa*pdiff*pdiff + cupidGC.sc4*t;

/* Store diagnostic info */
      if( cupidGC.nf == 1 ) {
         for( i = 0; i < 11; i++ ) cupidGC.initpars[ i ] = par[ i ];

         pim = cupidGC.initmodel;
         pm = cupidGC.model;
         for( iel = 0; iel < cupidGC.nel; iel++ ) *(pim++) = *(pm++);

      } else {
         for( i = 0; i < 11; i++ ) cupidGC.pars[ i ] = par[ i ];

      }
      cupidGC.chisq = chisq;

   }

/* Select or calculate the required return value.  If the chi squared
   value itself is required, just return the value found above. */
   if( what < 0 ) {
      ret = chisq;

/* If the rate of change of the chi squared with respect to one of the
   model parameters is required, we have more work. */
   } else {
    
/* Initialise pointer to the next element to be used in the array
   holding the scaled residuals at each pixel. */
      pr = cupidGC.res;

/* Initialise the grid coords (within the complete data array) of the
   first pixel in the section of the data array being fitted. */
      for( iax = 0; iax < ndim; iax++ ) x[ iax ] = cupidGC.lbnd[ iax ];

/* Loop over all pixels in the section of the data array which is being
   fitted, accumulating the contribution to the required value caused by the 
   rate of change of the model itself with respect to the required
   parameter. */
      ret = 0.0;
      for( iel = 0; iel < cupidGC.nel; iel++ ){

/* Get the rate of change of the Gaussian model value with respect to the
   required parameter, at the centre of the current pixel. */
         g = cupidGCModel( ndim, x, par, what, 1, 0 );

/* Increment the running sum of the returned value. */
         ret += *pr*g;

/* Move the pointer on to the next pixel in the section of the data
   array being fitted. */
         pr++;

/* Get the grid coords (within the full size original data array) of the
   next pixel in the section currently being fitted. */
         iax = 0;
         x[ iax ] += 1.0;
         while( x[ iax ] > cupidGC.ubnd[ iax ] ) {
            x[ iax ] = cupidGC.lbnd[ iax ];
            if( ++iax == ndim ) break;
            x[ iax ] += 1.0;
         }
      }

/* Scale the returned value to relate to a normalised chi-squared. */
      ret *= -2.0/cupidGC.ndf;

/* If the parameter for which we are finding the gradient is involved in
   the extra terms added to chi squared by the Stutski & Gusten paper,
   then we have extra terms to add to the gradient found above. */
      if( what == 0 || what == 1 ) {
         ret += 2*cupidGC.sa*pdiff;

      } else if( what == 2 ) {
         if( cupidGC.beam_sq > 0.0 ) ret += 2*cupidGC.sc4*x0_off/cupidGC.beam_sq;

      } else if( what == 4 ) {
         if( cupidGC.beam_sq > 0.0 ) ret += 2*cupidGC.sc4*x1_off/cupidGC.beam_sq;

      } else if( what == 7 ) {
         if( cupidGC.velres_sq > 0.0 ) ret += 2*cupidGC.sc4*v_off/cupidGC.velres_sq;
      }

   }


/* Return the required value */
   return ret;   

}

