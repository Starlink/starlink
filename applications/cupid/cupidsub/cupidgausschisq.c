#include "sae_par.h"
#include "cupid.h"

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm 
   needed by this function. These are set by function cupidGaussClumps. */
extern CupidGC cupidGC;


double cupidGaussChiSq( int ndim, int npar, double *par, int what, int newp ){
/*
*  Name:
*     cupidGaussChiSq

*  Purpose:
*     The merit function to be minimised by the GaussClumps algorithm.

*  Synopsis:
*     double cupidGaussChiSq( int ndim, int npar, double *par, int what, 
*                             int newp )

*  Description:
*     This function evaluates the modified chi squared used to estimate
*     the goodness of fit between a given Gaussian clump model and the
*     residual data array, or the rate of change of the modified
*     chi-squared with respect to one of the model parameters.

*  Parameters:
*     ndim
*        The number of axes in the data array being fitted.
*     npar
*        The number of parameters defining the model clump.
*     par
*        Pointer to an array holding the parameters which define the
*        model to be measured against the data. All axis values are 
*        represented in GRID pixels: 
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
*           par[9]: Axis 0 of internal velocity gradient vector ("alpha_1" 
*                                                       in Stutski & Gusten)
*           par[10]: Axis 0 of internal velocity gradient vector ("alpha_1" 
*                                                       in Stutski & Gusten)
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
   double demdp;           /* Rate of change of "em" wrt par[what] */
   double ret;             /* Returned value */

   static double X0;       /* Rotated spatial axis 0 offset */
   static double X1;       /* Rotated spatial axis 1 offset */
   static double cosv;     /* Cos of spatial rotation angle */
   static double dv_sq;    /* Total FWHM in pixels on vel axis, squared */
   static double dx0_sq;   /* Total FWHM in pixels on axis 0, squared */
   static double dx1_sq;   /* Total FWHM in pixels on axis 1, squared */
   static double em;       /* Scalar argument to exp function  */
   static double expv;     /* Value of exp function */
   static double m;        /* Finalmodel value */
   static double sinv;     /* Sin of spatial rotation angle */
   static double v_off;    /* Offset on vel axis from "x" to model peak */
   static double vt_off;   /* Total offset on vel axis from "x" to model peak */
   static double x0_off;   /* Offset on axis 0 from "x" to model peak */
   static double x1_off;   /* Offset on axis 1 from "x" to model peak */

/* Initialise */
   ret = VAL__BADD;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If neccessary, re-calculate cached intermediate values */
   if( newp ) {

/* The difference between the model peak value and the data peak value. */
      pdiff = par[ 0 ] + par[ 1 ] - cupidGC.ymax;

/* The offset from the model centre to the data peak */
      x0_off = par[ 2 ] - cupidGC.x0_max;
      if( dim > 1 ) x1_off = par[ 4 ] - cupidGC.x1_max;
      if( dim > 2 ) v_off = par[ 7 ] - cupidGC.v_max;

/* Initialise the total chi squared value */
      chisq = 0.0;

/* Initialise pointers to the next element to be used in the arrays
   defining the data to be fitted. */
      py = cupidGC.data;
      pw = cupidGC.weight;
      pm = cupidGC.model;
      for( iax = 0; i < ndim; iax++ ) px[ iax ] = cupidGC.grid[ iax ];

/* Loop round every element in the section of the data array which is
   currently being fitted. */
      for( iel = 0; iel < cupidGC.nel; iel++ ){

/* Get the Gaussian model value at the centre of the current pixel. */
         *pm = cupidGaussModel( ndim, x, par, -1, 1, ( iel == 0 ) );

/* Store the residual between the Gaussian model at the centre of the current
   pixel and the current pixel's data value. */
         res = *py - *pm;

/* Determine a scale factor which encourages the fitted intensity to stay
   below the observed intensity. This does the same job as the 
   "s0.exp( Yi_fit - Yi )" term in the chi-squared expression given in
   the Stutski & Gusten paper. The form used here was inherited from the 
   implementation of GaussClumps (obtained from 
   ftp.astro.uni-bonn.de/pub/heith/gaussclumps on 27/9/05) upon which this 
   implementation was based. */
         rr = ( res > 0.0 ) ? 1.0 : cupidGC.s0p1;

/* Increment the running sum of chi-squared. */
         chisq += *pw*res*res*rr;

/* Move the pointers on to the next pixel in the section of the data
   array being fitted. */
         py++;
         pw++;
         pm++;
         for( iax = 0; i < ndim; iax++ ) px[ iax ]++;
      }

/* Divide by the number of degrees of freedom (the sum of the weights
   minus the number of parameters being fitted). */
      chisq /= cupidGC.ndf;











/* The rest are only calculated for 2 or 3 dimensions */
      if( ndim > 1 ) {

/* The total FWHM in pixels on axis 1, squared. */
         dx1_sq = cupidGC.beam_sq + par[ 5 ]*par[ 5 ];

/* Trig functions */
         cosv = cos( par[ 6 ] );
         sinv = sin( par[ 6 ] );

/* The rest is only calculated for 3 dimensions */
         if( ndim > 2 ) {

/* The total FWHM in pixels on the velocity axis, squared. */
            dv_sq = cupidGC.velres_sq + par[ 8 ]*par[ 8 ];
         }     
      }
   }

/* If neccessary, re-calculate cached items which depend both on "x" and
   "par" values. */
   if( newp || newx ) {

/* Offset in pixels on axis 0 of the supplied position from the peak. */
      x0_off = x[ 0 ] - par[ 2 ];

/* The 1D scalar value passed to the exp() function (excluding a factor of
   -4.ln(2) ) */
      if( ndim == 1 ) {
         em = x0_off*x0_off/dx0_sq;

/* The rest are only calculated for 2 or 3 dimensions */
      } else {

/* Offset in pixels on axis 1 of the supplied position from the peak. */
         x1_off = x[ 1 ] - par[ 4 ];

/* The offsets along the principle (rotated) axes of the 2D spatial
   ellipse */
         X0 = x0_off*cosv + x1_off*sinv;
         X1 = -x0_off*sinv + x1_off*cosv;

/* The scalar value passed to the exp() function (excluding a factor of
   -4.ln(2) ) */
         em = ( X0*X0/dx0_sq ) + ( X1*X1/dx1_sq );

/* The rest is only calculated for 3 dimensions */
         if( ndim > 2) {

/* Offset in pixels on the velocity axis of the supplied position from the 
   peak. */
            v_off = x[ 2 ] - par[ 7 ];

/* The total offset in pixels on the velocity axis, including velocity
   gradient. */
            vt_off = v_off - par[ 9 ]*x0_off - par[ 10 ]*x1_off;

/* The scalar value passed to the exp() function (excluding a factor of
   -4.ln(2) ) */
            em += vt_off*vt_off/dv_sq;
     
         }     
      }

/* The Gaussian term in the model. */
      expv = exp( -K*em );

/* The total model value. */
      m = par[ 0 ]*expv + par[ 1 ];

   }

/* If the function value is requested, return it. */
   if( what < 0 ) {
      ret = m;

/* Need to calculate the gradient if a gradient is required. */
   } else if( what == 0 ) {
      ret = expv;

   } else if( what == 1 ) {
      ret = 1.0;

/* For all others, we evaluated the rate of change of "em" with respect to 
   the required parameter, and then finally convert this to the rate of
   change of the model value with respect to the required parameter. */
   } else {

/* Handle 1D pronblems */
      if( ndim == 1 ) {
         if( what == 2 ) {
            demdp = -2*x0_off/dx0_sq;
         } else if( what == 3 ) {
            demdp = x0_off/dx0_sq;
         } else {
            demdp = VAL__BADD;
         }

/* Handle 2 and 3D pronblems */
      } else {

/* Establish the contribution to "demdp" from the 2 spatial axes. */
         if( what == 2 ) {
            demdp = -2*( X0*cosv/dx0_sq - X1*sinv/dx1_sq );

         } else if( what == 3 ) {
            demdp = X0/dx0_sq;
            demdp = -2*demdp*demdp*par[ 3 ];

         } else if( what == 4 ) {
            demdp = -2*( X0*sinv/dx0_sq + X1*cosv/dx1_sq );

         } else if( what == 5 ) {
            demdp = X1/dx1_sq;
            demdp = -2*demdp*demdp*par[ 5 ];

         } else if( what == 6 ) {
            demdp = -2*( X0*( x0_off*sinv - x1_off*cosv )/dx0_sq +
                         X1*( x0_off*cosv + x1_off*sinv )/dx1_sq );
         } else {
            demdp = VAL__BADD;
         }

/* If there is a velocity axis, modify the 2D "demdp" value (if any) 
   appropriately. */
         if( ndim > 2 ) {
            if( what == 2 ) {
               demdp += 2*par[ 9 ]*vt_off/dv_sq;
      
            } else if( what == 4 ) {
               demdp += 2*par[ 10 ]*vt_off/dv_sq;
      
            } else if( what == 7 ) {
               demdp = -2*vt_off/dv_sq;
      
            } else if( what == 8 ) {
               demdp = vt_off/dv_sq;
               demdp = -2*demdp*demdp*par[ 8 ];

            } else if( what == 9 ) {
               demdp = -2*vt_off*x0_off/dv_sq;

            } else if( what == 10 ) {
               demdp = -2*vt_off*x1_off/dv_sq;

            } else {
               demdp = VAL__BADD;
            }
         }
      } 

/* If we have a value for the rate of change of "em" with respect to the 
   required parameter, convert it to the rate of change of the model value 
   with respect to the required parameter, or report an error. */
      if( demdp != VAL__BADD ) {
         ret = -par[ 0 ]*expv*K*demdp;
      } else {
         *status = SAI__ERROR;
         errMsg( "cupidGaussChiSq_err1", "cupidGaussChiSq: Illegal value "
                 "(%d) supplied for \"what\" (internal CUPID programming "
                 "error).", status );
      }
   } 

/* Return the required value */
   return ret;   

}

