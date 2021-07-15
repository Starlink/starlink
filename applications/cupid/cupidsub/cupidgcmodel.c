#include "sae_par.h"
#include "cupid.h"
#include "prm_par.h"
#include "mers.h"
#include <math.h>

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm
   needed by this function. These are set by function cupidGaussClumps. */
extern CupidGC cupidGC;

/* Local Constants: */
/* ================ */
/* 4*ln( 2.0 ) */
#define K 2.772588722239781



double cupidGCModel( int ndim, double *x, double *par, double *ref, int what,
                     int newx, int newp, int *status ){
/*
*+
*  Name:
*     cupidGCModel

*  Purpose:
*     Evaluate a Gaussian model value or gradient at a given position.

*  Language:
*     Starlink C

*  Synopsis:
*     double cupidGCModel( int ndim, double *x, double *par, double *ref,
*                          int what, int newx, int newp, int *status )

*  Description:
*     This function evaluates the Gaussian model defined by the supplied
*     parameter values, at a given position. Either the model value
*     itself or the partial derivative of the model value with respect to
*     a given model parameter can be returned.

*  Parameters:
*     ndim
*        The number of axes in the data array.
*     x
*        Pointer to an array of "ndim" elements, holding the GRID coords
*        (within the user-supplied NDF) at which the model is to be
*        evaluated. If "ndim" is 3, then "x[2]" should be the velocity
*        value.
*     par
*        Pointer to an array holding the parameters which define the
*        model to be evaluated. It is assumed that the supplied values
*        are usable (e.g. width parameters are not zero, etc). How many of
*        these are used depends on the value of "ndim": if "ndim" is 1 only
*        elements 0 to 3 are used, if "ndim" is 2 only elements 0 to 6 are
*        used, if "ndim" is 3 all elements are used. All axis values are
*        represented in GRID pixels:
*
*           par[0]: Intrinsic peak intensity of clump ("a0" in Stutski & Gusten)
*           par[1]: Constant intensity offset ("b0" in Stutski & Gusten)
*           par[2]: Model centre offset on axis 0 ("x1_0" in Stutski & Gusten)
*           par[3]: Intrinsic FWHM on axis 0 ("D_xi_1" in Stutski & Gusten)
*           par[4]: Model centre offset on axis 1 ("x2_0" in Stutski & Gusten)
*           par[5]: Intrinsic FWHM on axis 1 ("D_xi_2" in Stutski & Gusten)
*           par[6]: Spatial orientation angle ("phi" in Stutski & Gusten)
*                   In rads, positive from +ve GRID1 axis to +ve GRID2 axis.
*           par[7]: Model centre offset on velocity axis ("v_0" in Stutski & Gusten)
*           par[8]: Intrinsic FWHM on velocity axis ("D_xi_v" in Stutski &
*                                                     Gusten)
*           par[9]: Axis 0 of internal velocity gradient vector ("alpha_0"
*                   in Stutski & Gusten), in vel. pixels per spatial pixel.
*           par[10]: Axis 1 of internal velocity gradient vector ("alpha_1"
*                   in Stutski & Gusten), in vel. pixels per spatial pixel.
*
*     ref
*        Pointer to an array of "ndim" elements, holding the GRID coords
*        (within the user-supplied NDF) of the reference position. The
*        model centre offsets supplied in elements 2, 4 and 7 of the
*        "par" array are added onto this reference position to get the
*        GRID positions of the Gaussian model peak. If "ref" is NULL, 
*        then the reference position is assumed to be zero on each axis.
*     what
*        If negative, then the function value at "x" is returned.
*        Otherwise, the partial derivative of the model value with
*        respect to the parameter "par[what]" is returned.
*     newx
*        If zero, it is assumed that "x" is the same as on the previous
*        invocation of this function. This causes cached intermediate values
*        to be re-used, thus speeding things up. A non-zero value should
*        be supplied if "x" is not the saem as on the previous invocation.
*     newp
*        If zero, it is assumed that "par" is the same as on the previous
*        invocation of this function. This causes cached intermediate values
*        to be re-used, thus speeding things up. A non-zero value should
*        be supplied if "par" is not the saem as on the previous invocation.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The model value or gradient.

*  Notes:
*     - Stutski & Gusten take account of instrumental smoothing only in so
*     far as they increase the supplied clump FWHM. This implementation
*     also reduces the peak value by a corresponding factor, since smoothing
*     will reduce the peak value in a clump. Thus par[ 0 ] represents the
*     intrinsic peak value rather than the observed peak value.

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
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     13-OCT-2005 (DSB):
*        Original version.
*     14-JUL-2021 (DSB):
*        Added argument "ref".
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */

   double demdp;           /* Rate of change of "em" wrt par[what] */
   double peakfactor_sq;   /* Square of the peak value factor */
   double t;               /* Temporary value */
   double ret;             /* Returned value */

   static double X0;       /* Rotated spatial axis 0 offset */
   static double X1;       /* Rotated spatial axis 1 offset */
   static double cosv;     /* Cos of spatial rotation angle */
   static double dv_sq;    /* Total FWHM in pixels on vel axis, squared */
   static double dx0_sq;   /* Total FWHM in pixels on axis 0, squared */
   static double dx1_sq;   /* Total FWHM in pixels on axis 1, squared */
   static double em;       /* Scalar argument to exp function  */
   static double expv;     /* Value of exp function */
   static double f3;       /* Constant for gradient wrt p[3] */
   static double f5;       /* Constant for gradient wrt p[5] */
   static double f8;       /* Constant for gradient wrt p[8] */
   static double m;        /* Finalmodel value */
   static double peak;     /* Peak value after instrumental smoothing */
   static double peakfactor;  /* Peak value factor */
   static double sinv;     /* Sin of spatial rotation angle */
   static double v_off;    /* Offset on vel axis from "x" to model peak */
   static double vt_off;   /* Total offset on vel axis from "x" to model peak */
   static double x0_off;   /* Offset on axis 0 from "x" to model peak */
   static double x1_off;   /* Offset on axis 1 from "x" to model peak */

/* Initialise */
   ret = VAL__BADD;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If neccessary, re-calculate cached items which depend only on the values
   supplied in "par" (i.e. which do not depend on values supplied in "x"). */
   if( newp ) {

/* The total FWHM in pixels on axis 0, squared. */
      t = par[ 3 ]*par[ 3 ];
      dx0_sq = cupidGC.beam_sq + t;
      f3 = cupidGC.beam_sq/( par[ 3 ]*dx0_sq );

/* The peak value factor (thsi takes account of the reduction in peak
   value caused by the instrumental smoothing). */
      peakfactor_sq = t/dx0_sq;

/* The rest are only calculated for 2 or 3 dimensions */
      if( ndim > 1 ) {

/* The total FWHM in pixels on axis 1, squared. */
         t = par[ 5 ]*par[ 5 ];
         dx1_sq = cupidGC.beam_sq + t;
         f5 = cupidGC.beam_sq/( par[ 5 ]*dx1_sq );

/* The peak value factor */
         peakfactor_sq *= t/dx1_sq;

/* Trig functions */
         cosv = cos( par[ 6 ] );
         sinv = sin( par[ 6 ] );

/* The rest is only calculated for 3 dimensions */
         if( ndim > 2 ) {

/* The total FWHM in pixels on the velocity axis, squared. */
            t = par[ 8 ]*par[ 8 ];
            dv_sq = cupidGC.velres_sq + t;
            peakfactor_sq *= t/dv_sq;
            f8 = cupidGC.velres_sq/( par[ 8 ]*dv_sq );
         }
      }

/* The peak value */
      if( peakfactor_sq > 0.0 ) {
         peakfactor = sqrt( peakfactor_sq );
      } else {
         peakfactor = 0.0;
      }
      peak = par[ 0 ]*peakfactor;
   }

/* If neccessary, re-calculate cached items which depend both on "x" and
   "par" values. */
   if( newp || newx ) {

/* Offset in pixels on axis 0 of the supplied position from the peak. */
      x0_off = x[ 0 ] - par[ 2 ];
      if( ref ) x0_off -= ref[ 0 ];

/* The 1D scalar value passed to the exp() function (excluding a factor of
   -4.ln(2) ) */
      if( ndim == 1 ) {
         em = x0_off*x0_off/dx0_sq;

/* The rest are only calculated for 2 or 3 dimensions */
      } else {

/* Offset in pixels on axis 1 of the supplied position from the peak. */
         x1_off = x[ 1 ] - par[ 4 ];
         if( ref ) x1_off -= ref[ 1 ];

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
            if( ref ) v_off -= ref[ 2 ];

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
      m = peak*expv + par[ 1 ];

   }

/* If the function value is requested, return it. */
   if( what < 0 ) {
      ret = m;

/* Need to calculate the gradient if a gradient is required. */
   } else if( what == 0 ) {
      ret = peakfactor*expv;

   } else if( what == 1 ) {
      ret = 1.0;

/* For all others, we evaluated the rate of change of "em" with respect to
   the required parameter, and then finally convert this to the rate of
   change of the model value with respect to the required parameter. */
   } else {

/* Handle 1D problems */
      if( ndim == 1 ) {
         if( what == 2 ) {
            demdp = -2*x0_off/dx0_sq;
         } else if( what == 3 ) {
            demdp = x0_off/dx0_sq;
            demdp = -2*demdp*demdp*par[ 3 ];
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

            }
         }
      }

/* If we have a value for the rate of change of "em" with respect to the
   required parameter, convert it to the rate of change of the model value
   with respect to the required parameter, or report an error. */
      if( demdp != VAL__BADD ) {
         ret = -K*demdp;

/* The clump size parameters (3, 5 and 8) affect the peak value of the
   clump because of the effect of instrumental smoothing. Add on
   appropriate terms. */
         if( what == 3 ) {
            ret += f3;

         } else if( what == 5 ) {
            ret += f5;

         } else if( what == 8 ) {
            ret += f8;
         }

/* Apply the final scaling */
         ret *= peak*expv;

      } else {
         *status = SAI__ERROR;
         msgSeti( "W", what );
         errRep( "cupidGCModel_err1", "cupidGCModel: Illegal value "
                 "(^W) supplied for \"what\" (internal CUPID programming "
                 "error).", status );
      }
   }

/* Return the required value */
   return ret;

}
