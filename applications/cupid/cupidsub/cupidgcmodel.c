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



double cupidGCModel( int ndim, const double *x, const double *par,
                     const double *ref, int what,
                     CupidGCModelCache *cache, int *status ){
/*
*+
*  Name:
*     cupidGCModel

*  Purpose:
*     Evaluate a Gaussian model value or gradient at a given position.

*  Language:
*     Starlink C

*  Synopsis:
*     double cupidGCModel( int ndim, const double *x, const double *par,
*                          const double *ref, int what,
*                          CupidGCModelCache *cache, int *status )

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
*     cache
*        Pointer to structure to store cached intermediate values
*        to be re-used, thus speeding things up.
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
*     3-NOV-2021 (DSB):
*        Replaced arguments "newx" and "newp" with "cache". This is for
*        thread safety.
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

/* Initialise */
   ret = VAL__BADD;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* If neccessary, re-calculate cached items which depend only on the values
   supplied in "par" (i.e. which do not depend on values supplied in "x"). */
   if( cache->newp ) {

/* The total FWHM in pixels on axis 0, squared. */
      t = par[ 3 ]*par[ 3 ];
      cache->dx0_sq = cupidGC.beam_sq + t;
      cache->f3 = cupidGC.beam_sq/( par[ 3 ]*cache->dx0_sq );

/* The peak value factor (thsi takes account of the reduction in peak
   value caused by the instrumental smoothing). */
      peakfactor_sq = t/cache->dx0_sq;

/* The rest are only calculated for 2 or 3 dimensions */
      if( ndim > 1 ) {

/* The total FWHM in pixels on axis 1, squared. */
         t = par[ 5 ]*par[ 5 ];
         cache->dx1_sq = cupidGC.beam_sq + t;
         cache->f5 = cupidGC.beam_sq/( par[ 5 ]*cache->dx1_sq );

/* The peak value factor */
         peakfactor_sq *= t/cache->dx1_sq;

/* Trig functions */
         cache->cosv = cos( par[ 6 ] );
         cache->sinv = sin( par[ 6 ] );

/* The rest is only calculated for 3 dimensions */
         if( ndim > 2 ) {

/* The total FWHM in pixels on the velocity axis, squared. */
            t = par[ 8 ]*par[ 8 ];
            cache->dv_sq = cupidGC.velres_sq + t;
            peakfactor_sq *= t/cache->dv_sq;
            cache->f8 = cupidGC.velres_sq/( par[ 8 ]*cache->dv_sq );
         }
      }

/* The peak value */
      if( peakfactor_sq > 0.0 ) {
         cache->peakfactor = sqrt( peakfactor_sq );
      } else {
         cache->peakfactor = 0.0;
      }
      cache->peak = par[ 0 ]*cache->peakfactor;
   }

/* If neccessary, re-calculate cached items which depend both on "x" and
   "par" values. */
   if( cache->newp || cache->newx ) {

/* Offset in pixels on axis 0 of the supplied position from the peak. */
      cache->x0_off = x[ 0 ] - par[ 2 ];
      if( ref ) cache->x0_off -= ref[ 0 ];

/* The 1D scalar value passed to the exp() function (excluding a factor of
   -4.ln(2) ) */
      if( ndim == 1 ) {
         cache->em = cache->x0_off*cache->x0_off/cache->dx0_sq;

/* The rest are only calculated for 2 or 3 dimensions */
      } else {

/* Offset in pixels on axis 1 of the supplied position from the peak. */
         cache->x1_off = x[ 1 ] - par[ 4 ];
         if( ref ) cache->x1_off -= ref[ 1 ];

/* The offsets along the principle (rotated) axes of the 2D spatial
   ellipse */
         cache->X0 = cache->x0_off*cache->cosv + cache->x1_off*cache->sinv;
         cache->X1 = -cache->x0_off*cache->sinv + cache->x1_off*cache->cosv;

/* The scalar value passed to the exp() function (excluding a factor of
   -4.ln(2) ) */
         cache->em = ( cache->X0*cache->X0/cache->dx0_sq ) + ( cache->X1*cache->X1/cache->dx1_sq );

/* The rest is only calculated for 3 dimensions */
         if( ndim > 2) {

/* Offset in pixels on the velocity axis of the supplied position from the
   peak. */
            cache->v_off = x[ 2 ] - par[ 7 ];
            if( ref ) cache->v_off -= ref[ 2 ];

/* The total offset in pixels on the velocity axis, including velocity
   gradient. */
            cache->vt_off = cache->v_off - par[ 9 ]*cache->x0_off - par[ 10 ]*cache->x1_off;

/* The scalar value passed to the exp() function (excluding a factor of
   -4.ln(2) ) */
            cache->em += cache->vt_off*cache->vt_off/cache->dv_sq;

         }
      }

/* The Gaussian term in the model. */
      cache->expv = exp( -K*cache->em );

/* The total model value. */
      cache->m = cache->peak*cache->expv + par[ 1 ];

   }

/* If the function value is requested, return it. */
   if( what < 0 ) {
      ret = cache->m;

/* Need to calculate the gradient if a gradient is required. */
   } else if( what == 0 ) {
      ret = cache->peakfactor*cache->expv;

   } else if( what == 1 ) {
      ret = 1.0;

/* For all others, we evaluated the rate of change of "em" with respect to
   the required parameter, and then finally convert this to the rate of
   change of the model value with respect to the required parameter. */
   } else {

/* Handle 1D problems */
      if( ndim == 1 ) {
         if( what == 2 ) {
            demdp = -2*cache->x0_off/cache->dx0_sq;
         } else if( what == 3 ) {
            demdp = cache->x0_off/cache->dx0_sq;
            demdp = -2*demdp*demdp*par[ 3 ];
         } else {
            demdp = VAL__BADD;
         }

/* Handle 2 and 3D pronblems */
      } else {

/* Establish the contribution to "demdp" from the 2 spatial axes. */
         if( what == 2 ) {
            demdp = -2*( cache->X0*cache->cosv/cache->dx0_sq - cache->X1*cache->sinv/cache->dx1_sq );

         } else if( what == 3 ) {
            demdp = cache->X0/cache->dx0_sq;
            demdp = -2*demdp*demdp*par[ 3 ];

         } else if( what == 4 ) {
            demdp = -2*( cache->X0*cache->sinv/cache->dx0_sq + cache->X1*cache->cosv/cache->dx1_sq );

         } else if( what == 5 ) {
            demdp = cache->X1/cache->dx1_sq;
            demdp = -2*demdp*demdp*par[ 5 ];

         } else if( what == 6 ) {
            demdp = -2*( cache->X0*( cache->x0_off*cache->sinv - cache->x1_off*cache->cosv )/cache->dx0_sq +
                         cache->X1*( cache->x0_off*cache->cosv + cache->x1_off*cache->sinv )/cache->dx1_sq );
         } else {
            demdp = VAL__BADD;
         }

/* If there is a velocity axis, modify the 2D "demdp" value (if any)
   appropriately. */
         if( ndim > 2 ) {
            if( what == 2 ) {
               demdp += 2*par[ 9 ]*cache->vt_off/cache->dv_sq;

            } else if( what == 4 ) {
               demdp += 2*par[ 10 ]*cache->vt_off/cache->dv_sq;

            } else if( what == 7 ) {
               demdp = -2*cache->vt_off/cache->dv_sq;

            } else if( what == 8 ) {
               demdp = cache->vt_off/cache->dv_sq;
               demdp = -2*demdp*demdp*par[ 8 ];

            } else if( what == 9 ) {
               demdp = -2*cache->vt_off*cache->x0_off/cache->dv_sq;

            } else if( what == 10 ) {
               demdp = -2*cache->vt_off*cache->x1_off/cache->dv_sq;

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
            ret += cache->f3;

         } else if( what == 5 ) {
            ret += cache->f5;

         } else if( what == 8 ) {
            ret += cache->f8;
         }

/* Apply the final scaling */
         ret *= cache->peak*cache->expv;

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
