
#include "sae_par.h"
#include "prm_par.h"
#include "cupid.h"
#include "mers.h"
#include "star/thr.h"
#include <math.h>
#include <string.h>

/* Global Variables: */
/* ================= */
/* A structure holding the global parameters of the GaussClump algorithm
   needed by this function. These are set by function cupidGaussClumps. */
extern CupidGC cupidGC;

/* Prototypes for local static functions. */
/* ====================================== */
static void cupid1GCChiSq( void *job_data_ptr, int *status );

/* Local data types */
/* ================ */
typedef struct cupid1GCChiSqData {
   size_t b1;
   size_t b2;
   double *par;
   double bg;
   double chisq;
   double wsum;
   double ret;
   int ndim;
   int oper;
   int what;
   size_t *stride;
   CupidGCModelCache *cache;
} cupid1GCChiSqData;

double cupidGCChiSq( ThrWorkForce *wfr, int ndim, double *xpar, int xwhat,
                     int newp, int *status ){
/*
*+
*  Name:
*     cupidGCChiSq

*  Purpose:
*     The merit function to be minimised by the GaussClumps algorithm.

*  Language:
*     Starlink C

*  Synopsis:
*     double cupidGCChiSq( ThrWorkForce *wfr, int ndim, double *xpar,
*                          int xwhat, int newp, int *status )

*  Description:
*     This function evaluates the modified chi squared used to estimate
*     the goodness of fit between a given Gaussian clump model and the
*     residual data array, or the rate of change of the modified
*     chi-squared with respect to one of the model parameters.
*
*     The basic chi-squared is normalised by the sum of the weights (not
*     the number of degrees of freedom as in the Stutzki & Gusten paper).

*  Parameters:
*     wfr = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     ndim
*        The number of axes in the data array being fitted.
*     xpar
*        Pointer to an array holding the parameters which define the
*        model to be measured against the data. How many of these are used
*        depends on the value of "ndim": if "ndim" is 1 only elements 0 to
*        3 are used, if "ndim" is 2 only elements 0 to 6 are used, if "ndim"
*        is 3 all elements are used. All axis values are represented in GRID
*        pixels:
*
*           xpar[0]: Intrinsic peak intensity of clump ("a0" in Stutski & Gusten)
*           xpar[1]: Constant intensity offset ("b0" in Stutski & Gusten)
*           xpar[2]: Model centre on axis 0 ("x1_0" in Stutski & Gusten)
*           xpar[3]: Intrinsic FWHM on axis 0 ("D_xi_1" in Stutski & Gusten)
*           xpar[4]: Model centre on axis 1 ("x2_0" in Stutski & Gusten)
*           xpar[5]: Intrinsic FWHM on axis 1 ("D_xi_2" in Stutski & Gusten)
*           xpar[6]: Spatial orientation angle ("phi" in Stutski & Gusten)
*                   In rads, positive from +ve GRID1 axis to +ve GRID2 axis.
*           xpar[7]: Model centre on velocity axis ("v_0" in Stutski & Gusten)
*           xpar[8]: Intrinsic FWHM on velocity axis ("D_xi_v" in Stutski &
*                                                     Gusten)
*           xpar[9]: Axis 0 of internal velocity gradient vector ("alpha_0"
*                   in Stutski & Gusten), in vel. pixels per spatial pixel.
*           xpar[10]: Axis 1 of internal velocity gradient vector ("alpha_1"
*                   in Stutski & Gusten), in vel. pixels per spatial pixel.
*
*           NOTE, if the "cupidGC.fixback" value is non-zero, then the
*           backgound level is not included in the list of free
*           parameters which are being varied by the fitting algorithm.
*           In this case, the above list changes: the background level is
*           moved from element 1 to the end of the list (the actual index
*           depends on the value of "ndim"), and the other values are shifted
*           down to fill the gap left at element 1.
*     xwhat
*        If negative, then the chi-squared value is returned. Otherwise, the
*        partial derivative of the chi-squared value with respect to the
*        parameter "xpar[what]" is returned.
*     newp
*        If zero, it is assumed that "xpar" is the same as on the previous
*        invocation of this function. This causes cached intermediate values
*        to be re-used, thus speeding things up. A non-zero value should
*        be supplied if "xpar" is not the same as on the previous invocation.
*     status
*        Pointer to the inherited status value.

*  Returned Value:
*     The chi-squared value or gradient.

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
*     13-OCT-2005 (DSB):
*        Original version.
*     9-MAR-2007 (DSB):
*        Fix bugs in the algorithm used to reduce the weights in regions
*        that do not contribute to the fit.
*     14-JAN-2009 (TIMJ):
*        Use MERS for message filtering.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/

/* Local Variables: */
   cupid1GCChiSqData *job_data = NULL;
   cupid1GCChiSqData *pdata;
   double *par;            /* Pointer to parameter array to be used */
   double *pim;            /* Pointer for next initial model value */
   double *pm;             /* Pointer for storing next model value */
   double back_term;       /* chi squared term to stop large shifts in bg level */
   double dx_sq;           /* Smoothed beam width */
   double gback_term;      /* Gradient term to stop large shifts in bg level */
   double ret;             /* Returned value */
   double t;               /* Temporary storage */
   double ypar[ 11 ];      /* "xpar" ordered as if bckgnd is being fitted */
   int i;                  /* Parameter index */
   int iel;                /* Index of pixel within section currently being fitted */
   int iworker;
   int nworker;
   int what;               /* "xwhat" value assuming bckgnd is being fitted */
   size_t step;
   size_t stride[ 3 ];     /* Strides between pixels within fitted area */


   static double bg;       /* Last times background value */
   static double chisq;    /* Total modified chi squared */
   static double f3;       /* Beam smoothing factor for p[3] */
   static double f5;       /* Beam smoothing factor for p[5] */
   static double f8;       /* Beam smoothing factor for p[8] */
   static double pdiff;    /* Difference between model and data peak values */
   static double peakfactor;/* Smoothing factor for peak value */
   static double v_off;    /* Offset on vel axis from data to model peak */
   static double x0_off;   /* Offset on axis 0 from data to model peak */
   static double x1_off;   /* Offset on axis 1 from data to model peak */
   static CupidGCModelCache *modelcaches = NULL;

/* Initialise */
   ret = VAL__BADD;

/* Abort if an error has already occurred. */
   if( *status != SAI__OK ) return ret;

/* Store diagnostic info */
   if( cupidGC.nf == 1 ) {
      for( i = 0; i < 11; i++ ) cupidGC.initpars[ i ] = xpar[ i ];
      bg = xpar[ 1 ];
   } else {
      for( i = 0; i < 11; i++ ) cupidGC.pars[ i ] = xpar[ i ];
   }

/* If the background is not included in the list of free parameters
   being varied, then the background value will be at the end of the
   supplied "xpar" array. Reorder the supplied parameter values to put
   the background value at its usual place (index [1]). This is needed
   since all the following code assumes that the background is stored at
   index [1]. Also set up terms to be added to the chi-squared value and
   gradient. These cause the returned chi-squared to rise if the background
   value wanders far from the initial background value (this is done because
   the background level is usually determined by data values with very low
   weight and so is not well constrained). */
   if( cupidGC.fixback ) {
      ypar[ 0 ] = xpar[ 0 ];
      ypar[ 1 ] = xpar[ cupidGC.npar - 1 ];
      memcpy( ypar + 2, xpar + 1, sizeof( double )*( cupidGC.npar - 2 ) );
      par = ypar;
      what = ( xwhat <= 0 ) ? xwhat : xwhat + 1;
      back_term = 0.0;
      gback_term = 0.0;

/* If the background is included in the list of free parameters being
   varied, then just use the parameter values as supplied. */
   } else {
      par = xpar;
      what = xwhat;
      back_term = xpar[ 1 ] - cupidGC.initpars[ 1 ];
      gback_term = 2*cupidGC.sb*back_term;
      back_term *= cupidGC.sb*back_term;
   }

/* If neccessary, re-calculate cached intermediate values */
   if( newp ) {

/* Strides within the section being fitted. */
      stride[ 0 ] = 1;
      if( ndim > 1 ) {
         stride[ 1 ] = cupidGC.ubnd[ 0 ] -  cupidGC.lbnd[ 0 ] + 1;
         if( ndim > 2 ) {
            stride[ 2 ] = stride[ 1 ]*( cupidGC.ubnd[ 1 ] - cupidGC.lbnd[ 1 ] + 1 );
         }
      }

/* Check the FWHM values are positive. */
      if( par[ 3 ] <= 0.0 ) return ret;
      if( ndim > 1 ){
         if( par[ 5 ] <= 0.0 ) return ret;
         if( ndim > 2 && par[ 8 ] <= 0.0 ) return ret;
      }

/* Get the factor by which to correct the peak amplitude of the model to
   take account of the smoothing by the instrumental beam. */
      t = par[ 3 ]*par[ 3 ];
      dx_sq = cupidGC.beam_sq + t;
      peakfactor = t/dx_sq;
      f3 = par[ 0 ]*cupidGC.beam_sq/( par[ 3 ]*dx_sq );

      if( ndim > 1 ) {
         t = par[ 5 ]*par[ 5 ];
         dx_sq = cupidGC.beam_sq + t;
         peakfactor *= t/dx_sq;
         f5 = par[ 0 ]*cupidGC.beam_sq/( par[ 5 ]*dx_sq );

         if( ndim > 2 ) {
            t = par[ 8 ]*par[ 8 ];
            dx_sq = cupidGC.velres_sq + t;
            peakfactor *= t/dx_sq;
            f8 = par[ 0 ]*cupidGC.velres_sq/( par[ 8 ]*dx_sq );
         }
      }

      if( peakfactor > 0.0 ) {
         peakfactor = sqrt( peakfactor );
      } else {
         peakfactor = 0.0;
      }

      f3 *= peakfactor;
      f5 *= peakfactor;
      f8 *= peakfactor;

/* The difference between the model peak value (after being reduced to
   take account of instrumental smoothing) and the data peak value. */
      pdiff = peakfactor*par[ 0 ] + par[ 1 ] - cupidGC.ymax;

/* Get the number f worker threads to use. */
      nworker = wfr ? wfr->nworker : 1;

/* If not already allocated, allocate an array of structures to cache
   information used by cupidGCModel, one for each thread. Note this
   memory is never freed (except by termination of the monolith). */
      if( !modelcaches ) modelcaches = astMalloc( nworker*sizeof(*modelcaches) );

/* Allocate an array of structures, eahc of which described a job to be
   execited by a worker thread. */
      job_data = astMalloc( nworker*sizeof( *job_data ) );
      if( *status == SAI__OK ){

/* Get the number of pixels to process in each worker thread. */
         if( cupidGC.nel > 2*nworker ){
            step = cupidGC.nel/nworker;
         } else {
            step = cupidGC.nel;
            nworker = 1;
         }

/* If this is the first iteration, modify the Gaussian weights to weight
   down pixels that are in neighbouring overlapping sources. This is
   guessed by looking at how far the data is from the initial model. */
         if( cupidGC.nf == 1 ) {

            for( iworker = 0; iworker < nworker; iworker++ ) {
               pdata = job_data + iworker;

               pdata->b1 = iworker*step;
               if( iworker < nworker - 1 ) {
                  pdata->b2 = pdata->b1 + step - 1;
               } else {
                  pdata->b2 = cupidGC.nel - 1;
               }

               pdata->par = par;
               pdata->oper = 3;
               pdata->ndim = ndim;
               pdata->stride = stride;
               pdata->cache = modelcaches + iworker;

               thrAddJob( wfr, 0, pdata, cupid1GCChiSq, 0, NULL, status );
            }

            thrWait( wfr, status );

            cupidGC.wsum = 0;
            for( iworker = 0; iworker < nworker; iworker++ ) {
               pdata = job_data + iworker;
               cupidGC.wsum += pdata->wsum;
            }
         }

/* Submit a job to each worker thread to get the sum of the chi squared
   over a corresponding set of pixels. */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;

/* The index of the first and last pixel to be processed by the worker. */
            pdata->b1 = iworker*step;
            if( iworker < nworker - 1 ) {
               pdata->b2 = pdata->b1 + step - 1;
            } else {
               pdata->b2 = cupidGC.nel - 1;
            }

/* Other values neede by each worker thread. */
            pdata->par = par;
            pdata->oper = 1;
            pdata->bg = bg;
            pdata->ndim = ndim;
            pdata->stride = stride;
            pdata->cache = modelcaches + iworker;

/* Submit the job to the workforce. */
            thrAddJob( wfr, 0, pdata, cupid1GCChiSq, 0, NULL, status );
         }

/* Wait for all jobs to complete. */
         thrWait( wfr, status );

/* Sum the contributions from all threads. */
         chisq = 0.0;
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;
            chisq += pdata->chisq;
         }

/* The offset from the model centre to the data peak */
         x0_off = par[ 2 ];
         if( ndim > 1 ) x1_off = par[ 4 ];
         if( ndim > 2 ) v_off = par[ 7 ];

/* Remember the background value for next time. */
         bg = par[ 1 ];

/* Divide by the sum of the weights . */
         chisq /= cupidGC.wsum;

/* Modify this basic chi-squared value as described in the Stutski &
   Gusten paper. */
         if( ndim == 1 ) {
            t = ( cupidGC.beam_sq > 0.0 ) ? x0_off*x0_off/cupidGC.beam_sq : 0.0;
         } else {
            t = ( cupidGC.beam_sq > 0.0 ) ?
                  ( x0_off*x0_off + x1_off*x1_off )/cupidGC.beam_sq : 0.0;
            if( ndim == 3 && cupidGC.velres_sq > 0.0 ) t += v_off*v_off/cupidGC.velres_sq;
         }
         chisq += cupidGC.sa*pdiff*pdiff + cupidGC.sc4*t + back_term;

/* Store more diagnostic info */
         if( cupidGC.nf == 1 ) {
            pim = cupidGC.initmodel;
            pm = cupidGC.model;
            for( iel = 0; iel < cupidGC.nel; iel++ ) *(pim++) = *(pm++);
         }
         cupidGC.chisq = chisq;
      }
   }

/* Select or calculate the required return value.  If the chi squared
   value itself is required, just return the value found above. */
   if( what < 0 ) {
      ret = chisq;

      cupidGCDumpF( MSG__DEBUG3, NULL, 0, NULL, NULL, status );

      msgSeti( "NF", cupidGC.nf );
      msgOutif( MSG__DEBUG3, "", "   Fit attempt ^NF:", status );

      msgSetd( "C", ret );
      msgOutif( MSG__DEBUG3, "", "      Chi-squared: ^C", status );

      msgSetd( "V", par[ 0 ] );
      msgOutif( MSG__DEBUG3, "", "      Peak intensity: ^V", status );
      msgSetd( "V", par[ 1 ] );
      msgOutif( MSG__DEBUG3, "", "      Constant background: ^V", status );
      msgSetd( "V", cupidGC.x_max[ 0 ] + par[ 2 ] );
      msgOutif( MSG__DEBUG3, "", "      Centre on 1st axis: ^V", status );
      msgSetd( "V", par[ 3 ] );
      msgOutif( MSG__DEBUG3, "", "      FWHM on 1st axis: ^V", status );

      if( ndim > 1 ) {
         msgSetd( "V", cupidGC.x_max[ 1 ] + par[ 4 ] );
         msgOutif( MSG__DEBUG3, "", "      Centre on 2nd axis: ^V", status );
         msgSetd( "V", par[ 5 ] );
         msgOutif( MSG__DEBUG3, "", "      FWHM on 2nd axis: ^V", status );
         msgSetd( "V", par[ 6 ] );
         msgOutif( MSG__DEBUG3, "", "      Position angle: ^V", status );

         if( ndim > 2 ) {
            msgSetd( "V", cupidGC.x_max[ 2 ] + par[ 7 ] );
            msgOutif( MSG__DEBUG3, "", "      Centre on vel axis: ^V", status );
            msgSetd( "V", par[ 8 ] );
            msgOutif( MSG__DEBUG3, "", "      FWHM on vel axis: ^V", status );
            msgSetd( "V", par[ 9 ] );
            msgOutif( MSG__DEBUG3, "", "      Vel gradient on 1st axis: ^V", status );
            msgSetd( "V", par[ 10 ] );
            msgOutif( MSG__DEBUG3, "", "      Vel gradient on 2nd axis: ^V", status );
         }
      }

/* If the rate of change of the chi squared with respect to one of the
   model parameters is required, we have more work. */
   } else if( *status == SAI__OK ){

/* Ensure we have the structures to pass information to the worker threads. */
      if( !job_data ){

/* Strides within the section being fitted. */
         stride[ 0 ] = 1;
         if( ndim > 1 ) {
            stride[ 1 ] = cupidGC.ubnd[ 0 ] -  cupidGC.lbnd[ 0 ] + 1;
            if( ndim > 2 ) {
               stride[ 2 ] = stride[ 1 ]*( cupidGC.ubnd[ 1 ] - cupidGC.lbnd[ 1 ] + 1 );
            }
         }

/* Get the number f worker threads to use. */
         nworker = wfr ? wfr->nworker : 1;

/* Allocate an array of structures, ach of which described a job to be
   execited by a worker thread. */
         job_data = astMalloc( nworker*sizeof( *job_data ) );
         if( *status == SAI__OK ){

/* Get the number of pixels to process in each worker thread. */
            if( cupidGC.nel > 2*nworker ){
               step = cupidGC.nel/nworker;
            } else {
               step = cupidGC.nel;
               nworker = 1;
            }

/* Store basic info in each structure. */
            for( iworker = 0; iworker < nworker; iworker++ ) {
               pdata = job_data + iworker;

/* The index of the first and last pixel to be processed by the worker. */
               pdata->b1 = iworker*step;
               if( iworker < nworker - 1 ) {
                  pdata->b2 = pdata->b1 + step - 1;
               } else {
                  pdata->b2 = cupidGC.nel - 1;
               }

               pdata->par = par;
               pdata->bg = bg;
               pdata->ndim = ndim;
               pdata->stride = stride;
               pdata->cache = modelcaches + iworker;
            }
         }
      }

/* Check all went well. */
      if( *status == SAI__OK ) {

/* Submit a job to each worker thread to get the sum of the rate of
   change of the Gaussian model value with respect to the required
   parameter, at the centre of each pixel in the thread's section. */
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;
            pdata->oper = 2;
            pdata->what = what;
            thrAddJob( wfr, 0, pdata, cupid1GCChiSq, 0, NULL, status );
         }

/* Wait for all jobs to complete. */
         thrWait( wfr, status );

/* Sum the contributions from all threads. */
         ret = 0.0;
         for( iworker = 0; iworker < nworker; iworker++ ) {
            pdata = job_data + iworker;
            ret += pdata->ret;
         }

/* Scale the returned value to relate to a normalised chi-squared. */
         ret *= -2.0/cupidGC.wsum;

/* If the parameter for which we are finding the gradient is involved in
   the extra terms added to chi squared by the Stutski & Gusten paper,
   then we have extra terms to add to the gradient found above. */
         if( what == 0 ) {
            ret += 2*cupidGC.sa*pdiff*peakfactor;

         } else if( what == 1 ) {
            ret += 2*cupidGC.sa*pdiff + gback_term;

         } else if( what == 2 ) {
            if( cupidGC.beam_sq > 0.0 ) ret += 2*cupidGC.sc4*x0_off/cupidGC.beam_sq;

         } else if( what == 3 ) {
            ret += 2*cupidGC.sa*pdiff*f3;

         } else if( what == 4 ) {
            if( cupidGC.beam_sq > 0.0 ) ret += 2*cupidGC.sc4*x1_off/cupidGC.beam_sq;

         } else if( what == 5 ) {
            ret += 2*cupidGC.sa*pdiff*f5;

         } else if( what == 7 ) {
            if( cupidGC.velres_sq > 0.0 ) ret += 2*cupidGC.sc4*v_off/cupidGC.velres_sq;

         } else if( what == 8 ) {
            ret += 2*cupidGC.sa*pdiff*f8;

         }
      }
   }

/* Free resources. */
   job_data = astFree( job_data );

/* Return the required value */
   return ret;

}


static void cupid1GCChiSq( void *job_data_ptr, int *status ) {
/*
*  Name:
*     cupid1GCChiSq

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     cupidGCChiSq.

*  Invocation:
*     cupid1GCChiSq( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = cupid1GCChiSqData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   cupid1GCChiSqData *pdata;
   size_t b1;
   size_t b2;
   double *par;
   double *pm;             /* Pointer for storing next model value */
   double *pr;             /* Pointer for storing next scaled residual */
   double *prs;            /* Pointer for storing next absolute residual */
   double *pu;             /* Pointer for storing next unscaled residual */
   double *pw;             /* Pointer to next weight value to use */
   double *py;             /* Pointer to next data value to use */
   double bg;		   /* Background value */
   double g;               /* Rate of change of model value */
   double m;               /* Model value */
   double res;             /* Difference between data and model value */
   double rr;              /* A factor for the residual to suppress -ve residuals */
   double wf;              /* Weight factor */
   double x[ 3 ];          /* Next pixel position at which to get model value */
   int dbg;                /* Has background changed? */
   int iax;
   int ndim;
   int what;
   size_t *stride;
   size_t iel;             /* Index of pixel within section currently being fitted */
   size_t off;
   size_t rem;
   CupidGCModelCache *cache;


/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (cupid1GCChiSqData *) job_data_ptr;

/* Save values inlocal variables for ease of reference. */
   b1 = pdata->b1;
   b2 = pdata->b2;
   par = pdata->par;
   stride = pdata->stride;
   ndim = pdata->ndim;
   bg = pdata->bg;
   cache = pdata->cache;

/* Calculate the ch squared for a section of the array being fitted. */
   if( pdata->oper == 1 ) {

/* Initialise pointers to the next element to be used in the arrays
   defining the data to be fitted. Note, the elements in these arays have
   fortran ordering (i.e. axis 0 varies most rapidly). */
      py = cupidGC.data + b1;
      pw = cupidGC.weight + b1;
      pr = cupidGC.res + b1;
      pu = cupidGC.resu + b1;
      pm = cupidGC.model + b1;
      prs = cupidGC.resids + b1;

/* Initialise running sums. */
      pdata->chisq = 0.0;

/* Pixel indices, within the full NDF, of the first pixel to be processed by
   this worker thread. */
      rem = b1;
      for( iax = ndim - 1; iax >= 0; iax-- ) {
         off = rem/stride[ iax ];
         rem -= off*stride[ iax ];
         x[ iax ] = cupidGC.lbnd[ iax ] + off;
      }

/* Loop round all elements assigned to this thread in the section of the
   data array which is currently being fitted. */
      for( iel = b1; iel <= b2; iel++ ){

/* Get the Gaussian model value at the centre of the current pixel. Store
   the residual between the Gaussian model at the centre of the current
   pixel and the current pixel's data value. */
         cache->newx = 1;
         cache->newp = ( iel == b1 );
         m = cupidGCModel( ndim, x, par, cupidGC.x_max, -1, cache,
                           status );
         res = *py - m;

/* Save the residual and model value at this pixel */
         *pu = res;
         *pm = m;

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
         pdata->chisq += *pr*res;
         *prs = *pr*res;

/* Move the pointers on to the next pixel in the section of the data
   array being fitted. */
         py++;
         pw++;
         pr++;
         pu++;
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

/* Calculate the rate of change of chi squared with respect to a model
   parameter for a section of the array being fitted. */
   } else if( pdata->oper == 2 ) {
      what = pdata->what;

/* Initialise pointer to the next element to be used in the array
   holding the scaled residuals at each pixel. */
      pr = cupidGC.res + b1;

/* Pixel indices, within the full NDF, of the first pixel to be processed by
   this worker thread. */
      rem = b1;
      for( iax = ndim - 1; iax >= 0; iax-- ) {
         off = rem/stride[ iax ];
         rem -= off*stride[ iax ];
         x[ iax ] = cupidGC.lbnd[ iax ] + off;
      }

/* Initialise running sums. */
      pdata->ret = 0;

/* Loop over all pixels in the section of the data array which is being
   fitted and assigned to the current thread, accumulating the contribution
   to the required value caused by the rate of change of the model itself
   with respect to the required parameter. */
      for( iel = b1; iel <= b2; iel++ ){

/* Get the rate of change of the Gaussian model value with respect to the
   required parameter, at the centre of the current pixel. */
         cache->newx = 1;
         cache->newp = 0;
         g = cupidGCModel( ndim, x, par, cupidGC.x_max, what, cache,
                           status );

/* Increment the running sum of the returned value. */
         pdata->ret += *pr*g;

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

/* Modify the Gaussian weight to weight down pixels that include
   contributions form neighbouring clumps. */
   } else if( pdata->oper == 3 ) {

/* Initialise pointers to the next element to be used in the arrays
   defining the data to be fitted. Note, the elements in these arays have
   fortran ordering (i.e. axis 0 varies most rapidly). */
      py = cupidGC.data + b1;
      pw = cupidGC.weight + b1;

/* Initialise running sums. */
      pdata->wsum = 0.0;

/* Pixel indices, within the full NDF, of the first pixel to be processed by
   this worker thread. */
      rem = b1;
      for( iax = ndim - 1; iax >= 0; iax-- ) {
         off = rem/stride[ iax ];
         rem -= off*stride[ iax ];
         x[ iax ] = cupidGC.lbnd[ iax ] + off;
      }

/* Loop round all elements assigned to this thread in the section of the
   data array which is currently being fitted. */
      for( iel = b1; iel <= b2; iel++ ){

/* Get the Gaussian model value at the centre of the current pixel. */
         cache->newx = 1;
         cache->newp = ( iel == b1 );
         m = cupidGCModel( ndim, x, par, cupidGC.x_max, -1, cache,
                           status );

/* Get the residual. If the residual is not zero, modify the Gaussian
   weight in order to give lower weight to the points that are far from the
   initial model. Never allow a weight to increase (only decrease). */
         res = *py - m;
         if( res != 0.0 ){
            wf = fabs( m/res );
            if( wf < 1.0 ) *pw *= fabs( m/res );
         }

/* Increment running sums. */
         pdata->wsum += *pw;

/* Move the pointers on to the next pixel in the section of the data
   array being fitted. */
         py++;
         pw++;

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

/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "cupid1GCChiSq: Invalid operation (%d) supplied.",
               status, pdata->oper );
   }
}

