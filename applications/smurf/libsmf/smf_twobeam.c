/*
*+
*  Name:
*     smf_twobeam

*  Purpose:
*     Fit a two component circular Gaussian to a supplied point source,
*     incorporating deconvolution with a tophat function.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     SMURF subroutine

*  Invocation:
*     smf_twobeam( ThrWorkForce *wf, double *array, dim_t nx, dim_t ny,
*                  double pixsize, double *cx, double *cy, double radius,
*                  const char *log, int fitback, int fittwo, double *a1, double *a2,
*                  double *fwhm1, double *fwhm2, double *back, double *resids,
*                  double *rms, int *status )

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     array = double * (Given)
*        Point to 2D array holding data to be fitted.
*     nx = dim_t (Given)
*        Number of pixels along one row of the array.
*     ny = dim_t (Given)
*        Number of rows in the array.
*     pixsize = double (Given)
*        Pixel size in arc-sec.
*     cx = double * (Given and Returned)
*        GRID coord on axis 1 of beam centre. Updated on exit to hold the
*        fitted value.
*     cy = double * (Given and Returned)
*        GRID coord on axis 2 of beam centre. Updated on exit to hold the
*        fitted value.
*     radius = double (Given)
*        Radius of tophat function, in arc-sec.
*     log = const char * (Given)
*        Pointer to a string holding the name of a log file. No log file
*        is created if the string has zero length. The log file has the
*        format of a topcat "ascii" table.
*     fitback = int (Given)
*        If non-zero, the background level is included in the fitting
*        process as a free parameter. If zero, the background level is
*        fixed at zero.
*     fittwo = int (Given)
*        If non-zero, two Gaussians are used in the fit. Otherwise, only
*        one Gaussian is fitted.
*     a1 = double * (Returned)
*        Returned holding the amplitude of the first component Gaussian.
*     a2 = double * (Returned)
*        Returned holding the amplitude of the second component Gaussian.
*        Zero is always returned if "fittwo" is zero.
*     fwhm1 = double * (Returned)
*        Returned holding the FWHM (arc-sec) of the first component
*        Gaussian.
*     fwhm2 = double * (Returned)
*        Returned holding the FWHM (arc-sec) of the second component
*        Gaussian.  Zero is always returned if "fittwo" is zero.
*     back = double * (Returned)
*        Returned holding the background level. This will be zero if
*        "fitback" is zero.
*     resids = double * (Returned)
*        An array in which is returned the residuals between the supplied
*        array and the fitted model. It should have the same shape and
*        size as the supplied array.
*     rms = double * (Returned)
*        Returned holding the RMS residual between "array" and the fit.
*     status = int * (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine finds the parameters of a two-component (or one-component
*     if "fittwo" is zero) Gaussian beam by doing a least squares fit to a
*     supplied 2D NDF containing an image of a compact source. The source
*     need not be a point source - it is assumed to be a sharp-edged circular
*     disc of specified radius. On each iteration of the fitting process, this
*     source is convolved with the candidate beam and the residuals with the
*     supplied image are then found. The parameters of the beam are modified
*     on each iteration to minimise the sum of the squared residuals. The
*     beam consists of two concentric Gaussians added together. The first
*     Gaussian has a fixed amplitude of 1.0. The following parameters are
*     varied during the minimisation process:
*
*     - The FWHM of the first Gaussian.
*     - The FWHM and amplitude of the second Gaussian (if "fittwo" is non-zero)
*     - The centre position of the beam within the supplied image.
*     - The peak value in the source.
*     - The background level (if "fitback" is non-zero)

*  Authors:
*     DSB: David Berry (EAO)
*     {enter_new_authors_here}

*  History:
*     12-NOV-2020 (DSB):
*        Initial version
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2020 East Asian Observatory.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 3 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* Standard includes */
#include <math.h>
#include "gsl/gsl_multimin.h"

/* Starlink includes */
#include "sae_par.h"
#include "prm_par.h"
#include "mers.h"
#include "star/thr.h"

/* SMURF includes */
#include "smf.h"

/* Factor for converting standard deviations to FWHM (sqrt(8*ln(2))) */
#define S2F 2.35482

/* The convolutions are done on a finer grid than that of the supplied
   array. This macro holds the pixel size of the fine grid, in arc-sec. */
#define PSIZE 0.2

/* Local data types; */
typedef struct Params {
   FILE *fd;
   ThrWorkForce *wf;
   double *array;  /* Array of data values */
   double *out;
   double *source;
   double beamrad;
   double maxm;
   double fmin;
   double ssr;
   double pixsize;
   double srcrad;  /* Radius of tophat source (arc-sec) */
   hdsdim nx;      /* Length of above arrays */
   hdsdim nxs;
   hdsdim ny;      /* Length of above arrays */
   int dims[2];
   int fitback;
   int fittwo;
   int last_is_best;
   int reset;
} Params;

typedef struct SmfTwoBeamData {
   double *array;
   double *beam;
   double *lut;
   double *out;
   double *result;
   double *source;
   double a1;
   double a2;
   double back;
   double bsum;
   double cx;
   double cy;
   double f;
   double fac1;
   double fac2;
   double pixsize;
   double maxm;
   double sigma2;
   double sigma1;
   double srchgt;
   hdsdim l1;
   hdsdim l2;
   hdsdim nx;
   hdsdim nxb;
   hdsdim nxf;
   hdsdim nxs;
   hdsdim yoff;
   int ncomp;
   int operation;
} SmfTwoBeamData;




/* Prototypes for local functions: */
static void smf1_twobeam( void *job_data_ptr, int *status );
static double *smf1_source( double radius, hdsdim *nxs, int *status );
static void smf1_init_guess( Params *params, double cx, double cy,
                             double *a1, double *a2, double *srchgt,
                             double *fwhm1, double *fwhm2, double *back,
                             hdsdim *ngood, int *status );
static double smf1_f( const gsl_vector *v, void *pars );
static double *smf1_beam( ThrWorkForce *wf, SmfTwoBeamData *job_data,
                          hdsdim nxb, double a1, double a2,
                          double sigma1, double sigma2, double *beamsum,
                          int *status );
static double smf1_domin( Params *params, hdsdim ngood, double *srchgt, double *a1,
                          double *a2, double *fwhm1, double *fwhm2,
                          double *back, double *cx, double *cy, double *rms,
                          int *iter, int *status );




/* Main entry point */
void smf_twobeam( ThrWorkForce *wf, double *array, dim_t nx, dim_t ny,
                  double pixsize, double *cx, double *cy, double radius,
                  const char *log, int fitback, int fittwo, double *a1, double *a2,
                  double *fwhm1, double *fwhm2, double *back, double *resids,
                  double *rms, int *status ){

/* Local Variables: */
   FILE *fd = NULL;
   Params params;
   double f;
   double f_last;
   double srchgt;
   hdsdim ngood;
   int ifit;
   int iter;
   int nfit;

/* Initialise returned values */
   *a1 = VAL__BADD;
   *a2 = VAL__BADD;
   *back = VAL__BADD;
   *fwhm1 = VAL__BADD;
   *fwhm2 = VAL__BADD;
   *rms = VAL__BADD;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Store information to be passed to the cost function. */
   params.wf = wf;
   params.out = resids;
   params.array = array;
   params.nx = nx;
   params.ny = ny;
   params.pixsize = pixsize;
   params.srcrad = radius;
   params.source = smf1_source( radius, &(params.nxs), status );
   params.fitback = fitback;
   params.fd = NULL;

/* Get an initial guess at the beam parameters (an analytical single Gaussian
   approximation to a point source). */
   smf1_init_guess( &params, *cx, *cy, a1, a2, &srchgt, fwhm1, fwhm2, back,
                    &ngood, status );

/* Check a reasonable initial guess was obtained. */
   if( srchgt == VAL__BADD || *fwhm1 > 1000.0 || *fwhm2 > 1000.0 ){
      if( *status == SAI__OK ) {
         *status = SAI__ERROR;
         errRep( " ", "Could not get a believable initial guess at the "
                 "beam size.", status );
      }
   } else {

/* If we are fitting two Gaussian, we do an initial fit with only one
   Gaussian, then do a subsequent second fit with two Gaussian that uses the
   result of the first fit as the initial guess at the parameters. */
      nfit = fittwo ? 2 : 1;
      for( ifit = 0; ifit < nfit; ifit++ ){
         params.fittwo = ifit;
         params.reset = 1;
         if( fittwo ){
            if( ifit == 0 ) {
               msgOut(" ", "Doing initial single Gaussian fit...", status );
            } else {
               msgOut(" ", "Doing full two Gaussian fit...", status );

/* Set an initial guess at the width of the second Gaussian. Using zero
   produces a bad fit. */
               *fwhm2 = 2*(*fwhm1);
            }
         }

/* If this is the last fit, open the log file if required. */
         if( ifit == nfit - 1 ){
            if( log[ 0 ] ) {
               fd = fopen( log, "w" );
               fprintf( fd, "# Iter srchgt a1 a2 sigma1 sigma2 back f cx cy\n" );
            } else {
               fd = NULL;
            }
            params.fd = fd;
         }

/* Do an initial minimisation, starting from the guess found above. */
         iter = 0;
         f = smf1_domin( &params, ngood, &srchgt, a1,  a2, fwhm1, fwhm2, back, cx, cy,
                         rms, &iter, status );

/* If the above minimisation managed to reduce the cost to less than 0.9 of
   its original value, then do another minimisation starting from the best
   point found in the previous minimisation. */
         f_last = 2*f;
         while( f < 0.9*f_last && *status == SAI__OK ) {
            f_last = f;
            f = smf1_domin( &params, ngood, &srchgt, a1,  a2, fwhm1, fwhm2, back, cx, cy,
                            rms, &iter, status );
         }
      }

/* Clean up. */
      if( fd ) fclose( fd );
      params.source = astFree( params.source );
   }
}




/*-------------------------------------------------------------------------
  Do a single minimisation, starting from the supplied parameter values
*/
static double smf1_domin( Params *params, hdsdim ngood, double *srchgt, double *a1,
                          double *a2, double *fwhm1, double *fwhm2,
                          double *back, double *cx, double *cy, double *rms,
                          int *iter, int *status ){

/* Local Variables: */
   double cost = 0.0;
   double sigma1;
   double sigma2;
   double size = 0.0;
   gsl_multimin_function my_func;
   gsl_multimin_fminimizer *s;
   gsl_vector *step_size;
   gsl_vector *x;
   int gsl_status;
   int thisiter;

/* Check inherited status */
   if( *status != SAI__OK ) return 1.0;

/* Store details of the service routines that calculate the function to
   be minimised and its derivatives. */
   my_func.n = params->fitback ? 5 : 4;
   if( params->fittwo ) my_func.n += 2;
   my_func.f = &smf1_f;
   my_func.params = (void *) params;

/* Store the starting point. Convert FWHM values to standard deviations.
   The minimisation keeps the amplitude of the primary Gaussian fixed at 1.0,
   and varies the amplitude of the secondary Gaussian and source height
   (plus the beam centre, the two Gaussian widths and - maybe - background
   level). */
   x = gsl_vector_alloc( my_func.n );
   if( params->fittwo ) {
      gsl_vector_set( x, 0, *a2 );
      gsl_vector_set( x, 1, *srchgt );
      gsl_vector_set( x, 2, (*fwhm1)/S2F );
      gsl_vector_set( x, 3, (*fwhm2)/S2F );
      gsl_vector_set( x, 4, *cx );
      gsl_vector_set( x, 5, *cy );
      if( params->fitback ) gsl_vector_set( x, 6, *back );
   } else {
      gsl_vector_set( x, 0, *srchgt );
      gsl_vector_set( x, 1, (*fwhm1)/S2F );
      gsl_vector_set( x, 2, *cx );
      gsl_vector_set( x, 3, *cy );
      if( params->fitback ) gsl_vector_set( x, 4, *back );
   }

/* Store the initial step sizes. */
   step_size = gsl_vector_alloc( my_func.n );
   if( params->fittwo ) {
      gsl_vector_set( step_size, 0, 0.1 );
      gsl_vector_set( step_size, 1, 0.1*(*srchgt) );
      gsl_vector_set( step_size, 2, params->pixsize );
      gsl_vector_set( step_size, 3, params->pixsize );
      gsl_vector_set( step_size, 4, params->pixsize );
      gsl_vector_set( step_size, 5, params->pixsize );
      if( params->fitback ) gsl_vector_set( step_size, 6, 0.1*(*srchgt) );
   } else {
      gsl_vector_set( step_size, 0, 0.1*(*srchgt) );
      gsl_vector_set( step_size, 1, params->pixsize );
      gsl_vector_set( step_size, 2, params->pixsize );
      gsl_vector_set( step_size, 3, params->pixsize );
      if( params->fitback ) gsl_vector_set( step_size, 4, 0.1*(*srchgt) );
   }

/* Create a minimiser. */
   s = gsl_multimin_fminimizer_alloc( gsl_multimin_fminimizer_nmsimplex2,
                                      my_func.n );

/* Store the service routine, step size and tolerance in the minimiser. */
   gsl_multimin_fminimizer_set( s, &my_func, x, step_size );

/* Iterate to a solution. */
   thisiter = *iter;
   gsl_status = GSL_CONTINUE;
   while( gsl_status == GSL_CONTINUE && ++thisiter < 2000 ){
      gsl_status = gsl_multimin_fminimizer_iterate( s );
      if( gsl_status ) break;
      size = gsl_multimin_fminimizer_size( s );
      cost = s->fval;
      msgOutiff( MSG__VERB, " ", "Iter: %d  cost: %g  size: %g", status, thisiter,
               cost, size );
      gsl_status = gsl_multimin_test_size( size, 0.02 );
   }

   msgOutf( " ", "Iter: %d  cost: %g  size: %g", status, thisiter,
            cost, size );

/* If the final call to smf1_f was not the call that generated the best
   fit, the values in params will refer to some sub-optimal fit. In this
   case call smf_f again to generate the values for the optimal fit,
   which we return to the caller. */
   if( !params->last_is_best ) (void) smf1_f( x, params );

/* Get the beam parameters. */
   *a1 = 1.0;
   if( params->fittwo ) {
      *a2 = gsl_vector_get( s->x, 0 );
      *srchgt = gsl_vector_get( s->x, 1 );
      sigma1 = gsl_vector_get( s->x, 2 );
      sigma2 = gsl_vector_get( s->x, 3 );
      *cx = gsl_vector_get( s->x, 4 );
      *cy = gsl_vector_get( s->x, 5 );
      if( params->fitback ) *back = gsl_vector_get( s->x, 6 );
   } else {
      *a2 = 0.0;
      *srchgt = gsl_vector_get( s->x, 0 );
      sigma1 = gsl_vector_get( s->x, 1 );
      sigma2 = 0.0;
      *cx = gsl_vector_get( s->x, 2 );
      *cy = gsl_vector_get( s->x, 3 );
      if( params->fitback ) *back = gsl_vector_get( s->x, 4 );
   }

/* Convert standard deviations to FWHMs. */
   *fwhm1 = sigma1*S2F;
   *fwhm2 = sigma2*S2F;

/* Return the minimum cost. */
   *iter = thisiter;

/* Return the minimum RMS residual. */
   *rms = sqrt( params->ssr/ngood );

/* Clean up. */
   gsl_multimin_fminimizer_free( s );
   gsl_vector_free( x );
   gsl_vector_free( step_size );

   return cost;
}




/*-------------------------------------------------------------------------
  Calculate an initial guess at the beam parameters.
*/
static void smf1_init_guess( Params *params, double cx, double cy,
                             double *a1, double *a2, double *srchgt,
                             double *fwhm1, double *fwhm2, double *back,
                             hdsdim *ngood, int *status ){

/* Local Variables: */
   dim_t ix;
   dim_t iy;
   dim_t nx;
   dim_t ny;
   double pixsize;
   double *pa;
   double dx;
   double dy;
   double dmax;
   double dmin;
   double s1;
   double s2;
   double s3;
   double radsq;
   gsl_vector *x;
   int s4;

/* Initialise returned values */
   *a1 = VAL__BADD;
   *a2 = VAL__BADD;
   *srchgt = VAL__BADD;
   *back = VAL__BADD;
   *fwhm1 = VAL__BADD;
   *fwhm2 = VAL__BADD;
   *ngood = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Copy values to local variables */
   nx = params->nx;
   ny = params->ny;
   pixsize = params->pixsize;

/* Initialise things and then loop round all good pixels in the supplied
   array */
   pa = params->array;
   dmax = VAL__MIND;
   dmin = VAL__MAXD;
   s1 = 0.0;
   s2 = 0.0;
   s3 = 0.0;
   s4 = 0;
   for( iy = 1; iy <= ny; iy++ ) {
      for( ix = 1; ix <= nx; ix++,pa++ ) {
         if( *pa != VAL__BADD ) {
            (*ngood)++;

/* Calculate the squared radius (i.e. the distance from the source centre)
   in arc-sec at the centre of the current pixel. Note, the centre position
   (cx,cy) is in GRID coords - the centre of the first pixel is at grid
   coords (1.0,1.0).  */
            dx = ix - cx;
            dy = iy - cy;
            radsq = ( dx*dx + dy*dy )*pixsize*pixsize;

/* Also find the max and min values in the data array. */
            if( *pa > dmax ) dmax = *pa;
            if( *pa < dmin ) dmin = *pa;

/* Also update the sums needed to find the weighted mean of the squared
   radius, where the weight is the data value minus the minimum data value. */
            s1 += (*pa)*radsq;
            s2 += (*pa);
            s3 += radsq;
            s4++;
         }
      }
   }

/* The initial guess is a single gaussian with peak value 1.0. */
   *a1 = 1.0;
   *a2 = 0.0;

/* Find the square root of half the weighted mean of the squared radius. If
   the source were a single Gaussian, this would be equal to the standard
   deviation of the source. Convert to FWHM and return. */
   *fwhm1 = sqrt( 0.5*(s1 - dmin*s3)/(s2 - s4*dmin) );

/* Since the second Gaussian has amplitude zero, its FWHM is immaterial.
   Arbitrarily set it to twice the FWHM of the first Gaussian. */
   *fwhm2 = 2*(*fwhm1);

/* The peak value of mars is the max data value. The background is
   assumed ot be zero. */
   *srchgt = dmax;
   *back = 0.0;

/* Use the cost function to get the maximum value in the model implied by
   the initial guess beam parameters. */
   if( params->fittwo ){
      x = gsl_vector_alloc( params->fitback ? 7 : 6 );
      gsl_vector_set( x, 0, *a2 );
      gsl_vector_set( x, 1, *srchgt );
      gsl_vector_set( x, 2, (*fwhm1)/S2F );
      gsl_vector_set( x, 3, (*fwhm2)/S2F );
      gsl_vector_set( x, 4, cx );
      gsl_vector_set( x, 5, cy );
      if( params->fitback ) gsl_vector_set( x, 6, *back );
   } else {
      x = gsl_vector_alloc( params->fitback ? 5 : 4 );
      gsl_vector_set( x, 0, *srchgt );
      gsl_vector_set( x, 1, (*fwhm1)/S2F );
      gsl_vector_set( x, 2, cx );
      gsl_vector_set( x, 3, cy );
      if( params->fitback ) gsl_vector_set( x, 4, *back );
   }
   (void) smf1_f( x, params );
   gsl_vector_free( x );

/* Store a better value for the source height. This takes account of the
   fact that the smoothing will reduce the peak value, so the real peak
   value will be higher than the observed peak value by a factor that
   depends on how heavy the smoothing is. */
   *srchgt *= *srchgt/( params->maxm - *back );

/* Store the radius, in arc-sec, of the circle in which the beam has a
   significant value. */
   params->beamrad = 6*(*fwhm1);
}




/*-------------------------------------------------------------------------
  Calculate the cost function given a supplied set of  beam parameters.
*/
static double smf1_f( const gsl_vector *v, void *pars ) {

/* Local Variables: */
   Params *params = (Params *) pars;
   SmfTwoBeamData *job_data = NULL;
   SmfTwoBeamData *pdata;
   ThrWorkForce *wf;
   double *beam = NULL;
   double *grid = NULL;
   double *lut = NULL;
   double *source;
   double a1;
   double a2;
   double back;
   double bsum;
   double cx;
   double cy;
   double f;
   double fac;
   double pixsize;
   double sigma1;
   double sigma2;
   double srchgt;
   hdsdim nx;
   hdsdim nxb;
   hdsdim nxs;
   hdsdim ny;
   hdsdim step;
   hdsdim yoff;
   int fittwo;
   int iw;
   int nw0;
   int nw;
   int status = SAI__OK;
   static int iter = 0;

/* Initialise */
   f = 0.0;

/* Copy fixed values into local variables for convenience. */
   nx = params->nx;
   ny = params->ny;
   source = params->source;
   nxs = params->nxs;
   pixsize = params->pixsize;
   wf = params->wf;
   fittwo = params->fittwo;

/* Reset the iteration number of we have started a new fit. */
   if( params->reset ) {
      iter = 1;
      params->fmin = VAL__MAXD;
      params->reset = 0;
   } else {
      iter++;
   }

/* Get the current parameters of the beam. */
   a1 = 1.0;
   if( fittwo ){
      a2 = gsl_vector_get( v, 0 );
      srchgt = gsl_vector_get( v, 1 );
      sigma1 = gsl_vector_get( v, 2 );
      sigma2 = gsl_vector_get( v, 3 );
      cx = gsl_vector_get( v, 4 );
      cy = gsl_vector_get( v, 5 );
      back = params->fitback ? gsl_vector_get( v, 6 ) : 0.0;
   } else {
      a2 = 0.0;
      srchgt = gsl_vector_get( v, 0 );
      sigma1 = gsl_vector_get( v, 1 );
      sigma2 = sigma1*2;
      cx = gsl_vector_get( v, 2 );
      cy = gsl_vector_get( v, 3 );
      back = params->fitback ? gsl_vector_get( v, 4 ) : 0.0;
   }

/* Get the number of pixels on the fine grid to use to represent the full
   significant width of the beam determined above. Ensure this is odd so
   that the beam is centred on the centre of a pixel. The fine grid has a
   pixel size of PSIZE arc-sec and the beam is centred on the centre of
   the fine grid. */
   nxb = 2*(params->beamrad/PSIZE) + 1;
   if( nxb < 100 ) nxb = 100;

/* Set the constant row offset between the centre of the beam array and
   source array. */
   yoff = nxb/2 - nxs/2;

/* How many threads do we get to play with */
   nw = wf ? wf->nworker : 1;

/* Allocate job data for threads. */
   job_data = astCalloc( nw, sizeof(*job_data) );
   if( job_data ) {

/* Create a square array holding the candidate beam values on this fine
   grid. This returns the total data sum in the beam array. */
      beam = smf1_beam( wf, job_data, nxb, a1, a2, sigma1, sigma2, &bsum,
                        &status );

/* Allocate memory to hold a look-up-table which gives the value of the
   convolution of the source and beam as a function of the distance between
   the source centre and beam centre. The source and beam are both circular
   and so there should be azimuthal symetry. The x separation between source
   and beam changes on each pass round this loop but the y separation remains
   constant at zero. Initially, the beam and source are concentric (i.e.
   the first element in this LUT corresponds to zero separation between
   beam centres and source centre). */
      lut = astMalloc( nxb*sizeof( *lut ) );

/* Find how many LUT entries to process in each worker thread. */
      if( nxb <= nw ) {
         step = 1;
         nw0 = (int) nxb;
      } else {
         step = nxb/nw;
         nw0 = nw;
      }

/* Store the range of LUT entries to be processed by each worker thread.
   Ensure that the last thread picks up any left-over entries. */
      for( iw = 0; iw < nw0; iw++ ) {
         pdata = job_data + iw;
         pdata->l1 = iw*step;
         if( iw < nw0 - 1 ) {
            pdata->l2 = pdata->l1 + step - 1;
         } else {
            pdata->l2 = nxb - 1 ;
         }

/* Store other info needed by the worker thread. */
         pdata->nxb = nxb;
         pdata->nxs = nxs;
         pdata->lut = lut;
         pdata->yoff = yoff;
         pdata->beam = beam;
         pdata->bsum = bsum;
         pdata->source = source;
         pdata->srchgt = srchgt;
         pdata->operation = 1;

/* Submit the job to the worker thread. */
         thrAddJob( wf, 0, pdata, smf1_twobeam, 0, NULL, &status );
      }

/* Wait until all jobs have been completed. */
      thrWait( wf, &status );

/* Fill the model grid with the value of the convolution of the source centred
   at (cx,cy) with the current beam, and add on the background. */
      if( ny <= nw ) {
         step = 1;
         nw0 = (int) ny;
      } else {
         step = ny/nw;
         nw0 = nw;
      }
      for( iw = 0; iw < nw0; iw++ ) {
         pdata = job_data + iw;
         pdata->l1 = iw*step;
         if( iw < nw0 - 1 ) {
            pdata->l2 = pdata->l1 + step - 1;
         } else {
            pdata->l2 = ny - 1 ;
         }

         pdata->cx = cx;
         pdata->cy = cy;
         pdata->pixsize = pixsize;
         pdata->nx = nx;
         pdata->back = back;
         pdata->out = params->out;
         pdata->array = params->array;
         pdata->operation = 2;
         thrAddJob( wf, 0, pdata, smf1_twobeam, 0, NULL, &status );
      }
      thrWait( wf, &status );

/* Add up the running sums returned by each thread and find the max model
   value. */
      f = 0.0;
      params->maxm = VAL__MIND;
      for( iw = 0; iw < nw0; iw++ ) {
         pdata = job_data + iw;
         f += pdata->f;
         if( pdata->maxm > params->maxm ) params->maxm = pdata->maxm;
      }

/* Record the cost function (i.e. the sum of the squared residuals) before
   we apply the following tension factor. This can be used to get the RMS
   residual. */
     params->ssr = f;

/* If the two Gaussians have the same width, they become redundant. So
   encourage the second Gaussian to be significantly wider than the
   first. */
#define SIGMA_TOL 5.0
#define BIGFAC 100.0
      if( params->fittwo ){
         if( sigma2 - sigma1 <= 0.0 ) {
            f *= BIGFAC;
         } else {
            fac = SIGMA_TOL/( sigma2 - sigma1 );
            if( fac > BIGFAC ) {
               f *= BIGFAC;
            } else if( fac > 1.0 ) {
               f *= fac;
            }
         }

/* The second Gaussian and the background level become redunant if the
   width of the second gaussian is comparable to or larger than the width of
   the array. So encourage the second Gaussian to be no more than the average
   of the FWHM of the first Gaussian and the width of the array. */
         if( params->fitback ) {
            fac = 2*sigma2/( sqrt(nx*ny)*pixsize/S2F + sigma1 );
            if( fac > 1 ) f *= BIGFAC*fac;
         }

/* Encourage smaller aplitude second Gaussian. */
         f *= ( 1.0 + 0.5*fabs(a2) );
      }
#undef SIGMA_TOL
#undef BIGFAC

/* If required, print a line holding the interesting values to the log
   file. */
      if( params->fd ) fprintf( params->fd, "%d %g %g %g %g %g %g %g %g %g\n", iter,
                                srchgt, a1, a2, sigma1, sigma2, back, f, cx, cy );

/* Tell the user the cost. */
      msgOutiff( MSG__DEBUG, " ", "   Call: %d   normalised cost: %g", &status, iter, f );

/* If the cost function is lower than the previous lowest, then the
   contents of params will now describe the best fit. */
      if( f < params->fmin ){
         params->fmin = f;
         params->last_is_best = 1;
      } else {
         params->last_is_best = 0;
      }
   }

/* Free resources */
   lut = astFree( lut );
   grid = astFree( grid );
   beam = astFree( beam );
   job_data = astFree( job_data );

   return f;
}



/*-------------------------------------------------------------------------
   Create a 2D array with fine pixels (size given by PSIZE) holding the
   beam shape specified by the supplied parameters. The beam is centred
   on the centre of the array, which is square with dimension "nxb". The
   total data sum in the array is returned in "*beamsum". The returned
   pointer should be freed using astFree when no longer needed.
*/
static double *smf1_beam( ThrWorkForce *wf, SmfTwoBeamData *job_data,
                          hdsdim nxb, double a1, double a2,
                          double sigma1, double sigma2, double *beamsum,
                          int *status ){

/* Local Variables: */
   SmfTwoBeamData *pdata;
   double *result;
   double fac1;
   double fac2;
   hdsdim step;
   int iw;
   int nw;

/* Initialise returned values */
   *beamsum = 0.0;

/* Check inherited status */
   if( *status != SAI__OK ) return NULL;

/* Allocate the returned array. */
   result = astMalloc( nxb*nxb*sizeof(*result) );
   if( result ){

/* Factors for exponential functions. */
      fac1 = -0.5/(sigma1*sigma1);
      fac2 = -0.5/(sigma2*sigma2);

/* How many threads do we get to play with */
      nw = wf ? wf->nworker : 1;

/* How many array lines are to be processed by each worker thread. */
      if( nxb < nw ) {
         step = 1;
         nw = (int) nxb;
      } else {
         step = nxb/nw;
      }

/* Store the range of array lines to be processed by each worker thread.
   Ensure that the last thread picks up any left-over lines. */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         pdata->l1 = iw*step;
         if( iw < nw - 1 ) {
            pdata->l2 = pdata->l1 + step - 1;
         } else {
            pdata->l2 = nxb - 1;
         }

/* Store other info needed by the worker thread. */
         pdata->nxb = nxb;
         pdata->a1 = a1;
         pdata->a2 = a2;
         pdata->fac1 = fac1;
         pdata->fac2 = fac2;
         pdata->sigma1 = sigma1;
         pdata->sigma2 = sigma2;
         pdata->result = result;
         pdata->operation = 3;

/* Submit the job to the worker thread. */
         thrAddJob( wf, 0, pdata, smf1_twobeam, 0, NULL, status );
      }

/* Wait until all jobs have been completed. */
      thrWait( wf, status );

/* Sum the contributions to beamsum */
      for( iw = 0; iw < nw; iw++ ) {
         pdata = job_data + iw;
         *beamsum += pdata->bsum;
      }
   }

   return result;
}


/*-------------------------------------------------------------------------
   Create a 2D array with fine pixels (size given by PSIZE) holding the
   source top-hat (1.0 in the source disc, zero outside the source disc).
   The source is centred on the centre of the array, which is square with
   dimension "*nxs".
*/
static double *smf1_source( double radius, hdsdim *nxs, int *status ){

/* Local Variables: */
   double *pr;
   double *result;
   double rad2lim;
   double radsq;
   double x;
   double y;
   double ysq;
   hdsdim ixc;
   hdsdim iyc;
   hdsdim ix;
   hdsdim iy;

/* Initialise */
   result = NULL;
   *nxs = 0;

/* Check inherited status */
   if( *status != SAI__OK ) return result;

/* Array dimensions. */
   *nxs = 2*(radius/PSIZE) + 1;

/* Allocate the returned array. */
   result = astMalloc( (*nxs)*(*nxs)*sizeof(*result ) );
   if( result ) {
      rad2lim = radius*radius;

      ixc = *nxs/2;
      iyc = ixc;

      pr = result;
      y = -iyc*PSIZE;
      for( iy = 0; iy < *nxs; iy++ ){
         ysq = y*y;

         x = -ixc*PSIZE;
         for( ix = 0; ix < *nxs; ix++ ){
            radsq = x*x + ysq;
            *(pr++) = (radsq > rad2lim ) ? 0.0: 1.0;
            x += PSIZE;
         }
         y += PSIZE;
      }
   }
   return result;
}


static void smf1_twobeam( void *job_data_ptr, int *status ) {
/*
*  Name:
*     smf1_twobeam

*  Purpose:
*     Executed in a worker thread to do various calculations for
*     smf_twobeam.

*  Invocation:
*     smf1_twobeam( void *job_data_ptr, int *status )

*  Arguments:
*     job_data_ptr = SmfTwoBeamData * (Given)
*        Data structure describing the job to be performed by the worker
*        thread.
*     status = int * (Given and Returned)
*        Inherited status.

*/

/* Local Variables: */
   AstLutMap *lutmap;
   SmfTwoBeamData *pdata;
   dim_t nv;
   double *beam;
   double *pa;
   double *pb0;
   double *pb;
   double *pg;
   double *pl;
   double *pr;
   double *ps;
   double *source;
   double a1;
   double a2;
   double back;
   double bsum;
   double cx;
   double cy;
   double dx;
   double dy;
   double dy2;
   double f;
   double fac1;
   double fac2;
   double m;
   double maxm;
   double pixsize;
   double radsq;
   double res;
   double srchgt;
   double sum;
   double v;
   double x;
   double y;
   double ysq;
   hdsdim deltax;
   hdsdim ix;
   hdsdim ixb;
   hdsdim ixc;
   hdsdim ixs;
   hdsdim iy;
   hdsdim iyb;
   hdsdim iyc;
   hdsdim iys;
   hdsdim l1;
   hdsdim l2;
   hdsdim nx;
   hdsdim nxb;
   hdsdim nxs;
   hdsdim xoff;
   hdsdim yoff;


/* Check inherited status */
   if( *status != SAI__OK ) return;

/* Get a pointer that can be used for accessing the required items in the
   supplied structure. */
   pdata = (SmfTwoBeamData *) job_data_ptr;

/* Create a look-up-table which gives the value of the convolution of the
   source and beam as a function of the distance between the source centre
   and beam centre. The source and beam are both circular and so there
   should be azimuthal symetry. The x separation between source and beam
   changes on each pass round this loop but the y separation remains constant
   at zero. Initially, the beam and source are concentric.
   ================================================================== */
   if( pdata->operation == 1 ) {

/* Copy stuff into local variables for convenience. */
      nxb = pdata->nxb;
      nxs = pdata->nxs;
      l2 = pdata->l2;
      yoff = pdata->yoff;
      beam = pdata->beam;
      source = pdata->source;
      srchgt = pdata->srchgt;
      bsum = pdata->bsum;

/* Set the initial X offset between beam array and source array. This decreases
   by one in each pass through the following loop. */
      xoff = yoff - pdata->l1;

/* Loop round every element of the lut to be created by the current
   worker thread. */
      pl = pdata->lut + pdata->l1;
      for( deltax = pdata->l1; deltax <= l2; deltax++,xoff-- ){

/* Pointer to pixel in beam array corresponding to the first pixel in the source
   array. */
         pb0 = beam + xoff + yoff*nxb;

/* Loop round all rows and columns in the source image that overlap the
   beam. */
         sum = 0.0;
         ps = source;
         iyb = yoff;
         for( iys = 0; iys < nxs; iys++,iyb++ ) {
            if( iyb >= 0 && iyb < nxb ) {
               pb = pb0;
               ixb = xoff;
               for( ixs = 0; ixs < nxs; ixs++,ixb++,ps++,pb++ ) {
                  if( ixb >= 0 && ixb < nxb ) {

/* Increment the product of the beam and source. */
                     sum += (*ps)*(*pb);
                  }
               }
            } else {
               ps += nxs;
            }

/* Move pb0 on to the start of the next row. */
            pb0 += nxb;
         }

/* The "source" array has a peak value of unity, not "srchgt". So scale
   the sum to take account of this. */
         sum *= srchgt;

/* Normalise the sum, by dividing by the sum of the beam values. */
         if( bsum > 0.0 ) {
            *(pl++) = sum / bsum;
         } else {
            *(pl++) = 0.0;
         }
      }


/* Store the convolution value on a fine grid.
   ========================================== */
   } else if( pdata->operation == 2 ) {

/* Copy stuff into local variables for convenience. */
      cx = pdata->cx;
      cy = pdata->cy;
      l1 = pdata->l1 + 1;
      l2 = pdata->l2 + 1;
      nx = pdata->nx;
      back = pdata->back;
      pixsize = pdata->pixsize;

/* Loop over the lines of the data array to be processed by this thread,
   storing the radial distance from the source centre to each pixel centre,
   in arc-sec. Note, (ix,iy) are the GRID coords of the pixel centre in
   the data array. */
      pg = pdata->out + ( l1 - 1 )*nx;
      for( iy = l1; iy <= l2; iy++ ) {
         dy = iy - cy;
         dy2 = dy*dy;
         for( ix = 1; ix <= nx; ix++,pg++) {
            dx = ix - cx;
            *pg = sqrt( dx*dx + dy2 )*pixsize;
         }
      }

/* Use a LutMap to look up values in the lut, since this gives us linear
   interpolation for free. The first value in the LUT corresponds to zero
   separation between beam and source. Note, the spacing in the LUT is
   "PSIZE", not "pixsize". */
      lutmap = astLutMap( (int) pdata->nxb, pdata->lut, 0.0, PSIZE, " " );
      nv = ( l2 - l1 + 1 )*nx;
      pg = pdata->out + ( l1 - 1 )*nx;
      astTran1( lutmap, nv, pg, 1, pg );

/* Initialise things. */
      maxm = VAL__MIND;  /* Maximum output data value found so far */
      f = 0.0;           /* Sum of squared residuals */

/* Loop again over the lines of the data array to be processed by this thread,
   forming the model value at each pixel and summing the squared
   residuals. */
      pa = pdata->array + ( l1 - 1 )*nx;
      pg = pdata->out + ( l1 - 1 )*nx;
      for( iy = l1; iy <= l2; iy++ ) {
         for( ix = 1; ix <= nx; ix++,pg++,pa++) {
            m = ( *pg += back );
            if( *pa != VAL__BADD ){
               res = *pa - m;
               f += res*res;
               if( m > maxm  ) maxm = m;
            }
         }
      }

      pdata->f = f;
      pdata->maxm = maxm;



/* Calculate the current beam on a fine grid.
   ================================================================== */
   } else if( pdata->operation == 3 ) {

/* Copy stuff into local variables for convenience. */
      a1 = pdata->a1;
      a2 = pdata->a2;
      fac1 = pdata->fac1;
      fac2 = pdata->fac2;
      l1 = pdata->l1;
      l2 = pdata->l2;
      nxb = pdata->nxb;

/* Calculate the beam value at each pixel in each of the rows to be
   processed by this thread. */
      ixc = nxb/2;
      iyc = ixc;

      bsum = 0.0;
      pr = pdata->result + l1*nxb;
      y = (l1 - iyc)*PSIZE;
      for( iy = l1; iy <= l2; iy++ ){
         ysq = y*y;

         x = -ixc*PSIZE;
         for( ix = 0; ix < nxb; ix++ ){
            radsq = x*x + ysq;
            v = a1*exp( fac1*radsq ) + a2*exp( fac2*radsq );
            *(pr++) = v;
            bsum += v;
            x += PSIZE;
         }
         y += PSIZE;
      }

      pdata->bsum = bsum;


/* Report an error if the worker was to do an unknown job.
   ====================================================== */
   } else {
      *status = SAI__ERROR;
      errRepf( "", "smf1_twobeam: Invalid operation (%d) supplied.",
               status, pdata->operation );
   }
}










