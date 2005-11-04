/* sc2math - SCUBA-2 mathematics library

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)
   History :
    22Feb2005 : original adapted from SCUBA transputer system (bdk)
*/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "Dits_Err.h"
#include "Ers.h"
#include "sc2math.h"


#define EPS 1.0e-20                      /* near-zero trap */


/*+  sc2math_convolve - convolve a dataset with a filter */

void sc2math_convolve
(
int filtlen,      /* number of elements in filter (given) */
int filtmid,      /* index of filter centre (given) */
double *filter,     /* convolving filter (given) */
int datalen,      /* length of dataset (given) */
double *input,      /* input dataset (given) */
double *output,     /* convolved dataset (returned) */
int *status       /* global status (given and returned) */
)

/*   Method :
      Put into each output value the sum of the input values weighted by 
      the filter.
     Author :
      B.D.Kelly (REVAD::BDK)
     History :
      30.06.1993: original, based loosely on JCMTDR_CONVOLVE 
                  (REVAD::BDK)
*/

{
   int endfilt;      /* convolution limit */
   int j;            /* index to output array */
   int joff;         /* position in input array corresponding to start 
                         of filter */
   int k;            /* index to filter */
   int startfilt;    /* convolution limit */

   double sum;         /* temporary accumulator */


   if ( !StatusOkP(status) ) return;

   for ( j=0; j<datalen; j++ )
   {
      sum = (double) 0.0;

/*   Set range of convolution to prevent k and inpos from going outside 
     their array bounds */

      startfilt = filtmid - j;
      if ( startfilt < 0 )
      {
         startfilt = 0;
      }

      endfilt = filtmid - j + datalen;
      if ( endfilt > filtlen )
      {
         endfilt = filtlen;
      }

/*   Calculating joff is a small optimisation not done by the compiler 
     */

      joff = j - filtmid;
      for ( k=startfilt; k<endfilt; k++ )
      {
         sum += input[joff+k] * filter[k];
      }
      output[j] = sum;
   }

}



/*+ sc2math_cubfit - fit a set of values with a cubic */

void sc2math_cubfit 
( 
int npts,                 /* number of data points (given) */
double *x,                /* observed values (given) */
double *y,                /* observed values (given) */
double *coeffs,           /* coefficients of fit (returned) */
double *variances,        /* variances of fit (returned) */
int *status               /* global status (given and returned) */
)

/*   Method :
      Fit a cubic polynomial to a dataset.
     Authors :
      B.D.Kelly (bdk@roe.ac.uk)
     History :
      19.05.2005: original (bdk)
      20.05.2005: pass-in both x and y (bdk)
     endhistory
*/

{


   int j;
   int despike;            /* flag for spike removal */
   int nterms;             /* number of combined waveforms */
   static double standard_waves[1024*3];   /* values of standard waveforms */
   double standard_weights[1024]; /* if y[j] is not to be used, 
                            standard_weights[j] = 0.0, otherwise
                            standard_weights[j] = 1.0 */
   double used_weights[1024];  /* if y[j] was not used, 
                                 used_weights[j] = 0.0, otherwise 
                                 used_weights[j] = 1.0  */
   double fitted_y[1024];  /* combined waveform computed from the fit */
   int nbad;              /* number of points rejected as suspected
                            "spikes" */



   if ( !StatusOkP(status) ) return;

   despike = 0;
   nterms = 3;

   for ( j=0; j<npts; j++ )
   {
      standard_waves[j] = x[j];
      standard_waves[j+npts] = x[j] * x[j];
      standard_waves[j+npts*2] = x[j] * x[j] * x[j];
      standard_weights[j] = 1.0;
   }

   sc2math_recurfit ( despike, npts, nterms, standard_waves,
     standard_weights, y, used_weights, fitted_y, coeffs,
     variances, &nbad, status );

}


/*+ sc2math_flatten - apply flat field correction to set of frames */

void sc2math_flatten
(
int nboll,          /* number of bolometers (given) */
int nframes,        /* number of frames in scan (given) */
char *flatname,     /* name of flatfield algorithm (given) */
int nflat,          /* number of flatfield parameters (given) */
double *fcal,       /* calibration coefficients (given) */
double *fpar,       /* calibration parameters (given) */
double *inptr,      /* measurement values (given and returned) */
int *status         /* global status (given and returned) */
)

/* Description :
    Apply the calibration corrections indicated by the given algorithm
    name. Currently two are supported.

    "POLYNOMIAL" requires six coefficients per bolometer.

    "TABLE" means use the fcal values for any bolometer as an
     interpolation table to determine the corresponding fpar value.

   Authors :
    B.D.Kelly (bdk@roe.ac.uk)

   History :
    12Jul2005 : original (bdk)
    05Oct2005 : generalise by adding flatname, nflat and fpar arguments
                (bdk)
*/

{
   int i;              /* loop counter */
   int j;              /* loop counter */
   double lincal[2];   /* interpolation coeffs for a bolometer */
   double t;           /* intermediate result */
   double *temp;       /* pointer to storage for a single bolometer */
   double temp1;
   double temp2;


   if ( !StatusOkP(status) ) return;

   if ( strcmp ( "POLYNOMIAL", flatname ) == 0 )
   {

     printf("Nbol = %d, Nframes = %d\n",nboll,nframes);
      for ( j=0; j<nframes; j++ )
      {

/* apply flatfield to each bolometer in frame */

         for ( i=0; i<nboll; i++ )
         {
	   temp1 = inptr[j*nboll+i];
	   temp2 = fcal[i+nboll];
            t =  temp1 - temp2;
            inptr[j*nboll+i] = fcal[i] 
               + fcal[i+2*nboll] 
               + fcal[i+3*nboll] * t 
               + fcal[i+4*nboll] * t * t
               + fcal[i+5*nboll] * t * t * t;
         }

      }
   }
   else if ( strcmp ( "TABLE", flatname ) == 0 )
   {
      temp = calloc ( nframes, sizeof(double) );

      for ( i=0; i<nboll; i++ )
      {

/* Get the time series for this bolometer and determine the interpolation
   formula for its intensity conversion */

         for ( j=0; j<nframes; j++ )
         {
            temp[j] = inptr[j*nboll+i];
         }

/* Tailor the flatfield calibration coefficients */

         sc2math_setcal ( nboll, i, nframes, temp, nflat, fpar, fcal,
           lincal, status );

/* apply flatfield for this bolometer in each frame */

         for ( j=0; j<nframes; j++ )
         {
            inptr[j*nboll+i] = inptr[j*nboll+i] * lincal[1] + lincal[0];
         }
      }
      free ( temp );
   }
   else
   {
      *status = DITS__APP_ERROR;
      printf ( "map_flatten: invalid flatfield name - %s\n", flatname );
   }

}



/*+  sc2math_martin - spike removal from chop-scan data */

void sc2math_martin
(
double period,       /* chop period in samples (given) */
int maxlen,        /* length of signal (given) */
double *signal,      /* signal to be cleaned (given and returned) */
char *badscan,      /* record of spikes removed (returned) */
int *nbad,         /* number of points removed (returned) */
int *status        /* global status (given and returned) */
)

/*   Method :
     Detect spikes using a Martin filter run at a single scale length,
     the scale corresponding to the chop period.
     Authors :
      B.D.Kelly (REVAD::BDK)
     History :
      15.06.1993: original (REVAD::BDK)
      28.09.1993: add badscan (REVAD::BDK)
*/

{
   double diff;                 /* data difference */
   double fnum;                 /* number of points used */
   int roundper;              /* rounded version of period */
   double sumdiff;              /* sum of data differences */
   double sumdiffsq;            /* sum of squares of data differences */
   double sigma;                /* sigma of data differences */
   double worstval;             /* most discrepant value */
   int finished;              /* loop controller */
   int j;                     /* loop counter */
   int worstpos;              /* position of worstval */


   if ( !StatusOkP(status) ) return;


/*  Initialise bad value marker */

   memset ( badscan, 0, (size_t) maxlen );

/*  The data ends are assumed to be padded with zeros */

   *nbad = 0;
   fnum = (double) maxlen - (double) 2.0 * period;
   roundper = (int) ( period + (double) 0.5 );
   finished = 0;
   while ( finished == 0 )
   {
      worstval = (double) 0.0;
      worstpos = (int) 0;
      sumdiff = (double) 0.0;
      sumdiffsq = (double) 0.0;
      for ( j=roundper; j<maxlen-roundper; j++ )
      {
         diff = signal[j] - 
           ( signal[j-roundper] + signal[j+roundper] ) / (double)2.0;
         sumdiff += diff;
         sumdiffsq += diff * diff;
         if ( diff < 0 )
         {
            diff = (-diff);
         }
         if ( diff > worstval )
         {
            worstval = diff;
            worstpos = j;
         }
      }
      sigma = sumdiffsq - sumdiff * sumdiff / fnum;
      sigma = (double) sqrt ( (double) sigma / (double) fnum );
      if ( worstval > ( (double)4.0 * sigma ) )
      {
         signal[worstpos] = 
           ( signal[worstpos-roundper] + signal[worstpos+roundper] ) / 
           (double)2.0;
         (*nbad)++;
         badscan[worstpos] = (char) 1;
      }
      else
      {
         finished = 1;
      }
   }
}

/*+  sc2math_matinv - invert a symmetric matrix */

void sc2math_matinv 
( 
int norder,          /* degree of matrix (given) */
double array[10][10],  /* given matrix, its inverse is returned 
                         (given and returned) */
double *det,           /* determinant of ARRAY (returned) */
int *status          /* global status (given and returned) */
)

/*   Method :
      Invert a symmetric matrix and calculate its determinant.
      Taken from "Data reduction and error analysis for the physical 
      sciences", P.R.Bevington.
      example: the inverse of 
                   1  2  3
                   2  1  1
                   3  1  2

      is
                 -0.25  0.25  0.25
                  0.26  1.75 -1.25
                  0.25 -1.25  0.75
     Authors :
      B.D.Kelly (REVAD::BDK)
     History :
      30.11.1988:  original (REVAD::BDK)
      02.02.1990:  rationalise names (REVAD::BDK)
      01.04.1992:  version for SCUSP library (REVAD::BDK)
      11.10.1993:  use the "+="-style construction, and test on a VAX 
                   against the example given in Method (REVAD::BDK)
     endhistory
*/

{
   double armax;
   double store;
   int ik[10];
   int jk[10];
   int i;
   int j;
   int k;

   if ( !StatusOkP(status) ) return;


   *det = (double) 1.0;

   for ( k=0; k<norder; k++ )
   {

/*   Find largest element array[j][i] in rest of matrix */

      armax = (double) 0.0;

      for ( i=k; i<norder; i++ )
      {
         for ( j=k; j<norder; j++ )
         {
            if ( fabs(armax) <= fabs(array[j][i]) ) 
            {
               armax = array[j][i];
               ik[k] = i;
               jk[k] = j;
            }
         }
      }

/*   Interchange rows and columns to put armax in array[k][k] */

      if ( armax == 0.0 ) 
      {
         *det = (double) 0.0;
         return;
      }

      i = ik[k];

      if ( i > k ) 
      {
         for ( j=0; j<norder; j++ )
         {
            store = array[j][k];
            array[j][k] = array[j][i];
            array[j][i] = -store;
         }
      }

      j = jk[k];

      if ( j > k ) 
      {
         for ( i=0; i<norder; i++ )
         {
            store = array[k][i];
            array[k][i] = array[j][i];
            array[j][i] = -store;
         }
      }

/*   Accumulate elements of inverse matrix */

      for ( i=0; i<norder; i++ )
      {
         if ( i != k) 
         {
            array[k][i] = -array[k][i] / armax;
         }
      }

      for ( i=0; i<norder; i++ )
      {
         if ( i != k ) 
         {
            for ( j=0; j<norder; j++ )
            {
               if ( j != k ) 
               {
                  array[j][i] += array[k][i] * array[j][k];
               }
            }
         }
      }

      for ( j=0; j<norder; j++ )
      {
         if ( j != k ) 
         {
            array[j][k] /= armax;
         }
      }

      array[k][k] = (double) 1.0 / armax;
      *det = (*det) * armax;

   }

/*   Restore ordering of matrix */

   for ( k=norder-1; k>=0; k-- )
   {
      j = ik[k];

      if ( j > k ) 
      {
         for ( i=0; i<norder; i++ )
         {
            store = array[k][i];
            array[k][i] = -array[j][i];
            array[j][i] = store;
         }
      }

      i = jk[k];

      if ( i > k ) 
      {
         for ( j=0; j<norder; j++ )
         {
            store = array[j][k];
            array[j][k] = - array[j][i];
            array[j][i] = store;
         }
      }

   }
 
}


/*+ sc2math_recurfit - fit a set of values with outlier rejection */

void sc2math_recurfit 
( 
int despike,            /* flag for spike removal (given) */
int npts,               /* number of data points (given) */
int nterms,             /* number of combined waveforms (given) */
double *standard_waves,   /* values of standard waveforms (given) */
double *standard_weights, /* if volts[j] is not to be used, 
                            standard_weights[j] = 0.0, otherwise
                            standard_weights[j] = 1.0 (given) */
double *volts,            /* observed values (given) */
double *used_weights,     /* if volts[j] was not used, 
                            used_weights[j] = 0.0, otherwise 
                            used_weights[j] = 1.0 (returned) */
double *fitted_volts,     /* combined waveform computed from the fit 
                            (returned) */
double *coeffs,           /* coefficients of fit (returned) */
double *variances,        /* variances of fit (returned) */
int *nbad,              /* number of points rejected as suspected
                            "spikes" (returned) */
int *status             /* status must be 0 on entry. 
                            If no valid fit was found, SAI__ERROR is
                            returned (given and returned) */
)

/*   Method :
      The observed waveform is assumed to be a linear combination of the 
      standard waveforms :
        volts[i] = coeffs[0] + coeffs[1] * standard_waves[0][i] + 
                   coeffs[2] * standard_waves[1][i] + .....
      The problem is solved by multiple linear regression. It is assumed
      that volts may contain bad values - ie "spikes", so after the fit
      a search is made for discrepant points which are rejected before
      the fit is repeated.

      Note there are (nterms+1) coefficients but nterms standard_waves,
      the extra term being the constant offset.
     Authors :
      B.D.Kelly (bdk@roe.ac.uk)
     History :
      01.12.1988: original (REVAD::BDK)
      02.02.1990: rationalise names (REVAD::BDK)
      01.04.1992: return CALIB_AMP and CHOP_AMP separately, separate 
                  import and export WEIGHTS arrays (REVAD::BDK)
      07.10.1992: return CALIB_VAR and CHOP_VAR (REVAD::BDK)
      11.05.1993: add despike, change threshold to 3.5 sigma 
                  (REVAD::BDK)
      16.10.1995: make despike ignore zero-weight points (BDK)
      28.02.2005: generalise SCUBA demodulation algorithm for SCUBA-2
                  function fitting (bdk)
     endhistory
*/

{
   int i;                 /* loop counter */
   int fitcnt;            /* counter for number of attempted fits */
   int done;              /* loop controller */
   double a0;               /* constant term in fit */
   double a[10];            /* other terms in fit */
   double noise;            /* rms noise in the overall fit */
   double sigma0;           /* sigma in constant term */
   double sigmaa[10];       /* sigma for other terms */
   double r[10];            /* linear correlation coefficients */
   double rmul;             /* multiple linear correlation coefficient */
   double chisqr;           /* reduced chi square for fit */
   double ftest;            /* value of "F" for test of fit */


   if ( !StatusOkP(status) ) return;


/*   Initialise weights */

   for ( i=0; i<npts; i++ )
   {
      used_weights[i] = standard_weights[i];
   }

/*   Repeatedly fit the data until no more outlying values are rejected 
     or the data seem too bad. */

   done = 0;
   *nbad = 0;
   fitcnt = 0;
   while ( ( done == 0 ) && ( fitcnt < 10 ) )
   {

/*   Perform fit */

      fitcnt++;
      sc2math_regres ( npts, nterms, standard_waves, volts, 
        used_weights, fitted_volts, &a0, a, &sigma0, sigmaa, r,
        &rmul, &chisqr, &ftest, &noise, status );

      if ( StatusOkP(status) )
      {

         done = (int) 1;
         if ( despike == 1 )
         {

/*   Search for bad values */

            for ( i=0; i<npts; i++ )
            {
               if ( used_weights[i] != (double)0.0 )
               {
                  if ( fabs ( (double)volts[i] - (double)fitted_volts[i] ) 
                    > ( 3.5 * noise ) )
                  {
                     used_weights[i] = (double) 0.0;
                     *nbad += (int) 1;
                     done = 0;
                  }
               }
            }
         }
      }
      else
      {

/*   fit failed completely */

         fitcnt = 10;
      }

   }

   if ( done == 1 )
   {

/*   A successful fit, return computed amplitudes */

      coeffs[0] = a0;
      variances[0] = sigma0 * sigma0;
      for ( i=0; i<nterms; i++ )
      {
         coeffs[i+1] = a[i];
         variances[i+1] = sigmaa[i] * sigmaa[i];
      }
   }
   else
   {

/*   Fit failed */

      *nbad = (int) npts;
      coeffs[0] = 0.0;
      variances[0] = 0.0;
      for ( i=0; i<nterms; i++ )
      {
         coeffs[i+1] = 0.0;
         variances[i+1] = 0.0;
      }
   }
}



/*+  sc2math_regres - multiple linear regression fit */

void sc2math_regres 
( 
int npts,      /* number of data points (given) */
int nterms,    /* number of combined waveforms (given) */
double *x,       /* values of standard waveforms (given) */
double *y,       /* observed values (given) */
double *weight,  /* weight for each observed value (given) */
double *yfit,    /* values of Y computed from the fit (returned) */
double *a0,      /* constant term in fit (returned) */
double *a,       /* coefficients of fit (returned) */
double *sigma0,  /* standard deviation of A0 (returned) */
double *sigmaa,  /* array of standard deviations for coefficients
                   (returned) */
double *r,       /* array of linear correlation coefficients 
                   (returned) */
double *rmul,    /* multiple linear correlation coefficient (returned) */
double *chisqr,  /* reduced chi square for fit (returned) */
double *ftest,   /* value of F for test of fit (returned) */
double *perr,    /* probable error in deviation of a single point from
                   the fit (returned) */
int *status    /* status must be OK on entry
                   on exit, STATUS = OK => fit ok
                   STATUS = DITS__APP_ERROR => exact fit (no noise) 
                   (given and returned) */
)

/*   Method :
      Make a multiple linear regression fit to data with a specified 
      function which is linear in coefficients. It is assumed that all 
      weights are 1.0 or 0.0, points with weight 0.0 being totally 
      ignored.
      Taken from "Data reduction and error analysis for the physical 
      sciences", P.R.Bevington.
     Authors :
      B.D.Kelly (REVAD::BDK)
     History :
      29.11.1988: original (REVAD::BDK)
      02.02.1990: rationalise names (REVAD::BDK)
      01.04.1992: use SAI errors, make part of SCUSP (REVAD::BDK)
      12.10.1993: fix various one-off errors, use += -style syntax 
                  (REVAD::BDK)
     endhistory
*/

{

   double array[10][10]; /* normal equations */
   double sigmax[10];    /* */
   double xmean[10];     /* weighted means of standard waveforms */
   double sum;           /* sum of weights */
   double ymean;         /* weighted mean of data to be fitted */
   double sigma;         /* */
   double chisq;         /* */
   double free1;         /* */
   double freej;         /* */
   double freen;         /* */
   double det;           /* determinant of normal equations */
   int i;              /* loop controller */
   int j;              /* loop controller */
   int k;              /* loop controller */



   if ( *status != 0 ) return;


/*   Initialise sums and arrays */

   sum = (double) 0.0;
   ymean = (double) 0.0;
   sigma = (double) 0.0;
   chisq = (double) 0.0;
   *rmul = (double) 0.0;

   for ( i=0; i<npts; i++ )
   {
      yfit[i] = (double) 0.0;
   }

   for ( j=0; j<nterms; j++ )
   {
      xmean[j] = (double) 0.0;
      sigmax[j] = (double) 0.0;
      r[j] = (double) 0.0;
      a[j] = (double) 0.0;
      sigmaa[j] = (double) 0.0;
      for ( k=0; k<nterms; k++ )
      {
         array[k][j] = (double) 0.0;
      }
   }

/*   Accumulate weighted sums */

   for ( i=0; i<npts; i++ )
   {
      sum += weight[i];
      ymean += weight[i]*y[i];
      for ( j=0; j<nterms; j++ )
      {
         xmean[j] += weight[i] * x[j*npts+i];
      }
   }

   ymean /= sum;

   for ( j=0; j<nterms; j++ )
   {
      xmean[j] /= sum;
   }

/*   Accumulate matrices R and ARRAY */

   for ( i=0; i<npts; i++ )
   {
      sigma += weight[i] * ( y[i] - ymean ) * ( y[i] - ymean );
      for ( j=0; j<nterms; j++ )
      {
         sigmax[j] += weight[i] * 
           ( x[j*npts+i] - xmean[j] ) * ( x[j*npts+i] - xmean[j] );
         r[j] += weight[i] * ( x[j*npts+i] - xmean[j] ) * 
           ( y[i] - ymean );
         for ( k=0; k<=j; k++ )
         {
            array[k][j] += weight[i] * 
              ( x[j*npts+i] - xmean[j] ) * ( x[k*npts+i] - xmean[k] );
         }
      }
   }

   free1 = sum - (double) 1.0;
   sigma = (double) sqrt ( (double) sigma / (double) free1 );

   for ( j=0; j<nterms; j++ )
   {
      sigmax[j] = 
        (double) sqrt ( (double) sigmax[j] / (double) free1 );
      r[j] /= ( free1 * sigmax[j] * sigma );
      for ( k=0; k<=j; k++ )
      {
         array[k][j] /= ( free1 * sigmax[j] * sigmax[k] );
         array[j][k] = array[k][j];
      }
   }

/*   Invert symmetric matrix */

   sc2math_matinv ( nterms, array, &det, status );

   if ( fabs ( (double) det ) < EPS ) 
   {
      *a0 = (double) 0.0;
      *sigma0 = (double) 0.0;
      *rmul = (double) 0.0;
      *chisqr = (double) 0.0;
      *ftest = (double) 0.0;
      *status = DITS__APP_ERROR;
   }
   else
   {

/*   Calculate coefficients, fit and chi square */

      *a0 = ymean;
      for ( j=0; j<nterms; j++ )
      {
         for ( k=0; k<nterms; k++ )
         {
            a[j] += r[k] * array[k][j];
         }
         a[j] *= sigma / sigmax[j];
         *a0 -= a[j] * xmean[j];
         for ( i=0; i<npts; i++ )
         {
            yfit[i] += a[j] * x[j*npts+i];
         }
      }

      for ( i=0; i<npts; i++ )
      {
         yfit[i] += (*a0);
         chisq += weight[i] * ( y[i] - yfit[i] ) * 
           ( y[i] - yfit[i] );
      }

      *perr = (double) sqrt ( (double) chisq / (double) sum );
      freen = sum - (double) nterms - (double) 1.0;
      *chisqr = chisq / freen;

/*   Calculate uncertainties */

      for ( j=0; j<nterms; j++ )
      {
         sigmaa[j] = array[j][j] * (*chisqr) / 
           ( free1 * sigmax[j] * sigmax[j] );
         sigmaa[j] = (double) sqrt ( (double) sigmaa[j] );
         *rmul += a[j] * r[j] * sigmax[j] / sigma;
      }

      freej = (double) nterms;
      *ftest = ( (*rmul) / freej ) / ( ( (double) 1.0 - (*rmul) ) 
        / freen );
      *rmul = (double) sqrt ( (double) *rmul );
      *sigma0 = (double) 1.0 / sum;
      for ( j=0; j<nterms; j++ )
      {
         for ( k=0; k<nterms; k++ )
         {
            *sigma0 += (*chisqr) * xmean[j] * xmean[k]
              * array[k][j] / ( free1 * sigmax[j] * sigmax[k] );
         }
      }

      *sigma0 = (double) sqrt ( (double) *sigma0 );

   }

}

/*+  sc2math_remsine - remove sine wave from scan data */

void sc2math_remsine
(
int phase,        /* position in scan corresponding to zero phase 
                      of the superimposed sine (given) */
double period,      /* period in samples of the sine wave (given) */
int scanlen,      /* length of scan (given) */
double *scan,       /* the time series of measurements (given and 
                      returned) */
double *amplitude,  /* amplitude of the sine wave (returned) */
int *status       /* global status (given and returned) */
)

/*  Method :
     Given the phase and period of the sine wave, multiply the scan 
     by the matching sine of unit amplitude and sum the result. This 
     gives the amplitude of the sine in the data. Subtract the 
     calculated sine from the data.
    Author :
     B.D.Kelly (REVAD::BDK)
    History :
     16.06.1993: original (REVAD::BDK)
*/

{
   int j;       /* loop counter */
   double rad;   /* conversion to radians */

   if ( !StatusOkP(status) ) return;

/*   Calculate amplitude of sine wave */

   *amplitude = (double) 0.0;
   rad = (double) ( 2.0 * 3.14159 ) / (double)period;
   for ( j=0; j<scanlen; j++ )
   {
     *amplitude += scan[j] * 
        (double) sin ( ( (double) j - (double) phase ) * rad );
   }
   *amplitude = (double) 2.0 * ( *amplitude / (double)scanlen );

/*   Subtract the sine wave from the scan */

   for ( j=0; j<scanlen; j++ )
   {
     scan[j] -= *amplitude * 
       (double) sin ( ( (double) j - (double) phase ) * rad );
   }

}

/*+ sc2math_setcal - set flatfield calibration for a bolometer */

void sc2math_setcal
( 
int nboll,         /* total number of bolometers (given) */
int bol,           /* number of current bolometer (given) */
int numsamples,    /* number of data samples (given) */
double values[],   /* measurements by bolometer (given) */
int ncal,          /* number of calibration measurements (given) */
double heat[],     /* calibration heater settings (given) */
double calval[],   /* calibration measurements for all bolometers (given) */
double lincal[2],  /* calibration parameters (returned) */
int *status        /* global status (given and returned) */
)

/*  Description :
     Given calibration measurements plus actual measurements, select the
     section of the calibration which matches the actual measurements and
     determine a suitable interpolation formulaa for calibrating the
     actual measurents.

     The approach is to find three calibration measurements which bracket
     the mean of the actual measurements and determine coefficients for a
     straight-line interpolation.

    Authors :
     B.D.Kelly (ROE)
    History :
     12Aug2005:  original (bdk@roe.ac.uk)
*/

{
   int j;
   double mean;

   if ( !StatusOkP(status) ) return;

/* calculate mean of current measurements */

   mean = 0.0;

   for ( j=0; j<numsamples; j++ )
   {
      mean += values[j];
   }
   mean /= (double)numsamples;

/* Choose interpolation point from calibration tables 
   Note the tables are assumed to be in increasing order of power,
   decreasing order of data number */

   if ( mean > calval[bol] )
   {
      lincal[1] = ( heat[1] - heat[0] ) / 
        ( calval[bol+nboll] - calval[bol] );
      lincal[0] = heat[1] - lincal[1] * calval[bol+nboll];
   }
   else if ( mean < calval[bol+(ncal-1)*nboll] )
   {
      lincal[1] = ( heat[ncal-1] - heat[ncal-2] ) / 
        ( calval[bol+(ncal-1)*nboll] - calval[bol+(ncal-2)*nboll] );
      lincal[0] = heat[ncal-1] - lincal[1] * calval[bol+(ncal-1)*nboll];
   }
   else
   {
      for ( j=1; j<ncal; j++ )
      {
         if ( calval[bol+j*nboll] < mean )
         {
            lincal[1] = ( heat[j] - heat[j-1] ) / 
              ( calval[bol+j*nboll] - calval[bol+(j-1)*nboll] );
            lincal[0] = heat[j] - lincal[1] * calval[bol+j*nboll];
            break;
         }
      }
   }
}



/*+  sc2math_sinedemod - sine wave demodulate a signal */

void sc2math_sinedemod 
( 
int length,          /* length of the signal array (given) */
double *sine,          /* sine wave (given) */
double *cosine,        /* cosine wave (given) */
double *signal,        /* signal (given) */
double *amplitude,     /* modulation amplitude (returned) */
double *phase,         /* phase of signal (returned) */
int *status          /* global status (given and returned) */
)

/*   Method :
      Demodulate by multiplying the given signal by sine and cosine and 
      summing.
     Authors :
      B.D.Kelly (REVAD::BDK)
     History :
      15.10.1993: original (REVAXD::BDK)
     endhistory
*/

{
   double avsig;                 /* average value of signal */
   int j;                      /* loop counter */
   double sumcos;                /* total of cosine multiplication */
   double sumsine;               /* total of sine multiplication */

   if ( !StatusOkP(status) ) return;


   avsig = (double)0.0;
   for ( j=0; j<length; j++ )
   {
      avsig += signal[j];
   }
   avsig /= (double)length;

   sumsine = (double)0.0;
   sumcos = (double)0.0;
   for ( j=0; j<length; j++ )
   {
      sumsine += ( signal[j] - avsig ) * sine[j];
      sumcos += ( signal[j] - avsig ) * cosine[j];
   }
   sumsine = (double)2.0 * sumsine / (double)length;
   sumcos = (double)2.0 * sumcos / (double)length;
   *amplitude = (double) sqrt ( ( (double)sumsine*(double)sumsine 
     + (double)sumcos*(double)sumcos ) );
   *phase = (double) atan2 ( (double) sumcos, (double) sumsine );
}

/*+  sc2math_wavegen - generate sine and cosine signals */

void sc2math_wavegen 
( 
double period,                 /* period in array elements (given) */
int length,                  /* length of arrays (given) */
double *sine,                  /* generated sine wave (returned) */
double *cosine,                /* generated cosine wave (returned) */
int *status                  /* global status (given and returned) */
)

/*   Method :
      Calculate sine and cosine waves, zero phase occuring at the first 
      element, of the given period.
     Authors :
      B.D.Kelly (REVAD::BDK)
     History :
      15.10.1993: original (REVAD::BDK)
     endhistory
*/

{

   int j;                        /* loop counter */
   double twopi;                  /* ( 2 * pi ) */

   if ( !StatusOkP(status) ) return;

   twopi = 2.0 * 3.14159;

   for ( j=0; j<length; j++ )
   {
      sine[j] = (double) sin ( twopi * (double)j / (double)period );
      cosine[j] = (double) cos ( twopi * (double)j / (double)period );
   }
}
