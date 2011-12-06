/*
                                                                  
  File name: 
    wvmOpt.c

  Description:  

  Functions:
    wvmEst
    wvmOpt
    wvmOptMulti

 History: 
   $Log: wvmOpt.c,v $
   Revision 1.6  2011/12/02 01:33:09  timj
   Fix multi-measurement fit

   The multi-measurement fit should have been minimizing pwvzen rather than
   pwvlos. wvmOpt still returns line-of-sight PWV.

   Revision 1.5  2011/11/30 00:02:49  timj
   wvmOpt now returns the RMS of the fit

   Revision 1.4  2011/11/29 23:57:39  timj
   Use new levmar minimization routine

   The old minimization algorithm rarely found the best parameters. We now
   use a Levenberg-Marquardt algorithm. The levmar code has been cut down
   from a distribution at http://www.ics.forth.gr/~lourakis/levmar/ (v2.6)
   to minimize the source code to carry around. We do not use the LAPACK
   version to remove external library dependencies.

   A routine wvmOptMulti has been added to allow multiple measurments
   to be minimized in one go. This allows for an error to be calculated.

   aFunction has been removed from wvmOpt.

   wvmOpt now uses const in its API and wvmEst uses double precision.

   Revision 1.3  2006/07/14 18:50:32  rkackley
   Corrected format specifier and added val to list of quantities printed because it looked like val was intended to be printed bu was not in the argument list

   Revision 1.2  2003/04/09 20:22:10  mrippa
   #include wvmCal.h

   Revision 1.1.1.1  2002/07/26 01:41:33  cwalther
   Original Version


   19-July-2002 - Craig Walther - Created (translated from FORTRAN)

*-
*/

/* C Includes */

#include <stdio.h>
#include <time.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <math.h>

/* WVM includes */
#include "wvmCal.h"
#include "levmar.h"

/* Data structure to store parameters for minimization routine */
typedef struct data {
  const float *airmass;
} wvmData;

/* The function to be minimized returns the theoretical result in an
   array so is a simplified layer on top of wvmEst */

static void atmEst_f ( double * p, double *x, int m, int n, void *data )
{
  double AEFF[3];
  double TBRI[3];
  double TEFF[3];
  double TTAU[3];

  int j;
  int i;
  wvmData *wvmdata;

  /* should be a three parameter fit */
  double WA;
  double TWAT;
  double TAUO;

  /* The support data are in the void struct */
  wvmdata = data;

  WA = p[0];
  TWAT = p[1];
  TAUO = p[2];

  j = 0; /* Actual measurement number */
  for (i=0; i < n; i+= 3 ) { /* 3 temperatures at a time */
    float airmass;
    float pwvlos;

    airmass = (wvmdata->airmass)[j];

    pwvlos = WA * airmass;

    wvmEst( airmass, pwvlos, TWAT, TAUO,
             TBRI, TTAU, TEFF, AEFF );

    x[i] = TBRI[0];
    x[i+1] = TBRI[1];
    x[i+2] = TBRI[2];

  /*  printf("Temp diff = %g - %g - %g\n",
         wvmdata->tSky[0] - TBRI[0],
         wvmdata->tSky[1] - TBRI[1],
         wvmdata->tSky[2] - TBRI[2]);
  */

    j++;
  }

}

/********************************************************************/
/*+		       w v m O p t

 *  Function name:
      wvmOpt

 *  Function:
      Calculate the best fitting water vapour content, broadband opacity
      and effective temperature from the effective sky temperatures.

 *  Description:
   Calculate the best fitting water, broadband opacity and effective
   temperature.

      
 *  Language:
      C

 *  Call:
 
      wvmOpt(float aMass, float tAmb, const float tSky[], float * waterDens, 
             float * tau0, float * tWat);

 *  Parameters:
      ("<" input, "!" modified, "W" workspace, ">" output)
      (<) aMass      The air Mass 
      (<) tAmb       The ambient temperature
      (<) tSky       The sky temperature of each receiver (3 data points)
      (>) waterDens  The line-of sight water density in mm
      (>) tau0       The line of site opacity
      (>) tWat       The effective temperature
      (>) rms        RMS of fit to model

 *  Support: Craig Walther, {JAC}


 *- 

 */

void wvmOpt(float aMass, float tAmb, const float tSky[], float * waterDens, 
             float * tau0, float * tWater, float * rms)
{
  float waterDensErr;
  float tau0Err;
  float tWaterErr;

  wvmOptMulti( 1, &aMass, &tAmb, tSky, waterDens, tau0, tWater,
           &waterDensErr, &tau0Err, &tWaterErr, rms );

  /* scale back to line of sight */
  *waterDens *= aMass;

  return;
}

/********************************************************************/
/*+		       w v m O p t M u l t i

 *  Function name:
      wvmOptMulti

 *  Function:
      Calculate the best fitting water vapour content, broadband opacity
      and effective temperature from the effective sky temperatures. Multiple
      measurements are fitted in one go.

 *  Description:
      Calculate the best fitting water, broadband opacity and effective
      temperature from multiple measurements using a Levenberg-Marquardt
      minimization routine.

      The tSky data are in groups of 3 for each of n measurements.

      Note that because multiple measurements can be supplied this
      routine returns zenith values for water density. The tauO is
      not corrected for airmass so will probably be in error.

 *  Language:
      C

 *  Call:
 
      wvmOptMulti(size_t n, const float aMass[], const float tAmb[],
             const float tSky[], float * waterDens,
             float * tau0, float * tWat, float *waterDensErr,
             float tau0Err, float *tWatErr );

 *  Parameters:
      ("<" input, "!" modified, "W" workspace, ">" output)
      (<) n          Number of discrete measurements
      (<) aMass      The air Mass for each measurement
      (<) tAmb       The ambient temperature for each measurement
      (<) tSky       The sky temperature of each receiver (3xn data points)
      (>) waterDens  The zenith water density in mm
      (>) tau0       The average opacity
      (>) tWat       The effective temperature
      (>) waterDensErr Error in water density
      (>) tau0Err    Error in tau0
      (>) tWaterrErr Error in tWater
      (>) rms        RMS of fit to model

 *  Support: Tim Jenness {JAC}


 *-

 */

void wvmOptMulti(size_t n, const float aMass[], const float tAmb[], const float tSky[],
                 float * waterDens, float * tau0, float * tWater,
                 float * waterDensErr, float * tau0Err, float *tWaterErr, float *rms )
{

  wvmData data;

  const size_t m = 3;
  int i;

  double params[3];   /* initial guesses */
  double tp[] = {19.9, 285., 0.25};  /* upper limits */
  double bp[] = { 0.0, 240., -0.1};  /* lower limits */
  double covar[9];   /* 3x3 matric for covariance data */
  double opts[LM_OPTS_SZ], info[LM_INFO_SZ];
  double *x = NULL;
  int ret;
  double tAmbMean;

  /* Calculate the mean ambient temperature */
  tAmbMean = 0.0;
  for (i=0;i<n;i++) {
    tAmbMean += tAmb[i];
  }
  tAmbMean /= n;

 /* Make simple initial estimates */
  *tWater = tAmbMean - 10.0;
  *tau0 = 0.0;

  *waterDensErr = 1.0e10;
  *tWaterErr = 1.0e10;
  *tau0Err = 1.0e10;

  /* If we do not have reasonable values, return with the defaults only */
  if((tSky[2] > bp[1]) || (tSky[1] < tSky[2]))
    {
      *waterDens = 99.0;
      return;
    }

  /* make a first guess at the water density (assumes channel 3 is 
     optically thin) from the first measurement */

  *waterDens = (tSky[2] - 3.0)/29.0;

  /* we are minimizing zenith pwv though */
  *waterDens /= aMass[0];

  /* We are going to use an array for the parameters in the order
     waterDens, tWater, tau0. Fill the array with the initial estimate */
  params[0] = *waterDens;
  params[1] = *tWater;
  params[2] = *tau0;

  /* Populate the support struct */
  data.airmass = aMass;

  /* There are actually 3 x n measurements */
  n *= 3;

  /* Copy the input data */
  x = malloc( n * sizeof(*x) );
  for (i=0; i<n; i++) {
    x[i] = tSky[i];
  }

  /* optimization control parameters; passing to levmar NULL instead of opts reverts to defaults */
  opts[0]=LM_INIT_MU; opts[1]=1E-35; opts[2]=1E-35; opts[3]=1E-40;
  opts[4]=LM_DIFF_DELTA; /* relevant only if the finite difference Jacobian version is used */

  /* No jacobian to start with */
  ret = dlevmar_bc_dif( atmEst_f,          params, x, m, n, bp, tp, 10000, opts, info, NULL, covar, &data );

  *waterDens = params[0];
  *tWater = params[1];
  *tau0 = params[2];

  *waterDensErr = sqrt(covar[0]);
  *tWaterErr = sqrt(covar[4]);
  *tau0Err = sqrt(covar[8]);

  *rms = sqrt(info[1]);

  free(x);

}

void wvmEst(double aMass, double WA, double TWAT, double TAUO,
	    double *TBRI, double *TTAU, double *TEFF, double *AEFF)
{
  /* Given the water vapour in the line of sight, the effective temperature 
     of the water and the excess broadband opacity (e.g. due to clouds), 
     calculates the expected (Rayleigh Jeans) brightness temperatures for 
     the three channels of the radiometer plus the total opacity and the 
     effective temperature of the water.
     Uses a simple 2-slab model but with coefficients adjusted to match ATM
  */


  int   j;                                 /* counter */
  /* double AR[] = {1.212,0.2634,0.09928 }; nominal tau per mm of water */
  const double AR[] = {1.08,0.2634,0.1125 };	   /* empirical values */
  const double AO[] = {-0.0022,0.00038,0.00017};  /* slope (due to pressure change) */
  const double AT[] = {  0.0,    0.0,    0.0 };   /* temperature coefficient */
  const double TDRY = 245.;                       /* physical temp of dry component */
  const double B[] = {0.0200,0.0090,0.0110 };     /* dry opacity (ozone, etc.) */
  const double TOFF[] = { 3.5,   0.6,  0.1  };    /* offset of temp at zero water */
  const double DELT[] = { 14.2,  7.0,  7.0 };     /* effect of water on eff temp */
  const double RJC =  4.4;   		           /* R-J correction for 183 GHz (T large) */
  const double CMB = 0.367;		           /* R-J temp for 2.7 K         */
  double TOUT;                           /* Brightness temp above water */
  double TAUW;			   /* Water opacity */
  double ABSP;				   /* Absorption */
  double temporary;

  /* Loop over 3 channels with increasing offset from line centre.*/
  for(j=0; j<3; j++)
    {

      /* Dry component - assumed to be optically thin */
      TOUT = CMB + TDRY * B[j] * aMass;

      /* Opacity of water (quadratic term is mostly effect of altitude) */
      AEFF[j] = AR[j] + AO[j] * WA + AT[j] * (TWAT - 269.);
      TAUW = WA * AEFF[j];

      /* Effective temperature (includes effect of opacity, crudely) */
      temporary = -1. * TAUW/3.5;
      TEFF[j] = TWAT - RJC - TOFF[j] 
	+ DELT[j]*(1. - exp(temporary));

      /* Radiative transfer */
      temporary = -1. * TAUW-TAUO; 
      ABSP = exp(temporary);
      TBRI[j] = TOUT*ABSP + TEFF[j]*(1.-ABSP);

      /* Total opacity */ 
      TTAU[j] = TAUW + TAUO + B[j] * aMass;
      
    }
}
