/*
*+
*  Name:
*     fts2_phasecorrection.c

*  Purpose:
*     Applies phase correction to the given interferogram.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:
*    void fts2_phasecorrection( interferogram, size, zpdIndex,
                                dsHalfLength, ssHalfLength, polynomialDegree,
                                phaseFunctionHalfLength, wnLBoundPercent,
                                wnUBoundPercent, weightLimit, sigma,
                                coefficients,  phase, phaseCorrectionFunction,
                                phaseCorrectedInterferogram, status);

*  Arguments:
*   interferogram = double* (Given and returned)
      Pointer to the interferogram
    size = int (Given)
      Sample size of the interferogram
    zpdIndex = int (Given)
      Index of ZPD
    dsHalfLength = int (Given)
      Double-Sided interferogram half-length
    ssHalfLength = int (Given)
      Single-Sided interferogram half-length
    polynomialDegree = int (Given)
      Degree of the fitting polynomial
    phaseFunctionHalfLength = int (Given)
      Phase correction function half-length
    wnLBoundPercent = double (Given)
      The lower bound (%) of the wavenumber range
    wnUBoundPercent = double (Given)
      The upper bound (%) of the wavenumber range
    weightLimit = double (Given)
      Weight limit
    sigma = double* (Given and returned)
      Pointer to the array of standard deviations
    coefficients = double* (Given and returned)
      Pointer to the array of polynomial coefficients
    phase = double* (Given and returned)
      Pointer to the phase
    phaseCorrectionFunction = double* (Given and returned)
      Pointer to the phase correction function
    phaseCorrectedInterferogram = double* (Given and returned)
      Pointer to the phase corrected interferogram
    status = int* (Given and return)
      Status

*  Description:
*    Applies phase correction to the given interferogram.
*    This function destroys the original interferogram and
*    creates a new interferogram with (ssHalfLength + 1) sample size.
*    The interferogram is single-sided.

*  Authors:
*     COBA: Coskun OBA (UoL)

*  History :
*     2010-08-26 (COBA):
*        Original version.

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

*  License:
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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

/* STANDARD INCLUDES */
#include <math.h>
#include <stdlib.h>
#include <stdio.h>

/* STARLINK INCLUDES */
#include "ast.h"
#include "mers.h"
#include "sae_par.h"

/* FFTW INCLUDES */
#include <fftw3.h>

/* SMURF INCLUDES */
#include "libsmf/smf.h"
#include "fts2.h"

void fts2_phasecorrection(
    double* interferogram,        /* The interferogram */
    int size,                     /* Sample size of the interferogram */
    int zpdIndex,                 /* Index of ZPD */
    int dsHalfLength,             /* Double-Sided interferogram half-length */
    int ssHalfLength,             /* Single-Sided interferogram half-length */
    int polynomialDegree,     	  /* Degree of the fitting polynomial */
    int phaseFunctionHalfLength,  /* Phase correction function half-length  */
    double wnLBoundPercent,   	  /* The lower bound (%) of the wavenumber range */
    double wnUBoundPercent,   	  /* The upper bound (%) of the wavenumber range */
    double weightLimit,       	  /* Weight limit */
    double* sigma,                /* Standard Deviation (returned) */
    double* coefficients,         /* Pointer to hold polynomial coefficients (returned) */
    double* phase,                /* Phase (returned) */
    double* phaseCorrectionFunction, /* Phase correction function (returned) */
    double* phaseCorrectedInterferogram, /* Phase corrected interferogram (returned) */
    int* status)                  /* Status */
{
  if(*status != SAI__OK) { return; }

  int bandNumber			        = 0;  /* Number of bands */
  int cosSize       		      = 0;  /* Size of the cos phase */
  int dsLength                = 0;  /* Double-sided interferogram length */
  int i                       = 0;  /* Loop counter */
  int index					          = 0;  /* index */
  int interval				        = 0;  /* Interval */
  int j                       = 0;  /* Loop counter */
  int k                       = 0;  /* Loop counter */
  int leftShift               = 0;  /* Amount of left shifting */
  int numPoints				        = 0;  /* Number of points */
  int offset                  = 0;  /* Offset */
  int phaseFunctionSize       = 0;  /* Phase function length */
  int phaseSize               = 0;  /* Phase length */
  int sinSize       		      = 0;  /* Size of the sin phase */
  int tmp                     = 0;  /* temporary variable */
  int tmpSize    			        = 0;  /* Temporary size */
  int weightsSize			        = 0;  /* Weights length */
  int wnLBound                = 0;  /* Wave number factor lower bound */
  int wnUBound                = 0;  /* Wave number factor upper bound */
  int* bandIndex 			        = NULL; /* Index of the band */
  int* jitter 				        = NULL; /* Smoothing */
  double absminWeight         = 0.0;  /* Absolute min weight */
  double CLIP                 = 0.0;  /* Clipping param for the polynomial fit */
  double error                = 0.0;  /* Error */
  double maxWeight            = 0.0;  /* Max weight */
  double mean       		      = 0.0;  /* Mean */
  double sum        		      = 0.0;  /* Sum */
  double sumSquared  		      = 0.0;  /* Sum of squared values */
  double weightLimitSquared   = 0.0;  /* Square of the weight limit */
  double* bandPhase       	  = NULL; /* Band phase */
  double* bandPhaseInc    	  = NULL; /* Band phase increment */
  double* bandWavenumber  	  = NULL; /* Band wavenumber */
  double* bandWeights     	  = NULL; /* Band weights */
  double* cosPhase  		      = NULL; /* Cosine phase */
  double* dsInterferogram     = NULL; /* Double-sided interferogram */
  double* dsTmp               = NULL; /* Temporary double-sided interferogram */
  double* fit                 = NULL; /* The fitted phase */
  double* midPhase        	  = NULL; /* Mid phase */
  double* midPoints       	  = NULL; /* Mid points */
  double* sinPhase  		      = NULL; /* Sine phase */
  double* tmpInterferogram    = NULL; /* Temporary interferogram */
  double* wavenumber 		      = NULL; /* Wavenumber */
  double* weights             = NULL; /* Weights */
  fftw_complex* spectrum      = NULL; /* Spectrum */
  fftw_plan planCosine		    = NULL; /* fftw plan for cos */
  fftw_plan planSine		      = NULL; /* fftw plan for sine */
  fftw_plan plan			        = NULL; /* fftw plan */
  size_t nused;                       /* Number of used data points */

  dsLength = dsHalfLength << 1;
  wnLBound = (int)(dsHalfLength * wnLBoundPercent);
  wnUBound = (int)(dsHalfLength * wnUBoundPercent);
  weightLimitSquared = weightLimit * weightLimit;
  leftShift = zpdIndex + 1 - dsHalfLength;

  tmpSize = size - leftShift;
  tmpInterferogram = astCalloc(tmpSize, sizeof(*tmpInterferogram), 0.0);
  fts2_arraycopy(interferogram, size, tmpInterferogram, tmpSize, leftShift, 0, tmpSize - 1);

  /* GET DOUBLE-SIDED INTERFEROGRAM */
  dsInterferogram  = astCalloc(dsLength, sizeof(*dsInterferogram), 0.0);
  fts2_arraycopy(interferogram, size, dsInterferogram, dsLength, leftShift, 0, dsLength - 1);

  sum = 0.0;
  sumSquared = 0.0;
  for(i = 0; i < dsLength; i++) {
    sum += dsInterferogram[i];
  }
  mean = sum / dsLength;
  for(i = 0; i < dsLength; i++) {
    sumSquared += (dsInterferogram[i] - mean) * (dsInterferogram[i] - mean);
  }

  /* PHASE */
  if(!(sqrt(sumSquared / dsLength) < SMF__FLAT_THRESHOLD)) {
	  dsTmp  = astCalloc(dsLength, sizeof(*dsTmp), 0.0);
	  fts2_arraycopy(dsInterferogram, dsLength, dsTmp, dsLength, 0, 0, dsLength - 1);

	  spectrum  = (fftw_complex*) fftw_malloc(dsLength * sizeof(fftw_complex));
	  plan = fftw_plan_dft_r2c_1d(dsLength, dsTmp, spectrum, FFTW_ESTIMATE);
	  fftw_execute(plan);

	  /* WEIGHTS */
	  weightsSize = dsHalfLength;
	  weights = astCalloc(weightsSize, sizeof(*weights), 0.0);
	  for(i = 0; i < wnLBound; i++) {
	    weights[i] = 0.0;
	  }
	  for(i = wnUBound + 1; i < weightsSize; i++) {
	    weights[i] = 0.0;
	  }
	  for(i = wnLBound; i <= wnUBound; i++) {
	    weights[i] =  (spectrum[i][0] * spectrum[i][0]) + (spectrum[i][1] * spectrum[i][1]);
	    if(weights[i] > maxWeight) {
	      maxWeight = weights[i];
	    }
	  }
	  weights[0] = 0.0;
	  weights[weightsSize - 1] = 0.0;
	  if(maxWeight <= 0) {
	    maxWeight = 1;
	  }
	  absminWeight = maxWeight * weightLimitSquared;
	  for(i = 0; i < weightsSize; i++) {
	    if(weights[i] > absminWeight) {
	      weights[i] /= maxWeight;
	    }
	  }

	  /* PHASE */
	  phaseSize = dsHalfLength;
	  for(i = 0; i < phaseSize; i++) {
	    phase[i] = atan2(spectrum[i][1], spectrum[i][0]);
	    phase[i] -= PI / (phaseSize - 1) * i;
	    if(phase[i] < 0.0) {
	      phase[i] += PI;
	      if(phase[i] < 0.0) {
	        phase[i] += PI;
	      }
	    }
	  }

	  /* SMOOTH PHASE */
	  jitter = astCalloc(phaseSize, sizeof(*jitter), 0);
	  jitter[0] = 0;
	  for(i = 1; i < phaseSize; i++) {
	    jitter[i] = jitter[i - 1];
	    if(abs(phase[i] - phase[i - 1]) > (2.0 * PI / 3.0)) {
	      jitter[i] = (phase[i] > phase[i - 1]) ? jitter[i]-- : jitter[i]++;
	    }
	  }
	  for(i = 0; i < phaseSize; i++) {
	    phase[i] += (PI * jitter[i]);
	  }
	  astFree(jitter);

	  /* PIECEWISE PHASE FIT */
	  wavenumber= astCalloc(phaseSize, sizeof(*wavenumber), 0.0);
	  for( i = 0; i < phaseSize; i++) {
	    wavenumber[i] = i;
	  }

	  bandIndex = astCalloc(phaseSize, sizeof(*bandIndex), 0);
	  bandIndex[0] = 0;
	  for( i = 0; i < phaseSize; i++) {
	    if( i > 0 &&
	        (wavenumber[i] - wavenumber[i - 1]) > 1 &&
	        (i - bandIndex[bandNumber]) > polynomialDegree) {
	      bandIndex[++bandNumber] = i;
	    }
	  }
	  if((phaseSize - bandIndex[bandNumber]) > polynomialDegree) {
	    bandIndex[++bandNumber] = phaseSize;
	  }

	  if(bandNumber > 1) {
	    bandPhaseInc = astCalloc(bandNumber, sizeof(*bandPhaseInc), 0.0);
	    midPoints    = astCalloc((bandNumber + 1), sizeof(*midPoints), 0.0);
	    midPhase     = astCalloc((bandNumber * 2), sizeof(*midPhase), 0.0);

	    midPoints[0] = wavenumber[0];
	    for(i = 1; i < bandNumber; i++) {
	      index = bandIndex[i];
	      midPoints[i] = 0.5 * (wavenumber[index] + wavenumber[index - 1]);
	    }
	    midPoints[bandNumber] = wavenumber[phaseSize - 1];

	    for(i = 0; i < bandNumber; i++) {
	      numPoints = bandIndex[i + 1] - bandIndex[i];

	      bandPhase      = astCalloc(numPoints, sizeof(*bandPhase), 0.0);
	      bandWeights    = astCalloc(numPoints, sizeof(*bandWeights), 0.0);
	      bandWavenumber = astCalloc(numPoints, sizeof(*bandWavenumber), 0.0);

	      fts2_arraycopy(phase, phaseSize, bandPhase, numPoints, bandIndex[i], 0, numPoints - 1);
	      fts2_arraycopy(weights, phaseSize, bandWeights, numPoints, bandIndex[i], 0, numPoints - 1);
	      fts2_arraycopy(wavenumber, phaseSize, bandWavenumber, numPoints, bandIndex[i], 0, numPoints - 1);

	      smf_fit_poly1d( polynomialDegree, numPoints, CLIP,
	                      bandWavenumber, bandPhase, bandWeights, NULL, coefficients,
	                      NULL, NULL, &nused, status);

        index = i << 1;
	      EVALPOLY(midPhase[index], midPoints[i], polynomialDegree, coefficients);
	      EVALPOLY(midPhase[index + 1], midPoints[i + 1], polynomialDegree, coefficients);

	      astFree(bandWavenumber);
	      astFree(bandWeights);
	      astFree(bandPhase);
	    }

	    bandPhaseInc[0] = 0;
	    for(i = 1; i < bandNumber; i++) {
	      interval = 0;
	      index = i << 1;
	      if(midPhase[index] > midPhase[index - 1]) {
	        interval = (int) ((midPhase[index] - midPhase[index - 1]) / PI + 0.4444);
	      } else {
	        interval = (int) ((midPhase[index] - midPhase[index - 1]) / PI - 0.4444);
	      }
	      bandPhaseInc[i] = bandPhaseInc[i - 1] - interval * PI;
	    }
	    for(i = 0; i < bandNumber; i++) {
	      for(k = bandIndex[i]; k < bandIndex[i + 1]; k++) {
	  	    phase[k] += bandPhaseInc[i];
	      }
	    }
	    astFree(bandPhaseInc);
	    astFree(midPhase);
	    astFree(midPoints);
	  }

	  /* POLYNOMIAL FIT COEFFICIENTS */
	  fit = astCalloc(phaseSize, sizeof(*fit), 0.0);
	  smf_fit_poly1d( polynomialDegree, phaseSize, CLIP,
	                  wavenumber, phase, weights, NULL, coefficients,
	                  NULL, fit, &nused, status);

    /* STANDARD DEVIATION */
    sum = 0.0;
    for(i = 0; i < phaseSize; i++) {
      error += weights[i] * (phase[i] - fit[i]) * (phase[i] - fit[i]);
      sum   += weights[i];
    }
    *sigma = sqrt(error / sum);
	  astFree(fit);

	  // PHASE
	  phaseSize = dsHalfLength + 1;
	  for(i = 0; i < phaseSize; i++) {
	    EVALPOLY(phase[i], i, polynomialDegree, coefficients);
	  }

	  // FREE RESOURCES
	  astFree(dsTmp);
	  astFree(bandIndex);
	  astFree(wavenumber);
	  astFree(weights);
	  fftw_free(spectrum);
	  fftw_destroy_plan(plan);
  }

  /* PHASE CORRECTION FUNCTION */
  cosSize = dsHalfLength + 1;
  cosPhase = fftw_malloc(cosSize * sizeof(*cosPhase));
  for(i = 0; i < cosSize; i++) {
    cosPhase[i] = cos(phase[i]);
  }
  planCosine = fftw_plan_r2r_1d(cosSize, cosPhase, cosPhase, FFTW_REDFT10, FFTW_ESTIMATE);
  fftw_execute(planCosine);

  sinSize = dsHalfLength - 1;
  sinPhase = fftw_malloc(sinSize * sizeof(*sinPhase));
  for(i = 0; i < sinSize; i++) {
    sinPhase[i] = sin(phase[i]);
  }
  planSine = fftw_plan_r2r_1d(sinSize, sinPhase, sinPhase, FFTW_RODFT10, FFTW_ESTIMATE);
  fftw_execute(planSine);

  if(phaseFunctionHalfLength >= dsHalfLength) {
	  phaseFunctionSize = dsHalfLength << 1;
    index = dsHalfLength - 1;
    phaseCorrectionFunction[index] = cosPhase[0];
    for(i = 1; i < dsHalfLength; i++) {
      phaseCorrectionFunction[index + i] = cosPhase[i] + sinPhase[i - 1];
      phaseCorrectionFunction[index - i] = cosPhase[i] - sinPhase[i - 1];
    }
    phaseCorrectionFunction[phaseFunctionSize - 1] = cosPhase[index + 1];
  } else {
	  phaseFunctionSize = phaseFunctionHalfLength << 1;
    index = phaseFunctionHalfLength - 1;
    phaseCorrectionFunction[index] = cosPhase[0];
    for(i = 1; i < phaseFunctionHalfLength; i++) {
      phaseCorrectionFunction[index + i] = cosPhase[i] + sinPhase[i - 1];
      phaseCorrectionFunction[index - i] = cosPhase[i] - sinPhase[i - 1];
    }
    phaseCorrectionFunction[phaseFunctionSize - 1] = cosPhase[index + 1] + sinPhase[index];
  }

  /* CONVOLVE TEMPORARY INTERFEROGRAM WITH THE PHASE CORRECTION FUNCTION */
  offset = dsHalfLength + phaseFunctionHalfLength - 2;
  if(tmpSize < offset) {
    *status = SAI__ERROR;
    return;
  }
  size = ssHalfLength + 1;
  tmp = ssHalfLength + offset + 1;
  for(i = offset; i < tmp; i++) {
    sum = 0.0;
    for(j = 0; j < phaseFunctionSize; j++) {
      k = i - j;
      if(k < 0) {
        k += tmpSize;
      }
      if(k < tmpSize) {
    	  sum += (tmpInterferogram[k] * phaseCorrectionFunction[j]);
      }
    }
    phaseCorrectedInterferogram[i - offset] = sum / dsLength;
  }

  // FREE RESOURCES
  fftw_free(cosPhase);
  fftw_free(sinPhase);
  fftw_destroy_plan(planCosine);
  fftw_destroy_plan(planSine);
  astFree(dsInterferogram);
  astFree(tmpInterferogram);
}
