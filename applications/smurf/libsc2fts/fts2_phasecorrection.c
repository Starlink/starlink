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
    phaseFunction = double* (Given and returned)
      Pointer to the phase correction function
    newInterferogram = double* (Given and returned)
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
*     2010-09-20 (COBA):
*        Replaced PI with AST__DPI

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
    double* interferogram,
    int size,
    int zpdIndex,
    int dsHalfLength,
    int ssHalfLength,
    int polynomialDegree,
    int phaseFunctionHalfLength,
    double wnLBoundPercent,
    double wnUBoundPercent,
    double weightLimit,
    double* sigma,
    double* coefficients,
    double* phase,
    double* phaseFunction,
    double* newInterferogram,
    int* status)
{
  if(*status != SAI__OK) { return; }

  int bandNumber			        = 0;  /* Number of bands */
  int coeffLength             = 0;  /* Number of polynomial coefficients */
  int cosLength      		      = 0;  /* Size of the cos phase */
  int dsLength                = 0;  /* Double-sided interferogram length */
  int i                       = 0;  /* Loop counter */
  int index					          = 0;  /* index */
  int interval				        = 0;  /* Interval */
  int j                       = 0;  /* Loop counter */
  int k                       = 0;  /* Loop counter */
  int leftShift               = 0;  /* Amount of left shifting */
  int numPoints				        = 0;  /* Number of points */
  int offset                  = 0;  /* Offset */
  int phaseFunctionLength     = 0;  /* Phase function length */
  int phaseLength             = 0;  /* Phase length */
  int sinLength      		      = 0;  /* Size of the sin phase */
  int tmp                     = 0;  /* Temporary variable */
  int tmpLength  			        = 0;  /* Temporary size */
  int weightsLength		        = 0;  /* Weights length */
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
  double tmpVal  		          = 0.0;  /* Temporary value */
  double weightLimitSquared   = 0.0;  /* Square of the weight limit */
  double* bandPhase       	  = NULL; /* Band phase */
  double* bandPhaseInc    	  = NULL; /* Band phase increment */
  double* bandWavenumber  	  = NULL; /* Band wavenumber */
  double* bandWeights     	  = NULL; /* Band weights */
  double* cosPhase  		      = NULL; /* Cosine phase */
  double* dsInterferogram     = NULL; /* Double-sided interferogram */
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

  coeffLength = polynomialDegree + 1;
  dsLength = dsHalfLength << 1;
  wnLBound = (int)(dsHalfLength * wnLBoundPercent);
  wnUBound = (int)(dsHalfLength * wnUBoundPercent);
  weightLimitSquared = weightLimit * weightLimit;
  leftShift = zpdIndex + 1 - dsHalfLength;

  /* DOUBLE-SIDED INTERFEROGRAM */
  dsInterferogram  = astMalloc(dsLength * sizeof(*dsInterferogram));
  fts2_arraycopy(interferogram, size, dsInterferogram, dsLength, leftShift, 0, dsLength);

  sum = 0.0;
  sumSquared = 0.0;
  for(i = 0; i < dsLength; i++) { sum += dsInterferogram[i]; }
  mean = sum / dsLength;
  for(i = 0; i < dsLength; i++) {
    tmpVal = dsInterferogram[i] - mean;
    sumSquared += (tmpVal * tmpVal);
  }

  if(sqrt(sumSquared / dsLength) < SMF__FLAT_THRESHOLD) { // IF FLAT
    *sigma = 0.0;
    phaseLength = dsHalfLength + 1;
    for(i = 0; i < phaseLength; i++) {
      phase[i] = 0.0;
    }
  } else { /* IF THE DOUBLE-SIDED INTERFEROGRAM IS NOT FLAT */
	  spectrum  = fftw_malloc(dsLength * sizeof(*spectrum));
	  plan = fftw_plan_dft_r2c_1d(dsLength, dsInterferogram, spectrum, FFTW_ESTIMATE);
	  fftw_execute(plan);

	  /* WEIGHTS */
	  weightsLength = dsHalfLength;
	  weights = astMalloc(weightsLength * sizeof(*weights));
	  for(i = 0; i < wnLBound; i++) { weights[i] = 0.0; }
	  for(i = wnUBound + 1; i < weightsLength; i++) { weights[i] = 0.0; }
	  for(i = wnLBound; i <= wnUBound; i++) {
	    weights[i] =  (spectrum[i][0] * spectrum[i][0]) + (spectrum[i][1] * spectrum[i][1]);
	    if(weights[i] > maxWeight) { maxWeight = weights[i]; }
	  }
	  weights[0] = weights[weightsLength - 1] = 0.0;
	  if(maxWeight <= 0) { maxWeight = 1; }
	  absminWeight = maxWeight * weightLimitSquared;
	  for(i = 0; i < phaseLength; i++) {
	    if(weights[i] > absminWeight) { weights[i] /= maxWeight; }
	  }

	  /* COMPUTE PHASE */
	  phaseLength = dsHalfLength;
	  tmpLength = phaseLength - 1;
	  for(i = 0; i < phaseLength; i++) {
	    phase[i] = atan2(spectrum[i][1], spectrum[i][0]);
	    phase[i] -= AST__DPI / tmpLength * i;
	    if(phase[i] < 0.0) {
	      phase[i] += AST__DPI;
	      if(phase[i] < 0.0) { phase[i] += AST__DPI; }
	    }
	  }

	  /* SMOOTH PHASE */
	  jitter = astMalloc(phaseLength * sizeof(*jitter));
	  jitter[0] = 0;
	  for(i = 1; i < phaseLength; i++) {
	    j = i - 1;
	    jitter[i] = jitter[j];
	    if(abs(phase[i] - phase[j]) > (2.0 * AST__DPI / 3.0)) {
	      jitter[i] = (phase[i] > phase[j]) ? jitter[i]-- : jitter[i]++;
	    }
	  }
	  for(i = 0; i < phaseLength; i++) { phase[i] += AST__DPI * jitter[i]; }
	  astFree(jitter);

	  /* PIECEWISE PHASE FIT */
	  wavenumber = astMalloc(phaseLength * sizeof(*wavenumber));
	  for( i = 0; i < phaseLength; i++) { wavenumber[i] = i; }

	  bandIndex = astMalloc(phaseLength * sizeof(*bandIndex));
	  bandIndex[0] = 0;
	  for( i = 0; i < phaseLength; i++) {
	    if( i > 0 && (wavenumber[i] - wavenumber[i - 1]) > 1 &&
	        (i - bandIndex[bandNumber]) > polynomialDegree) {
	      bandIndex[++bandNumber] = i;
	    }
	  }
	  if((phaseLength - bandIndex[bandNumber]) > polynomialDegree) {
	    bandIndex[++bandNumber] = phaseLength;
	  }

	  if(bandNumber > 1) {
	    bandPhaseInc = astMalloc(bandNumber * sizeof(*bandPhaseInc));
	    midPoints    = astMalloc((1 + bandNumber) * sizeof(*midPoints));
	    midPhase     = astMalloc((2 * bandNumber) * sizeof(*midPhase));

	    midPoints[0] = wavenumber[0];
	    for(i = 1; i < bandNumber; i++) {
	      index = bandIndex[i];
	      midPoints[i] = 0.5 * (wavenumber[index] + wavenumber[index - 1]);
	    }
	    midPoints[bandNumber] = wavenumber[phaseLength - 1];
	    for(i = 0; i < bandNumber; i++) {
	      numPoints = bandIndex[i + 1] - bandIndex[i];

	      bandPhase      = astMalloc(numPoints * sizeof(*bandPhase));
	      bandWeights    = astMalloc(numPoints * sizeof(*bandWeights));
	      bandWavenumber = astMalloc(numPoints * sizeof(*bandWavenumber));

	      fts2_arraycopy(phase, phaseLength, bandPhase, numPoints, bandIndex[i], 0, numPoints);
	      fts2_arraycopy(weights, phaseLength, bandWeights, numPoints, bandIndex[i], 0, numPoints);
	      fts2_arraycopy(wavenumber, phaseLength, bandWavenumber, numPoints, bandIndex[i], 0, numPoints);

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
	      j = index - 1;
	      if(midPhase[index] > midPhase[j]) {
	        interval = (int) ((midPhase[index] - midPhase[j]) / AST__DPI + 0.4444);
	      } else {
	        interval = (int) ((midPhase[index] - midPhase[j]) / AST__DPI - 0.4444);
	      }
	      bandPhaseInc[i] = bandPhaseInc[i - 1] - interval * AST__DPI;
	    }
	    for(i = 0; i < bandNumber; i++) {
	      j = i + 1;
	      for(k = bandIndex[i]; k < bandIndex[j]; k++) {
	  	    phase[k] += bandPhaseInc[i];
	      }
	    }
	    astFree(bandPhaseInc);
	    astFree(midPhase);
	    astFree(midPoints);
	  }

    sum = 0.0;
    error = 0.0;
	  fit = astMalloc(phaseLength * sizeof(*fit));
	  smf_fit_poly1d( polynomialDegree, phaseLength, CLIP,
	                  wavenumber, phase, weights, NULL, coefficients,
	                  NULL, fit, &nused, status);
    for(i = 0; i < phaseLength; i++) {
      tmpVal = phase[i] - fit[i];
      error += weights[i] * tmpVal * tmpVal;
      sum   += weights[i];
    }
    *sigma = sqrt(error / sum);
	  astFree(fit);

	  /* PHASE */
	  phaseLength = dsHalfLength + 1;
	  for(i = 0; i < phaseLength; i++) {
	    EVALPOLY(phase[i], i, polynomialDegree, coefficients);
	  }

	  astFree(bandIndex);
	  astFree(wavenumber);
	  astFree(weights);
  }
  astFree(dsInterferogram);

  /* PHASE FUNCTION */
  cosLength = dsHalfLength + 1;
  cosPhase = fftw_malloc(cosLength * sizeof(*cosPhase));
  for(i = 0; i < cosLength; i++) {
    cosPhase[i] = cos(phase[i]);
  }
  planCosine = fftw_plan_r2r_1d(cosLength, cosPhase, cosPhase, FFTW_REDFT10, FFTW_ESTIMATE);
  fftw_execute(planCosine);

  sinLength = dsHalfLength - 1;
  sinPhase = fftw_malloc(sinLength * sizeof(*sinPhase));
  for(i = 0; i < sinLength; i++) {
    sinPhase[i] = sin(phase[i]);
  }
  planSine = fftw_plan_r2r_1d(sinLength, sinPhase, sinPhase, FFTW_RODFT10, FFTW_ESTIMATE);
  fftw_execute(planSine);

  if(dsHalfLength <= phaseFunctionHalfLength) {
	  phaseFunctionLength = dsHalfLength << 1;
    index = dsHalfLength - 1;
    phaseFunction[index] = cosPhase[0];
    for(i = 1; i < dsHalfLength; i++) {
      j = i - 1;
      phaseFunction[index + i] = cosPhase[i] + sinPhase[j];
      phaseFunction[index - i] = cosPhase[i] - sinPhase[j];
    }
    phaseFunction[phaseFunctionLength - 1] = cosPhase[index + 1];
  } else {
	  phaseFunctionLength = phaseFunctionHalfLength << 1;
    index = phaseFunctionHalfLength - 1;
    phaseFunction[index] = cosPhase[0];
    for(i = 1; i < phaseFunctionHalfLength; i++) {
      j = i - 1;
      phaseFunction[index + i] = cosPhase[i] + sinPhase[j];
      phaseFunction[index - i] = cosPhase[i] - sinPhase[j];
    }
    phaseFunction[phaseFunctionLength - 1] = cosPhase[index + 1] + sinPhase[index];
  }

  /* CONVOLVE TEMPORARY INTERFEROGRAM WITH THE PHASE CORRECTION FUNCTION */
  tmpLength = size - leftShift;
  tmpInterferogram = astMalloc(tmpLength * sizeof(*tmpInterferogram));
  fts2_arraycopy(interferogram, size, tmpInterferogram, tmpLength, leftShift, 0, tmpLength);
  offset = dsHalfLength + phaseFunctionHalfLength - 2;
  if(tmpLength < offset) {
    *status != SAI__ERROR;
    return;
  }
  size = ssHalfLength + 1;
  tmp = ssHalfLength + offset + 1;
  for(i = offset; i < tmp; i++) {
    sum = 0.0;
    for(j = 0; j < phaseFunctionLength; j++) {
      k = i - j;
      if(k < 0) { k += tmpLength; }
      if(k < tmpLength) { sum += (tmpInterferogram[k] * phaseFunction[j]); }
    }
    newInterferogram[i - offset] = sum / dsLength;
  }
  astFree(tmpInterferogram);

  /* CLEAN UP FFTW */
	if(plan) fftw_destroy_plan(plan);
  if(planSine) fftw_destroy_plan(planSine);
  if(planCosine) fftw_destroy_plan(planCosine);
	if(spectrum) fftw_free(spectrum);
  if(cosPhase) fftw_free(cosPhase);
  if(sinPhase) fftw_free(sinPhase);
  /* Call to fftw_cleanup ensures that memory allocated by fftw is freed properly
   * However, if there are multiple calls to this routine and, fftw_cleanup
   * is NOT called, the routine produces different results for the same input.
   * In order to produce same results for the same input make sure fftw_cleanup
   * is called. This significantly reduces the processing speed.
   * More advanced features of FFTW may need to be utilized to gain performance.
   */
	fftw_cleanup();
}
