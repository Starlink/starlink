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

*  Description:
*    Applies phase correction to the given interferogram.
*    This function destroys the original interferogram and 
*    creates a new interferogram with (ssHalfLength + 1) sample size. 
*    The interferogram is single-sided.

*  Authors:
*     Coskun (Josh) OBA (UoL)

*  History :
*     2010-08-26 (COBA):
*        Original.

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

/* FFTW INCLUDES */
#include <fftw3.h>

/* SMURF INCLUDES */
#include "fts2.h"

int fts2_phasecorrcetion(
    double* interferogram,        /* The interferogram */
    int size,                     /* Sample size of the interferogram */
    int zpdIndex,                 /* Index of ZPD */
    int* dsHalfLength,            /* Double-Sided interferogram half-length */
    int* ssHalfLength,            /* Single-Sided interferogram half-length */
    int polynomialDegree,     	  /* Degree of the fitting polynomial */
    int phaseFunctionHalfLength,  /* Phase correction function half-length  */
    double wnLBoundPercent,   	  /* The lower bound of the wavenumber range (as percentage) */
    double wnUBoundPercent,   	  /* The upper bound of the wavenumber range (as percentage) */
    double weightLimit,       	  /* Weight limit */
    double* sigma)				        /* Holds the value of the Standard Deviation after performing the Phase Correction */
{
  int bandNumber			        = 0;
  int coordinateIndex		      = 0;
  int cosSize       		      = 0;
  int dsLength                = 0;
  int i                       = 0;
  int index					          = 0;
  int interval				        = 0;
  int isFlat				          = 0;
  int j                       = 0;
  int k                       = 0;
  int leftShift               = 0;
  int nPoints				          = 0;
  int num                     = 0;
  int numPoints				        = 0;
  int offset                  = 0;
  int phaseIndex			        = 0;
  int phaseFunctionSize       = 0;
  int phaseSize               = 0;
  int phaseWeightsSize		    = 0;
  int sinSize       		      = 0;
  int spectrumSize            = 0;
  int ssHalfLengthInit        = 0;
  int tmp                     = 0;
  int tmpSize    			        = 0;
  int weightIndex			        = 0;
  int weightsSize			        = 0;
  int wnLBound                = 0;
  int wnUBound                = 0;
  int* bandIndex 			        = NULL;
  int* jitter 				        = NULL;
  double absminWeight         = 0.0;
  double maxWeight            = 0.0;
  double mean       		      = 0.0;
  double sum        		      = 0.0;
  double sumSquared  		      = 0.0;
  double tempVal              = 0.0;
  double weightLimitSquared   = 0.0;
  double* bandPhase       	  = NULL;
  double* bandPhaseInc    	  = NULL;
  double* bandWavenumber  	  = NULL;
  double* bandWeights     	  = NULL;
  double* coefficients    	  = NULL;
  double* cosPhase  		      = NULL;
  double* dsInterferogram     = NULL;
  double* midPhase        	  = NULL;
  double* midPoints       	  = NULL;
  double* newWavenumber   	  = NULL;
  double* phase               = NULL;
  double* phaseFunction       = NULL;
  double* phaseWeights        = NULL;
  double* sinPhase  		      = NULL;
  double* tmpInterferogram    = NULL;
  double* tmpPhase    		    = NULL;
  double* wavenumber 		      = NULL;
  double* weights             = NULL;
  fftw_complex* spectrum      = NULL;
  fftw_plan planCosine		    = NULL;
  fftw_plan planSine		      = NULL;
  fftw_plan plan			        = NULL;

  dsLength = *dsHalfLength << 1;
  wnLBound = (int)(*dsHalfLength * wnLBoundPercent);
  wnUBound = (int)(*dsHalfLength * wnUBoundPercent);
  weightLimitSquared = weightLimit * weightLimit;

  /* DOUBLE-SIDED INTERFEROGRAM HALF-LENGTH */
  if((zpdIndex + 1) < *dsHalfLength)
  {
    tmp = zpdIndex + 1;
    *dsHalfLength = 1;
    for(i = tmp; i >= 1; i--)
    {
      num = i;
      while(num % 2 == 0) { num /= 2; }
      while(num % 3 == 0) { num /= 3; }
      while(num % 5 == 0) { num /= 5; }
      if(num % 7 == 0) { num /= 7; }
      if(num == 1) { *dsHalfLength = i; break; }
    }
  }
  else
  {
    tmp = *dsHalfLength;
    *dsHalfLength = 1;
    for(i = tmp; i >= 1; i--)
    {
      num = i;
      while(num % 2 == 0) { num /= 2; }
      while(num % 3 == 0) { num /= 3; }
      while(num % 5 == 0) { num /= 5; }
      if(num % 7 == 0) { num /= 7; }
      if(num == 1) { *dsHalfLength = i; break; }
    }
  }

  /* SINGLE-SIDED INTERFEROGRAM HALF-LENGTH */
  ssHalfLengthInit = size - zpdIndex - phaseFunctionHalfLength;
  if(ssHalfLengthInit < *ssHalfLength)
  {
    tmp = ssHalfLengthInit;
    *ssHalfLength = 1;
    for(i = tmp; i >= 1; i--)
    {
      num = i;
      while(num % 2 == 0) { num /= 2; }
      while(num % 3 == 0) { num /= 3; }
      while(num % 5 == 0) { num /= 5; }
      if(num == 1) { *ssHalfLength = i; break; }
    }
  }
  else
  {
    tmp = *ssHalfLength;
    *ssHalfLength = 1;
    for(i = tmp; i >= 1; i--)
    {
      num = i;
      while(num % 2 == 0) { num /= 2; }
      while(num % 3 == 0) { num /= 3; }
      while(num % 5 == 0) { num /= 5; }
      if(num == 1) { *ssHalfLength = i; break; }
    }
  }

  /* LEFT-SHIFT */
  leftShift = zpdIndex + 1 - *dsHalfLength;

  /* GET DOUBLE-SIDED INTERFEROGRAM */
  dsInterferogram = (double*) malloc(dsLength * sizeof(double));
  fts2_arraycopy(interferogram, size, dsInterferogram, dsLength, leftShift, 0, dsLength - 1);

  tmpSize = size - leftShift;
  tmpInterferogram = (double*) malloc(tmpSize * sizeof(double));
  fts2_arraycopy(interferogram, size, tmpInterferogram, tmpSize, leftShift, 0, tmpSize - 1);

  /* CHECK IF THE DOUBLE-SIDED INTERFEROGRAM IS FLAT */
  for(i = 0; i < dsLength; i++) 
  { 
    sum += dsInterferogram[i]; 
  }
  mean = sum / dsLength;
  for(i = 0; i < dsLength; i++) 
  { 
    sumSquared += (dsInterferogram[i] - mean) * (dsInterferogram[i] - mean); 
  }
  isFlat = sqrt(sumSquared / dsLength) < SMF__FLAT_THRESHOLD;

  /*
  * GET PHASE
  */
  if(isFlat)
  {
    phaseSize = *dsHalfLength + 1;
	  phase = (double*) astMalloc(phaseSize * sizeof(double));
	  for(i = 0; i < phaseSize; i++) 
	  { 
	    phase[i] = 0.0; 
	  }
  }
  else
  {
	  tmpPhase  = (double*) astMalloc(dsLength * sizeof(double));
	  fts2_arraycopy(dsInterferogram, dsLength, tmpPhase, dsLength, 0, 0, dsLength - 1);

	  spectrumSize = dsLength;
	  spectrum  = (fftw_complex*) fftw_malloc(spectrumSize * sizeof(fftw_complex));
	  plan = fftw_plan_dft_r2c_1d(spectrumSize, dsInterferogram, spectrum, FFTW_ESTIMATE);
	  fftw_execute(plan);
	  spectrumSize >>= 1;

	  /* WEIGHTS */
	  weightsSize = spectrumSize;
	  weights = (double*) astMalloc(weightsSize * sizeof(double));
	  for(i = 0; i < wnLBound; i++) 
	  { 
	    weights[i] = 0.0; 
	  }
	  for(i = wnUBound + 1; i < weightsSize; i++) 
	  { 
	    weights[i] = 0.0; 
	  }
	  for(i = wnLBound; i <= wnUBound; i++)
	  {
	    weights[i] = spectrum[i][0] * spectrum[i][0] + spectrum[i][1] * spectrum[i][1];
	    if(weights[i] > maxWeight) { maxWeight = weights[i]; }
	  }
	  weights[0] = weights[weightsSize - 1] = 0.0;
	  if(maxWeight <= 0) 
	  { 
	    maxWeight = 1; 
	  }

	  /* COMPUTE PHASE */
	  phase = (double*) malloc(spectrumSize * sizeof(double));
	  for(i = 0; i < spectrumSize; i++)
	  {
	    phase[i] = atan2(spectrum[i][1], spectrum[i][0]);
	    phase[i] -= PI / (spectrumSize - 1) * i;
	    if(phase[i] < 0.0)
	    {
	      phase[i] += PI;
	      if(phase[i] < 0.0) { phase[i] += PI; }
	    }
	  }
	  astFree(tmpPhase); tmpPhase = NULL;

	  /* SMOOTH PHASE */
	  jitter = (int*) astMalloc(spectrumSize * sizeof(int));
	  jitter[0] = 0;
	  for(i = 1; i < spectrumSize; i++)
	  {
	    jitter[i] = jitter[i - 1];
	    if(abs(phase[i] - phase[i - 1]) > (2.0 * PI / 3.0))
	    {
	      jitter[i] = (phase[i] > phase[i - 1]) ? jitter[i]-- : jitter[i]++;
	    }
	  }
	  for(i = 0; i < spectrumSize; i++) 
	  { 
	    phase[i] += PI * jitter[i]; 
	  }
	  astFree(jitter); jitter = NULL;

	  /* PHASE WEIGHTS */
	  absminWeight      = maxWeight * weightLimitSquared;
	  phaseWeightsSize  = 3 * spectrumSize + 1;
	  phaseWeights      = (double*) astMalloc(phaseWeightsSize * sizeof(double));
	  coordinateIndex   = 0;
	  phaseIndex        = spectrumSize;
	  weightIndex       = 2 * spectrumSize;
	  for(i = 0; i < spectrumSize; i++)
	  {
	    if(weights[i] > absminWeight)
	    {
	      phaseWeights[phaseIndex++] = phase[i];
	      phaseWeights[weightIndex++] = weights[i] / maxWeight;
	      phaseWeights[coordinateIndex++] = i;
	    }
	  }
	  phaseWeights[phaseWeightsSize - 1] = coordinateIndex;

	  astFree(phase); phase = NULL;
	  astFree(weights); weights = NULL;

	  /* PIECEWISE PHASE FIT */
	  nPoints 	= (int) phaseWeights[phaseWeightsSize - 1];
	  bandIndex = (int*) astMalloc(nPoints * sizeof(int));
	  phase 		= (double*) astMalloc(nPoints * sizeof(double));
	  wavenumber= (double*) astMalloc(nPoints * sizeof(double));
	  weights 	= (double*) astMalloc(nPoints * sizeof(double));

	  bandIndex[0] = 0;
	  for( i = 0; i < nPoints; i++)
	  {
	    wavenumber[i] = phaseWeights[i];
	    phase[i]      = phaseWeights[*dsHalfLength + 1 + i];
	    weights[i]    = phaseWeights[2 * (*dsHalfLength + 1) + i];

	    if( i > 0 &&
	        (wavenumber[i] - wavenumber[i - 1]) > 1 &&
	        (i - bandIndex[bandNumber]) > polynomialDegree)
	    {
	      bandIndex[++bandNumber] = i;
	    }
	  }
	  if((nPoints - bandIndex[bandNumber]) > polynomialDegree)
	  {
	    bandIndex[++bandNumber] = nPoints;
	  }

	  if(bandNumber > 1)
	  {
	    midPoints = (double*) malloc((bandNumber + 1) * sizeof(double));
	    midPoints[0] = wavenumber[0];
	    for(i = 1; i < bandNumber; i++)
	    {
	      index = bandIndex[i];
	      midPoints[i] = 0.5 * (wavenumber[index] + wavenumber[index - 1]);
	    }
	    midPoints[bandNumber] = wavenumber[nPoints - 1];

	    midPhase = (double*) malloc(2 * bandNumber * sizeof(double));
	    for(i = 0; i < bandNumber; i++)
	    {
	      numPoints = bandIndex[i + 1] - bandIndex[i];

	      bandPhase = (double*) astMalloc(numPoints * sizeof(double));
	      bandWeights = (double*) astMalloc(numPoints * sizeof(double));
	      bandWavenumber = (double*) astMalloc(numPoints * sizeof(double));
  
	      fts2_arraycopy(phase, nPoints, bandPhase, numPoints, bandIndex[i], 0, numPoints - 1);
	      fts2_arraycopy(weights, nPoints, bandWeights, numPoints, bandIndex[i], 0, numPoints - 1);
	      fts2_arraycopy(wavenumber, nPoints, bandWavenumber, numPoints, bandIndex[i], 0, numPoints - 1);

	      coefficients = fts2_polyfitcoeffs(polynomialDegree, bandWavenumber, bandPhase, bandWeights, numPoints, sigma);

        index = 2 * i;
	      midPhase[index] = fts2_polyfit(polynomialDegree, coefficients, midPoints[i]);
	      midPhase[index + 1] = fts2_polyfit(polynomialDegree, coefficients, midPoints[i + 1]);

	      astFree(bandWavenumber); bandWavenumber = NULL;
	      astFree(bandPhase); bandPhase = NULL;
	      astFree(bandWeights); bandWeights = NULL;
	      astFree(coefficients); coefficients = NULL;
	    }

	    bandPhaseInc = (double*) astMalloc(bandNumber * sizeof(double));
	    bandPhaseInc[0] = 0;
	    for(i = 1; i < bandNumber; i++)
	    {
	      interval = 0;
	      index = 2 * i;
	      if(midPhase[index] > midPhase[index - 1])
	      {
	        interval = (int) ((midPhase[index] - midPhase[index - 1]) / PI + 0.4444);
	      }
	      else
	      {
	        interval = (int) ((midPhase[index] - midPhase[index - 1]) / PI - 0.4444);
	      }
	      bandPhaseInc[i] = bandPhaseInc[i - 1] - interval * PI;
	    }

	    for(i = 0; i < bandNumber; i++)
	    {
	      for(k = bandIndex[i]; k < bandIndex[i + 1]; k++)
	      {
	  	    phase[k] += bandPhaseInc[i];
	      }
	    }
	    astFree(midPhase); midPhase = NULL;
	    astFree(midPoints); midPoints = NULL;
	  }

	  coefficients = fts2_polyfitcoeffs(polynomialDegree, wavenumber, phase, weights, nPoints, sigma);
	  newWavenumber = (double*) astMalloc((*dsHalfLength + 1) * sizeof(double));
	  for(i = 0; i < *dsHalfLength + 1; i++)
	  {
	    newWavenumber[i] = i;
	  }
	  phase = fts2_polyfitarray(polynomialDegree, coefficients, newWavenumber, *dsHalfLength + 1);
	  phaseSize = *dsHalfLength + 1;

	  /* FREE RESOURCES */
	  astFree(bandPhaseInc); bandPhaseInc = NULL;
	  astFree(coefficients); coefficients = NULL;
	  astFree(newWavenumber); newWavenumber = NULL;
	  astFree(wavenumber); wavenumber = NULL;
	  astFree(weights); weights = NULL;
	  fftw_free(spectrum); spectrum = NULL;
	  fftw_destroy_plan(plan); plan = NULL;
  }

  /* GET PHASE CORRECTION FUNCTION */
  cosSize = *dsHalfLength + 1;
  cosPhase = (double*) fftw_malloc(cosSize * sizeof(double));
  for(i = 0; i < cosSize; i++) 
  { 
    cosPhase[i] = cos(phase[i]); 
  }
  planCosine = fftw_plan_r2r_1d(cosSize, cosPhase, cosPhase, FFTW_REDFT10, FFTW_ESTIMATE);
  fftw_execute(planCosine);

  sinSize = *dsHalfLength - 1;
  sinPhase = (double*) fftw_malloc(sinSize * sizeof(double));
  for(i = 0; i < sinSize; i++) 
  { 
    sinPhase[i] = sin(phase[i]); 
  }
  planSine = fftw_plan_r2r_1d(sinSize, sinPhase, sinPhase, FFTW_RODFT10, FFTW_ESTIMATE);
  fftw_execute(planSine);

  if(phaseFunctionHalfLength >= *dsHalfLength)
  {
	  phaseFunctionSize = *dsHalfLength << 1;
    phaseFunction = (double*) astMalloc(phaseFunctionSize * sizeof(double));
    index = *dsHalfLength - 1;
    phaseFunction[index] = cosPhase[0];
    for(i = 1; i < *dsHalfLength; i++)
    {
      phaseFunction[index + i] = cosPhase[i] + sinPhase[i - 1];
      phaseFunction[index - i] = cosPhase[i] - sinPhase[i - 1];
    }
    phaseFunction[phaseFunctionSize - 1] = cosPhase[index + 1];
  }
  else
  {
	  phaseFunctionSize = phaseFunctionHalfLength << 1;
    phaseFunction = (double*) astMalloc(phaseFunctionSize * sizeof(double));
    index = phaseFunctionHalfLength - 1;
    phaseFunction[index] = cosPhase[0];
    for(i = 1; i < phaseFunctionHalfLength; i++)
    {
      phaseFunction[index + i] = cosPhase[i] + sinPhase[i - 1];
      phaseFunction[index - i] = cosPhase[i] - sinPhase[i - 1];
    }
    phaseFunction[phaseFunctionSize - 1] = cosPhase[index + 1] + sinPhase[index];
  }

  /* CONVOLVE TMPINTERFEROGRAM WITH THE PHASE CORRECTION FUNCTION */
  offset = *dsHalfLength + (phaseFunctionSize >> 1) - 2;
  if(tmpSize < offset) { return 0; }

  size = *ssHalfLength + 1;
  interferogram = realloc(interferogram, size);
  tmp = *ssHalfLength + offset + 1;
  for(i = offset; i < tmp; i++)
  {
    tempVal = 0.0;
    for(j = 0; j < phaseFunctionSize; j++)
    {
      k = i - j;
      if(k < 0)
      {
    	  k += tmpSize;
      }

      if(k < tmpSize)
      {
    	  tempVal += (tmpInterferogram[k] * phaseFunction[j]);
      }
    }
    interferogram[i - offset] = tempVal / dsLength;
  }

  /* FREE RESOURCES */
  astFree(phase); phase = NULL;
  astFree(dsInterferogram); dsInterferogram = NULL;
  astFree(tmpInterferogram); tmpInterferogram = NULL;
  astFree(phaseFunction); phaseFunction = NULL;
  fftw_free(sinPhase); sinPhase = NULL;
  fftw_free(cosPhase); cosPhase = NULL;
  fftw_destroy_plan(planCosine); planCosine = NULL;
  fftw_destroy_plan(planSine); planSine = NULL;

  return 1;
}

