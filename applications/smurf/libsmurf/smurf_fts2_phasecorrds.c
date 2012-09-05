/*
*+
*  Name:
*     FTS2PHASECORRDS

*  Purpose:
*     Given a 3D data cube of double-sided interferograms, applies phase
*     correction and outputs the corresponding 3D spectrum cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_phasecorrds(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a 3D data cube of double-sided interferograms, applies phase
*     correction and outputs the corresponding 3D interferogram cube.
*     Although single-sided 3D interferogram cubes can be ingested, it is
*     recommended that the FTS2PHASECORR task is utilized to process them.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     DEGREE = _INTEGER (Read)
*          Order of the fitting polynomial.
*     WNLBOUND = _DOUBLE (Read)
*          The lower bound of the wavenumber range
*     WNUBOUND = _DOUBLE (Read)
*          The upper bound of the wavenumber range

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2011-08-03 (COBA):
*        New version.
*     2011-08-16 (COBA):
*        Add Zero-Padding.
*     2011-10-18 (COBA):
*        Updated to return phase corrected interferogram cube.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

// STANDARD includes
#include <string.h>
#include <stdio.h>

// STARLINK includes
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/grp.h"

// SMURF includes
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsc2fts/fts2.h"

// FFTW includes
#include <fftw3.h>

#define FUNC_NAME "smurf_fts2_phasecorrds"
#define TASK_NAME "FTS2PHASECORRDS"

void smurf_fts2_phasecorrds(int* status)
{
  if( *status != SAI__OK ) { return; }

  double const DTOR = AST__DPI / 180.0; // Degrees-to-Radians

  Grp* gIn          = NULL;     // Input group
  Grp* gOut         = NULL;     // Output group
  smfData* inData   = NULL;     // Pointer to input data
  smfData* outData  = NULL;     // Pointer to output data
  smfData* zpdData  = NULL;     // Pointer to ZPD data
  size_t nUsed      = 0;        // Number of used data points
  int pDegree       = 0;        // Degree of the polynomial used to fit phase
  int i             = 0;        // Counter
  int j             = 0;        // Counter
  int k             = 0;        // Counter
  double wnLower    = 0.0;      // Lower bound of wave number range
  double wnUpper    = 0.0;      // Upper bound of wave number range
  double fNyquist   = 0.0;      // Nyquist frequency
  double CLIP       = 0.0;      // Clipping param for the polynomial fit
  double maxWeight  = NUM__MIND;
  double* IFG       = NULL;     // Interferogram
  double* DS        = NULL;     // Double-Sided interferogram
  double* PHASE     = NULL;     // Phase
  double* WN        = NULL;     // Wavenumbers
  double* WEIGHTS   = NULL;     // Weighting factors
  double* FIT       = NULL;     // Fitted phase
  double* COEFFS    = NULL;     // Polynomial coefficients
  double* TMPPHASE  = NULL;     // Temporary phase
  fftw_complex* DSIN      = NULL; // Double-Sided interferogram, FFT input
  fftw_complex* DSOUT     = NULL; // Double-Sided interferogram, FFT output
  fftw_complex* PCF       = NULL; // Phase Correction Function
  fftw_complex* SPEC      = NULL; // Spectrum
  fftw_plan planA         = NULL; // fftw plan
  fftw_plan planB         = NULL; // fftw plan

  // Get Input & Output groups
  size_t nFiles = 0;
  kpg1Rgndf("IN", 0, 1, "", &gIn, &nFiles, status);
  size_t nOutFiles = 0;
  kpg1Wgndf("OUT", gOut, nFiles, nFiles, "Equal number of input and output files expected!", &gOut, &nOutFiles, status);

  // Read in ADAM parameters
  parGet0i("DEGREE",   &pDegree, status);
  parGet0d("WNLBOUND", &wnLower, status);
  parGet0d("WNUBOUND", &wnUpper, status);

  int coeffLength = pDegree + 1;

  // BEGIN NDF
  ndfBegin();

  // Loop through each input file
  size_t fIndex = 0;
  for(fIndex = 1; fIndex <= nFiles; fIndex++) {
    // Open Observation file
    smf_open_file(gIn, fIndex, "READ", 0, &inData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open the source file!", status);
      goto CLEANUP;
    }

    // Check if the file is initialized for FTS2 processing
    if(!(inData->fts) || !(inData->fts->zpd)) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "The file is NOT initialized for FTS2 data reduction!", status);
      goto CLEANUP;
    }
    zpdData = inData->fts->zpd;

    // Read in the Nyquist frequency from FITS component
    smf_fits_getD(inData->hdr, "FNYQUIST", &fNyquist, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to find the Nyquist frequency in FITS component!", status);
      goto CLEANUP;
    }

    // Data cube dimensions
    size_t nWidth  = inData->dims[0];
    size_t nHeight = inData->dims[1];
    size_t nFrames = inData->dims[2];
    size_t nFrames2= nFrames / 2;
    size_t nPixels = nWidth * nHeight;
    size_t wnL = (size_t) (nFrames2 * wnLower / fNyquist);
    size_t wnU = (size_t) (nFrames2 * wnUpper / fNyquist);

    double dSigma   = fNyquist / nFrames2;  // Spectral sampling interval

    // Copy input data into output data
    outData = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outData->dtype   = SMF__DOUBLE;
    outData->ndims   = 3;
    outData->dims[0] = nWidth;
    outData->dims[1] = nHeight;
    outData->dims[2] = nFrames;
    outData->pntr[0] = (double*) astMalloc((nPixels * nFrames) * sizeof(double));
    // MORE.FTS2.ZPD
    smfData* zpd = smf_deepcopy_smfData(inData->fts->zpd, 0, SMF__NOCREATE_FTS, 0, 0, status);
    // MORE.FTS2.FPM, Polynomial fit coefficients
    smfData* fpm = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = nWidth;
    fpm->dims[1] = nHeight;
    fpm->dims[2] = coeffLength;
    fpm->pntr[0] = (double*) astCalloc( (nPixels * coeffLength), sizeof(double));
    // MORE.FTS2.SIGMA, STANDARD DEVIATIONS
    smfData* sigma = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = nWidth;
    sigma->dims[1] = nHeight;
    sigma->pntr[0] = (double*) astCalloc(nPixels, sizeof(double));

    // Allocate memory for arrays
    IFG     = astCalloc(nFrames, sizeof(*IFG));
    DS      = astCalloc(nFrames, sizeof(*DS));
    PHASE   = astCalloc(nFrames, sizeof(*PHASE));
    COEFFS  = astCalloc(coeffLength, sizeof(*COEFFS));
    WN      = astCalloc((nFrames2 + 1), sizeof(*WN));
    WEIGHTS = astCalloc((nFrames2 + 1), sizeof(*WEIGHTS));
    FIT     = astCalloc((nFrames2 + 1), sizeof(*FIT));
    TMPPHASE= astCalloc((nFrames2 + 1), sizeof(*TMPPHASE));

    // Allocate memory for complex arrays
    DSIN    = fftw_malloc(nFrames * sizeof(*DSIN));
    DSOUT   = fftw_malloc(nFrames * sizeof(*DSOUT));
    PCF     = fftw_malloc(nFrames * sizeof(*PCF));
    SPEC    = fftw_malloc(nFrames * sizeof(*SPEC));

    // Apply phase correction to interferograms at each pixel
    for(i = 0; i < nWidth; i++) {
      for(j = 0; j < nHeight; j++) {
        int bolIndex = i + j * nWidth;

        // Get ZPD index
        int indexZPD = *((int*)(zpdData->pntr[0]) + bolIndex);

        // Check if the interferogram is inverted
        int W = 1;//(*((double*) (inData->pntr[0]) + (bolIndex + nPixels * indexZPD)) < 0.0) ? -1 : 1;

        int badPixel = 0;
        // Read in the interferogram, invert if flipped
        for(k = 0; k < nFrames; k++) {
          IFG[k] = W * (*((double*) (inData->pntr[0]) + (bolIndex + nPixels * k)));

          // See if this is a bad pixel
          if(IFG[k] == VAL__BADD) {
            badPixel = 1;
            break;
          }
        }
        // If this is a bad pixel, go to next
        if(badPixel) {
          for(k = 0; k < nFrames; k++) {
            int index = bolIndex + k * nPixels;
            *((double*) (outData->pntr[0]) + index) = VAL__BADD;
          }
          continue;
        }

        // Butterfly the interferogram
        for(k = indexZPD; k < nFrames; k++) { DS[k - indexZPD] = IFG[k]; }
        for(k = 0; k < indexZPD; k++)       { DS[nFrames - indexZPD + k] = IFG[k]; }

        // Convert real-valued interferogram to complex-valued interferogram
        for(k = 0; k < nFrames; k++) { DSIN[k][0] = DS[k]; DSIN[k][1] = 0.0; }

        // FFT Double-sided complex-valued interferogram
        planA = fftw_plan_dft_1d(nFrames, DSIN, DSOUT, FFTW_FORWARD, FFTW_ESTIMATE);
        fftw_execute(planA);

        // Compute phase
        for(k = 0; k < nFrames; k++) { PHASE[k] = atan2(DSOUT[k][1], DSOUT[k][0]); }

        // Compute wavenumbers within [0, FNYQ]
        for(k = 0; k <= nFrames2; k++) { WN[k] = k * dSigma; }

        // Compute weighting factors [0, FNYQ]
        maxWeight = NUM__MIND;
        for(k = 0; k <= nFrames2; k++) {
          if(k < wnL || k > wnU) { WEIGHTS[k] = 0.0; }
          else {
            WEIGHTS[k] = DSOUT[k][0] * DSOUT[k][0] + DSOUT[k][1] * DSOUT[k][1];
            WEIGHTS[k] = sqrt(WEIGHTS[k]);
            if(WEIGHTS[k] > maxWeight) { maxWeight = WEIGHTS[k]; }
          }
        }
        if(maxWeight <= 0) { maxWeight = 1; }
        for(k = 0; k <= nFrames2; k++) { WEIGHTS[k] /= maxWeight; }
        WEIGHTS[0] = WEIGHTS[nFrames2] = 0.0;

        // Polynimial fit to phase
        for(k = 0; k <= nFrames2; k++) { TMPPHASE[k] = PHASE[k]; }
        smf_fit_poly1d(pDegree, nFrames2 + 1, CLIP, WN, TMPPHASE, WEIGHTS, NULL, COEFFS, NULL, FIT, &nUsed, status);

        // Update MORE.FTS2.SIGMA values
        double sum   = 0.0;
        double error = 0.0;
        for(k = 0; k <= nFrames2; k++) {
          error += WEIGHTS[k] * (PHASE[k] - FIT[k]) * (PHASE[k] - FIT[k]);
          sum   += WEIGHTS[k];
        }
        *((double*)(sigma->pntr[0]) + bolIndex) = sqrt(error / sum);

        //Update MORE.FTS2.FPM values
        for(k = 0; k < coeffLength; k++) { *((double*) (fpm->pntr[0]) + (bolIndex + nPixels * k)) = COEFFS[k]; }

        // Polynomail Fit
        for(k = 0; k < nFrames2; k++) { EVALPOLY(PHASE[k], WN[k], pDegree, COEFFS); }
        for(k = 1; k < nFrames2; k++) { PHASE[nFrames2 + k] = -PHASE[nFrames2 - k]; } // PHASE(-k) = -PHASE(k)
        PHASE[0] = PHASE[nFrames2] = 0.0;
        for(k = 0; k < nFrames; k++) { PHASE[k] *= DTOR; }

        // Compute phase correction function, PCF, exp(-i * phase)
        for(k = 0; k < nFrames; k++) {
          PCF[k][0] =  cos(PHASE[k]);
          PCF[k][1] = -sin(PHASE[k]);
        }

        // Multiplication in frequency domain
        for(k = 0; k < nFrames; k++) {
          SPEC[k][0] = DSOUT[k][0] * PCF[k][0] - DSOUT[k][1] * PCF[k][1];
          SPEC[k][1] = DSOUT[k][0] * PCF[k][1] + DSOUT[k][1] * PCF[k][0];
        }

        // Inverse FFT spectrum to get the phase corrected interferogram
        planB = fftw_plan_dft_1d(nFrames, SPEC, SPEC, FFTW_BACKWARD, FFTW_ESTIMATE);
        fftw_execute(planB);

        // Phase corrected interferogram
        int M = indexZPD;
        for(k = 0; k < M; k++) { IFG[k] = SPEC[nFrames - M + k][0]; }
        for(k = M; k < nFrames; k++) { IFG[k] = SPEC[k - M][0]; }

        // Update output
        for(k = 0; k < nFrames; k++) {
          int index = bolIndex + nPixels * k;
          *((double*)(outData->pntr[0]) + index) = IFG[k] / nFrames;
        }
      }
    }

    // Deallocate memory used by arrays
    if(IFG)      { IFG      = astFree(IFG); }
    if(DS)       { DS       = astFree(DS); }
    if(PHASE)    { PHASE    = astFree(PHASE); }
    if(COEFFS)   { COEFFS   = astFree(COEFFS); }
    if(TMPPHASE) { TMPPHASE = astFree(TMPPHASE); }
    if(WN)       { WN       = astFree(WN); }
    if(WEIGHTS)  { WEIGHTS  = astFree(WEIGHTS); }
    if(FIT)      { FIT      = astFree(FIT); }

    // Deallocate memory used by complex arrays
    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
    if(PCF)      { fftw_free(PCF);            PCF       = NULL; }
    if(SPEC)     { fftw_free(SPEC);           SPEC      = NULL; }

    // Close the file
    if(inData) { smf_close_file(&inData, status); }

    // Write output
    outData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, status);
    smf_write_smfData(outData, NULL, NULL, gOut, fIndex, 0, MSG__VERB, status);
    smf_close_file(&outData, status);
  }

  CLEANUP:
  // Deallocate memory used by arrays
  if(IFG)      { IFG      = astFree(IFG); }
  if(DS)       { DS       = astFree(DS); }
  if(PHASE)    { PHASE    = astFree(PHASE); }
  if(COEFFS)   { COEFFS   = astFree(COEFFS); }
  if(TMPPHASE) { TMPPHASE = astFree(TMPPHASE); }
  if(WN)       { WN       = astFree(WN); }
  if(WEIGHTS)  { WEIGHTS  = astFree(WEIGHTS); }
  if(FIT)      { FIT      = astFree(FIT); }

  // Deallocate memory used by complex arrays
  if(DSIN)     { fftw_free(DSIN);   DSIN  = NULL; }
  if(DSOUT)    { fftw_free(DSOUT);  DSOUT = NULL; }
  if(PCF)      { fftw_free(PCF);    PCF   = NULL; }
  if(SPEC)     { fftw_free(SPEC);   SPEC  = NULL; }

  // Close files if still open
  if(inData) { smf_close_file(&inData, status); }

  // END NDF
  ndfEnd(status);

  // Delete Groups
  grpDelet(&gIn, status);
  grpDelet(&gOut, status);
}