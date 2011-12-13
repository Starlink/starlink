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

/* STANDARD INCLUDES */
#include <string.h>
#include <stdio.h>

/* STARLINK INCLUDES */
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/grp.h"

/* SMURF INCLUDES */
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsc2fts/fts2.h"

/* FFTW INCLUDES */
#include <fftw3.h>

#define FUNC_NAME "smurf_fts2_phasecorrds"
#define TASK_NAME "FTS2PHASECORRDS"

void smurf_fts2_phasecorrds(int* status)
{
  if(*status != SAI__OK) { return; }

  // ADAM VARIABLES
  int       PDEGREE       = 0;    // Degree of the polynomial used to fit phase
  double    WNLOWER       = 0.0;  // Lower bound of wave number range
  double    WNUPPER       = 0.0;  // Upper bound of wave number range

  // INTERNAL VARIABLES
  Grp*      grpInput      = NULL; // Input group
  Grp*      grpOutput     = NULL; // Output group
  smfData*  inputData     = NULL; // Pointer to input data
  smfData*  zpdData       = NULL; // Pointer to ZPD data
  size_t    fileIndex     = 0;    // File loop counter
  size_t    numInputFile  = 0;    // Size of the input group
  size_t    numOutputFile = 0;    // Size of the output group
  size_t    nUsed         = 0;    // Number of used data points
  int       srcH          = 0;    // Subarray Height
  int       srcW          = 0;    // Subarray Width
  int       N             = 0;    // Source interferogram length
  int       numBol        = 0;    // Number of bolometers in the subarray
  int       coeffLength   = 0;    // Length of the COEFFS array
  int       wnL           = 0;    // Index for the lower limit of the band
  int       wnU           = 0;    // Index for the upper limit of the band
  int       i             = 0;    // Row index
  int       j             = 0;    // Column index
  int       k             = 0;    // Frame index
  int       n             = 0;    // Helper index
  int       index         = 0;    // Helper index
  int       bolIndex      = 0;    // Bolometer index
  int       zpdIndex      = 0;    // Frame index at ZPD
  double    CLIP          = 0.0;  // Clipping param for the polynomial fit
  double*   IFG           = NULL; // Source interferogram
  double*   DS            = NULL; // Double-Sided interferogram
  double*   PHASE         = NULL; // Phase
	double*   POS           = NULL; // FTS-2 mirror positions
  double*   COEFFS        = NULL; // Polynomial coefficients
	double*   WEIGHTS       = NULL; // Weighting factors
	double*   WN            = NULL; // Wavenumbers
  double*   TMPPHASE      = NULL; // Temporary phase
	double*   FIT           = NULL; // Fitted phase
  fftw_complex* DSIN      = NULL; // Double-Sided interferogram, FFT input
  fftw_complex* DSOUT     = NULL; // Double-Sided interferogram, FFT output
  fftw_complex* PCF       = NULL; // Phase Correction Function
  fftw_complex* SPEC      = NULL; // Spectrum
  fftw_plan planA			    = NULL; // fftw plan
  fftw_plan planB			    = NULL; // fftw plan

  double const DTOR       = AST__DPI / 180.0;

  // BEGIN NDF
  ndfBegin();

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &numInputFile, status);
  kpg1Wgndf("OUT", grpOutput, numInputFile, numInputFile,
            "Equal number of input and output files expected!",
            &grpOutput, &numOutputFile, status);

  // READ IN ADAM PARAMETERS
  parGet0i("DEGREE",   &PDEGREE, status);
  parGet0d("WNLBOUND", &WNLOWER, status);
  parGet0d("WNUBOUND", &WNUPPER, status);

  // SET ARRAY LENGTHS
  coeffLength = PDEGREE + 1;

  // BEGIN NDF
  ndfBegin();

  // ===========================================================================
  // LOOP THROUGH EACH INPUT FILE
  // ===========================================================================
  for(fileIndex = 1; fileIndex <= numInputFile; fileIndex++) {
    // OPEN FILE
    smf_open_file(grpInput, fileIndex, "READ", 0, &inputData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      goto CLEANUP;
    }

    // CHECK FOR ZPD
    if(!(inputData->fts) || !(inputData->fts->zpd)) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "The file is NOT initialized for FTS2 data reduction!", status);
      goto CLEANUP;
    }
    zpdData = inputData->fts->zpd;

    // SOURCE DATA DIMENSIONS
    srcW   = inputData->dims[0];
    srcH   = inputData->dims[1];
    N      = inputData->dims[2];
    int N2 = N / 2;
    numBol = srcW * srcH;  // NUMBER OF BOLOMETERS IN THE SUBARRAY

    // GET SOURCE MIRROR POSITIONS
    POS     = astMalloc(N * sizeof(*POS));
    int num = 0;
    fts2_getmirrorpositions(inputData, POS, &num, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to get the source mirror positions!", status);
      goto CLEANUP;
    }
    double FNYQUIST = 1.0 / (8.0 * (POS[1] - POS[0]));
    double DSIGMA   = FNYQUIST / N2;
    smf_fits_updateD(inputData->hdr, "FNYQUIST", FNYQUIST, "Nyquist Frequency", status);
    if(POS) { astFree(POS); POS = NULL; }

    // WAVENUMBER INDICES
    wnL = (int) (N2 * WNLOWER / FNYQUIST);
    wnU = (int) (N2 * WNUPPER / FNYQUIST);

    // READY OUTPUT FILE
    // =========================================================================
    smfData* outputData = smf_deepcopy_smfData( inputData, 0,
                                                SMF__NOCREATE_DATA |
                                                SMF__NOCREATE_FTS, 0, 0,
                                                status);
    // DATA ARRAY
    outputData->dtype   = SMF__DOUBLE;
    outputData->ndims   = 3;
    outputData->dims[0] = srcW;
    outputData->dims[1] = srcH;
    outputData->dims[2] = N;
    outputData->pntr[0] = (double*) astMalloc((numBol * N) * sizeof(double));
    // MORE.FTS2.ZPD, ZPD INDICES
    smfData* zpd = smf_deepcopy_smfData(inputData->fts->zpd, 0, SMF__NOCREATE_FTS, 0, 0, status);
    // MORE.FTS2.FPM, POLYNOM COEFFICIENTS THAT FITS THE PHASE
    smfData* fpm = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = srcW;
    fpm->dims[1] = srcH;
    fpm->dims[2] = coeffLength;
    fpm->pntr[0] = (double*) astCalloc( (numBol * coeffLength), sizeof(double));
    // MORE.FTS2.SIGMA, STANDARD DEVIATIONS
    smfData* sigma = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = srcW;
    sigma->dims[1] = srcH;
    sigma->pntr[0] = (double*) astCalloc(numBol, sizeof(double));
    // =========================================================================

    // LOOP THROUGH EACH PIXEL IN THE SUBARRAY
    // =========================================================================
    for(i = 0; i < srcH; i++) { // LOOP THROUGH ROWS
      for(j = 0; j < srcW; j++) { // LOOP THROUGH COLUMNS
        bolIndex = i + j * srcH;

        // GET ZPD INDEX
        zpdIndex = *((int*)(zpdData->pntr[0]) + bolIndex);  // 1-Based index
        zpdIndex -= 1; // 0-Based index

        // ### ALLOCATE MEMORY
        IFG       = astMalloc(N               * sizeof(*IFG));
        DS        = astMalloc(N               * sizeof(*DS));
        PHASE     = astMalloc(N               * sizeof(*PHASE));
        COEFFS    = astMalloc(coeffLength     * sizeof(*COEFFS));
        WN        = astMalloc((N2 + 1)        * sizeof(*WN));
        WEIGHTS   = astMalloc((N2 + 1)        * sizeof(*WEIGHTS));
        FIT       = astMalloc((N2 + 1)        * sizeof(*FIT));
        TMPPHASE  = astMalloc((N2 + 1)        * sizeof(*TMPPHASE));
	      DSIN      = fftw_malloc(N             * sizeof(*DSIN));
	      DSOUT     = fftw_malloc(N             * sizeof(*DSOUT));
        PCF       = fftw_malloc(N             * sizeof(*PCF));
        SPEC      = fftw_malloc(N             * sizeof(*SPEC));

        // ### INGEST INTERFEROGRAM FOR THE CURRENT PIXEL
        for(k = 0; k < N; k++) {
          IFG[k] = *((double*)(inputData->pntr[0]) + (bolIndex + numBol * k));
        }

        // ### BUTTERFLY THE INTERFEROGRAM ABOUT THE ZPD
        for(k = zpdIndex; k < N; k++) { DS[k - zpdIndex] = IFG[k]; }
        for(k = 0; k < zpdIndex; k++) { DS[N - zpdIndex + k] = IFG[k]; }

        // COMPLEX DOUBLE-SIDED BUTTERFLIED INTERFEROGRAM
        for(k = 0; k < N; k++) { DSIN[k][0] = DS[k]; DSIN[k][1] = 0.0; }

        // ### FORWARD FFT DOUBLE-SIDED INTERFEROGRAM
	      planA = fftw_plan_dft_1d(N, DSIN, DSOUT, FFTW_FORWARD, FFTW_ESTIMATE);
	      fftw_execute(planA);

        // ### PHASE
	      for(k = 0; k < N; k++) { PHASE[k] = atan2(DSOUT[k][1], DSOUT[k][0]); }

        // ### WAVENUMBERS [0, FNYQ]
        for(k = 0; k <= N2; k++) { WN[k] = k * DSIGMA; }

        // ### WEIGHTS [0, FNYQ]
	      double maxWeight = NUM__MIND;
        for(k = 0; k <= N2; k++) {
          if(k < wnL || k > wnU) { WEIGHTS[k] = 0.0; }
          else {
            WEIGHTS[k] = DSOUT[k][0] * DSOUT[k][0] + DSOUT[k][1] * DSOUT[k][1];
            WEIGHTS[k] = sqrt(WEIGHTS[k]);
            if(WEIGHTS[k] > maxWeight) { maxWeight = WEIGHTS[k]; }
          }
        }
        // NORMALIZE WEIGHTS
	      if(maxWeight <= 0) { maxWeight = 1; }
	      for(k = 0; k <= N2; k++) { WEIGHTS[k] /= maxWeight; }
	      WEIGHTS[0] = WEIGHTS[N2] = 0.0;

	      // ### PHASE FIT
	      for(k = 0; k <= N2; k++) { TMPPHASE[k] = PHASE[k]; }
        smf_fit_poly1d(PDEGREE, N2 + 1, CLIP, WN, TMPPHASE, WEIGHTS, NULL, COEFFS, NULL, FIT, &nUsed, status);
        // UPDATE MORE.FTS2 IN THE OUTPUT FILE
        // MORE.FTS2.SIGMA, STANDARD DEVIATION
        double sum   = 0.0;
        double error = 0.0;
        for(k = 0; k <= N2; k++) {
          error += WEIGHTS[k] * (PHASE[k] - FIT[k]) * (PHASE[k] - FIT[k]);
          sum   += WEIGHTS[k];
        }
        *((double*)(sigma->pntr[0]) + bolIndex) = sqrt(error / sum);
        // MORE.FTS2.FPM, POLYNOMIAL COEFFICIENTS
        for(k = 0; k < coeffLength; k++) { *((double*) (fpm->pntr[0]) + (bolIndex + numBol * k)) = COEFFS[k]; }
        for(k = 0; k < N2; k++) { EVALPOLY(PHASE[k], WN[k], PDEGREE, COEFFS); }
        for(k = 1; k < N2; k++) { PHASE[N2 + k] = -PHASE[N2 - k]; } // PHASE(-k) = -PHASE(k)
        PHASE[0] = PHASE[N2] = 0.0;
        for(k = 0; k < N; k++) { PHASE[k] *= DTOR; }

	      // ### PHASE CORRECTION FUNCTION, PCF, EXP(-iPHASE)
	      for(k = 0; k < N; k++) {
	        PCF[k][0] =  cos(PHASE[k]);
	        PCF[k][1] = -sin(PHASE[k]);
	      }

	      // ### MULTIPLICATION IN FREQUENCY DOMAIN
	      for(k = 0; k < N; k++) {
	        SPEC[k][0] = DSOUT[k][0] * PCF[k][0] - DSOUT[k][1] * PCF[k][1];
	        SPEC[k][1] = DSOUT[k][0] * PCF[k][1] + DSOUT[k][1] * PCF[k][0];
	      }

	      // ### INVERSE FFT SPECTRUM TO GET THE PHASE CORRECTED INTERFEROGRAM
	      planB = fftw_plan_dft_1d(N, SPEC, SPEC, FFTW_BACKWARD, FFTW_ESTIMATE);
	      fftw_execute(planB);
        int M = zpdIndex + 1;
        for(k = 0; k < M; k++) { IFG[k] = SPEC[N - M + k][0]; }
        for(k = M; k < N; k++) { IFG[k] = SPEC[k - M][0]; }

        // ### WRITE PHASE CORRECTED INTERFEROGRAM TO OUTPUT FILE
        for(k = 0; k < N; k++) {
          index = bolIndex + numBol * k;
          *((double*)(outputData->pntr[0]) + index) = IFG[k] / N;
        }

        // ### FREE RESOURCES
        if(IFG)      { astFree(IFG);              IFG       = NULL; }
        if(DS)       { astFree(DS);               DS        = NULL; }
        if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
        if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
        if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
        if(WN)       { astFree(WN);               WN        = NULL; }
        if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
        if(FIT)      { astFree(FIT);              FIT       = NULL; }

        if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
        if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
	      if(PCF)      { fftw_free(PCF);            PCF       = NULL; }
	      if(SPEC)     { fftw_free(SPEC);           SPEC      = NULL; }
      } // END LOOP THROUGH COLUMNS
    } // END LOOP THROUGH ROWS

    // CLOSE FILE
    if(inputData) { smf_close_file(&inputData, status); }

    // WRITE OUTPUT
    outputData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, status);
    smf_write_smfData(outputData, NULL, NULL, grpOutput, fileIndex, 0, MSG__VERB, status);
    smf_close_file(&outputData, status);
  }  // END FILE LOOP

  CLEANUP:
    // FREE RESOURCES
    if(POS)      { astFree(POS);              POS       = NULL; }
    if(IFG)      { astFree(IFG);              IFG       = NULL; }
    if(DS)       { astFree(DS);               DS        = NULL; }
    if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
    if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
    if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
    if(WN)       { astFree(WN);               WN        = NULL; }
    if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
    if(FIT)      { astFree(FIT);              FIT       = NULL; }

    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
	  if(PCF)      { fftw_free(PCF);            PCF       = NULL; }
	  if(SPEC)     { fftw_free(SPEC);           SPEC      = NULL; }
	  if(planA)    { fftw_destroy_plan(planA);  planA     = NULL; }
	  if(planB)    { fftw_destroy_plan(planB);  planB     = NULL; }
	  fftw_cleanup();

    // CLOSE FILE
    if(inputData) { smf_close_file(&inputData, status); }

    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    grpDelet(&grpInput, status);
    grpDelet(&grpOutput, status);
}
