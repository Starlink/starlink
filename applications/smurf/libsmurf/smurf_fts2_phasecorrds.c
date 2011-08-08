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
*     correction and outputs the corresponding 3D spectrum cube.
*     Although single-sided 3D interferogram cubes can be ingested, it is
*     recommended that the FTS2PHASECORRSS task is utilized to process them.
*     IMPORTANT NOTE: Unlike FTS2PHASECORRSS which outputs the 3D cube of phase
*     corrected interferograms, FTS2PHASECORRDS does NOT output the phase
*     corrected interferograms, instead, it phase corrects the interferograms
*     within and computes their sectrum. Hence outputs a 3D spectrum cube.

*  ADAM Parameters:
*     DEGREE = _INTEGER (Read)
*          Order of the fitting polynomial.
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     WNLBOUND = _DOUBLE (Read)
*          The lower bound of the wavenumber range
*     WNUBOUND = _DOUBLE (Read)
*          The upper bound of the wavenumber range

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2011-08-03 (COBA):
*        New version.

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
  int       pDegree       = 0;    // Degree of the polynomial used to fit phase
  double    wnLower       = 0.0;  // Lower bound of wave number range
  double    wnUpper       = 0.0;  // Upper bound of wave number range

  // INTERNAL VARIABLES
  Grp*      grpInput      = NULL; // Input group
  Grp*      grpOutput     = NULL; // Output group
  smfData*  inputData     = NULL; // Pointer to input data
  smfData*  zpdData       = NULL; // Pointer to ZPD data
  size_t    fileIndex     = 0;    // File loop counter
  size_t    numInputFile  = 0;    // Size of the input group
  size_t    numOutputFile = 0;    // Size of the output group
  size_t    nUsed         = 0;    // Number of used data points
  int       H             = 0;    // Subarray Height
  int       W             = 0;    // Subarray Width
  int       N             = 0;    // Source interferogram length
  int       numBol        = 0;    // Number of bolometers in the subarray
  int       wnL           = 0;    // Index for the lower limit of the band
  int       wnU           = 0;    // Index for the upper limit of the band
  int       coeffLength   = 0;    // Length of the COEFFS array
  int       phaseLength   = 0;    // Phase length
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
  fftw_plan planA			    = NULL; // fftw plan
  fftw_plan planB			    = NULL; // fftw plan
  fftw_complex* DSIN      = NULL; // Double-Sided interferogram, FFT input
  fftw_complex* DSOUT     = NULL; // Double-Sided interferogram, FFT output
  fftw_complex* PCF       = NULL; // Phase Correction Function (exp(-iPhase))
  fftw_complex* SPEC      = NULL; // Spectrum

  // BEGIN NDF
  ndfBegin();

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &numInputFile, status);
  kpg1Wgndf("OUT", grpOutput, numInputFile, numInputFile,
            "Equal number of input and output files expected!",
            &grpOutput, &numOutputFile, status);

  // READ IN ADAM PARAMETERS
  parGet0i("DEGREE",   &pDegree, status);
  parGet0d("WNLBOUND", &wnLower, status);
  parGet0d("WNUBOUND", &wnUpper, status);

  coeffLength = pDegree + 1;

  // ===========================================================================
  // LOOP THROUGH EACH INPUT FILE
  // ===========================================================================
  for(fileIndex = 1; fileIndex <= numInputFile; fileIndex++) {
    smf_open_file(grpInput, fileIndex, "READ", 0, &inputData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      goto CLEANUP;
    }

    // CHECK FOR ZPD
    if(!(inputData->fts) || !(inputData->fts->zpd)) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "The file is NOT initialized for FTS2 data reduction!",
              status);
      goto CLEANUP;
    }
    zpdData = inputData->fts->zpd;

    // SOURCE DATA DIMENSIONS
    W = inputData->dims[0];
    H = inputData->dims[1];
    N = inputData->dims[2];
    numBol = W * H;  // NUMBER OF BOLOMETERS IN THE SUBARRAY

    int N2 = N >> 1;

    // GET SOURCE MIRROR POSITIONS
    int num = 0;
    POS = astMalloc(N * sizeof(*POS));
    fts2_getmirrorpositions(inputData, POS, &num, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to get the source mirror positions!", status);
      goto CLEANUP;
    }
    double dz = 4.0 * (POS[1] - POS[0]);  // Sampling Interval in OPD
    double fs = 1.0 / dz;                 // Sampling Frequency
    double fnyq = fs / 2.0;               // Nyquist Frequency
    double df = fnyq / N2;
    smf_fits_updateD(inputData->hdr, "FNYQUIST", fnyq, "Nyquist Frequency", status);
    smf_fits_updateD(inputData->hdr, "WNFACT", df, "Wavenumber factor", status);
    if(POS) { astFree(POS); POS = NULL; }

    // WAVENUMBER INDICES
    wnL = (int) (N2 * wnLower / fnyq);
    wnU = (int) (N2 * wnUpper / fnyq);

    // OUTPUT SMFDATA
    smfData* outputData = smf_deepcopy_smfData( inputData, 0,
                                                SMF__NOCREATE_DATA |
                                                SMF__NOCREATE_FTS, 0, 0,
                                                status);
    // DATA ARRAY
    outputData->dtype   = SMF__DOUBLE;
    outputData->ndims   = 3;
    outputData->dims[0] = W;
    outputData->dims[1] = H;
    outputData->dims[2] = (N2 + 1);
    outputData->pntr[0] = (double*) astMalloc((numBol * (N2 + 1)) * sizeof(double));

    // MORE.FTS2.ZPD, ZPD INDICES
    smfData* zpd = smf_deepcopy_smfData(inputData->fts->zpd, 0, SMF__NOCREATE_FTS, 0, 0, status);

    // MORE.FTS2.FPM, POLYNOM COEFFICIENTS THAT FITS THE PHASE
    smfData* fpm = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = W;
    fpm->dims[1] = H;
    fpm->dims[2] = coeffLength;
    fpm->pntr[0] = (double*) astCalloc( (numBol * coeffLength), sizeof(double));

    // MORE.FTS2.SIGMA, STANDARD DEVIATIONS
    smfData* sigma = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = W;
    sigma->dims[1] = H;
    sigma->pntr[0] = (double*) astCalloc(numBol, sizeof(double));

    // ALLOCATE MEMORY FOR ARRAYS
    IFG      = astMalloc(N           * sizeof(*IFG));
    DS       = astMalloc(N           * sizeof(*DS));
    PHASE    = astMalloc(N           * sizeof(*PHASE));
    COEFFS   = astMalloc(coeffLength * sizeof(*COEFFS));
    TMPPHASE = astMalloc((N2 + 1)    * sizeof(*TMPPHASE));
    WN       = astMalloc((N2 + 1)    * sizeof(*WN));
    WEIGHTS  = astMalloc((N2 + 1)    * sizeof(*WEIGHTS));
    FIT      = astMalloc((N2 + 1)    * sizeof(*FIT));
	  DSIN     = fftw_malloc(N         * sizeof(*DSIN));
	  DSOUT    = fftw_malloc(N         * sizeof(*DSOUT));
    PCF      = fftw_malloc(N         * sizeof(*PCF));
    SPEC     = fftw_malloc(N         * sizeof(*SPEC));
    // =========================================================================
    // LOOP THROUGH EACH PIXEL IN THE SUBARRAY
    // =========================================================================
    for(i = 0; i < H; i++) { // LOOP THROUGH ROWS
      for(j = 0; j < W; j++) { // LOOP THROUGH COLUMNS
        bolIndex = i + j * H;

        // GET ZPD INDEX
        zpdIndex = *((int*)(zpdData->pntr[0]) + bolIndex); // 1-Based index
	      zpdIndex -= 1;  // 0-Based index

        // SOURCE INTERFEROGRAM
        for(k = 0; k < N; k++) {
          index = bolIndex + numBol * k;
          IFG[k] = *((double*)(inputData->pntr[0]) + index);
        }
        // PRINT
        // ---------------------------------------------------------------------
        if(i == 0 && j == 0) {
          FILE* file = NULL;
	        file = fopen("/home/oba/jach/TEST/INPUT.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", IFG[k]); }
	        fclose(file);
	      }
	      // ---------------------------------------------------------------------

        // BUTTERFLIED INTERFEROGRAM FOR FFT
        int M = N - zpdIndex;
        for(k = 0; k < M; k++) { DS[k] = IFG[zpdIndex + k]; }
        for(k = 0; k < zpdIndex; k++) { DS[M + k] = IFG[k]; }
        // PRINT
        // ---------------------------------------------------------------------
        if(i == 0 && j == 0) {
          FILE* file = NULL;
	        file = fopen("/home/oba/jach/TEST/INPUT_FFT.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", DS[k]); }
	        fclose(file);
	      }
	      // ---------------------------------------------------------------------

        // TO COMPLEX FOR FFT, FOR NOW C2C FFT
        for(k = 0; k < N; k++) {
          DSIN[k][0] = DS[k];
          DSIN[k][1] = 0.0;
        }

        // FORWARD FFT DOUBLE-SIDED INTERFEROGRAM
	      planA = fftw_plan_dft_1d(N, DSIN, DSOUT, FFTW_FORWARD, FFTW_ESTIMATE);
	      fftw_execute(planA);
        // PRINT
        // ---------------------------------------------------------------------
        if(i == 0 && j == 0) {
          FILE* file = NULL;
	        file = fopen("/home/oba/jach/TEST/OUTPUT_FFT_RE.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", DSOUT[k][0]); }
	        fclose(file);
          file = NULL;
	        file = fopen("/home/oba/jach/TEST/OUTPUT_FFT_IM.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", DSOUT[k][1]); }
	        fclose(file);
	      }
	      // ---------------------------------------------------------------------

        // PHASE
	      for(k = 0; k < N; k++) { PHASE[k] = atan2(DSOUT[k][1], DSOUT[k][0]); }
        // PRINT
        // ---------------------------------------------------------------------
        if(i == 0 && j == 0) {
          FILE* file = NULL;
	        file = fopen("/home/oba/jach/TEST/PHASE.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", PHASE[k]); }
	        fclose(file);
	      }
	      // ---------------------------------------------------------------------

        // WAVENUMBERS [0, FNYQ]
        for(k = 0; k <= N2; k++) { WN[k] = k * df; }

        // COMPUTE WEIGHTS [0, FNYQ]
	      double maxWeight = NUM__MIND;
        for(k = 0; k <= N2; k++) {
          if(k < wnL || k > wnU) { WEIGHTS[k] = 0.0; }
          else {
            WEIGHTS[k] = DSOUT[k][0] * DSOUT[k][0] + DSOUT[k][1] * DSOUT[k][1];
            WEIGHTS[k] = sqrt(WEIGHTS[k]);
            if(WEIGHTS[k] > maxWeight) {
              maxWeight = WEIGHTS[k];
            }
          }
        }
        // NORMALIZE WEIGHTS
	      if(maxWeight <= 0) { maxWeight = 1; }
	      for(k = 0; k <= N2; k++) { WEIGHTS[k] /= maxWeight; }
	      WEIGHTS[0] = WEIGHTS[N2] = 0.0;

        // PHASE [0, FNYQ]
	      for(k = 0; k <= N2; k++) { TMPPHASE[k] = PHASE[k]; }

	      // POLYNOMIAL FIT
        smf_fit_poly1d(pDegree, N2 + 1, CLIP, WN, TMPPHASE, WEIGHTS,
                       NULL, COEFFS, NULL, FIT, &nUsed, status);

        // STANDARD DEVIATION, MORE.FTS2.SIGMA
        double sum   = 0.0;
        double error = 0.0;
        for(k = 0; k <= N2; k++) {
          error += WEIGHTS[k] * (PHASE[k] - FIT[k]) * (PHASE[k] - FIT[k]);
          sum   += WEIGHTS[k];
        }
        *((double*)(sigma->pntr[0]) + bolIndex) = sqrt(error / sum);

        // POLYNOMIAL COEFFICIENTS, MORE.FTS2.FPM
        for(k = 0; k < coeffLength; k++) {
          index = bolIndex + numBol * k;
          *((double*) (fpm->pntr[0]) + index) = COEFFS[k];
        }

        // POLYNOMIAL FITTED PHASE
        int p = 1;
        for(k = 1; k <= N2; k++) { EVALPOLY(PHASE[k], WN[k], pDegree, COEFFS); }
        for(k = N2 + 1; k < N; k++) { PHASE[k] = -PHASE[N2 - p++]; } // P(-k) = -P(k)
        PHASE[0] = PHASE[N - 1] = 0.0;
        // PRINT
        // ---------------------------------------------------------------------
        if(i == 0 && j == 0) {
          FILE* file = NULL;
	        file = fopen("/home/oba/jach/TEST/PHASE_FIT.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", PHASE[k]); }
	        fclose(file);
	      }
	      // ---------------------------------------------------------------------


	      // PHASE CORRECTION FUNCTION, PCF, EXP(-iPHASE)
	      for(k = 0; k < N; k++) {
	        PCF[k][0] =  cos(PHASE[k]);
	        PCF[k][1] = -sin(PHASE[k]);
	      }
        // PRINT
        // ---------------------------------------------------------------------
        if(i == 0 && j == 0) {
          FILE* file = NULL;
	        file = fopen("/home/oba/jach/TEST/PCF_RE.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", PCF[k][0]); }
	        fclose(file);
          file = NULL;
	        file = fopen("/home/oba/jach/TEST/PCF_IM.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", PCF[k][1]); }
	        fclose(file);
	      }
	      // ---------------------------------------------------------------------

	      // MULTIPLICATION IN FREQUENCY DOMAIN
	      for(k = 0; k < N; k++) {
	        SPEC[k][0] = DSOUT[k][0] * PCF[k][0] - DSOUT[k][1] * PCF[k][1];
	        SPEC[k][1] = DSOUT[k][0] * PCF[k][1] + DSOUT[k][1] * PCF[k][0];
	      }
        // PRINT
        // ---------------------------------------------------------------------
        if(i == 0 && j == 0) {
          FILE* file = NULL;
	        file = fopen("/home/oba/jach/TEST/SPEC_RE.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", SPEC[k][0]); }
	        fclose(file);
          file = NULL;
	        file = fopen("/home/oba/jach/TEST/SPEC_IM.txt", "w");
	        for(k = 0; k < N; k++) { fprintf(file, "%e\n", SPEC[k][1]); }
	        fclose(file);
	      }
	      // ---------------------------------------------------------------------

        // WRITE OUT REAL COMPONENT OF THE SPECTRUM
        for(k = 0; k <= N2; k++) {
          index = bolIndex + numBol * k;
          *((double*)(outputData->pntr[0]) + index) = SPEC[k][0];
        }
      } // END FOR LOOP - COLUMNS
    } // END FOR LOOP - ROWS

    // CLOSE FILE
    if(inputData) { smf_close_file(&inputData, status); }

    // FREE RESOURCES
    if(IFG)      { astFree(IFG);              IFG       = NULL; }
    if(DS)       { astFree(DS);               DS        = NULL; }
    if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
    if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
    if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
    if(WN)       { astFree(WN);               WN        = NULL; }
    if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
    if(FIT)      { astFree(FIT);              FIT       = NULL; }
    if(POS)      { astFree(POS);              POS       = NULL; }
    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
    if(SPEC)     { fftw_free(SPEC);           SPEC      = NULL; }

	  if(planA)    { fftw_destroy_plan(planA);  planA     = NULL; }
	  if(planB)    { fftw_destroy_plan(planB);  planB     = NULL; }

    // WRITE OUTPUT
    outputData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, status);
    smf_write_smfData(outputData, NULL, NULL, grpOutput, fileIndex, 0,
                      MSG__VERB, status);
    smf_close_file(&outputData, status);
  } // END FOR LOOP - FILES

  CLEANUP:
    // FREE RESOURCES
    if(IFG)      { astFree(IFG);              IFG       = NULL; }
    if(DS)       { astFree(DS);               DS        = NULL; }
    if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
    if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
    if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
    if(WN)       { astFree(WN);               WN        = NULL; }
    if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
    if(FIT)      { astFree(FIT);              FIT       = NULL; }
    if(POS)      { astFree(POS);              POS       = NULL; }
    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
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
