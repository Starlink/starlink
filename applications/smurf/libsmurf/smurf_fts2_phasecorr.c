/*
*+
*  Name:
*     FTS2PHASECORR

*  Purpose:
*     Given a 3D data cube of single-sided interferograms, applies phase
*     correction and outputs the corresponding 3D interferogram cube.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_phasecorr(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Given a 3D data cube of single-sided interferograms, applies phase
*     correction and outputs the corresponding 3D interferogram cube.
*     Although double-sided 3D interferogram cubes can be ingested, it is
*     recommended that the FTS2PHASECORRDS task is utilized to process them.

*  ADAM Parameters:
*     DEGREE = _INTEGER (Read)
*          Order of the fitting polynomial.
*     DSHALFLENGTH = _INTEGER (Read)
*          Double-Sided Interferogram half-length.
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     PFHALFLENGTH = _INTEGER (Read)
*          Phase Correction Function half-length.
*     WNLBOUND = _DOUBLE (Read)
*          The lower bound of the wavenumber range
*     WNUBOUND = _DOUBLE (Read)
*          The upper bound of the wavenumber range
*     APODIZATION
*          Apodization Method

*  Authors:
*     COBA: Coskun Oba (UoL)
*     MS: Matt Sherwood (UofL)

*  History :
*     2011-06-20 (COBA):
*        New version.
*     2011-10-18 (COBA):
*        Updated the algorithm. Includes apodization of the phase correction
*        function
*     2013-11-25 (MS)
*         Include RTS timing values.
*     2015-02-20 (MS):
*        Added new smfFts fields for quality statistics

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

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

#if HAVE_CONFIG_H
#include <config.h>
#endif

/* STANDARD INCLUDES */
#include <string.h>
#include <stdio.h>
#include <stdint.h>

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

#define FUNC_NAME "smurf_fts2_phasecorr"
#define TASK_NAME "FTS2PHASECORR"

void smurf_fts2_phasecorr(int* status)
{
  if(*status != SAI__OK) { return; }

  // ADAM VARIABLES
  int       PDEGREE       = 0;    // Degree of the polynomial used to fit phase
  int       DSLENGTH2     = 0;    // Half-Length of double-sided interferogram
  int       PCFLENGTH2    = 0;    // Half-Length of phase correction function
  double    WNLOWER       = 0.0;  // Lower limit of the band, wavenumber
  double    WNUPPER       = 0.0;  // Upper limit of the band, wavenumber
  int       APODIZATION   = -1;   // SMF__FTS2_APODIZATION_NONE  = -1

  // INTERNAL VARIABLES
  Grp*      grpInput      = NULL; // Input group
  Grp*      grpOutput     = NULL; // Output group
  smfData*  inputData     = NULL; // Pointer to input data
  smfData*  zpdData       = NULL; // Pointer to ZPD data
  size_t    fileIndex     = 0;    // File loop counter
  size_t    numInputFile  = 0;    // Size of the input group
  size_t    numOutputFile = 0;    // Size of the output group
  int64_t   nUsed         = 0;    // Number of used data points
  int       srcH          = 0;    // Subarray Height
  int       srcW          = 0;    // Subarray Width
  int       srcN          = 0;    // Source interferogram length
  int       numBol        = 0;    // Number of bolometers in the subarray
  int       coeffLength   = 0;    // Length of the COEFFS array
  int       pcfLength     = 0;    // Phase correction function size
  int       dsLength      = 0;    // Length of the double-sided interferogram
  int       outN          = 0;    // Output interferogram length
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
  double*   PCF           = NULL; // Phase Correction Function
  double*   POS           = NULL; // FTS-2 mirror positions
  double*   RTS           = NULL; // FTS-2 mirror times
  double*   COEFFS        = NULL; // Polynomial coefficients
    double*   WEIGHTS       = NULL; // Weighting factors
    double*   WN            = NULL; // Wavenumbers
  double*   TMPPHASE      = NULL; // Temporary phase
    double*   FIT           = NULL; // Fitted phase
    double*   WINDOW        = NULL; // Apodization function
  fftw_plan planA               = NULL; // fftw plan
  fftw_plan planB               = NULL; // fftw plan
  fftw_complex* DSIN      = NULL; // Double-Sided interferogram, FFT input
  fftw_complex* DSOUT     = NULL; // Double-Sided interferogram, FFT output
  fftw_complex* PCFIN     = NULL; // Phase Correction Function, FFT input
  fftw_complex* PCFOUT    = NULL; // Phase Correction Function, FFT output

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &numInputFile, status);
  kpg1Wgndf("OUT", grpOutput, numInputFile, numInputFile,
            "Equal number of input and output files expected!",
            &grpOutput, &numOutputFile, status);

  // READ IN ADAM PARAMETERS
  parGet0i("DEGREE",        &PDEGREE,    status);
  parGet0i("DSHALFLENGTH",  &DSLENGTH2,  status);
  parGet0i("PCFHALFLENGTH", &PCFLENGTH2, status);
  parGet0d("WNLBOUND",      &WNLOWER,    status);
  parGet0d("WNUBOUND",      &WNUPPER,    status);
  parGet0i("APODIZATION",   &APODIZATION,status);

  // SET ARRAY LENGTHS
  coeffLength = PDEGREE + 1;
  PCFLENGTH2  = (PCFLENGTH2 > DSLENGTH2) ? DSLENGTH2 : PCFLENGTH2;
  dsLength    = DSLENGTH2 << 1;
  pcfLength   = PCFLENGTH2 << 1;

  // BEGIN NDF
  ndfBegin();

  // ===========================================================================
  // LOOP THROUGH EACH INPUT FILE
  // ===========================================================================
  for(fileIndex = 1; fileIndex <= numInputFile; fileIndex++) {
    // OPEN FILE
    smf_open_file(NULL,grpInput, fileIndex, "READ", 0, &inputData, status);
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
    srcN   = inputData->dims[2];
    numBol = srcW * srcH;  // NUMBER OF BOLOMETERS IN THE SUBARRAY
    outN   = srcN - pcfLength + 1;

    // GET SOURCE MIRROR POSITIONS
    POS     = astMalloc(srcN * sizeof(*POS));
    RTS     = astMalloc(srcN * sizeof(*RTS));
    int num = 0;
    fts2_getmirrorpositions(inputData, POS, RTS, &num, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to get the source mirror positions!", status);
      goto CLEANUP;
    }
    double FNYQUIST   = 1.0 / (8.0 * (POS[1] - POS[0]));
    double DSIGMA     = FNYQUIST / DSLENGTH2;
    smf_fits_updateD(inputData->hdr, "FNYQUIST", FNYQUIST, "Nyquist Frequency", status);
    if(POS) { astFree(POS); POS = NULL; }

    // READY OUTPUT FILE
    // =========================================================================
    smfData* outputData = smf_deepcopy_smfData( NULL, inputData, 0,
                                                SMF__NOCREATE_DATA |
                                                SMF__NOCREATE_FTS, 0, 0,
                                                status);
    // DATA ARRAY
    outputData->dtype   = SMF__DOUBLE;
    outputData->ndims   = 3;
    outputData->dims[0] = srcW;
    outputData->dims[1] = srcH;
    outputData->dims[2] = outN;
    outputData->pntr[0] = (double*) astMalloc((numBol * outN) * sizeof(double));
    // MORE.FTS2.ZPD, NEW ZPD INDICES
    smfData* zpd = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    zpd->dtype   = SMF__INTEGER;
    zpd->ndims   = 2;
    zpd->dims[0] = srcW;
    zpd->dims[1] = srcH;
    zpd->pntr[0] = (int*) astMalloc(numBol * sizeof(int));
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

    smfData* dead             = NULL;     /* Dead pixel flag m x n array */
    smfData* a                = NULL;     /* Pointer to a band (1/f low frequency) integrated powers */
    smfData* b                = NULL;     /* Pointer to b band (in band signal) integrated powers */
    smfData* c                = NULL;     /* Pointer to c band (noise) integrated powers */
    smfData* d                = NULL;     /* Pointer to d band (first harmonic) integrated powers */
    smfData* phaseFit         = NULL;     /* Pointer to Phase X^2 goodness of fit measures */
    smfData* cosmicRays       = NULL;     /* Pointer to numbers of cosmic rays occuring */
    smfData* fluxJumps        = NULL;     /* Pointer to numbers of flux jumps occuring */

    /* Create a 2D empty dead pixel array */
    dead = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    dead->dtype   = SMF__INTEGER;
    dead->ndims   = 2;
    dead->dims[0] = srcW;
    dead->dims[1] = srcH;
    dead->pntr[0] = (int*) astCalloc(numBol, sizeof(int));

    /* Create a 2D empty a band array */
    a = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    a->dtype   = SMF__DOUBLE;
    a->ndims   = 2;
    a->dims[0] = srcW;
    a->dims[1] = srcH;
    a->pntr[0] = (double*) astCalloc(numBol, sizeof(double));

    /* Create a 2D empty b band array */
    b = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    b->dtype   = SMF__DOUBLE;
    b->ndims   = 2;
    b->dims[0] = srcW;
    b->dims[1] = srcH;
    b->pntr[0] = (double*) astCalloc(numBol, sizeof(double));

    /* Create a 2D empty c band array */
    c = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    c->dtype   = SMF__DOUBLE;
    c->ndims   = 2;
    c->dims[0] = srcW;
    c->dims[1] = srcH;
    c->pntr[0] = (double*) astCalloc(numBol, sizeof(double));

    /* Create a 2D empty d band array */
    d = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    d->dtype   = SMF__DOUBLE;
    d->ndims   = 2;
    d->dims[0] = srcW;
    d->dims[1] = srcH;
    d->pntr[0] = (double*) astCalloc(numBol, sizeof(double));

    /* Create a 2D empty phaseFit array */
    phaseFit = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    phaseFit->dtype   = SMF__DOUBLE;
    phaseFit->ndims   = 2;
    phaseFit->dims[0] = srcW;
    phaseFit->dims[1] = srcH;
    phaseFit->pntr[0] = (double*) astCalloc(numBol, sizeof(double));

    /* Create a 2D empty cosmicRays array */
    cosmicRays = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    cosmicRays->dtype   = SMF__INTEGER;
    cosmicRays->ndims   = 2;
    cosmicRays->dims[0] = srcW;
    cosmicRays->dims[1] = srcH;
    cosmicRays->pntr[0] = (int*) astCalloc(numBol, sizeof(int));

    /* Create a 2D empty fluxJumps array */
    fluxJumps = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fluxJumps->dtype   = SMF__INTEGER;
    fluxJumps->ndims   = 2;
    fluxJumps->dims[0] = srcW;
    fluxJumps->dims[1] = srcH;
    fluxJumps->pntr[0] = (int*) astCalloc(numBol, sizeof(int));

    // =========================================================================

    // LOOP THROUGH EACH PIXEL IN THE SUBARRAY
    // =========================================================================
    for(i = 0; i < srcH; i++) { // LOOP THROUGH ROWS
      for(j = 0; j < srcW; j++) { // LOOP THROUGH COLUMNS
        bolIndex = i + j * srcH;

        // GET ZPD INDEX
        zpdIndex = *((int*)(zpdData->pntr[0]) + bolIndex);
        // ENSURE THAT DSLENGTH2 <= ZPDINDEX
        DSLENGTH2  = (DSLENGTH2 < zpdIndex) ? DSLENGTH2 : zpdIndex;
        PCFLENGTH2 = (PCFLENGTH2 > DSLENGTH2) ? DSLENGTH2 : PCFLENGTH2;
        dsLength   = DSLENGTH2 << 1;
        pcfLength  = PCFLENGTH2 << 1;

        // WAVENUMBER INDICES
        wnL = (int) (DSLENGTH2 * WNLOWER / FNYQUIST);
        wnU = (int) (DSLENGTH2 * WNUPPER / FNYQUIST);

        // ### ALLOCATE MEMORY
        IFG       = astMalloc(srcN            * sizeof(*IFG));
        DS        = astMalloc(dsLength        * sizeof(*DS));
        PHASE     = astMalloc(dsLength        * sizeof(*PHASE));
        COEFFS    = astMalloc(coeffLength     * sizeof(*COEFFS));
        PCF       = astMalloc(pcfLength       * sizeof(*PCF));
        WN        = astMalloc((DSLENGTH2 + 1) * sizeof(*WN));
        WEIGHTS   = astMalloc((DSLENGTH2 + 1) * sizeof(*WEIGHTS));
        FIT       = astMalloc((DSLENGTH2 + 1) * sizeof(*FIT));
        TMPPHASE  = astMalloc((DSLENGTH2 + 1) * sizeof(*TMPPHASE));
          DSIN      = fftw_malloc(dsLength      * sizeof(*DSIN));
          DSOUT     = fftw_malloc(dsLength      * sizeof(*DSOUT));
        PCFIN     = fftw_malloc(dsLength      * sizeof(*PCFIN));
        PCFOUT    = fftw_malloc(dsLength      * sizeof(*PCFOUT));

        // ### INGEST INTERFEROGRAM FOR THE CURRENT PIXEL
        for(k = 0; k < srcN; k++) {
          IFG[k] = *((double*)(inputData->pntr[0]) + (bolIndex + numBol * k));
        }

        // ### EXTRACT DOUBLE-SIDED BUTTERFLIED INTERFEROGRAM
        for(k = 0; k < dsLength; k++) {
          DS[k] = (k < (zpdIndex - 1)) ? IFG[zpdIndex + k - 1] : IFG[k - zpdIndex + 1];
        }
        // COMPLEX DOUBLE-SIDED BUTTERFLIED INTERFEROGRAM
        for(k = 0; k < dsLength; k++) { DSIN[k][0] = DS[k]; DSIN[k][1] = 0.0; }

        // ### FORWARD FFT DOUBLE-SIDED INTERFEROGRAM
          planA = fftw_plan_dft_1d(dsLength, DSIN, DSOUT, FFTW_FORWARD, FFTW_ESTIMATE);
          fftw_execute(planA);

        // ### PHASE
          for(k = 0; k < dsLength; k++) { PHASE[k] = atan2(DSOUT[k][1], DSOUT[k][0]); }

        // ### WAVENUMBERS [0, FNYQ]
        for(k = 0; k <= DSLENGTH2; k++) { WN[k] = k * DSIGMA; }

        // ### WEIGHTS [0, FNYQ]
        wnL = (int) (DSLENGTH2 * WNLOWER / FNYQUIST);
        wnU = (int) (DSLENGTH2 * WNUPPER / FNYQUIST);
          double maxWeight = NUM__MIND;
        for(k = 0; k <= DSLENGTH2; k++) {
          if(k < wnL || k > wnU) { WEIGHTS[k] = 0.0; }
          else {
            WEIGHTS[k] = DSOUT[k][0] * DSOUT[k][0] + DSOUT[k][1] * DSOUT[k][1];
            WEIGHTS[k] = sqrt(WEIGHTS[k]);
            if(WEIGHTS[k] > maxWeight) { maxWeight = WEIGHTS[k]; }
          }
        }
        // NORMALIZE WEIGHTS
          if(maxWeight <= 0) { maxWeight = 1; }
          for(k = 0; k <= DSLENGTH2; k++) { WEIGHTS[k] /= maxWeight; }
          WEIGHTS[0] = WEIGHTS[DSLENGTH2] = 0.0;

          // ### PHASE FIT
          for(k = 0; k <= DSLENGTH2; k++) { TMPPHASE[k] = PHASE[k]; }
        smf_fit_poly1d(PDEGREE, DSLENGTH2 + 1, CLIP, WN, TMPPHASE, WEIGHTS, NULL, COEFFS, NULL, FIT, &nUsed, status);
        // MORE.FTS2.SIGMA, STANDARD DEVIATION,
        double sum   = 0.0;
        double error = 0.0;
        for(k = 0; k <= DSLENGTH2; k++) {
          error += WEIGHTS[k] * (PHASE[k] - FIT[k]) * (PHASE[k] - FIT[k]);
          sum   += WEIGHTS[k];
        }
        *((double*)(sigma->pntr[0]) + bolIndex) = sqrt(error / sum);
        // MORE.FTS2.FPM, POLYNOMIAL COEFFICIENTS
        for(k = 0; k < coeffLength; k++) { *((double*) (fpm->pntr[0]) + (bolIndex + numBol * k)) = COEFFS[k]; }
        // MORE.FTS2.ZPD, NEW ZPD INDICES
        *((int*) (zpd->pntr[0]) + bolIndex) = zpdIndex - PCFLENGTH2 + 1;
        // POLYNOMIAL FITTED PHASE
        for(k = 0; k < DSLENGTH2; k++) { EVALPOLY(PHASE[k], WN[k], PDEGREE, COEFFS); }
        for(k = 1; k < DSLENGTH2; k++) { PHASE[DSLENGTH2 + k] = -PHASE[DSLENGTH2 - k]; } // PHASE(-k) = -PHASE(k)
        PHASE[0] = PHASE[DSLENGTH2] = 0.0;

        // ### PHASE CORRECTION FUNCTION, PCF
          // EXP(-iPHASE)
          for(k = 0; k < dsLength; k++) {
            PCFIN[k][0] =  cos(PHASE[k]);
            PCFIN[k][1] = -sin(PHASE[k]);
          }
          // COMPUTE PCF, INVERSE FFT OF EXP(-iPHASE)
          planB = fftw_plan_dft_1d(dsLength, PCFIN, PCFOUT, FFTW_BACKWARD, FFTW_ESTIMATE);
          fftw_execute(planB);
          // NORMALIZE PCF - REAL COMPONENT
          for(k = 0; k < dsLength; k++) { PCFOUT[k][0] /= dsLength; }
        // SHIFT PCF - REAL COMPONENT BY HALF
          for(k = 0; k < DSLENGTH2; k++) {
            double t = PCFOUT[dsLength - 1][0];
            for(n = 1; n < dsLength; n++) {
              PCFOUT[dsLength - n][0] = PCFOUT[dsLength - (n + 1)][0];
            }
            PCFOUT[0][0] = t;
          }
          // TRUNCATE PCF - REAL COMPONENT
          int M = DSLENGTH2 - PCFLENGTH2;
        for(k = 0; k < pcfLength; k++) { PCF[k] = PCFOUT[M + k][0]; }
        // REVERSE PCF FOR CONVOLUTION
        for(k = 0; k < PCFLENGTH2; k++) {
          double t = PCF[k];
          PCF[k] = PCF[pcfLength - k - 1];
          PCF[pcfLength - k - 1] = t;
        }

        // ### APODIZE PCF
        WINDOW = astMalloc(pcfLength * sizeof(*WINDOW));
        fts2_apodization(PCF, pcfLength, -1.0, 1.0, WINDOW, APODIZATION, status);
        if(*status != SAI__OK) {
          *status = SAI__ERROR;
          errRep(FUNC_NAME, "Unable to apodize phase correction function!", status);
          goto CLEANUP;
        }

        // ### CONVOLVE SOURCE INTERFEROGRAM WITH PCF & UPDATE OUTPUT
        for(k = 0; k < outN; k++) {
          double t = 0.0;
          for(n = 0; n < pcfLength; n++) {
            t += (IFG[k + pcfLength - 1 - n] * PCF[pcfLength - 1 - n]);
          }
          index = bolIndex + numBol * k;
          *((double*)(outputData->pntr[0]) + index) = t;
        }

        // ### FREE RESOURCES
        if(POS)      { astFree(POS);              POS       = NULL; }
        if(RTS)      { astFree(RTS);              RTS       = NULL; }
        if(IFG)      { astFree(IFG);              IFG       = NULL; }
        if(DS)       { astFree(DS);               DS        = NULL; }
        if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
        if(PCF)      { astFree(PCF);              PCF       = NULL; }
        if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
        if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
        if(WN)       { astFree(WN);               WN        = NULL; }
        if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
        if(FIT)      { astFree(FIT);              FIT       = NULL; }
        if(WINDOW)   { astFree(WINDOW);           WINDOW    = NULL; }
        if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
        if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
          if(PCFIN)    { fftw_free(PCFIN);          PCFIN     = NULL; }
          if(PCFOUT)   { fftw_free(PCFOUT);         PCFOUT    = NULL; }
      } // END LOOP THROUGH COLUMNS
    } // END LOOP THROUGH ROWS

    // CLOSE FILE
    if(inputData) { smf_close_file( NULL,&inputData, status); }
      if(planA)     { fftw_destroy_plan(planA);  planA     = NULL; }
      if(planB)     { fftw_destroy_plan(planB);  planB     = NULL; }

    // WRITE OUTPUT
    outputData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, dead, a, b, c, d, phaseFit, cosmicRays, fluxJumps, status);
    smf_write_smfData(NULL, outputData, NULL, NULL, grpOutput, fileIndex, 0,
                      MSG__VERB, 0, NULL, NULL, status);
    smf_close_file( NULL,&outputData, status);
  } // END FILE LOOP

  CLEANUP:
    // FREE RESOURCES
    if(POS)      { astFree(POS);              POS       = NULL; }
    if(RTS)      { astFree(RTS);              RTS       = NULL; }
    if(IFG)      { astFree(IFG);              IFG       = NULL; }
    if(DS)       { astFree(DS);               DS        = NULL; }
    if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
    if(PCF)      { astFree(PCF);              PCF       = NULL; }
    if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
    if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
    if(WN)       { astFree(WN);               WN        = NULL; }
    if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
    if(FIT)      { astFree(FIT);              FIT       = NULL; }
    if(WINDOW)   { astFree(WINDOW);           WINDOW    = NULL; }
    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
      if(PCFIN)    { fftw_free(PCFIN);          PCFIN     = NULL; }
      if(PCFOUT)   { fftw_free(PCFOUT);         PCFOUT    = NULL; }
      if(planA)    { fftw_destroy_plan(planA);  planA     = NULL; }
      if(planB)    { fftw_destroy_plan(planB);  planB     = NULL; }
      fftw_cleanup();

    // CLOSE FILE
    if(inputData)  { smf_close_file( NULL,&inputData, status); }

    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    grpDelet(&grpInput, status);
    grpDelet(&grpOutput, status);
}
