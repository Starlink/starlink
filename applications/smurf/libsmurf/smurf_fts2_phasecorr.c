/*
*+
*  Name:
*     FTS2PHASECORR

*  Purpose:
*     Applies phase correction to the source interferograms.

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
*     Applies phase correction to the source interferograms.

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

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2011-06-20 (COBA):
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

#define FUNC_NAME "smurf_fts2_phasecorr"
#define TASK_NAME "FTS2PHASECORR"

void smurf_fts2_phasecorr(int* status)
{
  if(*status != SAI__OK) { return; }

  // ADAM VARIABLES
  int       pDegree       = 0;    // Degree of the polynomial used to fit phase
  int       dsLength2     = 0;    // Half-Length of double-sided interferogram
  int       pcfLength2    = 0;    // Half-Length of phase correction function
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
  int       srcH          = 0;    // Subarray Height
  int       srcW          = 0;    // Subarray Width
  int       srcN          = 0;    // Source interferogram length
  int       numBol        = 0;    // Number of bolometers in the subarray
  int       pcfLength     = 0;    // Phase correction function size
  int       dsLength      = 0;    // Length of the double-sided interferogram
  int       outN          = 0;    // Output interferogram length
  int       wnL           = 0;    // Index for the lower limit of the band
  int       wnU           = 0;    // Index for the upper limit of the band
  int       coeffLength   = 0;    // Length of the COEFFS array
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
  double*   COEFFS        = NULL; // Polynomial coefficients
	double*   WEIGHTS       = NULL; // Weighting factors
	double*   WN            = NULL; // Wavenumbers
  double*   TMPPHASE      = NULL; // Temporary phase
	double*   FIT           = NULL; // Fitted phase
  fftw_plan planA			    = NULL; // fftw plan
  fftw_plan planB			    = NULL; // fftw plan
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
  parGet0i("DEGREE",        &pDegree,    status);
  parGet0i("DSHALFLENGTH",  &dsLength2,  status);
  parGet0i("PCFHALFLENGTH", &pcfLength2, status);
  parGet0d("WNLBOUND",      &wnLower,    status);
  parGet0d("WNUBOUND",      &wnUpper,    status);

  // SET ARRAY LENGTHS
  coeffLength = pDegree + 1;
  pcfLength2  = (pcfLength2 > dsLength2) ? dsLength2 : pcfLength2;
  dsLength    = dsLength2 << 1;
  pcfLength   = pcfLength2 << 1;

  // BEGIN NDF
  ndfBegin();

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
    srcW   = inputData->dims[0];
    srcH   = inputData->dims[1];
    srcN   = inputData->dims[2];
    numBol = srcW * srcH;  // NUMBER OF BOLOMETERS IN THE SUBARRAY

    outN = srcN - pcfLength + 1;

    // GET SOURCE MIRROR POSITIONS
    int num = 0;
    POS = astMalloc(srcN * sizeof(*POS));
    fts2_getmirrorpositions(inputData, POS, &num, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to get the source mirror positions!", status);
      goto CLEANUP;
    }
    double dz = 4.0 * (POS[1] - POS[0]);  // Sampling Interval in OPD
    double fs = 1.0 / dz;                 // Sampling Frequency
    double fnyq = fs / 2.0;               // Nyquist Frequency
    double df = fnyq / dsLength2;
    smf_fits_updateD(inputData->hdr, "FNYQUIST", fnyq, "Nyquist Frequency", status);
    if(POS) { astFree(POS); POS = NULL; }

    // WAVENUMBER INDICES
    wnL = (int) (dsLength2 * wnLower / fnyq);
    wnU = (int) (dsLength2 * wnUpper / fnyq);

    // OUTPUT SMFDATA
    smfData* outputData = smf_deepcopy_smfData( inputData, 0,
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

    // ALLOCATE MEMORY FOR ARRAYS
    IFG      = astMalloc(srcN            * sizeof(*IFG));
    PHASE    = astMalloc(dsLength        * sizeof(*PHASE));
    DS       = astMalloc(dsLength        * sizeof(*DS));
	  DSIN     = fftw_malloc(dsLength      * sizeof(*DSIN));
	  DSOUT    = fftw_malloc(dsLength      * sizeof(*DSOUT));
    PCF      = astMalloc(pcfLength       * sizeof(*PCF));
    PCFIN    = fftw_malloc(dsLength      * sizeof(*PCFIN));
    PCFOUT   = fftw_malloc(dsLength      * sizeof(*PCFOUT));
    COEFFS   = astMalloc(coeffLength     * sizeof(*COEFFS));
    TMPPHASE = astMalloc((dsLength2 + 1) * sizeof(*TMPPHASE));
    WN       = astMalloc((dsLength2 + 1) * sizeof(*WN));
    WEIGHTS  = astMalloc((dsLength2 + 1) * sizeof(*WEIGHTS));
    FIT      = astMalloc((dsLength2 + 1) * sizeof(*FIT));

    // =========================================================================
    // LOOP THROUGH EACH PIXEL IN THE SUBARRAY
    // =========================================================================
    for(i = 0; i < srcH; i++) { // LOOP THROUGH ROWS
      for(j = 0; j < srcW; j++) { // LOOP THROUGH COLUMNS
        bolIndex = i + j * srcH;

        // GET ZPD INDEX
        zpdIndex = *((int*)(zpdData->pntr[0]) + bolIndex);

        //
        // INTERFEROGRAM
        //

        // GET SOURCE INTERFEROGRAM
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          IFG[k] = *((double*)(inputData->pntr[0]) + index);
        }

        // EXTRACT DOUBLE-SIDED INTERFEROGRAM FOR FFT
        fts2_arraycopy(IFG, srcN, DS, dsLength, zpdIndex - 1, 0, dsLength2 + 1);
        fts2_arraycopy(IFG, srcN, DS, dsLength, zpdIndex - dsLength2,
                        dsLength2 + 1, dsLength2 - 1);

        for(k = 0; k < dsLength; k++) {
          DSIN[k][0] = DS[k];
          DSIN[k][1] = 0.0;
        }

        // FORWARD FFT DOUBLE-SIDED INTERFEROGRAM
	      planA = fftw_plan_dft_1d(dsLength, DSIN, DSOUT, FFTW_FORWARD, FFTW_ESTIMATE);
	      fftw_execute(planA);

        //
        // PHASE
        //

        // COMPUTE PHASE
	      for(k = 0; k < dsLength; k++) {
	        PHASE[k] = atan2(DSOUT[k][1], DSOUT[k][0]);
	      }

        // COMPUTE WAVENUMBERS [0, FNYQ]
        for(k = 0; k <= dsLength2; k++) {
          WN[k] = k * df;
        }

        // COMPUTE WEIGHTS [0, FNYQ]
	      double maxWeight = NUM__MIND;
        for(k = 0; k <= dsLength2; k++) {
          if(k < wnL || k > wnU) {
            WEIGHTS[k] = 0.0;
          } else {
            WEIGHTS[k] = DSOUT[k][0] * DSOUT[k][0] + DSOUT[k][1] * DSOUT[k][1];
            WEIGHTS[k] = sqrt(WEIGHTS[k]);
            if(WEIGHTS[k] > maxWeight) {
              maxWeight = WEIGHTS[k];
            }
          }
        }

        // NORMALIZE WEIGHTS
	      if(maxWeight <= 0) { maxWeight = 1; }
	      for(k = 0; k <= dsLength2; k++) {
	        WEIGHTS[k] /= maxWeight;
	      }
	      WEIGHTS[0] = WEIGHTS[dsLength2] = 0.0;

        // COMPUTE PHASE [0, FNYQ]
	      for(k = 0; k <= dsLength2; k++) {
	        TMPPHASE[k] = PHASE[k];
	      }

	      // COMPUTE PHASE FIT
        smf_fit_poly1d(pDegree, dsLength2 + 1, CLIP, WN, TMPPHASE, WEIGHTS,
                       NULL, COEFFS, NULL, FIT, &nUsed, status);

        // STANDARD DEVIATION
        // MORE.FTS2.SIGMA
        double sum   = 0.0;
        double error = 0.0;
        for(k = 0; k <= dsLength; k++) {
          error += WEIGHTS[k] * (PHASE[k] - FIT[k]) * (PHASE[k] - FIT[k]);
          sum   += WEIGHTS[k];
        }
        *((double*)(sigma->pntr[0]) + bolIndex) = sqrt(error / sum);

        // POLYNOMIAL COEFFICIENTS
        // MORE.FTS2.FPM
        for(k = 0; k < coeffLength; k++) {
          index = bolIndex + numBol * k;
          *((double*) (fpm->pntr[0]) + index) = COEFFS[k];
        }

        // NEW ZPD INDICES
        // MORE.FTS2.ZPD
        *((int*) (zpd->pntr[0]) + bolIndex) = zpdIndex - pcfLength2 + 1;

        // POLYNOMIAL FITTED PHASE
        for(k = 0; k < dsLength2; k++) {
          //PHASE[k] = FIT[k];
	        EVALPOLY(PHASE[k], WN[k], pDegree, COEFFS);
        }

        // PHASE(-k) = -PHASE(k)
        for(k = 1; k < dsLength2; k++) {
          PHASE[dsLength2 + k] = -PHASE[dsLength2 - k];
        }
        PHASE[0] = PHASE[dsLength2] = 0.0;

        //
	      // PHASE CORRECTION FUNCTION, PCF
	      //

	      // COMPUTE EXP(-iPHASE)
	      for(k = 0; k < dsLength; k++) {
	        PCFIN[k][0] =  cos(PHASE[k]);
	        PCFIN[k][1] = -sin(PHASE[k]);
	      }

	      // COMPUTE PCF, INVERSE FFT OF EXP(-iPHASE)
	      planB = fftw_plan_dft_1d(dsLength, PCFIN, PCFOUT, FFTW_BACKWARD, FFTW_ESTIMATE);
	      fftw_execute(planB);

	      // NORMALIZE PCF - REAL COMPONENT
	      for(k = 0; k < dsLength; k++) {
	        PCFOUT[k][0] /= dsLength;
	      }

	      // SHIFT PCF - REAL COMPONENT BY HALF
	      for(k = 0; k <= dsLength2; k++) {
	        double t = PCFOUT[dsLength - 1][0];
	        for(n = 1; n < dsLength; n++) {
	          PCFOUT[dsLength - n][0] = PCFOUT[dsLength - (n + 1)][0];
	        }
	        PCFOUT[0][0] = t;
	      }

	      // TRUNCATE PCF - REAL COMPONENT
	      int M = dsLength2 - pcfLength2;
        for(k = 0; k < pcfLength; k++) {
          PCF[k] = PCFOUT[M + k][0];
        }

        // REVERSE PCF FOR CONVOLUTION
        for(k = 0; k < pcfLength2; k++) {
          double t = PCF[k];
          PCF[k] = PCF[pcfLength - k - 1];
          PCF[pcfLength - k - 1] = t;
        }

        //
        // CONVOLUTION
        //

        // CONVOLVE SOURCE INTERFEROGRAM WITH PCF & UPDATE OUTPUT
        for(k = 0; k < outN; k++) {
          double t = 0.0;
          for(n = 0; n < pcfLength; n++) {
            t += (IFG[k + pcfLength - 1 - n] * PCF[pcfLength - 1 - n]);
          }
          index = bolIndex + numBol * k;
          *((double*)(outputData->pntr[0]) + index) = t;
        }
      } // END FOR LOOP - COLUMNS
    } // END FOR LOOP - ROWS

    // CLOSE FILE
    if(inputData) { smf_close_file(&inputData, status); }

    // FREE RESOURCES
    if(IFG)      { astFree(IFG);              IFG       = NULL; }
    if(DS)       { astFree(DS);               DS        = NULL; }
    if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
    if(PCF)      { astFree(PCF);              PCF       = NULL; }
    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
	  if(PCFIN)    { fftw_free(PCFIN);          PCFIN     = NULL; }
	  if(PCFOUT)   { fftw_free(PCFOUT);         PCFOUT    = NULL; }
    if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
    if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
    if(WN)       { astFree(WN);               WN        = NULL; }
    if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
    if(FIT)      { astFree(FIT);              FIT       = NULL; }
    if(POS)      { astFree(POS);              POS       = NULL; }
	  if(planA)    { fftw_destroy_plan(planA);  planA     = NULL; }
	  if(planB)    { fftw_destroy_plan(planB);  planB     = NULL; }

    // WRITE OUTPUT
    outputData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, status);
    smf_write_smfData(outputData, NULL, NULL, grpOutput, fileIndex, 0, status);
    smf_close_file(&outputData, status);
  } // END FOR LOOP - FILES

  CLEANUP:
    // FREE RESOURCES
    if(IFG)      { astFree(IFG);              IFG       = NULL; }
    if(DS)       { astFree(DS);               DS        = NULL; }
    if(PHASE)    { astFree(PHASE);            PHASE     = NULL; }
    if(PCF)      { astFree(PCF);              PCF       = NULL; }
    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
	  if(PCFIN)    { fftw_free(PCFIN);          PCFIN     = NULL; }
	  if(PCFOUT)   { fftw_free(PCFOUT);         PCFOUT    = NULL; }
    if(COEFFS)   { astFree(COEFFS);           COEFFS    = NULL; }
    if(TMPPHASE) { astFree(TMPPHASE);         TMPPHASE  = NULL; }
    if(WN)       { astFree(WN);               WN        = NULL; }
    if(WEIGHTS)  { astFree(WEIGHTS);          WEIGHTS   = NULL; }
    if(FIT)      { astFree(FIT);              FIT       = NULL; }
    if(POS)      { astFree(POS);              POS       = NULL; }
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
