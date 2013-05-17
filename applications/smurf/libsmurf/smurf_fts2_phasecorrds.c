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
*     MSHERWOOD: Matt Sherwood (UofL)
*     GBELL: Graham Bell (JAC)

*  History :
*     2011-08-03 (COBA):
*        New version.
*     2011-08-16 (COBA):
*        Add Zero-Padding.
*     2011-10-18 (COBA):
*        Updated to return phase corrected interferogram cube.
*     2013-01-30 (MSHERWOOD):
*        Zero out imaginary part of the spectrum to remove noise
*     2013-03-12 (MSHERWOOD)
*        Add DEBUG mode for analyzing intermediary results
*        When enabled, by defining DEBUG 1 and recompiling,
*        the following menu is displayed:
*
*        Select the type of output file you want:
*        'd' : <Default> phase corrected output          => _phs
*        'r' : Real part of spectra                      => _phs_SR
*        'i' : Imaginary part of spectra                 => _phs_SI
*        'p' : Phase part of spectra                     => _phs_SP
*        'f' : Fitted phase part of spectra              => _phs_SPF
*        'w' : Wave numbers                              => _phs_WN
*        't' : Weights                                   => _phs_WT
*        'R' : Real part of phase corrected spectra      => _phs_SRC
*        'I' : Imaginary part of phase corrected spectra => _phs_SIC
*
*        which results in the named suffix being added to a new
*        output NDF file to hold the corresponding data.
*     2013-04-03 (GBELL)
*       smurf: allow phasecorrds to save all debug files
*       If compiled with DEBUG set, all debug ouput files will be
*       saved, avoiding the need for the file selection menu.
*     2013-03-12 (MSHERWOOD)
*       Added Phase Correction Function debug output
*     2013-04-12 (MSHERWOOD)
*       Correct wave number trimming
*     2013-05-07 (MSHERWOOD)
*       Added digital filtering to interferogram prior to phase correction
*     2013-05-08 (MSHERWOOD)
*       Bracketed KERNEL_LENGTH macro
*     2013-05-16 (MSHERWOOD)
*       Retain imaginary spectrum for DEBUG output

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010, 2013 University of Lethbridge. All Rights Reserved.

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

/* STANDARD includes */
#include <string.h>
#include <stdio.h>
#include <stdint.h>

/* STARLINK includes */
#include "ast.h"
#include "ndf.h"
#include "mers.h"
#include "par.h"
#include "sae_par.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "star/one.h"
#include "one_err.h"

/* SMURF includes */
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsc2fts/fts2.h"

/* FFTW includes */
#include <fftw3.h>

#define FUNC_NAME "smurf_fts2_phasecorrds"
#define TASK_NAME "FTS2PHASECORRDS"

#define IFG_PEAK_THRESHOLD          1.0           /* Maximum offset of IFG peak from ZPD in OPD cm */
#define WAVE_NUMBER_RANGE           5             /* The range of interest of wave numbers */
#define PHASE_ROLLOVER_THRESHOLD    1.0           /* 1 radian */
#define FWHM                        8.0           /* Full Width at Half Maximum (standard Gaussian measure) */
#define KERNEL_LENGTH               (1/FWHM)      /* Area to trim from both ends of IFG in OPD cm: 1/FWHM */

#ifndef DEBUG
#define DEBUG 0
#endif

void smurf_fts2_phasecorrds(int* status)
{
  if( *status != SAI__OK ) { return; }

  double const DTOR         = AST__DPI / 180.0;   /* Degrees-to-Radians */
  Grp* gIn                  = NULL;               /* Input group */
  Grp* gOut                 = NULL;               /* Output group */
  smfData* inData           = NULL;               /* Pointer to input data */
  smfData* outData          = NULL;               /* Pointer to output data */
  double* outData_pntr      = NULL;               /* Pointer to output data values array */
#if DEBUG
  smfData* outDataIFGDFR    = NULL;               /* Pointer to output data Interferogram Digitally Filtered Real DEBUG */
  double* outDataIFGDFR_pntr= NULL;               /* Pointer to output data Interferogram Digitally Filtered Real values array */
  smfData* outDataIFGDFI    = NULL;               /* Pointer to output data Interferogram Digitally Filtered Imaginary DEBUG */
  double* outDataIFGDFI_pntr= NULL;               /* Pointer to output data Interferogram Digitally Filtered Imaginary values array */
  smfData* outDataSR        = NULL;               /* Pointer to output data Spectrum Real DEBUG */
  double* outDataSR_pntr    = NULL;               /* Pointer to output data Spectrum Real values array */
  smfData* outDataSI        = NULL;               /* Pointer to output data Spectrum Imaginary DEBUG */
  double* outDataSI_pntr    = NULL;               /* Pointer to output data Spectrum Imaginary values array */
  smfData* outDataSRC       = NULL;               /* Pointer to output data Spectrum Real phase Corrected DEBUG */
  double* outDataSRC_pntr   = NULL;               /* Pointer to output data Spectrum Real phase Corrected values array */
  smfData* outDataSIC       = NULL;               /* Pointer to output data Spectrum Imaginary phase Corrected DEBUG */
  double* outDataSIC_pntr   = NULL;               /* Pointer to output data Spectrum Imaginary phase Corrected values array */
  smfData* outDataSP        = NULL;               /* Pointer to output data Spectrum Phase DEBUG */
  double* outDataSP_pntr    = NULL;               /* Pointer to output data Spectrum Phase values array */

  smfData* outDataPCFR      = NULL;               /* Pointer to output data Phase Correction Function Real DEBUG */
  double* outDataPCFR_pntr  = NULL;               /* Pointer to output data Phase Correction Function Real values array */
  smfData* outDataPCFI      = NULL;               /* Pointer to output data Phase Correction Function Imaginary DEBUG */
  double* outDataPCFI_pntr  = NULL;               /* Pointer to output data Phase Correction Function Imaginary values array */

  smfData* outDataSPF       = NULL;               /* Pointer to output data Spectrum Phase Fitted DEBUG */
  double* outDataSPF_pntr   = NULL;               /* Pointer to output data Spectrum Phase Fitted values array */
  smfData* outDataWN        = NULL;               /* Pointer to output data Wave Numbers DEBUG */
  double* outDataWN_pntr    = NULL;               /* Pointer to output data Wave Numbers values array */
  smfData* outDataWT        = NULL;               /* Pointer to output data Weights DEBUG */
  double* outDataWT_pntr    = NULL;               /* Pointer to output data Weights values array */
#endif
  /*smfData* zpdData          = NULL;*/               /* Pointer to ZPD data */
  smfData* zpd              = NULL;               /* Pointer to ZPD index data */
  smfData* fpm              = NULL;               /* Pointer polynomial fit parameters */
  smfData* sigma            = NULL;
  int64_t nUsed             = 0;                  /* Number of used data points */
  int pDegree               = 0;                  /* Degree of the polynomial used to fit phase */
  int i                     = 0;                  /* Counter */
  int j                     = 0;                  /* Counter */
  int k                     = 0;                  /* Counter */
  double wnLower            = 0.0;                /* Lower bound of wave number range */
  double wnUpper            = 0.0;                /* Upper bound of wave number range */
  double fNyquist           = 0.0;                /* Nyquist frequency */
  double dz                 = 0.0;               /* Step size in evenly spaced OPD grid */
  double CLIP               = 0.0;                /* Clipping param for the polynomial fit */
  double maxWeight          = NUM__MIND;          /* Max weighting factor */
  double* IFG               = NULL;               /* Interferogram */
  double* DS                = NULL;               /* Double-Sided interferogram */
  double* PHASE             = NULL;               /* Phase */
#if DEBUG
  double* PHASES            = NULL;               /* DEBUG Phase Saved */
#endif
  double* WN                = NULL;               /* Wavenumbers */
  double* WEIGHTS           = NULL;               /* Weighting factors */
  double* FIT               = NULL;               /* Fitted phase */
#if DEBUG
  double* FITS              = NULL;               /* DEBUG Fitted phase Saved */
#endif
  double* COEFFS            = NULL;               /* Polynomial coefficients */
  double* TMPPHASE          = NULL;               /* Temporary phase */
  fftw_complex* DSIN        = NULL;               /* Double-Sided interferogram, FFT input */
  fftw_complex* DSOUT       = NULL;               /* Double-Sided interferogram, FFT output */
  fftw_complex* IFGDF       = NULL;               /* Interferogram, Digitally Filtered */
  fftw_complex* PCF         = NULL;               /* Phase Correction Function */
  fftw_complex* SPEC        = NULL;               /* Spectrum */
#if DEBUG
  fftw_complex* SPECS       = NULL;               /* DEBUG Spectrum Saved */
#endif
  fftw_plan planA           = NULL;               /* fftw plan */
  fftw_plan planB           = NULL;               /* fftw plan */

  size_t nFiles             = 0;                  /* Size of the input group */
  size_t nOutFiles          = 0;                  /* Size of the output group */
  size_t fIndex             = 0;                  /* File index */
  size_t nWidth             = 0;                  /* Data cube width */
  size_t nHeight            = 0;                  /* Data cube height */
  size_t nFrames            = 0;                  /* Data cube depth */
  size_t nFrames2           = 0;
  size_t nPixels            = 0;                  /* Number of bolometers in the subarray */
  size_t wnL                = 0;
  size_t wnU                = 0;
  double wnTrim             = WAVE_NUMBER_RANGE;  /* Trim the first wnTrim wave numbers (zero Real part of spectrum) */

  double dSigma             = 0;
  double sum                = 0;
  double error              = 0;
  int coeffLength           = 0;
  int bolIndex              = 0;
  int index                 = 0;
  int badPixel              = 0;
  int M                     = 0;
  int indexZPD              = 0;
  int W                     = 1;

  char fileName[SMF_PATH_MAX+1];                /* DEBUG */
  int n                     = 0;

  /* DF: Digital Filter */
  double peakIFG            = 0.0;              /* Value of interferogram peak */
  int peakIFGIndex          = 0;                /* Index of interferogram peak */
  double phaseDiff          = 0.0;              /* Difference of phase between consecutive values */
  double phaseRollover      = 0.0;              /* Phase rollover accumulator */

  /* Get Input & Output groups */
  kpg1Rgndf("IN", 0, 1, "", &gIn, &nFiles, status);
  kpg1Wgndf("OUT", gOut, nFiles, nFiles, "Equal number of input and output files expected!", &gOut, &nOutFiles, status);

  /* Read in ADAM parameters */
  parGet0i("DEGREE",   &pDegree, status);
  parGet0d("WNLBOUND", &wnLower, status);
  parGet0d("WNUBOUND", &wnUpper, status);


  coeffLength = pDegree + 1;

  /* BEGIN NDF */
  ndfBegin();

  /* Loop through each input file */
  for(fIndex = 1; fIndex <= nFiles; fIndex++) {
    /* Open Observation file */
    smf_open_file(gIn, fIndex, "READ", 0, &inData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open the source file!", status);
      goto CLEANUP;
    }

    /* Check if the file is initialized for FTS2 processing */
    if(!(inData->fts) || !(inData->fts->zpd)) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "The file is NOT initialized for FTS2 data reduction!", status);
      goto CLEANUP;
    }
    /*zpdData = inData->fts->zpd;*/

    /* Read in the Nyquist frequency from FITS component */
    smf_fits_getD(inData->hdr, "FNYQUIST", &fNyquist, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to find the Nyquist frequency in FITS component!", status);
      goto CLEANUP;
    }

    /* Read in the evenly spaced OPD step size from FITS component */
    smf_fits_getD(inData->hdr, "OPDSTEP", &dz, status);
    if(*status != SAI__OK) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "Unable to find the OPDSTEP size in FITS component!", status);
        goto CLEANUP;
    }

    /* Data cube dimensions */
    nWidth  = inData->dims[0];
    nHeight = inData->dims[1];
    nFrames = inData->dims[2];
    nFrames2= nFrames / 2;
    nPixels = nWidth * nHeight;
    wnL = (size_t) (nFrames2 * wnLower / fNyquist);
    wnU = (size_t) (nFrames2 * wnUpper / fNyquist);

    dSigma = fNyquist / nFrames2;      /* Spectral sampling interval */

    /* Copy input data into output data */
    outData = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outData->dtype   = SMF__DOUBLE;
    outData->ndims   = 3;
    outData->dims[0] = nWidth;
    outData->dims[1] = nHeight;
    outData->dims[2] = nFrames;
    outData_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outData_pntr));
    outData->pntr[0] = outData_pntr;

    /* DEBUG: Process default or alternative output type */
#if DEBUG
    /* Copy input data into output data Interferogram Digitally Filtered Real DEBUG */
    outDataIFGDFR = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataIFGDFR->dtype   = SMF__DOUBLE;
    outDataIFGDFR->ndims   = 3;
    outDataIFGDFR->dims[0] = nWidth;
    outDataIFGDFR->dims[1] = nHeight;
    outDataIFGDFR->dims[2] = nFrames;
    outDataIFGDFR_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataIFGDFR_pntr));
    outDataIFGDFR->pntr[0] = outDataIFGDFR_pntr;

    /* Copy input data into output data Interferogram Digitally Filtered Imaginary DEBUG */
    outDataIFGDFI = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataIFGDFI->dtype   = SMF__DOUBLE;
    outDataIFGDFI->ndims   = 3;
    outDataIFGDFI->dims[0] = nWidth;
    outDataIFGDFI->dims[1] = nHeight;
    outDataIFGDFI->dims[2] = nFrames;
    outDataIFGDFI_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataIFGDFI_pntr));
    outDataIFGDFI->pntr[0] = outDataIFGDFI_pntr;

    /* Copy input data into output data Spectrum Real DEBUG */
    outDataSR = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataSR->dtype   = SMF__DOUBLE;
    outDataSR->ndims   = 3;
    outDataSR->dims[0] = nWidth;
    outDataSR->dims[1] = nHeight;
    outDataSR->dims[2] = nFrames;
    outDataSR_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataSR_pntr));
    outDataSR->pntr[0] = outDataSR_pntr;

    /* Copy input data into output data Spectrum Imaginary DEBUG */
    outDataSI = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataSI->dtype   = SMF__DOUBLE;
    outDataSI->ndims   = 3;
    outDataSI->dims[0] = nWidth;
    outDataSI->dims[1] = nHeight;
    outDataSI->dims[2] = nFrames;
    outDataSI_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataSI_pntr));
    outDataSI->pntr[0] = outDataSI_pntr;

    /* Copy input data into output data Spectrum Phase DEBUG */
    outDataSP = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataSP->dtype   = SMF__DOUBLE;
    outDataSP->ndims   = 3;
    outDataSP->dims[0] = nWidth;
    outDataSP->dims[1] = nHeight;
    outDataSP->dims[2] = nFrames;
    outDataSP_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataSP_pntr));
    outDataSP->pntr[0] = outDataSP_pntr;

    /* Copy input data into output data Phase Correction Function Real DEBUG */
    outDataPCFR = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataPCFR->dtype   = SMF__DOUBLE;
    outDataPCFR->ndims   = 3;
    outDataPCFR->dims[0] = nWidth;
    outDataPCFR->dims[1] = nHeight;
    outDataPCFR->dims[2] = nFrames;
    outDataPCFR_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataPCFR_pntr));
    outDataPCFR->pntr[0] = outDataPCFR_pntr;

    /* Copy input data into output data Phase Correction Function Imaginary DEBUG */
    outDataPCFI = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataPCFI->dtype   = SMF__DOUBLE;
    outDataPCFI->ndims   = 3;
    outDataPCFI->dims[0] = nWidth;
    outDataPCFI->dims[1] = nHeight;
    outDataPCFI->dims[2] = nFrames;
    outDataPCFI_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataPCFI_pntr));
    outDataPCFI->pntr[0] = outDataPCFI_pntr;

    /* Copy input data into output data Wave Numbers DEBUG */
    outDataSPF = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataSPF->dtype   = SMF__DOUBLE;
    outDataSPF->ndims   = 3;
    outDataSPF->dims[0] = nWidth;
    outDataSPF->dims[1] = nHeight;
    outDataSPF->dims[2] = nFrames2;
    outDataSPF_pntr = (double*) astMalloc((nPixels * nFrames2) * sizeof(*outDataSPF_pntr));
    outDataSPF->pntr[0] = outDataSPF_pntr;

    /* Copy input data into output data Wave Numbers DEBUG */
    outDataWN = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataWN->dtype   = SMF__DOUBLE;
    outDataWN->ndims   = 3;
    outDataWN->dims[0] = nWidth;
    outDataWN->dims[1] = nHeight;
    outDataWN->dims[2] = nFrames2;
    outDataWN_pntr = (double*) astMalloc((nPixels * nFrames2) * sizeof(*outDataWN_pntr));
    outDataWN->pntr[0] = outDataWN_pntr;

    /* Copy input data into output data Weights DEBUG */
    outDataWT = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataWT->dtype   = SMF__DOUBLE;
    outDataWT->ndims   = 3;
    outDataWT->dims[0] = nWidth;
    outDataWT->dims[1] = nHeight;
    outDataWT->dims[2] = nFrames2;
    outDataWT_pntr = (double*) astMalloc((nPixels * nFrames2) * sizeof(*outDataWT_pntr));
    outDataWT->pntr[0] = outDataWT_pntr;

    /* Copy input data into output data Spectrum Real phase Corrected DEBUG */
    outDataSRC = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataSRC->dtype   = SMF__DOUBLE;
    outDataSRC->ndims   = 3;
    outDataSRC->dims[0] = nWidth;
    outDataSRC->dims[1] = nHeight;
    outDataSRC->dims[2] = nFrames;
    outDataSRC_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataSRC_pntr));
    outDataSRC->pntr[0] = outDataSRC_pntr;

    /* Copy input data into output data Spectrum Imaginary phase Corrected DEBUG */
    outDataSIC = smf_deepcopy_smfData(inData, 0, SMF__NOCREATE_DATA | SMF__NOCREATE_FTS, 0, 0, status);
    outDataSIC->dtype   = SMF__DOUBLE;
    outDataSIC->ndims   = 3;
    outDataSIC->dims[0] = nWidth;
    outDataSIC->dims[1] = nHeight;
    outDataSIC->dims[2] = nFrames;
    outDataSIC_pntr = (double*) astMalloc((nPixels * nFrames) * sizeof(*outDataSIC_pntr));
    outDataSIC->pntr[0] = outDataSIC_pntr;
#endif

    /* MORE.FTS2.ZPD */
    zpd = smf_deepcopy_smfData(inData->fts->zpd, 0, SMF__NOCREATE_FTS, 0, 0, status);
    /* MORE.FTS2.FPM, Polynomial fit coefficients */
    fpm = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = nWidth;
    fpm->dims[1] = nHeight;
    fpm->dims[2] = coeffLength;
    fpm->pntr[0] = (double*) astCalloc( (nPixels * coeffLength), sizeof(double));
    /* MORE.FTS2.SIGMA, STANDARD DEVIATIONS */
    sigma = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = nWidth;
    sigma->dims[1] = nHeight;
    sigma->pntr[0] = (double*) astCalloc(nPixels, sizeof(double));

    /* Allocate memory for arrays */
    IFG     = astCalloc(nFrames, sizeof(*IFG));
    DS      = astCalloc(nFrames, sizeof(*DS));
    PHASE   = astCalloc(nFrames, sizeof(*PHASE));
#if DEBUG
    PHASES  = astCalloc(nFrames, sizeof(*PHASE));    /* DEBUG PHASE Saved */
#endif
    COEFFS  = astCalloc(coeffLength, sizeof(*COEFFS));
    WN      = astCalloc((nFrames2 + 1), sizeof(*WN));
    WEIGHTS = astCalloc((nFrames2 + 1), sizeof(*WEIGHTS));
    FIT     = astCalloc((nFrames2 + 1), sizeof(*FIT));
#if DEBUG
    FITS    = astCalloc((nFrames2 + 1), sizeof(*FITS));
#endif
    TMPPHASE= astCalloc((nFrames2 + 1), sizeof(*TMPPHASE));
    DSIN    = fftw_malloc(nFrames * sizeof(*DSIN));
    DSOUT   = fftw_malloc(nFrames * sizeof(*DSOUT));
    IFGDF   = fftw_malloc(nFrames * sizeof(*IFGDF));
    PCF     = fftw_malloc(nFrames * sizeof(*PCF));
    SPEC    = fftw_malloc(nFrames * sizeof(*SPEC));
#if DEBUG
    SPECS   = fftw_malloc(nFrames * sizeof(*SPECS));
#endif

    /* Apply phase correction to interferograms at each pixel */
    for(i = 0; i < nWidth; i++) {
      for(j = 0; j < nHeight; j++) {
        bolIndex = i + j * nWidth;

        /* Get ZPD index */
        /*indexZPD = *((int*)(zpdData->pntr[0]) + bolIndex);*/
        /* The IFG is now supposed to be centered on its evenly spaced grid */
        indexZPD = nFrames2;

        /* Check if the interferogram is inverted */
        W = 1;    /* (*((double*) (inData->pntr[0]) + (bolIndex + nPixels * indexZPD)) < 0.0) ? -1 : 1; */

        badPixel = 0;
        peakIFGIndex = 0;
        peakIFG = 0.0;
        /* Read in the interferogram, invert if flipped */
        for(k=0; k < nFrames; k++) {
          IFG[k] = W * (*((double*) (inData->pntr[0]) + (bolIndex + nPixels * k)));

          /* See if this is a bad pixel */
          if(IFG[k] == VAL__BADD) {
            badPixel = 1;
            break;
          }
        }

        /* If this is a bad pixel, go to next */
        if(badPixel) {
          for(k = 0; k < nFrames; k++) {
            index = bolIndex + k * nPixels;
            outData_pntr[index] = VAL__BADD;
#if DEBUG
                    outDataIFGDFR_pntr[index] = VAL__BADD;  /* DEBUG */
                    outDataIFGDFI_pntr[index] = VAL__BADD;  /* DEBUG */
                    outDataSR_pntr[index] = VAL__BADD;      /* DEBUG */
                    outDataSI_pntr[index] = VAL__BADD;      /* DEBUG */
                    outDataSRC_pntr[index] = VAL__BADD;     /* DEBUG */
                    outDataSIC_pntr[index] = VAL__BADD;     /* DEBUG */
                    outDataSP_pntr[index] = VAL__BADD;      /* DEBUG */
                    outDataPCFR_pntr[index] = VAL__BADD;    /* DEBUG */
                    outDataPCFI_pntr[index] = VAL__BADD;    /* DEBUG */
                    if(k < nFrames2) {
                        outDataSPF_pntr[index] = VAL__BADD; /* DEBUG */
                        outDataWN_pntr[index] = VAL__BADD;  /* DEBUG */
                        outDataWT_pntr[index] = VAL__BADD;  /* DEBUG */
                    }
#endif
          }
          continue;
        }

        /* Shift the left half of the interferogram to the end of the right half */
        /* i.e.,  --/\--  ->  \----/  */
        for(k = indexZPD; k < nFrames; k++) { DS[k - indexZPD] = IFG[k]; }
        for(k = 0; k < indexZPD; k++)       { DS[nFrames - indexZPD + k] = IFG[k]; }

        /* Convert real-valued interferogram to complex-valued interferogram */
        for(k = 0; k < nFrames; k++) { DSIN[k][0] = DS[k]; DSIN[k][1] = 0.0; }

        /* FFT Double-sided complex-valued interferogram */
        planA = fftw_plan_dft_1d(nFrames, DSIN, DSOUT, FFTW_FORWARD, FFTW_ESTIMATE);
        fftw_execute(planA);

        /* Compute wavenumbers within [0, FNYQ] */
        for(k = 0; k <= nFrames2; k++) { WN[k] = k * dSigma; }

        /* DF: Trim (zero out) the first wnTrim wavenumbers of the spectrum (on both ends)
        for(k = 0; k <= wnTrim/dSigma; k++) {
            DSOUT[k][0] = 0.0;
            DSOUT[k][1] = 0.0;
            DSOUT[k+1][0] = 0.0;             Maintain an odd number of adjustments
            DSOUT[k+1][1] = 0.0;
            DSOUT[nFrames-1-k][0] = 0.0;
            DSOUT[nFrames-1-k][1] = 0.0;
        }*/

        /* DF: Apodize spectrum */
        for(k = 0; k <= nFrames2; k++) {
            DSOUT[k][0] = DSOUT[k][0] * (1 - exp( -(WN[k] * WN[k]) / (0.36 * FWHM * FWHM) ));
            DSOUT[k][1] = DSOUT[k][1] * (1 - exp( -(WN[k] * WN[k]) / (0.36 * FWHM * FWHM) ));
        }
        for(k=nFrames-1; k > nFrames2; k--) {
            DSOUT[k][0] = DSOUT[k][0] * (1 - exp( -(WN[nFrames-k] * WN[nFrames-k]) / (0.36 * FWHM * FWHM) ));
            DSOUT[k][1] = DSOUT[k][1] * (1 - exp( -(WN[nFrames-k] * WN[nFrames-k]) / (0.36 * FWHM * FWHM) ));
        }

        /* DF: Inverse FFT spectrum to get the back interferogram */
        planB = fftw_plan_dft_1d(nFrames, DSOUT, IFGDF, FFTW_BACKWARD, FFTW_ESTIMATE);
        fftw_execute(planB);

        /* DF: Normalize the reverse FFT interferogram back to original scale */
        for(k=0; k<nFrames; k++) {
            IFGDF[k][0] /= nFrames;
            IFGDF[k][1] /= nFrames;
        }

        /* DF: Zero out kernel length area from both ends of IFG from the middle now, since it was shifted! */
        for(k = 0; k*dSigma <= KERNEL_LENGTH; k++) {
            IFGDF[nFrames2+k][0] = 0.0;
            IFGDF[nFrames2+k][1] = 0.0;
            IFGDF[nFrames2-1-k][0] = 0.0;
            IFGDF[nFrames2-1-k][1] = 0.0;
        }

        /* DF: If index of IFG peak is more than IFG_PEAK_TRESHOLD from ZPD, flag as problematic */
        for(k=0; k<nFrames; k++) {
            /* DF: Track IFG peak index */
            if(peakIFG < fabs(IFGDF[k][0])) {
                peakIFG = fabs(IFGDF[k][0]);
                peakIFGIndex = k;
            }
        }
        /* DF: TODO: Save problem flagged pixels to a bolometer mask array */
        /* DF: TODO: Make peak test more robust so as to handle noisy data better */
        if(!badPixel && abs(indexZPD - peakIFGIndex) * dz > IFG_PEAK_THRESHOLD){  /* TODO: use dSigma instead of dz? */
            /*badPixel = 1;*/
            /*printf("%s DEBUG: Flagging bad pixel[%d,%d] having peak: %f at index %d too far (%f cm) from ZPD index: %d\n",
                   TASK_NAME, i, j, peakIFG, peakIFGIndex, (abs(indexZPD - peakIFGIndex) * dz), indexZPD);*/
        } else if(!badPixel) {
            /*printf("%s DEBUG: Leaving good pixel[%d,%d] having peak: %f at index %d this far (%f cm) from ZPD index: %d\n",
                   TASK_NAME, i, j, peakIFG, peakIFGIndex, (abs(indexZPD - peakIFGIndex) * dz), indexZPD);*/
        }

        /* FFT interferogram */
        planA = fftw_plan_dft_1d(nFrames, IFGDF, DSOUT, FFTW_FORWARD, FFTW_ESTIMATE);
        fftw_execute(planA);

        /* Compute phase */
        phaseDiff = 0.0;
        phaseRollover = 0.0;
        for(k = 0; k < nFrames; k++) {
          PHASE[k] = atan2(DSOUT[k][1], DSOUT[k][0]);

          /* Fix any phase rollover in the band... */
          /* If consecutive phase values in the band differ by more than PHASE_ROLLOVER_THRESHOLD (in radians)
             then subtract the discontinuity to maintain the slope from that point forward in the band        */
          if(k*dSigma >= wnLower && k*dSigma <= wnUpper) {
              phaseDiff = PHASE[k]-PHASE[k-1];
              if(fabs(phaseDiff) > PHASE_ROLLOVER_THRESHOLD) {
              /*printf("%s DEBUG: Pixel [%d,%d] Phase Rollover Detected at %f cm: PHASE[%d] (%f) - PHASE[%d] (%f) = %f \n",
                       TASK_NAME, i, j, k*dSigma, k, PHASE[k], k-1, PHASE[k-1], PHASE[k]-PHASE[k-1]); */
                if(phaseDiff > 0) {
                    phaseRollover = -M_PI * 2;
                } else {
                    phaseRollover =  M_PI * 2;
                }
                PHASE[k] += phaseRollover;
                /*printf("%s DEBUG: Pixel [%d,%d] Phase Rollover Corrected at %f cm: PHASE[%d] (%f) - PHASE[%d] (%f) = %f \n",
                       TASK_NAME, i, j, k*dSigma, k, PHASE[k], k-1, PHASE[k-1], PHASE[k]-PHASE[k-1]);*/
              } else {
                  /*printf("%s DEBUG: Pixel [%d,%d] at %f cm: PHASE[%d] (%f) - PHASE[%d] (%f) = %f \n",
                         TASK_NAME, i, j, k*dSigma, k, PHASE[k], k-1, PHASE[k-1], PHASE[k]-PHASE[k-1]);*/
              }
          }

#if DEBUG
          PHASES[k] = PHASE[k];    /* DEBUG */
#endif
        }

        /* Compute weighting factors [0, FNYQ] */
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

        /* Polynomial fit to phase */
        for(k = 0; k <= nFrames2; k++) { TMPPHASE[k] = PHASE[k]; }
        smf_fit_poly1d(pDegree, nFrames2, CLIP, WN, TMPPHASE, WEIGHTS, NULL, COEFFS, NULL, FIT, &nUsed, status);
        /* printf("%s DEBUG: smf_fit_poly1d: pDegree=%d, nelem=%d, CLIP=%f, COEFFS[0]=%f, COEFFS[1]=%f, i=%d, j=%d\n",
               TASK_NAME, pDegree, (nFrames2+1), CLIP, COEFFS[0], COEFFS[1], i, j); */
#if DEBUG
        for(k = 0; k <= nFrames2; k++) { FITS[k] = FIT[k]; }    /* DEBUG */
#endif

        /* TODO: If the error on the fit is beyond certain thresholds, flag the pixel as problematic... */

        /* Update MORE.FTS2.SIGMA values */
        sum   = 0.0;
        error = 0.0;
        for(k = 0; k <= nFrames2; k++) {
          error += WEIGHTS[k] * (PHASE[k] - FIT[k]) * (PHASE[k] - FIT[k]);
          sum   += WEIGHTS[k];
        }
        *((double*)(sigma->pntr[0]) + bolIndex) = sqrt(error / sum);

        /* Update MORE.FTS2.FPM values */
        for(k = 0; k < coeffLength; k++) { *((double*) (fpm->pntr[0]) + (bolIndex + nPixels * k)) = COEFFS[k]; }

        /* Polynomial Fit */
        for(k = 0; k < nFrames2; k++) { EVALPOLY(PHASE[k], WN[k], pDegree, COEFFS); }
        for(k = 1; k < nFrames2; k++) { PHASE[nFrames2 + k] = -PHASE[nFrames2 - k]; }       /* PHASE(-k) = -PHASE(k) */
        PHASE[0] = PHASE[nFrames2] = 0.0;

        /* Compute phase correction function, PCF, exp(-i * phase) */
        for(k = 0; k < nFrames; k++) {
          PCF[k][0] =  cos(PHASE[k]);
          PCF[k][1] = -sin(PHASE[k]);
        /* printf("%s DEBUG: PCF[k][0]=%f, PCF[k][1]=%f\n", TASK_NAME, PCF[k][0], PCF[k][1]); */
        }

        /* Multiplication in frequency domain */
        for(k = 0; k < nFrames; k++) {
          SPEC[k][0] = DSOUT[k][0] * PCF[k][0] - DSOUT[k][1] * PCF[k][1];
          SPEC[k][1] = DSOUT[k][0] * PCF[k][1] + DSOUT[k][1] * PCF[k][0];
#if DEBUG
          SPECS[k][0] = SPEC[k][0];    /* Debug */
          SPECS[k][1] = SPEC[k][1];    /* Debug */
#endif
          /* We should just zero out the imaginary part of the spectrum since all that is left there is noise */
          SPEC[k][1] = 0.0;    /* Debug */
        }

        /* Inverse FFT spectrum to get the phase corrected interferogram */
        planB = fftw_plan_dft_1d(nFrames, SPEC, SPEC, FFTW_BACKWARD, FFTW_ESTIMATE);
        fftw_execute(planB);

        /* Phase corrected interferogram */
        M = indexZPD;
        /* Adjust for an even number of points by shifting data to the left by one position */
        for(k = 0; k < M-1; k++) { IFG[k] = SPEC[M+k+1][0]; }
        for(k = M-1; k < nFrames; k++) { IFG[k] = SPEC[k+1-M][0]; }

        /* Update output */
        for(k = 0; k < nFrames; k++) {
          index = bolIndex + nPixels * k;
          outData_pntr[index] = IFG[k] / nFrames;
#if DEBUG
          outDataIFGDFR_pntr[index] = IFGDF[k][0] / nFrames;    /* DEBUG */
          outDataIFGDFI_pntr[index] = IFGDF[k][1] / nFrames;    /* DEBUG */
          outDataSR_pntr[index] = DSOUT[k][0] / nFrames;        /* DEBUG */
          outDataSI_pntr[index] = DSOUT[k][1] / nFrames;        /* DEBUG */
          outDataSRC_pntr[index] = SPECS[k][0] / nFrames;       /* DEBUG */
          outDataSIC_pntr[index] = SPECS[k][1] / nFrames;       /* DEBUG */
          outDataSP_pntr[index] = PHASES[k];                    /* DEBUG */
          outDataPCFR_pntr[index] = PCF[k][0];                  /* DEBUG */
          outDataPCFI_pntr[index] = PCF[k][1];                  /* DEBUG */
          if(k < nFrames2) {
             outDataSPF_pntr[index] = FITS[k];                  /* DEBUG */
             outDataWN_pntr[index] = WN[k];                     /* DEBUG */
             outDataWT_pntr[index] = WEIGHTS[k];                /* DEBUG */
          }
#endif
        }
      }
    }

    /* Deallocate memory used by arrays */
    if(IFG)      { IFG      = astFree(IFG); }
    if(DS)       { DS       = astFree(DS); }
    if(PHASE)    { PHASE    = astFree(PHASE); }
#if DEBUG
    if(PHASES)   { PHASES   = astFree(PHASES); }    /* DEBUG PHASE Saved */
#endif
    if(COEFFS)   { COEFFS   = astFree(COEFFS); }
    if(TMPPHASE) { TMPPHASE = astFree(TMPPHASE); }
    if(WN)       { WN       = astFree(WN); }
    if(WEIGHTS)  { WEIGHTS  = astFree(WEIGHTS); }
    if(FIT)      { FIT      = astFree(FIT); }
#if DEBUG
    if(FITS)     { FITS     = astFree(FITS); }
#endif
    if(DSIN)     { fftw_free(DSIN);           DSIN      = NULL; }
    if(DSOUT)    { fftw_free(DSOUT);          DSOUT     = NULL; }
    if(IFGDF)    { fftw_free(IFGDF);          IFGDF     = NULL; }
    if(PCF)      { fftw_free(PCF);            PCF       = NULL; }
    if(SPEC)     { fftw_free(SPEC);           SPEC      = NULL; }
#if DEBUG
    if(SPECS)    { fftw_free(SPECS);          SPECS     = NULL; }
#endif

    /* Create a temporary base file name from input file name - DEBUG */
    one_strlcpy(fileName, inData->file->name,
        astMIN(SMF_PATH_MAX + 1, strlen(inData->file->name) - 2), status);
    if (*status == ONE__TRUNC) {
      errAnnul(status);
    }

    /* Close the file */
    if(inData) { smf_close_file(&inData, status); }
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing inData file %s", status, inData->file->name);
        goto CLEANUP;
    }

#if DEBUG
    /* Write output Interferogram Digitally Filtered Real DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataIFGDFR->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "IFGDFR");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataIFGDFR->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataIFGDFR, NULL, outDataIFGDFR->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataIFGDFR file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataIFGDFR, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataIFGDFR file", status);
        goto CLEANUP;
    }

    /* Write output Interferogram Digitally Filtered Imaginary DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataIFGDFI->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "IFGDFI");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataIFGDFI->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataIFGDFI, NULL, outDataIFGDFI->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataIFGDFI file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataIFGDFI, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataIFGDFI file", status);
        goto CLEANUP;
    }

    /* Write output Spectrum Real DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataSR->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "SR");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataSR->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataSR, NULL, outDataSR->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataSR file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataSR, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataSR file", status);
        goto CLEANUP;
    }

    /* Write output Spectrum Imaginary DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataSI->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "SI");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataSI->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataSI, NULL, outDataSI->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataSI file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataSI, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataSI file", status);
        goto CLEANUP;
    }

    /* Write output Spectrum Real Phase Corrected DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataSRC->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "SRC");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataSRC->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataSRC, NULL, outDataSRC->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataSRC file", status);
        goto CLEANUP;
    }
    /* printf("%s DEBUG: Closing outFileNameSRC=%s\n", TASK_NAME, outDataSRC->file->name); */
    smf_close_file(&outDataSRC, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataSRC file", status);
        goto CLEANUP;
    }

    /* Write output Spectrum Imaginary Phase Corrected DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataSIC->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "SIC");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataSIC->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataSIC, NULL, outDataSIC->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataSIC file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataSIC, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataSIC file", status);
        goto CLEANUP;
    }

    /* Write output Spectrum Phase DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataSP->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "SP");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataSP->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataSP, NULL, outDataSP->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataSP file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataSP, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataSP file", status);
        goto CLEANUP;
    }

    /* Write output Phase Correction Function Real DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataPCFR->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "PCFR");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataPCFR->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataPCFR, NULL, outDataPCFR->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataPCFR file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataPCFR, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataPCFR file", status);
        goto CLEANUP;
    }

    /* Write output Phase Correction Function Imaginary DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataPCFI->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "PCFI");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataPCFI->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataPCFI, NULL, outDataPCFI->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataPCFI file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataPCFI, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataPCFI file", status);
        goto CLEANUP;
    }

    /* Write output Fitted Phase DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataSPF->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "SPF");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataSPF->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataSPF, NULL, outDataSPF->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataSPF file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataSPF, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataSPF file", status);
        goto CLEANUP;
    }

    /* Write output Wave Numbers DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataWN->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "WN");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataWN->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataWN, NULL, outDataWN->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataWN file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataWN, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataWN file", status);
        goto CLEANUP;
    }

    /* Write output Weights DEBUG */
    /* Append unique suffix to fileName */
    n = one_snprintf(outDataWT->file->name, SMF_PATH_MAX, "%sphs_%s", status, fileName, "WT");
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outDataWT->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outDataWT, NULL, outDataWT->file->name, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outDataWT file", status);
        goto CLEANUP;
    }
    smf_close_file(&outDataWT, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outDataWT file", status);
        goto CLEANUP;
    }

#endif
    /* Write output */
    outData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, status);
    n = one_snprintf(outData->file->name, SMF_PATH_MAX, "%sphs", status, fileName);
    if(n < 0 || n >= SMF_PATH_MAX) {
        errRepf(TASK_NAME, "Error creating outData->file->name", status);
        goto CLEANUP;
    }
    smf_write_smfData(outData, NULL, NULL, gOut, fIndex, 0, MSG__VERB, 0, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error writing outData file", status);
        goto CLEANUP;
    }
    smf_close_file(&outData, status);
    if(*status != SAI__OK) {
        errRepf(TASK_NAME, "Error closing outData file", status);
        goto CLEANUP;
    }

  }

  CLEANUP:
  /* Deallocate memory used by arrays */
  if(IFG)      { IFG      = astFree(IFG); }
  if(DS)       { DS       = astFree(DS); }
  if(PHASE)    { PHASE    = astFree(PHASE); }
#if DEBUG
  if(PHASES)   { PHASES   = astFree(PHASES); }    /* DEBUG PHASE Saved */
#endif
  if(COEFFS)   { COEFFS   = astFree(COEFFS); }
  if(TMPPHASE) { TMPPHASE = astFree(TMPPHASE); }
  if(WN)       { WN       = astFree(WN); }
  if(WEIGHTS)  { WEIGHTS  = astFree(WEIGHTS); }
  if(FIT)      { FIT      = astFree(FIT); }
#if DEBUG
  if(FITS)     { FITS     = astFree(FITS); }
#endif
  if(DSIN)     { fftw_free(DSIN);   DSIN  = NULL; }
  if(DSOUT)    { fftw_free(DSOUT);  DSOUT = NULL; }
  if(IFGDF)    { fftw_free(IFGDF);  IFGDF = NULL; }
  if(PCF)      { fftw_free(PCF);    PCF   = NULL; }
  if(SPEC)     { fftw_free(SPEC);   SPEC  = NULL; }
#if DEBUG
  if(SPECS)    { fftw_free(SPECS);  SPECS = NULL; }
#endif

  /* Close files if still open */
  if(inData) {
    smf_close_file(&inData, status);
    if(*status != SAI__OK)
        errRepf(TASK_NAME, "Error closing inData file", status);
  }
  /* END NDF */
  ndfEnd(status);

  /* Delete Groups */
  grpDelet(&gIn, status);
  grpDelet(&gOut, status);

}
