/*
*+
*  Name:
*     FTS2SPECTRUM

*  Purpose:
*     Computes the spectrum of the interferograms.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_spectrum(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Computes the spectrum of the interferograms.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     ZEROPAD = LOGICAL (Read)
*          Determines whether to zeropad.
*     RESOLUTION = _INTEGER (Read)
*          Spectral Grid Resolution.
*          0 : Low Resolution
*          1 : Medium Resolution
*          Any other value : High Resolution
*          Default behaviour is the High Resolution

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2011-06-24 (COBA):
*        Original version.
*     2011-08-16 (COBA):
*        Add Zero-Padding.
*     2011-10-18 (COBA):
*        Add subarray check.

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
#include "star/one.h"

/* SMURF INCLUDES */
#include "smurflib.h"
#include "libsmf/smf.h"
#include "libsc2fts/fts2.h"

/* FFTW INCLUDES */
#include <fftw3.h>

#define FUNC_NAME "smurf_fts2_spectrum"
#define TASK_NAME "FTS2SPECTRUM"

void smurf_fts2_spectrum(int* status)
{
  if(*status != SAI__OK) { return; }

  const char*  dataLabel  = "Spectrum";

  // ADAM VARIABLES
  int       ZEROPAD       = 1;    // Determines whether to zeropad
  int       RESOLUTION    = 2;    // Spectral Resolution, 0=LOW, 1=MEDIUM, *=HIGH

  // INTERNAL VARIABLES
  Grp*      grpInput      = NULL; // Input group
  Grp*      grpOutput     = NULL; // Output group
  smfData*  inputData     = NULL; // Pointer to input data
  smfData*  zpdData       = NULL; // Pointer to ZPD data
  size_t    fileIndex     = 0;    // File loop counter
  size_t    numInputFile  = 0;    // Size of the input group
  size_t    numOutputFile = 0;    // Size of the output group
  int       srcH          = 0;    // Subarray Height
  int       srcW          = 0;    // Subarray Width
  int       srcN          = 0;    // Source interferogram length
  int       numBol        = 0;    // Number of bolometers in the subarray
  int       i             = 0;    // Row index
  int       j             = 0;    // Column index
  int       k             = 0;    // Frame index
  int       m             = 0;    // Helper index
  int       index         = 0;    // Helper index
  int       bolIndex      = 0;    // Bolometer index
  int       zpdIndex      = 0;    // Frame index at ZPD
  int       outN          = 0;    // Output spectrum length
  double    DSIGMA        = 0.0;  // Spectral Sampling Interval
  double*   IFG           = NULL; // Interferogram
  fftw_complex* SPEC      = NULL; // Spectrum
  fftw_plan plan 			    = NULL; // fftw plan
  sc2ast_subarray_t srcSubnum   = 0; // Source subarray ID

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &numInputFile, status);
  kpg1Wgndf("OUT", grpOutput, numInputFile, numInputFile,
            "Equal number of input and output files expected!",
            &grpOutput, &numOutputFile, status);

  // READ IN ADAM PARAMETERS
  parGet0i("ZEROPAD", &ZEROPAD, status);
  parGet0i("RESOLUTION", &RESOLUTION, status);

  switch(RESOLUTION) {
    case 0:
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Specified resolution is NOT supported!", status);
      goto CLEANUP;
      // DSIGMA = SMF__FTS2_LOWRES_SSI;
      // break;
    case 1:
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Specified resolution is NOT supported!", status);
      goto CLEANUP;
      // DSIGMA = SMF__FTS2_MEDRES_SSI;
      // break;
    default:
      DSIGMA = SMF__FTS2_HIGHRES_SSI;
      break;
  }

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
      errRep(FUNC_NAME, "The file is NOT initialized for FTS2 data reduction!", status);
      goto CLEANUP;
    }
    zpdData = inputData->fts->zpd;

    // GET SOURCE SUBARRAY ID
    smf_find_subarray(inputData->hdr, NULL, 0, &srcSubnum, status);
    if( srcSubnum == SC2AST__NULLSUB ||
        srcSubnum == S8A || srcSubnum == S8B ||
        srcSubnum == S4C || srcSubnum == S4D) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Source has invalid subarray ID!", status);
      goto CLEANUP;
    }

    // INPUT FILE DIMENSIONS
    srcW   = inputData->dims[0];
    srcH   = inputData->dims[1];
    srcN   = inputData->dims[2];
    numBol = srcW * srcH; // NUMBER OF BOLOMETERS IN THE SUBARRAY

    int zMin = NUM__MAXI; // MIN INDEX OF THE ZPD IN THE SUBARRAY
    for(i = 0; i < srcH; i++) {
      for(j = 0; j < srcW; j++) {
        bolIndex = i + j * srcH;
        int z = *((int*)(zpdData->pntr[0]) + bolIndex);
        if(z < zMin) { zMin = z; }
      }
    }

    // GET NYQUIST FREQUENCY
    double FNYQUIST = 0.0;
    smf_fits_getD(inputData->hdr, "FNYQUIST", &FNYQUIST, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to find the Nyquist frequency in FITS header!", status);
      goto CLEANUP;
    }

    // INTERFEROGRAM HALF-LENGTH
    int N2 = (ZEROPAD > 0) ? ceil(FNYQUIST / DSIGMA) : (srcN - zMin + 1);
    int N = N2 << 1;  // INTERFEROGRAM LENGTH
    outN = N2 + 1;    // SPECTRUM LENGTH

    // SAVE ACTUAL WAVENUMBER FACTOR (REQUIRES NO ZERO PADDING)
    smf_fits_updateD(inputData->hdr, "WNFACT", FNYQUIST / N2, "Wavenumber factor", status);

    // ALLOCATE MEMORY FOR THE ARRAYS
    IFG  = astMalloc(N * sizeof(*IFG));
	  SPEC = fftw_malloc(outN * sizeof(*SPEC));

    // OUTPUT SMFDATA
    smfData* outputData = smf_deepcopy_smfData(inputData, 0, SMF__NOCREATE_DATA, 0, 0, status);

    // SET DATA LABEL
    if (dataLabel) {
      one_strlcpy(outputData->hdr->dlabel, dataLabel, sizeof(outputData->hdr->dlabel), status );
    }

    // DATA ARRAY
    outputData->dtype   = SMF__DOUBLE;
    outputData->ndims   = 3;
    outputData->dims[0] = srcW;
    outputData->dims[1] = srcH;
    outputData->dims[2] = outN;
    outputData->pntr[0] = (double*) astMalloc((numBol * outN) * sizeof(double));

    // =========================================================================
    // LOOP THROUGH EACH PIXEL IN THE SUBARRAY
    // =========================================================================
    for(i = 0; i < srcH; i++) { // LOOP THROUGH ROWS
      for(j = 0; j < srcW; j++) { // LOOP THROUGH COLUMNS
        bolIndex = i + j * srcH;

        // GET ZPD INDEX
        zpdIndex = *((int*)(zpdData->pntr[0]) + bolIndex);

        // BUTTERFLIED INTERFEROGRAM
        for(k = 0; k <= N2; k++) {
          m = zpdIndex + k;
          index = bolIndex + numBol * m;
          IFG[k] = (m < srcN) ? *((double*)(inputData->pntr[0]) + index) : 0.0;
        }
        for(k = N2 + 1; k < N; k++) { IFG[k] = IFG[N - k]; }

        // FORWARD FFT INTERFEROGRAM
	      plan = fftw_plan_dft_r2c_1d(N, IFG, SPEC, FFTW_ESTIMATE);
	      fftw_execute(plan);

        // WRITE OUT THE REAL COMPONENT OF THE SPECTRUM
        // SUBARRAYS S8C AND S8D ARE COMPLEMENTARY TO S4A AND S4B, RESPECTIVELY.
        // THE INTERFEROGRAMS ARE FLIPPED AND HENCE THE SPECTRUM.
        if( srcSubnum == S8C || srcSubnum == S8D ) {
          for(k = 0; k < outN; k++) {
            index = bolIndex + numBol * k;
            *((double*)(outputData->pntr[0]) + index) = SPEC[k][0];
          }
        } else {
          for(k = 0; k < outN; k++) {
            index = bolIndex + numBol * k;
            *((double*)(outputData->pntr[0]) + index) = -SPEC[k][0];
          }
        }
      } // END FOR LOOP - COLUMNS
    } // END FOR LOOP - ROWS

    // CLOSE CURRENT FILE
    if(inputData) { smf_close_file(&inputData, status); }

    // FREE RESOURCES
    if(IFG) { astFree(IFG); IFG = NULL; }
	  if(SPEC){ fftw_free(SPEC); SPEC = NULL; }

    // WRITE OUTPUT
    smf_write_smfData(outputData, NULL, NULL, grpOutput, fileIndex, 0, MSG__VERB, status);
    smf_close_file(&outputData, status);

  } // END FOR LOOP - FILES

  CLEANUP:
    // CLOSE FILES
    if(inputData) { smf_close_file(&inputData, status); }

    // FREE RESOURCES
    if(IFG) { astFree(IFG); IFG = NULL; }
	  if(SPEC){ fftw_free(SPEC); SPEC = NULL; }
	  fftw_cleanup();

    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    grpDelet(&grpInput, status);
    grpDelet(&grpOutput, status);
}
