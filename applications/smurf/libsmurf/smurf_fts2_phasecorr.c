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
*          Double-Sided interferogram half length.
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     PFHALFLENGTH = _INTEGER (Read)
*          Phase function half length.
*     SSHALFLENGTH = _INTEGER (Read)
*          Single-Sided interferogram half length.
*     WEIGHTLIMIT = _DOUBLE (Read)
*          Weighting factor limit.
*     WNLBOUND = _DOUBLE (Read)
*          The lower bound of the wavenumber range (as percentage)
*     WNUBOUND = _DOUBLE (Read)
*          The upper bound of the wavenumber range (as percentage)

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2010-09-17 (COBA):
*        Original version.
*     2010-09-21 (COBA):
*        Updated prologue with ADAM params
*     2010-10-01 (COBA):
*        - Replaced single ZPD value input with 2D ZPD array input
*        - FFTW does NOT need suitable ssHalfLength and dsHalfLength lengths
*     2010-10-15 (COBA):
*        - Removed explicit casts
*     2010-11-26 (COBA):
*        - Read ZPD index from smfFts->zpd insted of ZPD calibration file
*     2010-11-26 (COBA):
*        - Read ZPD index from smfFts->zpd insted of ZPD calibration file

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

#define FUNC_NAME "smurf_fts2_phasecorr"
#define TASK_NAME "FTS2PHASECORR"

void smurf_fts2_phasecorr(int* status)
{
  if(*status != SAI__OK) { return; }

  int bolIndex              = 0;    /* Current bolometer index */
  int coeffLength           = 0;    /* Number of polynomial coefficients */
  int dsHalfLength          = 0;    /* Size of the double sided interferogram */
  int index                 = 0;    /* Index */
  int i                     = 0;    /* Loop counter */
  int j                     = 0;    /* Loop counter */
  int k                     = 0;    /* Loop counter */
  int newN                  = 0;    /* Time series length of the output data */
  int numBol                = 0;    /* Number of bolometers in the subarray */
  int phaseFunctionHalfLength = 0;  /* Half-length of phase function */
  int polynomialDegree      = 0;    /* Degree of the polynomial used to fit */
  int ssHalfLength          = 0;    /* Size of the single sided interferogram */
  int phaseFunctionLength   = 0;    /* Phase correction function size */
  int phaseLength           = 0;    /* Phase length */
  int srcH                  = 0;    /* Height of the subarray */
  int srcW                  = 0;    /* Width of the subarray */
  int srcN                  = 0;    /* Time series length of the input data */
  int zpdIndex              = 0;    /* Index of ZPD */
  double stddev             = 0.0;  /* Standard deviation */
  double weightLimit        = 0.0;  /* Weighting factor limit */
  double wnFact             = 0.0;  /* Wavenumber factor */
  double wnLBoundPercent    = 0.0;  /* Lower bound(%) of wave number range */
  double wnUBoundPercent    = 0.0;  /* Upper bound(%) of wave number range */
  double* coefficients      = NULL; /* Coefficients of the polynomial */
  double* interferogram     = NULL; /* Single bolometer interferogram */
  double* phase             = NULL; /* Phase */
  double* newInterferogram  = NULL; /* Phase corrected interferogram */
  double* phaseFunction     = NULL; /* Phase correction function */
  float* fPositions         = NULL; /* Mirror positions */
  Grp* grpInput             = NULL; /* Input group */
  Grp* grpOutput            = NULL; /* Output group */
  size_t count              = 0;    /* Mirror positions count */
  size_t fIndex             = 0;    /* File loop counter */
  size_t inSize             = 0;    /* Size of the input group */
  size_t outSize            = 0;    /* Size of the output group */
  smfData* fpm              = NULL; /* Fitting params data */
  smfData* sigma            = NULL; /* smfFts standard deviation */
  smfData* srcData          = NULL; /* Pointer to input data */
  smfData* newSrcData       = NULL; /* Pointer to output data */
  smfData* zpd              = NULL; /* Pointer to ZPD data */
  smfData* zpdData          = NULL; /* Pointer to ZPD data */
  HDSLoc* hdsLoc            = NULL; /* Pointer to HDS location */
  HDSLoc* hdsLocPosition    = NULL; /* Pointer to mirror positions */
  void* srcCube             = NULL; /* Pointer to the input data cube */

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &inSize, status);
  kpg1Wgndf("OUT", grpOutput, inSize, inSize,
            "Equal number of input and output files expected!",
            &grpOutput, &outSize, status);

  // GET PARAMS
  parGet0i("DSHALFLENGTH", &dsHalfLength, status);
  parGet0i("SSHALFLENGTH", &ssHalfLength, status);
  parGet0i("DEGREE", &polynomialDegree, status);
  parGet0i("PFHALFLENGTH", &phaseFunctionHalfLength, status);
  parGet0d("WNLBOUND", &wnLBoundPercent, status);
  parGet0d("WNUBOUND", &wnUBoundPercent, status);
  parGet0d("WEIGHTLIMIT", &weightLimit, status);

  coeffLength = polynomialDegree + 1;

  // VERIFY WAVE NUMBER RANGES
  wnLBoundPercent = (wnLBoundPercent < 0.0) ? 0.0 :
                        (wnLBoundPercent > 1.0) ? 1.0 : wnLBoundPercent;
  wnUBoundPercent = (wnUBoundPercent < 0.0) ? 0.0 :
                        (wnUBoundPercent > 1.0) ? 1.0 : wnUBoundPercent;
  if(wnUBoundPercent <= wnLBoundPercent) {
    wnLBoundPercent = 0.0;
    wnUBoundPercent = 1.0;
  }

  // BEGIN NDF
  ndfBegin();

  // LOOP THROUGH EACH NDF FILE
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_file(grpInput, fIndex, "READ", 0, &srcData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
    }

    // ZPD
    if(srcData->fts && srcData->fts->zpd) {
      zpdData = srcData->fts->zpd;
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "The input file is NOT initialized!", status);
    }

    // GET DATA CUBE
    srcCube = srcData->pntr[0];
    srcW = srcData->dims[0];
    srcH = srcData->dims[1];
    srcN = srcData->dims[2];
    numBol = srcW * srcH;

    phaseLength = dsHalfLength + 1;
    phaseFunctionLength = (dsHalfLength <= phaseFunctionHalfLength) ?
                              dsHalfLength << 1 : phaseFunctionHalfLength << 1;
    phaseFunctionHalfLength = phaseFunctionLength >> 1;

    // WAVENUMBER FACTOR
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "READ", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    fPositions = astMalloc(count * sizeof(*fPositions));
    datGetVR(hdsLocPosition, count, fPositions, &count, status);
    wnFact = 1.0 / (fPositions[1] - fPositions[0]) / (2.0 * ssHalfLength);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }
    astFree(fPositions);
    smf_fits_updateD(srcData->hdr, "WNFACT", wnFact, "Wavenumber factor", status);

    // DATA CUBE SIZE WILL CHANGE, MAKE A DEEP COPY OF srcData
    newN = ssHalfLength + 1;
    newSrcData = smf_deepcopy_smfData( srcData, 0,
                                       SMF__NOCREATE_DATA |
                                       SMF__NOCREATE_FTS, 0, 0, status);
    newSrcData->dtype   = SMF__DOUBLE;
    newSrcData->ndims   = 3;
    newSrcData->dims[0] = srcW;
    newSrcData->dims[1] = srcH;
    newSrcData->dims[2] = newN;
    newSrcData->pntr[0] = (double*) astMalloc((numBol * newN) * sizeof(double));

    // ZPD
    zpd = smf_deepcopy_smfData(zpdData, 0, 0, 0, 0, status);

    // FPM (Polynomial Fit Coefficients)
    fpm = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = srcW;
    fpm->dims[1] = srcH;
    fpm->dims[2] = coeffLength;
    fpm->pntr[0] = (double*) astMalloc((numBol * coeffLength) * sizeof(double));

    // SIGMA (Standard Deviations)
    sigma = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = srcW;
    sigma->dims[1] = srcH;
    sigma->pntr[0] = (double*) astMalloc(numBol * sizeof(double));

    // APPLY PHASE CORRECTION TO EACH BOLOMETER INTERFEROGRAM
    interferogram    = astMalloc(srcN * sizeof(*interferogram));
    newInterferogram = astMalloc(newN * sizeof(*newInterferogram));
    coefficients     = astMalloc(coeffLength * sizeof(*coefficients));
    phase            = astMalloc(phaseLength * sizeof(*phase));
    phaseFunction    = astMalloc(phaseFunctionLength * sizeof(*phaseFunction));
    for(i = 0; i < srcH; i++) {
      for(j = 0; j < srcW; j++) {
        bolIndex = j + i * srcW;

        // GET INTERFEROGRAM FOR THE BOLOMETER
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          interferogram[k] = *((double*)srcCube + index);
        }

        // GET CORRESPONDING ZPD INDEX
        zpdIndex = *((int*)zpd->pntr[0] + bolIndex);

        // APPLY PHASE CORRECTION
        fts2_phasecorrection( interferogram, srcN, zpdIndex, dsHalfLength,
                              ssHalfLength, polynomialDegree,
                              phaseFunctionHalfLength,
                              wnLBoundPercent, wnUBoundPercent, weightLimit,
                              &stddev, coefficients,
                              phase, phaseFunction, newInterferogram,
                              status);

        // INSERT PHASE CORRECTED INTERFEROGRAM
        for(k = 0; k < newN; k++) {
          index = bolIndex + numBol * k;
          *((double*) (newSrcData->pntr[0]) + index) = newInterferogram[k];
        }

        // FPM & SIGMA
        *((double*)(sigma->pntr[0]) + bolIndex) = stddev;
        for(k = 0; k <= polynomialDegree; k++) {
          index = bolIndex + numBol * k;
          *((double*) (fpm->pntr[0]) + index) = coefficients[k];
        }
      }
    }
    astFree(interferogram);
    astFree(newInterferogram);
    astFree(coefficients);
    astFree(phase);
    astFree(phaseFunction);
    smf_close_file(&srcData, status);

    newSrcData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, status);
    smf_write_smfData(newSrcData, NULL, NULL, grpOutput, fIndex, 0, status);
    smf_close_file(&newSrcData, status);
  }

  CLEANUP:
    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    grpDelet(&grpInput, status);
    grpDelet(&grpOutput, status);
}
