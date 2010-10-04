/*
*+
*  Name:
*     smurf_fts2_phasecorr.c

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
*     ZPDINDEX = _INTEGER (Read)
*          ZPD index.

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
#define TASK_NAME "FTS2_PHASECORR"

void smurf_fts2_phasecorr(int* status)
{
  if(*status != SAI__OK) { return; }

  char datatype[DAT__SZNAM + 1];    /* String for DATA/VARIANCE type */
  int coeffLength           = 0;    /* Number of polynomial coefficients */
  int dsHalfLength          = 0;    /* Size of the double sided interferogram */
  int fIndex                = 0;    /* File loop counter */
  int index                 = 0;    /* Index */
  int i                     = 0;    /* Loop counter */
  int ii                    = 0;    /* Loop counter */
  int j                     = 0;    /* Loop counter */
  int jj                    = 0;    /* Loop counter */
  int k                     = 0;    /* Loop counter */
  int kk                    = 0;    /* Loop counter */
  int newN                  = 0;    /* Time series length of the output data */
  int num                   = 0;    /* Temporary place holder */
  int pixelCount            = 0;    /* Number of bolometers in the subarray */
  int phaseFunctionHalfLength = 0;  /* Half-length of phase function */
  int polynomialDegree      = 0;    /* Degree of the polynomial used to fit */
  int srcSubarray           = 0;    /* Source sub-array */
  int ssHalfLength          = 0;    /* Size of the single sided interferogram */
  int phaseFunctionLength   = 0;    /* Phase correction function size */
  int phaseLength           = 0;    /* Phase length */
  int pixelIndex            = 0;    /* Current bolometer index */
  int srcHeight             = 0;    /* Height of the subarray */
  int srcWidth              = 0;    /* Width of the subarray */
  int srcN                  = 0;    /* Time series length of the input data */
  int tmp                   = 0;    /* Temporary place holder */
  int zpdHeight             = 0;    /* Height of the ZPD array */
  int zpdIndex              = 0;    /* Index of ZPD */
  int zpdSubarray           = 0;    /* ZPD sub-array */
  int zpdWidth              = 0;    /* Width of the ZPD array */
  double stddev             = 0.0;  /* Standard deviation */
  double weightLimit        = 0.0;  /* Weighting factor limit */
  double wnFact             = 0.0;  /* Wavenumber factor */
  double wnLBoundPercent    = 0.0;  /* Lower bound(%) of wave number range */
  double wnUBoundPercent    = 0.0;  /* Upper bound(%) of wave number range */
  double zpdValue           = 0.0;  /* ZPD value */
  double* coefficients      = NULL; /* Coefficients of the polynomial */
  double* interferogram     = NULL; /* Single bolometer interferogram */
  float* fPositions         = NULL; /* Mirror positions */
  double* phase             = NULL; /* Phase */
  double* newInterferogram  = NULL; /* Phase corrected interferogram */
  double* phaseFunction     = NULL; /* Phase correction function */
  Grp* igrp                 = NULL; /* Input group */
  Grp* ogrp                 = NULL; /* Output group */
  Grp* zpdgrp               = NULL; /* Output group */
  size_t count              = 0;    /* Mirror positions count */
  size_t insize             = 0;    /* Size of the input group */
  size_t outsize            = 0;    /* Size of the output group */
  size_t zpdsize            = 0;    /* Size of the ZPD group */
  smfData* fpm              = NULL; /* Fitting params data */
  smfData* sigma            = NULL; /* smfFts standard deviation */
  smfData* srcData          = NULL; /* Pointer to input data */
  smfData* newSrcData       = NULL; /* Pointer to output data */
  smfData* zpdData          = NULL; /* Pointer to ZPD data */
  HDSLoc* hdsLoc            = NULL; /* Pointer to HDS location */
  HDSLoc* hdsLocPosition    = NULL; /* Pointer to mirror positions */
  void* srcCube             = NULL; /* Pointer to the input data cube */
  void* zpdArray            = NULL; /* Pointer to 2D ZPD data values */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &insize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf("OUT", ogrp, insize, insize,
            "Equal number of input and output files expected!",
            &ogrp, &outsize, status);
  /* GET ZPD GROUP */
  kpg1Gtgrp("ZPD", &zpdgrp, &zpdsize, status);

  /* GET PARAMS */
  parGet0i("DSHALFLENGTH", &dsHalfLength, status);
  parGet0i("SSHALFLENGTH", &ssHalfLength, status);
  parGet0i("DEGREE", &polynomialDegree, status);
  parGet0i("PFHALFLENGTH", &phaseFunctionHalfLength, status);
  parGet0d("WNLBOUND", &wnLBoundPercent, status);
  parGet0d("WNUBOUND", &wnUBoundPercent, status);
  parGet0d("WEIGHTLIMIT", &weightLimit, status);

  /* VERIFY WAVE NUMBER RANGES */
  wnLBoundPercent = (wnLBoundPercent < 0.0) ? 0.0 :
                        (wnLBoundPercent > 1.0) ? 1.0 : wnLBoundPercent;
  wnUBoundPercent = (wnUBoundPercent < 0.0) ? 0.0 :
                        (wnUBoundPercent > 1.0) ? 1.0 : wnUBoundPercent;
  if(wnUBoundPercent <= wnLBoundPercent) {
    wnLBoundPercent = 0.0;
    wnUBoundPercent = 1.0;
  }

  coeffLength = polynomialDegree + 1;

  ndfBegin();

  /* GET ZPD */
  smf_open_file(zpdgrp, 1, "READ", SMF__NOCREATE_QUALITY, &zpdData, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to open the ZPD file!", status);
    goto CLEANUP;
  }
  if(zpdData->dtype == SMF__FLOAT) {
    zpdArray = (float*) (zpdData->pntr[0]);
  } else if(zpdData->dtype == SMF__DOUBLE) {
    zpdArray = (double*) (zpdData->pntr[0]);
  } else {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Invalid data type found!", status);
    smf_close_file(&zpdData, status);
    goto CLEANUP;
  }
  zpdWidth   = zpdData->dims[0];
  zpdHeight  = zpdData->dims[1];
  smf_find_subarray(zpdData->hdr, NULL, 0, &zpdSubarray, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to determine the ZPD subarray ID!", status);
    smf_close_file(&zpdData, status);
    goto CLEANUP;
  }

  /* LOOP THROUGH EACH NDF FILE IN THE GROUP */
  for(fIndex = 1; fIndex <= insize; fIndex++) {
    smf_open_file( ogrp, fIndex, "UPDATE",
                   SMF__NOCREATE_QUALITY |
                   SMF__NOCREATE_VARIANCE |
                   SMF__NOCREATE_DA |
                   SMF__NOCREATE_FTS,
                   &srcData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    if(srcData->dtype == SMF__FLOAT) {
      srcCube = (float*) (srcData->pntr[0]);
    } else if(srcData->dtype == SMF__DOUBLE) {
      srcCube = (double*) (srcData->pntr[0]);
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid data type found!", status);
      smf_close_file(&srcData, status);
      break;
    }
    srcWidth   = srcData->dims[0];
    srcHeight  = srcData->dims[1];
    srcN       = srcData->dims[2];
    pixelCount = srcWidth * srcHeight;

    smf_find_subarray(srcData->hdr, NULL, 0, &srcSubarray, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to determine the source subarray ID!", status);
      smf_close_file(&srcData, status);
      break;
    }
    if( zpdSubarray != srcSubarray ||
        zpdWidth != srcWidth || zpdHeight != srcHeight) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible subarray found!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* WAVENUMBER FACTOR */
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "UPDATE", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    fPositions = astMalloc(count * sizeof(*fPositions));
    datGetVR(hdsLocPosition, count, fPositions, &count, status);
    wnFact = 1.0 / (fPositions[1] - fPositions[0]) / (2.0 * ssHalfLength);
    smf_fits_updateD( srcData->hdr, "WNFACT", wnFact,
                      "Wavenumber factor", status);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }

    phaseLength = dsHalfLength + 1;
    phaseFunctionLength = (dsHalfLength <= phaseFunctionHalfLength) ?
                              dsHalfLength << 1 : phaseFunctionHalfLength << 1;
    phaseFunctionHalfLength = phaseFunctionLength >> 1;

    /* DATA CUBE SIZE WILL CHANGE, MAKE A DEEP COPY OF srcData */
    newN = ssHalfLength + 1;
    newSrcData = smf_deepcopy_smfData( srcData, 0,
                                       SMF__NOCREATE_DATA |
                                       SMF__NOCREATE_FTS, 0, 0, status);
    newSrcData->dtype   = SMF__DOUBLE;
    newSrcData->ndims   = 3;
    newSrcData->dims[0] = srcWidth;
    newSrcData->dims[1] = srcHeight;
    newSrcData->dims[2] = newN;
    newSrcData->pntr[0] = (double*) astMalloc( pixelCount * newN *
                                               sizeof(double));

    /* FPM (Polynomial Fit Coefficients) */
    fpm = smf_create_smfData( SMF__NOCREATE_DA |
                              SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = srcWidth;
    fpm->dims[1] = srcHeight;
    fpm->dims[2] = coeffLength;
    fpm->pntr[0] = (double*) astMalloc( pixelCount * coeffLength *
                                        sizeof(double));

    /* SIGMA (Standard Deviations) */
    sigma = smf_create_smfData( SMF__NOCREATE_DA |
                                SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = srcWidth;
    sigma->dims[1] = srcHeight;
    sigma->pntr[0] = (double*) astMalloc( pixelCount *
                                          sizeof(double));

    /* APPLY PHASE CORRECTION TO EACH BOLOMETER INTERFEROGRAM */
    interferogram = astMalloc(srcN * sizeof(*interferogram));
    newInterferogram = astMalloc(newN * sizeof(*newInterferogram));
    coefficients = astMalloc(coeffLength * sizeof(*coefficients));
    phase = astMalloc(phaseLength * sizeof(*phase));
    phaseFunction = astMalloc(phaseFunctionLength * sizeof(*phaseFunction));
    for(i = 0; i < srcHeight; i++) {
      for(j = 0; j < srcWidth; j++) {
        pixelIndex = i + j * srcHeight;

        /* GET INTERFEROGRAM */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) {
            interferogram[k] = (double)(*((float*)srcCube + index));
          } else {
            interferogram[k] = *((double*)srcCube + index);
          }
        }

        /* GET ZPD VALUE & DETERMINE ZPD INDEX */
        if(zpdData->dtype == SMF__FLOAT) {
          zpdValue = (double)(*((float*)zpdArray + pixelIndex));
        } else {
          zpdValue = *((double*)zpdArray + pixelIndex);
        }
        jj = 0;
        kk = srcN - 1;
        while(kk - jj > 1) {
          ii = (kk + jj) >> 1;
          if(zpdValue < fPositions[ii]) {
            kk = ii;
          } else {
            jj = ii;
          }
        }
        zpdIndex = jj + 1;

        /* APPLY PHASE CORRECTION */
        fts2_phasecorrection( interferogram, srcN, zpdIndex, dsHalfLength,
                              ssHalfLength, polynomialDegree,
                              phaseFunctionHalfLength,
                              wnLBoundPercent, wnUBoundPercent, weightLimit,
                              &stddev, coefficients,
                              phase, phaseFunction, newInterferogram,
                              status);

        for(k = 0; k < newN; k++) {
          index = pixelIndex + pixelCount * k;
          *((double*) (newSrcData->pntr[0]) + index) = newInterferogram[k];
        }

        /* INSERT FTS2 SPECIFIC DATA */
        *((double*)(sigma->pntr[0]) + pixelIndex) = stddev;
        for(k = 0; k <= polynomialDegree; k++) {
          index = pixelIndex + pixelCount * k;
          *((double*) (fpm->pntr[0]) + index) = coefficients[k];
        }
      }
    }
    astFree(interferogram);
    astFree(newInterferogram);
    astFree(coefficients);
    astFree(phase);
    astFree(phaseFunction);
    astFree(fPositions);
    smf_close_file(&srcData, status);

    newSrcData->fts = smf_construct_smfFts(NULL, fpm, sigma, status);
    smf_write_smfData(newSrcData, NULL, NULL, ogrp, fIndex, 0, status);
    smf_close_file(&newSrcData, status);
  }

  // ===========================================================================
  // UPDATE MIRROR POSITIONS
  // THERE SHOULD BE A BETTER WAY OF DOING THIS WITHIN THE LOOP ABOVE (COBA)
  int dims[1];
  int exist = 0;
  int offset = 0;
  float* fNewPositions = NULL;
  // LOOP THROUGH EACH NDF FILE IN THE GROUP
  for(fIndex = 1; fIndex <= insize; fIndex++) {
    smf_open_file( ogrp, fIndex, "UPDATE",
                   SMF__NOCREATE_QUALITY |
                   SMF__NOCREATE_VARIANCE |
                   SMF__NOCREATE_DA |
                   SMF__NOCREATE_FTS,
                   &srcData, status);

    if(*status != SAI__OK) { break; }

    srcN = srcData->dims[2];
    dims[0] = srcN;
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "UPDATE", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    fPositions = astMalloc(count * sizeof(*fPositions));
    datGetVR(hdsLocPosition, count, fPositions, &count, status);
    datThere(hdsLoc, "FTS_POS", &exist, status );
    if(exist) {
      datErase(hdsLoc, "FTS_POS", status);
      if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }
      datNew(hdsLoc, "FTS_POS", "_REAL", 1, dims, status);
      datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
      fNewPositions = astMalloc(srcN * sizeof(*fNewPositions));
      offset = dsHalfLength + phaseFunctionHalfLength - 2;
      tmp = srcN + offset;
      for(i = offset; i < tmp; i++) {
        fNewPositions[i - offset] = fPositions[i];
      }
      datPutVR(hdsLocPosition, srcN, fNewPositions, status);
      astFree(fNewPositions);
    }
    astFree(fPositions);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }
    smf_close_file(&srcData, status);
  }
  // ===========================================================================

  CLEANUP:
    ndfEnd(status);
    grpDelet(&igrp, status);
    grpDelet(&ogrp, status);
    grpDelet(&zpdgrp, status);
}
