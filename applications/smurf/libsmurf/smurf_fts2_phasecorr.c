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

  char datatype[DAT__SZNAM + 1];  /* String for DATA/VARIANCE type */
  int coeffLength           = 0;    /* Number of polynomial coefficients */
  int dsHalfLength          = 0;    /* Size of the double sided interferogram */
  int fIndex                = 0;    /* File loop counter */
  int index                 = 0;    /* Index */
  int i                     = 0;    /* Loop counter */
  int j                     = 0;    /* Loop counter */
  int k                     = 0;    /* Loop counter */
  int newN                  = 0;    /* Time series length of the output data */
  int num                   = 0;    /* Temporary place holder */
  int pixelCount            = 0;    /* Number of bolometers in the subarray */
  int phaseFunctionHalfLength = 0;  /* Half-length of phase function */
  int polynomialDegree      = 0;    /* Degree of the polynomial used to fit */
  int ssHalfLength          = 0;    /* Size of the single sided interferogram */
  int phaseFunctionLength   = 0;    /* Phase correction function size */
  int phaseLength           = 0;    /* Phase length */
  int pixelIndex            = 0;    /* Current bolometer index */
  int srcHeight             = 0;    /* Height of the subarray */
  int srcWidth              = 0;    /* Width of the subarray */
  int srcN                  = 0;    /* Time series length of the input data */
  int ssHalfLengthInit      = 0;    /* Initial half-length of the single-sided interferogram */
  int tmp                   = 0;    /* Temporary place holder */
  int zpdIndex              = 0;    /* Index of ZPD */
  double stddev             = 0.0;  /* Standard deviation */
  double weightLimit        = 0.0;  /* Determines whether a point needs to be taken into consideration for phase correction */
  double wnFact             = 0.0;  /* Wavenumber factor */
  double wnLBoundPercent    = 0.0;  /* The lower bound (%) of wave number range */
  double wnUBoundPercent    = 0.0;  /* The upper bound (%) of wave number range */
  double* coefficients      = NULL; /* Coefficients of the polynomial */
  double* interferogram     = NULL; /* Single bolometer interferogram */
  float* fPositions         = NULL; /* Mirror positions */
  double* phase             = NULL; /* Phase */
  double* newInterferogram  = NULL; /* Phase corrected interferogram */
  double* phaseFunction     = NULL; /* Phase correction function */
  Grp* igrp                 = NULL; /* Input group */
  Grp* ogrp                 = NULL; /* Output group */
  size_t count              = 0;    /* Mirror positions count */
  size_t insize             = 0;    /* Size of the input group */
  size_t outsize            = 0;    /* Size of the output group */
  smfData* fpm              = NULL; /* Fitting params data */
  smfData* sigma            = NULL; /* smfFts standard deviation */
  smfData* srcData          = NULL; /* Pointer to input data */
  smfData* newSrcData       = NULL; /* Pointer to output data */
  HDSLoc* hdsLoc            = NULL; /* Pointer to HDS location */
  HDSLoc* hdsLocPosition    = NULL; /* Pointer to HDS location for mirror positions */
  void* srcCube             = NULL; /* Pointer to the input data cube */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &insize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf("OUT", ogrp, insize, insize,
            "Equal number of input and output files expected!",
            &ogrp, &outsize, status);

  /* GET PARAMS */
  parGet0i("ZPDINDEX", &zpdIndex, status);
  parGet0i("DSHALFLENGTH", &dsHalfLength, status);
  parGet0i("SSHALFLENGTH", &ssHalfLength, status);
  parGet0i("DEGREE", &polynomialDegree, status);
  parGet0i("PFHALFLENGTH", &phaseFunctionHalfLength, status);
  parGet0d("WNLBOUND", &wnLBoundPercent, status);
  parGet0d("WNUBOUND", &wnUBoundPercent, status);
  parGet0d("WEIGHTLIMIT", &weightLimit, status);

  coeffLength = polynomialDegree + 1;

  /* DOUBLE-SIDED INTERFEROGRAM HALF-LENGTH */
  tmp = ((zpdIndex + 1) < dsHalfLength) ? (zpdIndex + 1) : dsHalfLength;
  dsHalfLength = 1;
  for(i = tmp; i >= 1; i--) {
    num = i;
    while(num % 2 == 0) { num /= 2; }
    while(num % 3 == 0) { num /= 3; }
    while(num % 5 == 0) { num /= 5; }
    while(num % 7 == 0) { num /= 7; }
    if(num == 1) { dsHalfLength = i; break; }
  }

  ndfBegin();
  /* LOOP THROUGH EACH NDF FILE IN THE GROUP */
  for(fIndex = 1; fIndex <= insize; fIndex++) {
    smf_open_file( ogrp, fIndex, "UPDATE",
                   SMF__NOCREATE_QUALITY |
                   SMF__NOCREATE_VARIANCE |
                   SMF__NOCREATE_DA |
                   SMF__NOCREATE_FTS,
                   &srcData, status);
    if(*status != SAI__OK) {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    if(srcData->dtype == SMF__FLOAT) {
      srcCube = (float*) (srcData->pntr[0]);
    } else if(srcData->dtype == SMF__DOUBLE) {
      srcCube = (double*) (srcData->pntr[0]);
    } else {
      errRep(FUNC_NAME, "Invalid data type found!", status);
      break;
    }
    srcWidth   = srcData->dims[0];
    srcHeight  = srcData->dims[1];
    srcN       = srcData->dims[2];
    pixelCount = srcWidth * srcHeight;

    phaseLength = dsHalfLength + 1;
    phaseFunctionLength = (dsHalfLength <= phaseFunctionHalfLength) ?
                              dsHalfLength << 1 : phaseFunctionHalfLength << 1;
    phaseFunctionHalfLength = phaseFunctionLength >> 1;

    /* SINGLE-SIDED INTERFEROGRAM HALF-LENGTH */
    ssHalfLengthInit = srcN - zpdIndex - phaseFunctionHalfLength;
    tmp = (ssHalfLengthInit < ssHalfLength) ? ssHalfLengthInit : ssHalfLength;
    ssHalfLength = 1;
    for(i = tmp; i >= 1; i--) {
      num = i;
      while(num % 2 == 0) { num /= 2; }
      while(num % 3 == 0) { num /= 3; }
      while(num % 5 == 0) { num /= 5; }
      while(num % 7 == 0) { num /= 7; }
      if(num == 1) { ssHalfLength = i; break; }
    }

    /* WAVENUMBER FACTOR */
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "UPDATE", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    fPositions = astMalloc(count * sizeof(*fPositions));
    datGetVR(hdsLocPosition, count, fPositions, &count, status);
    wnFact = 1.0 / (fPositions[1] - fPositions[0]) / (2.0 * ssHalfLength);
    smf_fits_updateD(srcData->hdr, "WNFACT", wnFact, "Wavenumber factor", status);
    astFree(fPositions);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }

    /* DATA CUBE SIZE WILL CHANGE MAKE A DEEP COPY OF srcData */
    newN = ssHalfLength + 1;
    newSrcData = smf_deepcopy_smfData( srcData, 0,
                                       SMF__NOCREATE_DATA |
                                       SMF__NOCREATE_FTS, 0, 0, status);
    newSrcData->dtype   = SMF__DOUBLE;
    newSrcData->ndims   = 3;
    newSrcData->dims[0] = srcWidth;
    newSrcData->dims[1] = srcHeight;
    newSrcData->dims[2] = newN;
    newSrcData->pntr[0] = (double*) astMalloc((srcWidth * srcHeight * newN) * sizeof(double));

    /* FPM (Polynomial Fit Coefficients) */
    fpm = smf_create_smfData( SMF__NOCREATE_DA |
                              SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = srcWidth;
    fpm->dims[1] = srcHeight;
    fpm->dims[2] = coeffLength;
    fpm->pntr[0] = (double*) astMalloc((srcWidth * srcHeight * coeffLength) * sizeof(double));

    /* SIGMA (Standard Deviations) */
    sigma = smf_create_smfData( SMF__NOCREATE_DA |
                                SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = srcWidth;
    sigma->dims[1] = srcHeight;
    sigma->pntr[0] = (double*) astMalloc((srcWidth * srcHeight) * sizeof(double));

    /* APPLY PHASE CORRECTION TO EACH BOLOMETER INTERFEROGRAM */
    interferogram = astMalloc(srcN * sizeof(*interferogram));
    newInterferogram = astMalloc(newN * sizeof(*newInterferogram));
    coefficients = astMalloc(coeffLength * sizeof(*coefficients));
    phase = astMalloc(phaseLength * sizeof(*phase));
    phaseFunction = astMalloc(phaseFunctionLength * sizeof(*phaseFunction));
    for(i = 0; i < srcHeight; i++) {
      for(j = 0; j < srcWidth; j++) {
        pixelIndex = i + j * srcHeight;

        // INTERFEROGRAM
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) {
            interferogram[k] = (double)(*((float*)srcCube + index));
          } else {
            interferogram[k] = *((double*)srcCube + index);
          }
        }

        // APPLY PHASE CORRECTION
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

        // INSERT FTS2 SPECIFIC DATA
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

    smf_close_file(&srcData, status);

    newSrcData->fts = smf_construct_smfFts(NULL, fpm, sigma, status);
    smf_write_smfData(newSrcData, NULL, NULL, ogrp, fIndex, 0, status);

    smf_close_file(&newSrcData, status);
  }
  ndfEnd(status);

  // ===========================================================================
  // UPDATE MIRROR POSITIONS
  // THERE SHOULD BE A BETTER WAY OF DOING THIS WITHIN THE LOOP ABOVE (COBA)
  int dims[1];
  int exist = 0;
  int offset = 0;
  float* fNewPositions = NULL;
  ndfBegin();
  // LOOP THROUGH EACH NDF FILE IN THE GROUP
  for(fIndex = 1; fIndex <= insize; fIndex++) {
    smf_open_file( ogrp, fIndex, "UPDATE",
                   SMF__NOCREATE_QUALITY | SMF__NOCREATE_FTS, &srcData, status);
    if(*status != SAI__OK) {
      break;
    }

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
  ndfEnd(status);
  // ===========================================================================

  grpDelet(&igrp, status);
  grpDelet(&ogrp, status);
}
