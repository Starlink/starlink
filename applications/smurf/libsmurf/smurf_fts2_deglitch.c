/*
*+
*  Name:
*     smurf_fts2_deglitch.c

*  Purpose:
*     Removes the glitches from the source.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_deglitch(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Removes the glitches from the source.

*  ADAM Parameters:
*     CCSIZE = _INTEGER (Read)
*          Core cluster size.
*     DSHALFLENGTH = _INTEGER (Read)
*          Double-Sided interferogram half length.
*     DEGLITCHMODE = _INTEGER (Read)
*          Deglitching mode, 1=CORE, 2=TAIL and any other value means ALL
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     TCSIGMA = _DOUBLE (Read)
*          Tail cluster standard deviation percentage.
*     TCSIGMAMUL = _DOUBLE (Read)
*          Tail cluster standard deviation multiplier.
*     TCSIZE = _INTEGER (Read)
*          Tail cluster size.
*     ZPD = _CHAR (Read)
*          2D ZPD calibration file.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2010-08-28 (COBA):
*        Original version.
*     2010-09-21 (COBA):
*        Updated prologue with ADAM params
*     2010-10-04 (COBA):
*        - Replaced single ZPD value input with 2D ZPD array input
*        - Added ADAM param to control deglitching mode

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

#define FUNC_NAME "smurf_fts2_deglitch"
#define TASK_NAME "FTS2_DEGLITCH"

void smurf_fts2_deglitch(int* status)
{
  if(*status != SAI__OK) { return; }

  int coreClusterSize       = 0;  /* Core cluster size */
  int dsHalfLength          = 0;  /* Size of the double sided interferogram */
  int index                 = 0;  /* Index */
  int fIndex                = 0;  /* File loop counter */
  int i                     = 0;    /* Loop counter */
  int ii                    = 0;    /* Loop counter */
  int j                     = 0;    /* Loop counter */
  int jj                    = 0;    /* Loop counter */
  int k                     = 0;    /* Loop counter */
  int kk                    = 0;    /* Loop counter */
  int mode                  = 0;    /* Deglitch mode 0 = ALL */
  int pixelCount            = 0;  /* Number of bolometers in the subarray */
  int pixelIndex            = 0;  /* Current bolometer index */
  int srcHeight             = 0;  /* Height of the subarray */
  int srcSubarray           = 0;  /* Source sub-array */
  int srcWidth              = 0;  /* Width of the subarray */
  int srcN                  = 0;  /* Time series length of the input data */
  int tailClusterSize       = 0;  /* Tail cluster size */
  int zpdHeight             = 0;    /* Height of the ZPD array */
  int zpdIndex              = 0;    /* Index of ZPD */
  int zpdSubarray           = 0;    /* ZPD sub-array */
  int zpdWidth              = 0;    /* Width of the ZPD array */
  double tcSigma            = 0.0;  /* Tail cutoff standard deviation percentage */
  double tcSigmaMul         = 0.0;  /* Tail cutoff standard deviation multiplier */
  double zpdValue           = 0.0;  /* ZPD value */
  double* interferogram     = NULL; /* Single bolometer interferogram */
  float* fPositions         = NULL; /* Mirror positions */
  Grp* igrp                 = NULL; /* Input group */
  Grp* ogrp                 = NULL; /* output group */
  Grp* zpdgrp               = NULL; /* Output group */
  HDSLoc* hdsLoc            = NULL; /* Pointer to HDS location */
  HDSLoc* hdsLocPosition    = NULL; /* Pointer to mirror positions */
  size_t count              = 0;    /* Mirror positions count */
  size_t inSize             = 0;    /* Size of the input group */
  size_t outSize            = 0;    /* Size of the output group */
  size_t zpdSize            = 0;    /* Size of the ZPD group */
  smfData* srcData          = NULL; /* Pointer to input data */
  smfData* zpdData          = NULL; /* Pointer to ZPD data */
  void* srcCube             = NULL; /* Pointer to the input data cube */
  void* zpdArray            = NULL; /* Pointer to 2D ZPD data values */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &inSize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf("OUT", ogrp, inSize, inSize,
            "Equal number of input and output files expected!",
            &ogrp, &outSize, status);
  /* GET ZPD GROUP */
  kpg1Gtgrp("ZPD", &zpdgrp, &zpdSize, status);

  /* GET PARAMS */
  parGet0i("CCSIZE", &coreClusterSize, status);
  parGet0i("TCSIZE", &tailClusterSize, status);
  parGet0d("TCSIGMA", &tcSigma, status);
  parGet0d("TCSIGMAMUL", &tcSigmaMul, status);
  parGet0i("DSHALFLENGTH", &dsHalfLength, status);
  parGet0i("DEGLITCHMODE", &mode, status);

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

  /* LOOP THROUGH EACH NDF FILE  */
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_and_flatfield(igrp, ogrp, fIndex, NULL, NULL, &srcData, status);
    if(*status != SAI__OK) {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }

    /* GET DATA CUBE */
    if(srcData->dtype == SMF__FLOAT) {
      srcCube = (float*) (srcData->pntr[0]);
    } else if(srcData->dtype == SMF__DOUBLE) {
      srcCube = (double*) (srcData->pntr[0]);
    } else {
      errRep(FUNC_NAME, "Invalid data type found!", status);
      break;
    }
    srcWidth    = srcData->dims[0];
    srcHeight   = srcData->dims[1];
    srcN        = srcData->dims[2];
    pixelCount  = srcWidth * srcHeight;

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

    /* MIRROR POSITIONS */
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "UPDATE", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    fPositions = astMalloc(count * sizeof(*fPositions));
    datGetVR(hdsLocPosition, count, fPositions, &count, status);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }

    /* LOOP THROUGH THE SUBARRAY */
    interferogram = astMalloc(srcN * sizeof(*interferogram));
    for(i = 0; i < srcHeight; i++) {
      for(j = 0; j < srcWidth; j++) {
        pixelIndex = i + j * srcHeight;

        /* GET INTERFEROGRAM FOR BOLOMETER AT LOCATION (i, j) IN SUBARRAY */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) {
            interferogram[k] = *((float*)srcCube + index);
          }
          else if(srcData->dtype == SMF__DOUBLE) {
            interferogram[k] = *((double*)srcCube + index);
          } else {
            *status = SAI__ERROR;
            errRep(FUNC_NAME, "Invalid data type found!", status);
            break;
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

        /* REMOVE GLITCHES */
        fts2_deglitch( interferogram, srcN, coreClusterSize, tailClusterSize,
                       tcSigma, tcSigmaMul, zpdIndex, dsHalfLength, mode,
                       SMF__DEGLITCH_THRESHOLD);

        /* UPDATE BOLOMETER INTERFEROGRAM AT LOCATION (i, j) */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) {
            *((float*)srcCube + index) = (float) interferogram[k];
          } else if(srcData->dtype == SMF__DOUBLE) {
            *((double*)srcCube + index) = (double) interferogram[k];
          }
        }
      }
    }
    astFree(interferogram);
    astFree(fPositions);
    smf_close_file(&srcData, status);
  }

  CLEANUP:
    ndfEnd(status);
    grpDelet(&igrp, status);
    grpDelet(&ogrp, status);
    grpDelet(&zpdgrp, status);
}
