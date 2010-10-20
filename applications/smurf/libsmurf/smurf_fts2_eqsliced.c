/*
*+
*  Name:
*     smurf_fts2_eqsliced.c

*  Purpose:
*     Interpolates the input source cube at evenly spaced mirror positions.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_eqsliced(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Interpolates the input source cube at evenly spaced mirror positions.

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2010-09-29 (COBA):
*        Original version.

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

#define FUNC_NAME "smurf_fts2_eqsliced"
#define TASK_NAME "FTS2_EQSLICED"

void smurf_fts2_eqsliced(int* status)
{
  if(*status != SAI__OK) { return; }

  char datatype[DAT__SZNAM + 1];  /* String for DATA/VARIANCE type */
  int exist                 = 0;  /* Represents a boolean */
  int dims[1]                  ;/* Mirror position dimensions */
  int index                 = 0;  /* Index */
  int fIndex                = 0;  /* File loop counter */
  int i                     = 0;  /* Loop counter */
  int j                     = 0;  /* Loop counter */
  int k                     = 0;  /* Loop counter */
  int pixelCount            = 0;  /* Number of bolometers in the subarray */
  int pixelIndex            = 0;  /* Current bolometer index */
  int srcHeight             = 0;  /* Height of the subarray */
  int srcWidth              = 0;  /* Width of the subarray */
  int srcN                  = 0;  /* Time series length of the input data */
  int tmpCount              = 0;  /* Temporary counter */
  double dx                 = 0.0;/* Mirror interval */
  double* dPositions        = NULL; /* Original mirror positions */
  double* dNewPositions     = NULL; /* Evenly spaced mirror positions */
  double* interferogram     = NULL; /* Single bolometer interferogram */
  double* tmpInterferogram  = NULL; /* Temporary single bolometer interferogram */
  float* fPositions         = NULL; /* Original mirror positions */
  float* fNewPositions      = NULL; /* Evenly spaced mirror positions */
  Grp* igrp                 = NULL; /* Input group */
  Grp* ogrp                 = NULL; /* Output group */
  HDSLoc* hdsLoc            = NULL; /* Pointer to HDS location */
  HDSLoc* hdsLocPosition    = NULL; /* Pointer to HDS location for mirror positions */
  size_t count              = 0;    /* Mirror positions count */
  size_t inSize             = 0;    /* Size of the input group */
  size_t outSize            = 0;    /* Size of the output group */
  smfData* srcData          = NULL; /* Pointer to input data */
  void* srcCube             = NULL; /* Pointer to the input data cube */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &inSize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf("OUT", ogrp, inSize, inSize,
            "Equal number of input and output files expected!",
            &ogrp, &outSize, status);

  ndfBegin();
  /* LOOP THROUGH EACH NDF FILE  */
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_and_flatfield(igrp, ogrp, fIndex, NULL, NULL, &srcData, status);
    if(*status != SAI__OK) {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    srcCube = srcData->pntr[0];

    srcWidth    = srcData->dims[0];
    srcHeight   = srcData->dims[1];
    srcN        = srcData->dims[2];
    pixelCount  = srcWidth * srcHeight;

    /* MIRROR POSITIONS */
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "UPDATE", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    if(count != srcN) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid number of mirror positions found!", status);
      break;
    }
    fPositions = astMalloc(count * sizeof(*fPositions));
    datGetVR(hdsLocPosition, count, fPositions, &count, status);
    dPositions = astMalloc(count * sizeof(*dPositions));
    for(i = 0; i < count; i++) {
      dPositions[i] = fPositions[i];
    }
    dNewPositions = astMalloc(count * sizeof(*dNewPositions));
    tmpCount = count - 1;
    dx = (dPositions[tmpCount] - dPositions[0]) / tmpCount;
    for(i = 0; i < count; i++) {
      dNewPositions[i] = dPositions[0] + i * dx;
    }

    /* LOOP THROUGH THE SUBARRAY */
    interferogram = astMalloc(srcN * sizeof(*interferogram));
    tmpInterferogram = astMalloc(srcN * sizeof(*tmpInterferogram));
    for(i = 0; i < srcHeight; i++) {
      for(j = 0; j < srcWidth; j++) {
        pixelIndex = i + j * srcHeight;

        /* GET INTERFEROGRAM */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) {
            tmpInterferogram[k] = (double) (*((float*)srcCube + index));
          } else {
            tmpInterferogram[k] = *((double*)srcCube + index);
          }
        }

        /* INTERPOLATE INTERFEROGRAM */
        fts2_naturalcubicsplineinterpolator(dPositions, tmpInterferogram, srcN,
                                            dNewPositions, interferogram, srcN);

        /* UPDATE INTERFEROGRAM */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) {
            *((float*)srcCube + index) = (float) interferogram[k];
          } else if(srcData->dtype == SMF__DOUBLE) {
            *((double*)srcCube + index) = interferogram[k];
          }
        }
      }
    }

    astFree(tmpInterferogram);
    astFree(interferogram);
    astFree(fPositions);
    astFree(dPositions);

    /* REPLACE MIRROR POSITIONS WITH EVENLY SPACED MIRROR POSITIONS */
    dims[0] = srcN;
    datThere(hdsLoc, "FTS_POS", &exist, status );
    if(exist) {
      datErase(hdsLoc, "FTS_POS", status);
      if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }
      datNew(hdsLoc, "FTS_POS", "_REAL", 1, dims, status);
      datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
      fNewPositions = astMalloc(count * sizeof(*fNewPositions));
      for(i = 0; i < count; i++) {
        fNewPositions[i] = (float) (dNewPositions[i]);
      }
      datPutVR(hdsLocPosition, srcN, fNewPositions, status);
      astFree(fNewPositions);
    }

    astFree(dNewPositions);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }

    /* CLOSE THE DATA FILE */
    smf_close_file(&srcData, status);
  }
  ndfEnd(status);

  grpDelet(&igrp, status);
  grpDelet(&ogrp, status);
}
