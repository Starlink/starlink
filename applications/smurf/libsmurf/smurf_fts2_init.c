/*
*+
*  Name:
*     FTS2INIT

*  Purpose:
*     Prepares the input to be processed by the FTS2 Data Reduction tasks

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_init(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Prepares the input to be processed by the FTS2 Data Reduction tasks

*  ADAM Parameters:
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.
*     ZPD = NDF (Read)
*          ZPD calibration file.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     23-NOV-2010 (COBA):
*        Original version.
*     2011-05-05 (COBA):
*        - Get mirror positions via fts2_getmirrorpositions
*        - Fixed possible memory leaks
*        - Removed redundancies

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2008 University of Lethbridge. All Rights Reserved.

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
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA

*  Bugs:
*     {note_any_bugs_here}
*-
*/

#if HAVE_CONFIG_H
#include <config.h>
#endif

#include <string.h>
#include <stdio.h>

// STARLINK includes
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"
#include "par.h"

// SMURF includes
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_init"
#define TASK_NAME "FTS2INIT"

void smurf_fts2_init(int* status)
{
  if( *status != SAI__OK ) { return; }

  int bolIndex          = 0;  /* Bolometer index */
  int i                 = 0;  /* Loop counter */
  int index             = 0;  /* Index */
  int j                 = 0;  /* Loop counter */
  int k                 = 0;  /* Loop counter */
  int numBol            = 0;  /* Number of bolometers in the subarray */
  int srcW              = 0;  /* Width of the source array */
  int srcH              = 0;  /* Height of the source array */
  int srcN              = 0;  /* Sample size */
  int srcID             = 0;  /* Input file sub-array ID */
  int zpdW              = 0;  /* Width of the ZPD array */
  int zpdH              = 0;  /* Height of the ZPD array */
  int zpdID             = 0;  /* ZPD calibration file sub-array ID */

  double* IFG           = NULL; /* Interferogram */
  double* IFGEVEN       = NULL; /* Interpolated interferogram */
  double* posArr        = NULL; /* Mirror positions */
  double* posArrEven    = NULL; /* Evenly spaced mirror positions */

  Grp* grpInput         = NULL; /* Input group */
  Grp* grpOutput        = NULL; /* Output group */
  Grp* grpZPD           = NULL; /* TAU WET group */

  size_t fileIndex      = 0;    /* File loop counter */
  size_t numInputFile   = 0;    /* Size of the input group */
  size_t numOutputFile  = 0;    /* Size of the output group */
  size_t numZPDFile     = 0;    /* Size of the tau group */

  smfData* inputData    = NULL; /* Pointer to input data */
  smfData* outputData   = NULL; /* Pointer to output data */
  smfData* zpdInData    = NULL; /* Pointer to input zpd data */

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &numInputFile, status);
  kpg1Wgndf("OUT", grpOutput, numInputFile, numInputFile,
            "Equal number of input and output files expected!",
            &grpOutput, &numOutputFile, status);
  kpg1Gtgrp("ZPD", &grpZPD, &numZPDFile, status);

  ndfBegin();

  // ===========================================================================
  // ZPD CALIBRATION FILE
  // ===========================================================================
  smf_open_file(grpZPD, 1, "READ", SMF__NOCREATE_QUALITY, &zpdInData, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to open ZPD calibration file!", status);
    goto CLEANUP;
  }

  // ZPD FILE DIMENSIONS
  zpdW = zpdInData->dims[0];
  zpdH = zpdInData->dims[1];

  // FIND ZPD FILE SUBARRAY ID
  smf_find_subarray(zpdInData->hdr, NULL, 0, &zpdID, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Unable to determine the ZPD sub-array ID!", status);
    goto CLEANUP;
  }

  // ===========================================================================
  // LOOP THROUGH EACH NDF FILE IN THE INPUT GROUP
  // ===========================================================================
  for(fileIndex = 1; fileIndex <= numInputFile; fileIndex++) {
    // OPEN INPUT FILE
    smf_open_and_flatfield(grpInput, grpOutput, fileIndex, NULL, NULL, &inputData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      goto CLEANUP;
    }

    // INPUT FILE DIMENSIONS
    srcW = inputData->dims[0];
    srcH = inputData->dims[1];
    srcN = inputData->dims[2];
    numBol = srcW * srcH;

    // FIND INPUT FILE SUBARRAY ID
    smf_find_subarray(inputData->hdr, NULL, 0, &srcID, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to determine the input sub-array ID!", status);
      goto CLEANUP;
    }

    // VERIFY THAT THE INPUT & THE CALIBRATION FILES ARE COMPATIBLE
    if(zpdID != srcID || zpdW != srcW || zpdH != srcH) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Incompatible ZPD calibration file!", status);
      goto CLEANUP;
    }

    // GET MIRROR POSITIONS FOR THE SCAN
    int num = 0;
    posArr = astMalloc(srcN * sizeof(*posArr));
    fts2_getmirrorpositions(inputData, posArr, &num, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME, "Unable to get the mirror positions!", status);
      goto CLEANUP;
    }

    // CREATE EVENLY SPACED MIRROR POSITIONS AND UPDATE MIRROR POSITIONS
    JCMTState* state = inputData->hdr->allState;
    posArrEven = astMalloc(srcN * sizeof(*posArrEven));
    double dPos = (posArr[srcN - 1] - posArr[0]) / (srcN - 1);
    for(i = 0; i < srcN; i++) {
      posArrEven[i] = posArr[0] + i * dPos;

      state->fts_pos = (float) posArrEven[i];
      state++;
    }

    // CREATE 2D ZPD INDEX ARRAY
    smfData* zpd = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    zpd->dtype   = SMF__INTEGER;
    zpd->ndims   = 2;
    zpd->dims[0] = zpdW;
    zpd->dims[1] = zpdH;
    zpd->pntr[0] = (int*) astMalloc(numBol * sizeof(int));

    // LOOP THROUGH THE SUBARRAY
    IFG = astMalloc(srcN * sizeof(*IFG));
    IFGEVEN = astMalloc(srcN * sizeof(*IFGEVEN));
    for(i = 0; i < srcH; i++) {
      for(j = 0; j < srcW; j++) {
        bolIndex = i + j * srcH;

        // FIND ZPD INDEX
        double zpdVal = *((double*) (zpdInData->pntr[0]) + bolIndex);
        int mid = 0;
        int a = 0;
        int b = srcN - 1;
        while(b - a > 1) {
          mid = (b + a) >> 1;
          if(zpdVal < posArrEven[mid]) {
            b = mid;
          } else {
            a = mid;
          }
        }
        *((int*) (zpd->pntr[0]) + bolIndex) = ++a;

        // INTERPOLATE INTERFEROGRAM ONTO EVENLY SPACED GRID
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          IFG[k] = *((double*) (inputData->pntr[0]) + index);
        }
        fts2_naturalcubicsplineinterpolator(posArr, IFG, srcN, posArrEven, IFGEVEN, srcN);
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          *((double*) (inputData->pntr[0]) + index) = IFGEVEN[k];
        }


      }
    }
    if(IFG) { astFree(IFG); IFG = NULL; }
    if(IFGEVEN) { astFree(IFGEVEN); IFGEVEN = NULL; }
    if(posArr) { astFree(posArr); posArr = NULL; }
    if(posArrEven) { astFree(posArrEven); posArrEven = NULL; }

    outputData = smf_deepcopy_smfData(inputData, 0, SMF__NOCREATE_FTS, 0, 0, status);
    smf_close_file(&inputData, status);

    // CREATE EMPTY FPM & SIGMA
    smfData* fpm = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    fpm->dtype   = SMF__DOUBLE;
    fpm->ndims   = 3;
    fpm->dims[0] = 1;
    fpm->dims[1] = 1;
    fpm->dims[2] = 1;
    fpm->pntr[0] = (double*) astMalloc(1 * sizeof(double));
    *((double*) fpm->pntr[0]) = 0;
    smfData* sigma = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    sigma->dtype   = SMF__DOUBLE;
    sigma->ndims   = 2;
    sigma->dims[0] = 1;
    sigma->dims[1] = 1;
    sigma->pntr[0] = (double*) astMalloc(1 * sizeof(double));
    *((double*) sigma->pntr[0]) = 0;

    outputData->fts = smf_construct_smfFts(NULL, zpd, fpm, sigma, status);
    smf_write_smfData(outputData, NULL, NULL, grpOutput, fileIndex, 0, status);
    smf_close_file(&outputData, status);
  }

  CLEANUP:
    // DELETE ARRAYS
    if(IFG) { astFree(IFG); IFG = NULL; }
    if(IFGEVEN) { astFree(IFGEVEN); IFGEVEN = NULL; }
    if(posArr) { astFree(posArr); posArr = NULL; }
    if(posArrEven) { astFree(posArrEven); posArrEven = NULL; }

    // CLOSE FILES
    if(inputData) { smf_close_file(&inputData, status); }
    if(outputData) { smf_close_file(&outputData, status); }
    if(zpdInData) { smf_close_file(&zpdInData, status); }

    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    if(grpZPD) { grpDelet(&grpZPD, status); }
    if(grpInput) { grpDelet(&grpInput, status); }
    if(grpOutput) { grpDelet(&grpOutput, status); }
}
