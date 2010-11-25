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

  int bolIndex          = 0;
  int numBol            = 0;  /* Number of bolometers in the subarray */
  size_t posN           = 0;
  int srcW              = 0;
  int srcH              = 0;
  int srcN              = 0;
  int srcID             = 0;    /* Input file sub-array ID */
  int zpdW              = 0;
  int zpdH              = 0;
  int zpdID             = 0;    /* ZPD calibration file sub-array ID */
  float* posArr         = NULL; /* Mirror positions */
  Grp* grpInput         = NULL; /* Input group */
  Grp* grpOutput        = NULL; /* Output group */
  Grp* grpZPD           = NULL; /* TAU WET group */
  HDSLoc* hds           = NULL; /* Pointer to HDS location */
  HDSLoc* hdsPOS        = NULL; /* Pointer to mirror positions */
  size_t fileIndex      = 0;    /* File loop counter */
  size_t numInputFile   = 0;    /* Size of the input group */
  size_t numOutputFile  = 0;    /* Size of the output group */
  size_t numZPDFile     = 0;    /* Size of the tau group */
  smfData* inputData    = NULL; /* Pointer to input data */
  smfData* outputData   = NULL; /* Pointer to output data */
  smfData* zpdInData    = NULL; /* Pointer to input zpd data */
  smfData* zpdOutData   = NULL; /* Pointer to output zpd data */
  void* ptrSRC          = NULL; /* Pointer to 3D input data values */
  void* ptrZPD          = NULL; /* Pointer to 2D ZPD data values */

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
  ptrZPD = zpdInData->pntr[0];
  zpdW = zpdInData->dims[0];
  zpdH = zpdInData->dims[1];
  smf_find_subarray(zpdInData->hdr, NULL, 0, &zpdID, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
            "Unable to determine the sub-array ID for the ZPD calibration file!",
            status);
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
      break;
    }

    outputData = smf_deepcopy_smfData(inputData, 0, SMF__NOCREATE_FTS, 0, 0, status);

    ptrSRC = inputData->pntr[0];
    srcW = inputData->dims[0];
    srcH = inputData->dims[1];
    srcN = inputData->dims[2];
    numBol = srcW * srcH;
    smf_find_subarray(inputData->hdr, NULL, 0, &srcID, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Unable to determine the sub-array ID for the input file!",
              status);
      break;
    }

    // VERIFY THAT THE INPUT FILE & THE CALIBRATION FILE ARE COMPATIBLE
    if(zpdID != srcID || zpdW != srcW || zpdH != srcH) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "ZPD calibration file and the input file are incompatible!",
              status);
      break;
    }

    // GET MIRROR POSITIONS FOR THE SCAN
    hds = smf_get_xloc(inputData, "JCMTSTATE", "EXT", "UPDATE", 0, 0, status);
    datFind(hds, "FTS_POS", &hdsPOS, status);
    datSize(hdsPOS, &posN, status);
    posArr = astMalloc(posN * sizeof(*posArr));
    datGetVR(hdsPOS, posN, posArr, &posN, status);
    if(hds) { datAnnul(&hds, status); }
    if(hdsPOS) { datAnnul(&hdsPOS, status); }

    // LOOP THROUGH THE SUBARRAY & LOCATE ZPD INDICES
    zpdOutData = smf_create_smfData(SMF__NOCREATE_DA | SMF__NOCREATE_FTS, status);
    zpdOutData->dtype   = SMF__INTEGER;
    zpdOutData->ndims   = 2;
    zpdOutData->dims[0] = zpdW;
    zpdOutData->dims[1] = zpdH;
    zpdOutData->pntr[0] = (int*) astMalloc(numBol * sizeof(int));

    int i = 0, j = 0;
    for(i = 0; i < zpdH; i++) {
      for(j = 0; j < zpdW; j++) {
        bolIndex = i + j * zpdH;
        double ZPD = *((double*)ptrZPD + bolIndex);

        int ii = 0;
        int jj = 0;
        int kk = srcN - 1;
        while(kk - jj > 1) {
          ii = (kk + jj) >> 1;
          if(ZPD < posArr[ii]) {
            kk = ii;
          } else {
            jj = ii;
          }
        }
        jj++;

        *((int*) (zpdOutData->pntr[0]) + bolIndex) = jj;
      }
    }
    astFree(posArr);
    smf_close_file(&inputData, status);

    outputData->fts = smf_construct_smfFts(NULL, zpdOutData, NULL, NULL, status);
    smf_write_smfData(outputData, NULL, NULL, grpOutput, fileIndex, 0, status);
    smf_close_file(&outputData, status);
  }

  CLEANUP:
    // CLOSE FILES
    if(zpdInData) { smf_close_file(&zpdInData, status); }

    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    if(grpZPD) { grpDelet(&grpZPD, status); }
    if(grpInput) { grpDelet(&grpInput, status); }
    if(grpOutput) { grpDelet(&grpOutput, status); }
}
