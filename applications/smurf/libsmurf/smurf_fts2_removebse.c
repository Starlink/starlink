/*
*+
*  Name:
*     FTS2REMOVEBSE

*  Purpose:
*     Removes the Beam splitter Self Emission (BSE) from the source.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_removebse(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Removes the Beam splitter Self Emission (BSE) from the source.

*  ADAM Parameters:
*     BSE = CHAR (Read)
*          Beam Splitter Self Emission filepath.
*     IN = NDF (Read)
*          Input files to be transformed.
*     OUT = NDF (Write)
*          Output files.

*  Authors:
*     COBA: Coskun Oba (UoL)
*     MS: Matt Sherwood (UofL)

*  History:
*     2010-06-13 (COBA):
*        Original version.
*     2010-09-21 (COBA):
*        Updated prologue with ADAM params
*     2010-10-13 (COBA):
*        - Handle single/double precison data
*        - Adapted to SMURF coding style
*        - Handle invalid subarrays
*     2010-10-15 (COBA):
*        - Removed explicit casts
*     2011-05-04 (COBA):
*        - Get mirror positions via fts2_getmirrorpositions
*        - Fixed possible memory leaks
*        - Removed redundancies
*     2013-11-25 (MS):
*        Added mirror timing values treatment

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2010 University of Lethbridge. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 3 of
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
*     MA 02110-1301, USA.

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
#include <ctype.h>

/* STARLINK INCLUDES */
#include "ast.h"
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"

/* SMURF INCLUDES */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_removebse"
#define TASK_NAME "FTS2REMOVEBSE"

void smurf_fts2_removebse(int* status)
{
  if(*status != SAI__OK) { return; }

  int bolIndex          = 0; /* Bolometer index */
  int bseH              = 0; /* Height of the BSE calibration array */
  int bseN              = 0; /* Sample size of BSE */
  int bseW              = 0; /* Width of the BSE calibration array */
  int i                 = 0; /* Loop counter */
  int index             = 0; /* Index */
  int j                 = 0; /* Loop counter */
  int k                 = 0; /* Loop counter */
  int numBol            = 0; /* Number of bolometers in the subarray */
  int srcH              = 0; /* Height of the source array */
  int srcN              = 0; /* Sample size */
  int srcW              = 0; /* Width of the source array */

  size_t numBSEFile     = 0; /* BSE size */
  size_t numInputFile   = 0; /* Input size */
  size_t numOutputFile  = 0; /* Output size */
  size_t fileIndex      = 0; /* File counter */

  double* bseIFG        = NULL; /* BSE interferogram */
  double* bseIFGNew     = NULL; /* New BSE interferogram */
  double* bseX          = NULL; /* BSE mirror positions */
  double* RTS           = NULL; /* BSE mirror times */
  double* srcX          = NULL; /* Source mirror positions */

  Grp* grpBSE           = NULL; /* BSE group */
  Grp* grpInput         = NULL; /* Input group */
  Grp* grpOutput        = NULL; /* Output group */

  smfData* bseData      = NULL; /* Pointer to BSE data */
  smfData* inputData    = NULL; /* Pointer to source data */

  sc2ast_subarray_t bseSubnum   = 0; /* BSE subarray ID */
  sc2ast_subarray_t srcSubnum   = 0; /* Source subarray ID */

  // GROUPS
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &numInputFile, status);
  kpg1Wgndf( "OUT", grpOutput, numInputFile, numInputFile,
             "Equal number of input and output files expected!",
             &grpOutput, &numOutputFile, status);
  kpg1Gtgrp("BSE", &grpBSE, &numBSEFile, status);

  ndfBegin();

  // ===========================================================================
  // BSE CALIBRATION FILE
  // ===========================================================================
  smf_open_file( NULL, grpBSE, 1, "READ",
                 SMF__NOCREATE_DA |
                 SMF__NOCREATE_FTS,
                 &bseData, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Unable to open BSE calibration file!", status);
    goto CLEANUP;
  }

  // CALIBRATION FILE DIMENSIONS
  bseW = (int) bseData->dims[0];
  bseH = (int) bseData->dims[1];
  bseN = (int) bseData->dims[2];

  // GET SUBARRAY ID
  smf_find_subarray(bseData->hdr, NULL, 0, &bseSubnum, status);
  if( bseSubnum == SC2AST__NULLSUB ||
      bseSubnum == S8A || bseSubnum == S8B ||
      bseSubnum == S4C || bseSubnum == S4D) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "BSE has invalid subarray ID!", status);
    goto CLEANUP;
  }

  // GET BSE MIRROR POSITIONS
  int num = 0;
  bseX = astMalloc(bseN * sizeof(*bseX));
  RTS = astMalloc(bseN * sizeof(*RTS));
  fts2_getmirrorpositions(bseData, bseX, RTS, &num, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME, "Unable to get the BSE mirror positions!", status);
    goto CLEANUP;
  }

  // ===========================================================================
  // LOOP THROUGH EACH NDF FILE IN THE INPUT GROUP
  // ===========================================================================
  for(fileIndex = 1; fileIndex <= numInputFile; fileIndex++) {
    smf_open_and_flatfield(NULL, grpInput, grpOutput, fileIndex, NULL, NULL,
                           NULL, &inputData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      goto CLEANUP;
    }

    // INPUT FILE DIMENSIONS
    srcW   = (int) inputData->dims[0];
    srcH   = (int) inputData->dims[1];
    srcN   = (int) inputData->dims[2];
    numBol = srcW * srcH;

    // GET SOURCE SUBARRAY ID
    smf_find_subarray(inputData->hdr, NULL, 0, &srcSubnum, status);
    if( srcSubnum == SC2AST__NULLSUB ||
        srcSubnum == S8A || srcSubnum == S8B ||
        srcSubnum == S4C || srcSubnum == S4D) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Source has invalid subarray ID!", status);
      goto CLEANUP;
    }

    // VERIFY THAT THE CORRECT BSE INTERFEROGRAM IS USED
    if(bseSubnum != srcSubnum) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Incompatible BSE interferogram cube!"
              "BSE and source must have the same subarray ID.", status);
      goto CLEANUP;
    }

    /* VERIFY THAT THE SOURCE & BSE HAVE COMPATIBLE DIMENSIONS */
    if(srcW != bseW || srcH != bseH) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible BSE dimensions!", status);
      goto CLEANUP;
    }

    // GET SOURCE MIRROR POSITIONS
    num = 0;
    srcX = astMalloc(srcN * sizeof(*srcX));
    RTS = astMalloc(srcN * sizeof(*RTS));
    fts2_getmirrorpositions(inputData, srcX, RTS, &num, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to get the source mirror positions!", status);
      goto CLEANUP;
    }

    // =========================================================================
    // LOOP THROUGH EACH BOLOMETER
    // =========================================================================
    bseIFG    = astMalloc(bseN * sizeof(*bseIFG));
    bseIFGNew = astMalloc(srcN * sizeof(*bseIFGNew));
    for(i = 0; i < srcH; i++) {
      for(j = 0; j < srcW; j++) {
        bolIndex = i + j * srcH;

        /* GET BSE INTERFEROGRAM */
        for(k = 0; k < bseN; k++) {
          index = bolIndex + numBol * k;
          bseIFG[k] = *((double*) (bseData->pntr[0]) + index);
        }

        // INTERPOLATE BSE INTERFEROGRAM AT SOURCE MIRROR POSITIONS
        fts2_naturalcubicsplineinterpolator(bseX, bseIFG, bseN, srcX, bseIFGNew, srcN);

        // ADD/REMOVE BSE FROM SOURCE
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          if(srcSubnum == S8C || srcSubnum == S8D) {
            *((double*) (inputData->pntr[0]) + index) -= bseIFGNew[k];
          } else if(srcSubnum == S4A || srcSubnum == S4B) {
            *((double*) (inputData->pntr[0]) + index) += bseIFGNew[k];
          }
        }
      }
    }

    if(bseIFG) { astFree(bseIFG); bseIFG = NULL; }
    if(bseIFGNew) { astFree(bseIFGNew); bseIFGNew = NULL; }
    if(bseX) { astFree(bseX); bseX = NULL; }
    if(RTS) { astFree(RTS); RTS = NULL; }
    if(srcX) { astFree(srcX); srcX = NULL; }
    if(inputData) { smf_close_file( NULL,&inputData, status); }
  }

  CLEANUP:
    // DELETE ARRAYS
    if(bseIFG) { astFree(bseIFG); bseIFG = NULL; }
    if(bseIFGNew) { astFree(bseIFGNew); bseIFGNew = NULL; }
    if(bseX) { astFree(bseX); bseX = NULL; }
    if(RTS) { astFree(RTS); RTS = NULL; }
    if(srcX) { astFree(srcX); srcX = NULL; }

    // CLOSE FILES
    if(bseData) { smf_close_file( NULL,&bseData, status); }
    if(inputData) { smf_close_file( NULL,&inputData, status); }

    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    if(grpInput) { grpDelet(&grpInput, status); }
    if(grpOutput) { grpDelet(&grpOutput, status); }
    if(grpBSE) { grpDelet(&grpBSE, status); }
}
