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

*  History :
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

  char ftsMode[SZFITSCARD+1];
  int bseHeight     = 0; /* Height of the BSE calibration array */
  int bseN          = 0; /* Sample size of BSE */
  sc2ast_subarray_t bseSubnum   = 0; /* BSE subarray ID */
  int bseWidth      = 0; /* Width of the BSE calibration array */
  int fIndex        = 0; /* File counter */
  int i             = 0; /* Loop counter */
  int index         = 0; /* Index */
  int j             = 0; /* Loop counter */
  int k             = 0; /* Loop counter */
  int pixelCount    = 0; /* Number of bolometers in the subarray */
  int pixelIndex    = 0; /* Pixel index */
  int srcHeight     = 0; /* Height of the source array */
  int srcN          = 0; /* Sample size */
  sc2ast_subarray_t srcSubnum   = 0; /* Source subarray ID */
  int srcWidth      = 0;    /* Width of the source array */
  double* bseIFG    = NULL; /* BSE interferogram */
  double* bseIFGNew = NULL; /* New BSE interferogram */
  double* bseX      = NULL; /* BSE mirror positions */
  double* srcX      = NULL; /* Source mirror positions */
  float* tmp        = NULL; /* Temporary value */
  Grp* bsegrp       = NULL; /* BSE group */
  Grp* igrp         = NULL; /* Input group */
  Grp* ogrp         = NULL; /* Output group */
  size_t bseSize    = 0;    /* BSE size */
  size_t inSize       = 0;  /* Input size */
  size_t outSize    = 0;    /* Output size */
  size_t count      = 0;    /* Size */
  smfData* bseData  = NULL; /* Pointer to BSE data */
  smfData* srcData  = NULL; /* Pointer to source data */
  HDSLoc* hdsLoc    = NULL; /* Pointer to HDS location */
  HDSLoc* hdsLocPosition   = NULL; /* Pointer to mirror positions */
  smf_fts2scanmode srcMode = SMF__FTS2_SCANMODE_UNKNOWN; /* Source scan mode*/
  smf_fts2scanmode bseMode = SMF__FTS2_SCANMODE_UNKNOWN; /* BSE scan mode */
  void* bseCube     = NULL; /* Pointer to the BSE data cube */
  void* srcCube     = NULL; /* Pointer to the input data cube */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &inSize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf( "OUT", ogrp, inSize, inSize,
             "Equal number of input and output files expected!",
             &ogrp, &outSize, status);
  /* GET BSE GROUP */
  kpg1Gtgrp("BSE", &bsegrp, &bseSize, status);

  ndfBegin();

  /* BEAMSPLITTER SELF EMISSION, BSE */
  smf_open_file( bsegrp, 1, "READ",
                 SMF__NOCREATE_HAED |
                 SMF__NOCREATE_FILE |
                 SMF__NOCREATE_DA |
                 SMF__NOCREATE_FTS,
                 &bseData, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
            "Unable to open Beamsplitter Self Emission calibration file!",
            status);
    goto CLEANUP;
  }
  bseCube = bseData->pntr[0];

  bseWidth  = bseData->dims[0];
  bseHeight = bseData->dims[1];
  bseN      = bseData->dims[2];

  /* GET SUBARRAY ID */
  smf_find_subarray(bseData->hdr, NULL, 0, &bseSubnum, status);
  if( bseSubnum == SC2AST__NULLSUB ||
      bseSubnum == S8A || bseSubnum == S8B ||
      bseSubnum == S4C || bseSubnum == S4D) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "BSE has invalid subarray ID!", status);
    smf_close_file(&bseData, status);
    goto CLEANUP;
  }

  /* GET FTS-2 SCAN MODE */
  smf_fits_getS(bseData->hdr, "FTS_MODE", ftsMode, sizeof(ftsMode), status);
  if(strncmp(ftsMode, "FSCAN", 5) == 0 ) {
    bseMode = SMF__FTS2_SCANMODE_FSCAN;
  }
  if(bseMode != SMF__FTS2_SCANMODE_FSCAN) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Invalid FTS-2 scan mode in BSE data!", status);
    smf_close_file(&bseData, status);
    goto CLEANUP;
  }

  /* GET MIRROR POSITIONS */
  hdsLoc = smf_get_xloc(bseData, "JCMTSTATE", "EXT", "READ", 0, 0, status);
  datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
  datSize(hdsLocPosition, &count, status);
  bseX = astMalloc(count * sizeof(*bseX));
  tmp  = astMalloc(count * sizeof(*tmp));
  datGetVR(hdsLocPosition, count, tmp, &count, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to obtain BSE mirror positions!", status);
    astFree(tmp);
    astFree(bseX);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }
    smf_close_file(&bseData, status);
    goto CLEANUP;
  }
  for(int i = 0; i < (int) count; i++) {
    bseX[i] = (double) tmp[i];
  }
  astFree(tmp);
  if(hdsLoc) { datAnnul(&hdsLoc, status); }
  if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }

  /* LOOP THROUGH EACH NDF FILE IN THE GROUP */
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_and_flatfield(igrp, ogrp, fIndex, NULL, NULL, &srcData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    srcCube = srcData->pntr[0];

    srcWidth  = srcData->dims[0];
    srcHeight = srcData->dims[1];
    srcN      = srcData->dims[2];
    pixelCount = srcWidth * srcHeight;

    /* GET SUBARRAY ID */
    smf_find_subarray(srcData->hdr, NULL, 0, &srcSubnum, status);
    if( srcSubnum == SC2AST__NULLSUB ||
        srcSubnum == S8A || srcSubnum == S8B ||
        srcSubnum == S4C || srcSubnum == S4D) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Source has invalid subarray ID!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* VERIFY THAT THE CORRECT BSE INTERFEROGRAM IS USED */
    if(bseSubnum != srcSubnum) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Incompatible BSE interferogram cube!"
              "BSE and source must have the same subarray ID.", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* VERIFY THAT THE SOURCE & BSE HAVE COMPATIBLE DIMENSIONS */
    if(srcWidth != bseWidth || srcHeight != bseHeight) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible BSE dimensions!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* GET FTS-2 SCAN MODE */
    smf_fits_getS(srcData->hdr, "FTS_MODE", ftsMode, sizeof(ftsMode), status);
    if(strncmp(ftsMode, "FSCAN", 5) == 0 ) {
      srcMode = SMF__FTS2_SCANMODE_FSCAN;
    }
    if(srcMode != SMF__FTS2_SCANMODE_FSCAN) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid FTS-2 scan mode in source data!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* GET SOURCE MIRROR POSITIONS */
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "READ", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    srcX = astMalloc(count * sizeof(*srcX));
    tmp  = astMalloc(count * sizeof(*tmp));
    datGetVR(hdsLocPosition, count, tmp, &count, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to obtain source mirror positions!", status);
      astFree(tmp);
      astFree(srcX);
      if(hdsLoc) { datAnnul(&hdsLoc, status); }
      if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }
      smf_close_file(&srcData, status);
      break;
    }
    for(i = 0; i < (int) count; i++) {
      srcX[i] = (double) tmp[i];
    }
    astFree(tmp);
    if(hdsLoc) { datAnnul(&hdsLoc, status); }
    if(hdsLocPosition) { datAnnul(&hdsLocPosition, status); }

    /* REMOVE EACH BSE INTEREFROGRAM FROM CORRESPONDING SOURCE INTERFEROGRAM */
    bseIFG    = astMalloc(bseN * sizeof(*bseIFG));
    bseIFGNew = astMalloc(srcN * sizeof(*bseIFGNew));
    for(i = 0; i < srcHeight; i++) {
      for(j = 0; j < srcWidth; j++) {
        pixelIndex = i + j * srcHeight;

        /* GET BSE INTERFEROGRAM */
        for(k = 0; k < bseN; k++) {
          index = pixelIndex + pixelCount * k;
          if(bseData->dtype == SMF__FLOAT) {
            bseIFG[k] = (double)(*((float*)bseCube + index));
          } else {
            bseIFG[k] = *((double*)bseCube + index);
          }
        }

        /* INTERPOLATE BSE INTERFEROGRAM AT SOURCE MIRROR POSITIONS */
        fts2_naturalcubicsplineinterpolator( bseX, bseIFG, bseN,
                                             srcX, bseIFGNew, srcN);

        /* ADD/REMOVE BSE FROM SOURCE */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcSubnum == S8C || srcSubnum == S8D) {
            if(srcData->dtype == SMF__FLOAT) {
              *((float*)srcCube + index) -= bseIFGNew[k];
            } else {
              *((double*)srcCube + index) -= bseIFGNew[k];
            }
          } else if(srcSubnum == S4A || srcSubnum == S4B) {
            if(srcData->dtype == SMF__FLOAT) {
              *((float*)srcCube + index) += bseIFGNew[k];
            } else {
              *((double*)srcCube + index) += bseIFGNew[k];
            }
          }
        }
      }
    }
    astFree(bseIFGNew);
    astFree(bseIFG);
    astFree(bseX);
    astFree(srcX);
    smf_close_file(&srcData, status);
  }
  smf_close_file(&bseData, status);

  CLEANUP:
    ndfEnd(status);
    grpDelet(&igrp, status);
    grpDelet(&ogrp, status);
    grpDelet(&bsegrp, status);
}
