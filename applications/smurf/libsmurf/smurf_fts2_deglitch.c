/*
*+
*  Name:
*     FTS2DEGLITCH

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

#define FUNC_NAME "smurf_fts2_deglitch"
#define TASK_NAME "FTS2DEGLITCH"

void smurf_fts2_deglitch(int* status)
{
  if(*status != SAI__OK) { return; }

  int bolIndex           = 0;  /* Current bolometer index */
  int coreClusterSize    = 0;  /* Core cluster size */
  int dsHalfLength       = 0;  /* Size of the double sided interferogram */
  int index              = 0;  /* Index */
  int fIndex             = 0;  /* File loop counter */
  int i                  = 0;    /* Loop counter */
  int j                  = 0;    /* Loop counter */
  int k                  = 0;    /* Loop counter */
  int mode               = 0;    /* Deglitch mode 0 = ALL */
  int numBol             = 0;  /* Number of bolometers in the subarray */
  int srcH               = 0;  /* Height of the subarray */
  int srcW               = 0;  /* Width of the subarray */
  int srcN               = 0;  /* Time series length of the input data */
  int tailClusterSize    = 0;  /* Tail cluster size */
  int zpdIndex           = 0;    /* Index of ZPD */
  double tcSigma         = 0.0;  /* Tail cutoff standard deviation percentage */
  double tcSigmaMul      = 0.0;  /* Tail cutoff standard deviation multiplier */
  double* interferogram  = NULL; /* Single bolometer interferogram */
  Grp* grpInput          = NULL; /* Input group */
  Grp* grpOutput         = NULL; /* output group */
  size_t inSize          = 0;    /* Size of the input group */
  size_t outSize         = 0;    /* Size of the output group */
  smfData* srcData       = NULL; /* Pointer to input data */
  smfData* zpdData       = NULL; /* Pointer to ZPD data */
  void* srcCube          = NULL; /* Pointer to the input data cube */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &grpInput, &inSize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf("OUT", grpOutput, inSize, inSize,
            "Equal number of input and output files expected!",
            &grpOutput, &outSize, status);

  // GET PARAMS
  parGet0i("CCSIZE", &coreClusterSize, status);
  parGet0i("TCSIZE", &tailClusterSize, status);
  parGet0d("TCSIGMA", &tcSigma, status);
  parGet0d("TCSIGMAMUL", &tcSigmaMul, status);
  parGet0i("DSHALFLENGTH", &dsHalfLength, status);
  parGet0i("DEGLITCHMODE", &mode, status);

  // BEGIN NDF
  ndfBegin();

  // LOOP THROUGH EACH NDF FILE
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_and_flatfield(grpInput, grpOutput, fIndex, NULL, NULL, &srcData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
    }

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

    // LOOP THROUGH THE SUBARRAY
    interferogram = astMalloc(srcN * sizeof(*interferogram));
    for(i = 0; i < srcH; i++) {
      for(j = 0; j < srcW; j++) {
        bolIndex = j + i * srcW;

        // GET INTERFEROGRAM FOR THE BOLOMETER
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          interferogram[k] = *((double*)srcCube + index);
        }

        // GET CORRESPONDING ZPD INDEX
        zpdIndex = *((int*)zpdData->pntr[0] + bolIndex);

        // REMOVE GLITCHES
        fts2_deglitch( interferogram, srcN, coreClusterSize, tailClusterSize,
                       tcSigma, tcSigmaMul, zpdIndex, dsHalfLength, mode,
                       SMF__DEGLITCH_THRESHOLD);

        // UPDATE BOLOMETER INTERFEROGRAM
        for(k = 0; k < srcN; k++) {
          index = bolIndex + numBol * k;
          *((double*)srcCube + index) = (double) interferogram[k];
        }
      }
    }
    astFree(interferogram);
    smf_close_file(&srcData, status);
  }

  CLEANUP:
    // END NDF
    ndfEnd(status);

    // DELETE GROUPS
    grpDelet(&grpInput, status);
    grpDelet(&grpOutput, status);
}
