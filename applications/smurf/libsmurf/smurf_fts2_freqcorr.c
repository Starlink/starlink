/*
*+
*  Name:
*     FTS2FREQCORR

*  Purpose:
*     Off-Axis frequency correction.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_freqcorr(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Off-Axis Frequency Correction which scales the frequency grid according
*     to the different path difference that off-axis rays travel through the
*     interferometer.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     15-JUL-2010 (COBA):
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

/* STANDARD INCLUDES */
#include <string.h>
#include <stdio.h>

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

#define FUNC_NAME "smurf_fts2_freqcorr"
#define TASK_NAME "FTS2FREQCORR"

void smurf_fts2_freqcorr(int* status)
{
  printf("FTS2FREQCORR...: NOT IMPLEMENTED YET!\n");

  if( *status != SAI__OK ) { return; }

/*
  int fIndex      = 0;
  int i           = 0;
  int index       = 0;
  int j           = 0;
  int k           = 0;
  int pixelCount  = 0;
  int pixelIndex  = 0;
  int srcWidth    = 0;
  int srcHeight   = 0;
  int srcN        = 0;
  int thetaHeight = 0;
  int thetaWidth  = 0;
  double* ifg     = NULL;
  double* ifgNew  = NULL;
  double* src     = NULL;
  double* theta   = NULL;
  double* wn      = NULL;
  double* wnNew   = NULL;
  Grp* igrp       = NULL;
  Grp* ogrp       = NULL;
  Grp* thetagrp   = NULL;
  size_t size     = 0;
  size_t outsize  = 0;

  // Get input group
  kpg1Rgndf("IN", 0, 1, "", &igrp, &size, status);
  // Get output group
  kpg1Wgndf("OUT", ogrp, size, size, "Equal number of input and output files expected!", &ogrp, &outsize, status);
  // Get THETA group
  kpg1Gtgrp("THETA", &thetagrp, &size, status);

  ndfBegin();

  // OPEN THETA
  smfData* thetaData;
  smf_open_file(thetagrp, 1, "READ", SMF__NOCREATE_QUALITY, &thetaData, status);
  if(*status != SAI__OK)
  {
    errRep(FUNC_NAME, "Unable to open the THETA file!", status);
    return;
  }
  theta = (double*) (thetaData->pntr[0]);
  thetaWidth = thetaData->dims[0];
  thetaHeight = thetaData->dims[1];

  // PERFORM FREQUENCY CORRECTION FOR EACH SOURCE FILE
  for(fIndex = 1; fIndex <= size; fIndex++)
  {
    // OPEN SOURCE
    smfData* srcData;
    smf_open_file(ogrp, fIndex, "UPDATE", SMF__NOCREATE_QUALITY, &srcData, status);
    if(*status != SAI__OK)
    {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    src = (double*) (srcData->pntr[0]);

    // VERIFY THAT THE SOURCE & THETA HAVE COMPATIBLE DIMENSIONS
    srcWidth = srcData->dims[0];
    srcHeight = srcData->dims[1];
    if(srcWidth != thetaWidth || srcHeight != thetaHeight)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible Theta file!", status);
      smf_close_file(&thetaData, status);
      smf_close_file(&srcData, status);
      break;
    }

    // FREQUENCY CORRECTION
    srcN = srcData->dims[2];
    ifg    = (double*) astMalloc(srcN * sizeof(double));
    ifgNew = (double*) astMalloc(srcN * sizeof(double));
    wn     = (double*) astMalloc(srcN * sizeof(double));
    wnNew  = (double*) astMalloc(srcN * sizeof(double));
    pixelCount = srcWidth * srcHeight;
    index, pixelIndex;
    for(i = 0; i < srcHeight; i++)
    {
      for(j = 0; j < srcWidth; j++)
      {
        pixelIndex = i + j * srcHeight;
        for(k = 0; k < srcN; k++)
        {
          index = pixelIndex + pixelCount * k;
          ifg[k] = src[index];
          wn[k] = k;
          wnNew[k] = k * cos(theta[pixelIndex]);
        }
        // FREQUENCY SHIFT BY CUBIC SPLINE
        fts2_naturalcubicsplineinterpolator(wn, ifg, srcN, wnNew, ifgNew, srcN);

        for(k = 0; k < srcN; k++)
        {
          index = pixelIndex + pixelCount * k;
          src[index] = ifgNew[k];
        }
      }
    }
    astFree(ifgNew);
    astFree(ifg);
    astFree(wn);
    astFree(wnNew);
    smf_close_file(&srcData, status);
  }

  smf_close_file(&thetaData, status);

  ndfEnd(status);
  */
}
