/*
*+
*  Name:
*     smurf_fts2_removebse.c

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
*     
*     Calibration Data:
*        Beam splitter Self Emission Interferogram, BSE
*      
*        BSE <NDF>
*           DATA_ARRAY     <ARRAY>         {structure}
*           DATA(32, 40, M)    <_DOUBLE>     0,0.001,0.002,0.003,0.004,0.005,
*                                        ... 3.926,3.927,3.928,3.929,3.93,3.931
*
*           where M is the sample size for the BSE interferogram.

*  Authors:
*     COBA: Coskun (Josh) Oba, University of Lethbridge

*  History :
*     13-JUN-2010 (COBA):
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
#define TASK_NAME "FTS2_REMOVEBSE"

void smurf_fts2_removebse(int* status) 
{
  int i             = 0;
  int j             = 0;
  int k             = 0;
  int srcN          = 0; 
  int pixelCount    = 0;
  int index         = 0;
  int pixelIndex    = 0;
  int bseHeight     = 0; 
  int bseN          = 0;     
  int bseSubarray   = 0; 
  int fIndex        = 0;
  int bseWidth      = 0;  
  int srcSubarray   = 0;
  int srcWidth      = 0; 
  int srcHeight     = 0; 
  
  Grp* bsegrp       = NULL;
  Grp* igrp         = NULL;
  Grp* ogrp         = NULL; 
  size_t outsize    = 0;
  size_t size       = 0;
  size_t count      = 0;
  
  double* bseCube   = NULL;
  double* bseX      = NULL;
  double* srcCube   = NULL;
  double* srcX      = NULL;
  
  double* bseIFG    = NULL;
  double* bseIFGNew = NULL;
  FTSMode srcMode   = UNKNOWN;
  FTSMode bseMode   = UNKNOWN;
  
  smfData* bseData  = NULL;
  smfData* srcData  = NULL;
  
  HDSLoc* hdsLoc    = NULL;
  HDSLoc* hdsLocPosition = NULL;
  
  float* tmp        = NULL;  
  char ftsMode[SZFITSCARD+1];
  
  if(*status != SAI__OK) 
  {
    return;
  }

  /* Get input group */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &size, status); 
  /* Get output group */
  kpg1Wgndf("OUT", ogrp, size, size, "Equal number of input and output files expected!", &ogrp, &outsize, status);
  /* Get BSE group */
  kpg1Gtgrp("BSE", &bsegrp, &size, status);

  ndfBegin();

  /* 
  * BEAMSPLITTER SELF EMISSION, BSE
  */
  smf_open_file(bsegrp, 1, "READ", SMF__NOCREATE_QUALITY, &bseData, status);
  if(*status != SAI__OK)
  {
    errRep(FUNC_NAME, "Unable to open Beamsplitter Self Emission file!", status);
    return;
  }
  bseCube = (double*) (bseData->pntr[0]); 
  bseWidth = bseData->dims[0];  
  bseHeight = bseData->dims[1]; 
  bseN = bseData->dims[2];      
  
  /* GET SUBARRAY ID */
  smf_find_subarray(bseData->hdr, NULL, 0, &bseSubarray, status);

  /* GET FTS-2 SCAN MODE */
  smf_fits_getS(bseData->hdr, "FTS_MODE", ftsMode, sizeof(ftsMode), status);
  if(strncmp(ftsMode, "FSCAN", 5) == 0 ) { bseMode = FSCAN; }
  if(bseMode != FSCAN)
  {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Invalid FTS-2 scan mode in BSE data!", status);
    smf_close_file(&bseData, status);
    return;
  }

  /* GET MIRROR POSITIONS */
  hdsLoc = smf_get_xloc(bseData, "JCMTSTATE", "EXT", "READ", 0, 0, status);
  datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
  datSize(hdsLocPosition, &count, status);
  bseX = (double*) astMalloc(count * sizeof(double));
  tmp = (float*) astMalloc(count * sizeof(float));
  datGetVR(hdsLocPosition, count, tmp, &count, status);
  if(*status == SAI__OK)
  {
    for(int i = 0; i < (int) count; i++) { bseX[i] = (double) tmp[i]; }
  }
  /* FREE RESOURCES */
  astFree(tmp);
  datAnnul(&hdsLoc, status);
  datAnnul(&hdsLocPosition, status);  
  if(bseX == NULL)
  {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to obtain BSE mirror positions!", status);
    smf_close_file(&bseData, status);
    return;
  }

  /*
  * REMOVE BSE FROM EACH SOURCE FILE
  */
  for(fIndex = 1; fIndex <= size; fIndex++) 
  {
    /* OPEN SOURCE */
    smf_open_file(ogrp, fIndex, "UPDATE", SMF__NOCREATE_QUALITY, &srcData, status);
    if(*status != SAI__OK)
    {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    srcCube = (double*) (srcData->pntr[0]);

    /* GET SUBARRAY ID */
    smf_find_subarray(bseData->hdr, NULL, 0, &srcSubarray, status);

    /* VERIFY THAT THE CORRECT BSE INTERFEROGRAM IS BEING USED */
    if(bseSubarray != srcSubarray)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible BSE interferogram cube! BSE and source must have the same subarray ID.", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* VERIFY THAT THE SOURCE & BSE HAVE COMPATIBLE DIMENSIONS */
    srcWidth = srcData->dims[0]; 
    srcHeight = srcData->dims[1]; 
    if(srcWidth != bseWidth || srcHeight != bseHeight)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible BSE dimensions!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* GET FTS-2 SCAN MODE */
    smf_fits_getS(srcData->hdr, "FTS_MODE", ftsMode, sizeof(ftsMode), status);
    if(strncmp(ftsMode, "FSCAN", 5) == 0 ) { srcMode = FSCAN; }
    if(srcMode != FSCAN)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid FTS-2 scan mode in source data!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* GET SOURCE MIRROR POSITIONS */
    hdsLoc = smf_get_xloc(srcData, "JCMTSTATE", "EXT", "READ", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
    datSize(hdsLocPosition, &count, status);
    srcX = (double*) astMalloc(count * sizeof(double));
    tmp = (float*) astMalloc(count * sizeof(float));
    datGetVR(hdsLocPosition, count, tmp, &count, status);
    if(*status == SAI__OK)
    {
      for(int i = 0; i < (int) count; i++) { srcX[i] = (double) tmp[i]; }
    }
    /* FREE RESOURCES */
    astFree(tmp);
    datAnnul(&hdsLoc, status);
    datAnnul(&hdsLocPosition, status);  
    if(srcX == NULL)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to obtain source mirror positions!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* REMOVE BSE FROM SOURCE */
    srcN = srcData->dims[2]; // Source sample size
    bseIFG    = (double*) astMalloc(bseN * sizeof(double));
    bseIFGNew = (double*) astMalloc(srcN * sizeof(double));

    pixelCount = srcWidth * srcHeight;
    for(i = 0; i < srcHeight; i++)
    {
      for(j = 0; j < srcWidth; j++)
      {
        pixelIndex = i + j * srcHeight;
        /* GET BSE INTERFEROGRAM AT INDEX (i, j) */
        for(k = 0; k < bseN; k++)
        {
          index = pixelIndex + pixelCount * k;
          bseIFG[k] = bseCube[index];
        }
        /* INTERPOLATE BSE INTERFEROGRAM AT SOURCE MIRROR POSITIONS */
        fts2_naturalcubicsplineinterpolator(bseX, bseIFG, bseN, srcX, bseIFGNew, srcN);
        /* REMOVE INTERPOLATED BSE INTERFEROGRAM FROM SOURCE INTERFEROGRAM AT INDEX (i, j) */
        for(k = 0; k < srcN; k++)
        {
          index = pixelIndex + pixelCount * k;          
          if(srcSubarray == S8C || srcSubarray == S8D)
          {
            srcCube[index] -= bseIFGNew[k];
          }
          else if(srcSubarray == S4A || srcSubarray == S4B)
          {
            srcCube[index] += bseIFGNew[k];
          }
        }
      }
    }

    /* FREE RESOURCES */
    astFree(bseIFGNew);
    astFree(bseIFG);   
    astFree(bseX);
    astFree(srcX);
    smf_close_file(&srcData, status);
  }

  /* FREE RESOURCES */
  smf_close_file(&bseData, status);

  ndfEnd(status);
}
