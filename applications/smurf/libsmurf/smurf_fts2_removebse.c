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

#include <string.h>
#include <stdio.h>

// STARLINK includes
#include "star/ndg.h"
#include "star/grp.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"

// SMURF includes
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"
#include "libsc2fts/fts2_interpolation.h"

#define FUNC_NAME "smurf_fts2_removebse"
#define TASK_NAME "FTS2_REMOVEBSE"

void smurf_fts2_removebse(int* status) 
{
  // Requirement SUN/104: Do nothing if status is NOT SAI__OK
  if(*status != SAI__OK) return;

  // Get input group
  Grp* igrp = NULL;
  size_t size;
  kpg1Rgndf("IN", 0, 1, "", &igrp, &size, status); 

  // Get output group
  Grp* ogrp = NULL; 
  size_t outsize;
  kpg1Wgndf("OUT", ogrp, size, size, "Equal number of input and output files expected!", &ogrp, &outsize, status);

  // Get BSE group
  Grp* bsegrp = NULL;
  kpg1Gtgrp("BSE", &bsegrp, &size, status);

  ndfBegin();

  //
  // BEAMSPLITTER SELF EMISSION, BSE
  //
  smfData* bseData;
  smf_open_file(bsegrp, 1, "READ", SMF__NOCREATE_QUALITY, &bseData, status);
  if(*status != SAI__OK)
  {
    errRep(FUNC_NAME, "Unable to open Beamsplitter Self Emission file!", status);
    return;
  }
  double* bseCube = (double*) (bseData->pntr[0]); // Pointer to BSE interferogram cube
  int bseWidth = bseData->dims[0];  // Number of columns in subarray BSE is sampled
  int bseHeight = bseData->dims[1]; // Number of rows in subarray BSE is sampled
  int bseN = bseData->dims[2];      // BSE sample size
  
  // GET SUBARRAY ID
  int bseSubarray; // Subarray ID
  smf_find_subarray(bseData->hdr, NULL, 0, &bseSubarray, status);

  // GET FTS-2 SCAN MODE
  FTS2Mode bseMode = fts2_getScanMode(bseData, status);
  if(bseMode != FSCAN)
  {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Invalid FTS-2 scan mode in BSE data!", status);
    smf_close_file(&bseData, status);
    return;
  }

  // GET MIRROR POSITIONS
  double* bseX = fts2_getPositions(bseData, status);
  if(bseX == NULL)
  {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to obtain BSE mirror positions!", status);
    smf_close_file(&bseData, status);
    return;
  }

  //
  // REMOVE BSE FROM EACH SOURCE FILE
  //
  for(int fIndex = 1; fIndex <= size; fIndex++) 
  {
    // OPEN SOURCE
    smfData* srcData;
    smf_open_file(ogrp, fIndex, "UPDATE", SMF__NOCREATE_QUALITY, &srcData, status);
    if(*status != SAI__OK)
    {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    double* srcCube = (double*) (srcData->pntr[0]);

    // GET SUBARRAY ID
    int srcSubarray;
    smf_find_subarray(bseData->hdr, NULL, 0, &srcSubarray, status);

    // VERIFY THAT THE CORRECT BSE INTERFEROGRAM IS BEING USED
    if(bseSubarray != srcSubarray)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible BSE interferogram cube! BSE and source must have the same subarray ID.", status);
      smf_close_file(&srcData, status);
      break;
    }

    // VERIFY THAT THE SOURCE & BSE HAVE COMPATIBLE DIMENSIONS
    int srcWidth = srcData->dims[0]; 
    int srcHeight = srcData->dims[1]; 
    if(srcWidth != bseWidth || srcHeight != bseHeight)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible BSE dimensions!", status);
      smf_close_file(&srcData, status);
      break;
    }

    // GET FTS-2 SCAN MODE
    FTS2Mode srcMode = fts2_getScanMode(srcData, status);
    if(srcMode != FSCAN)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Invalid FTS-2 scan mode in source data!", status);
      smf_close_file(&srcData, status);
      break;
    }

    // GET SOURCE MIRROR POSITIONS
    double* srcX = fts2_getPositions(srcData, status);
    if(srcX == NULL)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to obtain source mirror positions!", status);
      smf_close_file(&srcData, status);
      break;
    }

    // REMOVE BSE FROM SOURCE
    int srcN = srcData->dims[2]; // Source sample size
    double* bseIFG    = smf_malloc(bseN, sizeof(double), 0, status);
    double* bseIFGNew = smf_malloc(srcN, sizeof(double), 0, status);
    int pixelCount = srcWidth * srcHeight;
    int index, pixelIndex;
    for(int i = 0; i < srcHeight; i++)
    {
      for(int j = 0; j < srcWidth; j++)
      {
        pixelIndex = i + j * srcHeight;

        // GET BSE INTERFEROGRAM AT INDEX (i, j)
        for(int k = 0; k < bseN; k++)
        {
          index = pixelIndex + pixelCount * k;
          bseIFG[k] = bseCube[index];
        }

        // INTERPOLATE BSE INTERFEROGRAM AT SOURCE MIRROR POSITIONS
        fts2_naturalCubicSplineInterpolator(bseX, bseIFG, bseN, srcX, bseIFGNew, srcN);

        // REMOVE INTERPOLATED BSE INTERFEROGRAM FROM SOURCE INTERFEROGRAM AT INDEX (i, j)
        for(int k = 0; k < srcN; k++)
        {
          index = pixelIndex + pixelCount * k;
          
          if(srcSubarray == s8c || srcSubarray == s8d)
          {
            srcCube[index] -= bseIFGNew[k];
          }
          else if(srcSubarray == s4a || srcSubarray == s4b)
          {
            srcCube[index] += bseIFGNew[k];
          }
        }
      }
    }

    // FREE RESOURCES
    bseIFGNew = smf_free(bseIFGNew, status);
    bseIFG = smf_free(bseIFG, status);   
    bseX = smf_free(bseX, status);
    srcX = smf_free(srcX, status);
    smf_close_file(&srcData, status);
  }

  // FREE RESOURCES
  smf_close_file(&bseData, status);

  // END
  ndfEnd(status);
}
