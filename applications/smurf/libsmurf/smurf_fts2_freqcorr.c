/*
*+
*  Name:
*     smurf_fts2_freqcorr.c

*  Purpose:
*     Off-Axis frequency correction.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_freqcorr(int *status)

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Off-Axis Frequency Correction which scales the frequency grid according
*     to the different path difference that off-axis rays travel through the
*     interferometer.
*     
*     Calibration Data:
*        Frequency Scale Factor Image, THETA
*
*        THETA  <NDF>
*           DATA_ARRAY     <ARRAY>         {structure}
*           DATA(40,32)    <_DOUBLE>       0,0.001,0.002,0.003,0.004,0.005,
*                                     ... 3.926,3.927,3.928,3.929,3.93,3.931
*        

*  Authors:
*     COBA: Coskun (Josh) Oba, University of Lethbridge

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

// SMURF includes
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_freqcorr"
#define TASK_NAME "FTS2_FREQCORR"

void smurf_fts2_freqcorr(int *status) 
{
  // Requirement SUN/104: Do nothing if status is NOT SAI__OK
  if( *status != SAI__OK ) return;

  // Get input group
  Grp* igrp = NULL;
  size_t size;
  kpg1Rgndf("IN", 0, 1, "", &igrp, &size, status); 

  // Get output group
  Grp* ogrp = NULL; 
  size_t outsize;
  kpg1Wgndf("OUT", ogrp, size, size, "Equal number of input and output files expected!", &ogrp, &outsize, status);

  // Get THETA group
  Grp* thetagrp = NULL;
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
  double* theta = (double*) (thetaData->pntr[0]);
  int thetaWidth = thetaData->dims[0]; 
  int thetaHeight = thetaData->dims[1]; 

  // PERFORM FREQUENCY CORRECTION FOR EACH SOURCE FILE
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
    double* src = (double*) (srcData->pntr[0]);

    // VERIFY THAT THE SOURCE & THETA HAVE COMPATIBLE DIMENSIONS
    int srcWidth = srcData->dims[0]; 
    int srcHeight = srcData->dims[1]; 
    if(srcWidth != thetaWidth || srcHeight != thetaHeight)
    {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible Theta file!", status);
      smf_close_file(&thetaData, status);
      smf_close_file(&srcData, status);
      break;
    }

    // FREQUENCY CORRECTION
    int srcN = srcData->dims[2]; // Source sample size
    double* ifg    = (double*) astMalloc(srcN * sizeof(double));
    double* ifgNew = (double*) astMalloc(srcN * sizeof(double));
    double* wn     = (double*) astMalloc(srcN * sizeof(double));
    double* wnNew  = (double*) astMalloc(srcN * sizeof(double));
    int pixelCount = srcWidth * srcHeight;
    int index, pixelIndex;
    for(int i = 0; i < srcHeight; i++)
    {
      for(int j = 0; j < srcWidth; j++)
      {
        pixelIndex = i + j * srcHeight;

        for(int k = 0; k < srcN; k++)
        {
          index = pixelIndex + pixelCount * k;
          ifg[k] = src[index];
          wn[k] = k;
          wnNew[k] = k * cos(theta[pixelIndex]);
        }

        // FREQUENCY SHIFT BY CUBIC SPLINE
        //csi_simplified(ifg, srcN, wn, srcN, ifgNew);
        fts2_naturalCubicSplineInterpolator(wn, ifg, srcN, wnNew, ifgNew, srcN);

        for(int k = 0; k < srcN; k++)
        {
          index = pixelIndex + pixelCount * k;
          src[index] = ifgNew[k];
        }
      }
    }

    // FREE RESOURCES
    astFree(ifgNew);
    astFree(ifg);   
    astFree(wn);      
    astFree(wnNew);  
    smf_close_file(&srcData, status);
  }

  // FREE RESOURCES
  smf_close_file(&thetaData, status);

  // END
  ndfEnd(status);
}
