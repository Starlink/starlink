/*
*+
*  Name:
*     smurf_fts2_deglitch.c

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

*  Authors:
*     COBA: Coskun (Josh) Oba, University of Lethbridge

*  History :
*     27-AUG-2010 (COBA):
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

#define FUNC_NAME "smurf_fts2_deglitch"
#define TASK_NAME "FTS2_DEGLITCH"

void smurf_fts2_deglitch(int* status) 
{  
  if(*status != SAI__OK) { return; }  
  
  int coreClusterSize       = 0;    /* Core cluster size */
  int dsHalfLength          = 0;    /* Size of the double sided interferogram */
  int fIndex                = 0;  
  int i                     = 0;
  int j                     = 0;
  int k                     = 0;
  int pixelCount            = 0;
  int index                 = 0;
  int pixelIndex            = 0;
  int srcHeight             = 0;
  int srcWidth              = 0;
  int srcN                  = 0; 
  int tailClusterSize       = 0;            /* Tail cluster size */
  int zpdIndex              = 0;            /* Index of ZPD */
  double tailCutoffStdDevPercent = 0.0;     /* Tail cutoff standard deviation (as percentage) */
  double tailCutoffStdDevMultiplier = 0.0;  /* Tail cutoff standard deviation multiplier */
  double* srcCube           = NULL;
  double* interferogram     = NULL;
  Grp* igrp                 = NULL;
  Grp* ogrp                 = NULL;
  size_t insize             = 0;
  size_t outsize            = 0;
  smfData* srcData          = NULL;  
  smf_deglitchmode deglitchMode = SMF__DEGLITCH_ALL; /* Deglitch mode */  

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &insize, status); 
  /* GET OUTPUT GROUP */
  kpg1Wgndf("OUT", ogrp, insize, insize, 
            "Equal number of input and output files expected!", 
            &ogrp, &outsize, status);
  
  /* GET PARAMS */
  parGet0i("CORECLUSTERSIZE", &coreClusterSize, status); 
  parGet0i("TAILCLUSTERSIZE", &tailClusterSize, status);
  parGet0d("TAILCUTOFFSTDDEVPERCENT", &tailCutoffStdDevPercent, status); 
  parGet0d("TAILCUTOFFSTDDEVMULTIPLIER", &tailCutoffStdDevMultiplier, status); 
  parGet0i("ZPDINDEX", &zpdIndex, status); 
  parGet0i("DSHALFLENGTH", &dsHalfLength, status);

  ndfBegin();  
  /* LOOP THROUGH EACH NDF FILE */
  for(fIndex = 1; fIndex <= insize; fIndex++) 
  {
    /* GET INPUT DATA AND ITS DIMENSIONS */
    smf_open_file(ogrp, fIndex, "UPDATE", SMF__NOCREATE_QUALITY, &srcData, status);
    if(*status != SAI__OK)
    {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    srcCube = (double*) (srcData->pntr[0]);
    srcWidth = srcData->dims[0]; 
    srcHeight = srcData->dims[1]; 
    srcN = srcData->dims[2];    
    pixelCount = srcWidth * srcHeight;
    
    /* DEGLITCH INTERFEROGRAM FROM EACH BOLOMETER */
    for(i = 0; i < srcHeight; i++)
    {
      for(j = 0; j < srcWidth; j++)
      {
        pixelIndex = i + j * srcHeight;
        interferogram = (double*) astMalloc(srcN * sizeof(double));
        for(k = 0; k < srcN; k++)
        {
          interferogram[k] = srcCube[pixelIndex + pixelCount * k];
        }
        fts2_deglitch(  
            interferogram, 
            srcN, 
            coreClusterSize, 
            tailClusterSize,
            tailCutoffStdDevPercent, 
            tailCutoffStdDevMultiplier,
            zpdIndex, 
            dsHalfLength, 
            deglitchMode, 
            SMF__DEGLITCH_THRESHOLD);
        for(k = 0; k < srcN; k++)
        {
          srcCube[pixelIndex + pixelCount * k] = interferogram[k];
        }
        astFree(interferogram); interferogram = NULL;
      }
    }    
    srcCube = NULL;
    smf_close_file(&srcData, status); srcData = NULL;
  }
  ndfEnd(status);  
}
