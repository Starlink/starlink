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
  
  int coreClusterSize       = 0;  /* Core cluster size */
  int dsHalfLength          = 0;  /* Size of the double sided interferogram */
  int index                 = 0;  /* Index */
  int fIndex                = 0;  /* File loop counter */
  int i                     = 0;  /* Loop counter */
  int j                     = 0;  /* Loop counter */
  int k                     = 0;  /* Loop counter */
  int pixelCount            = 0;  /* Number of bolometers in the subarray */
  int pixelIndex            = 0;  /* Current bolometer index */
  int srcHeight             = 0;  /* Height of the subarray */
  int srcWidth              = 0;  /* Width of the subarray */
  int srcN                  = 0;  /* Time series length of the input data */ 
  int tailClusterSize       = 0;  /* Tail cluster size */
  int zpdIndex              = 0;  /* Index of ZPD */
  double tcSigma            = 0.0;  /* Tail cutoff standard deviation percentage */
  double tcSigmaMul         = 0.0;  /* Tail cutoff standard deviation multiplier */
  double* interferogram     = NULL; /* Single bolometer interferogram */
  Grp* igrp                 = NULL; /* Input group */ 
  Grp* ogrp                 = NULL; /* output group */
  size_t insize             = 0;    /* Size of the input group */
  size_t outsize            = 0;    /* Size of the output group */
  smfData* srcData          = NULL; /* Pointer to input data */    
  smf_deglitchmode mode     = SMF__DEGLITCH_ALL; /* Deglitch mode */  
  void* srcCube             = NULL; /* Pointer to the input data cube */ 

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &insize, status); 
  /* GET OUTPUT GROUP */
  kpg1Wgndf("OUT", ogrp, insize, insize, 
            "Equal number of input and output files expected!", 
            &ogrp, &outsize, status);
  
  /* GET PARAMS */
  parGet0i("CCSIZE", &coreClusterSize, status); 
  parGet0i("TCSIZE", &tailClusterSize, status);
  parGet0d("TCSIGMA", &tcSigma, status); 
  parGet0d("TCSIGMAMUL", &tcSigmaMul, status); 
  parGet0i("ZPDINDEX", &zpdIndex, status); 
  parGet0i("DSHALFLENGTH", &dsHalfLength, status);

  ndfBegin();  
  /* LOOP THROUGH EACH NDF FILE  */
  for(fIndex = 1; fIndex <= insize; fIndex++) {
    /* OPEN THE FILE */ 
    smf_open_file( ogrp, fIndex, "UPDATE", 
                   SMF__NOCREATE_VARIANCE | SMF__NOCREATE_QUALITY |
                   SMF__NOCREATE_DA | SMF__NOCREATE_FTS, 
                   &srcData, status);
    if(*status != SAI__OK) {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }    
    
    /* GET DATA CUBE */
    if(srcData->dtype == SMF__FLOAT) { 
      srcCube = (float*) (srcData->pntr[0]); 
    } else if(srcData->dtype == SMF__DOUBLE) { 
      srcCube = (double*) (srcData->pntr[0]); 
    } else {
      errRep(FUNC_NAME, "Invalid data type found!", status);
      break;
    } 
    srcWidth    = srcData->dims[0]; 
    srcHeight   = srcData->dims[1]; 
    srcN        = srcData->dims[2];    
    pixelCount  = srcWidth * srcHeight;    
    
    /* LOOP THROUGH THE SUBARRAY */
    interferogram = (double*) astMalloc(srcN * sizeof(double));
    for(i = 0; i < srcHeight; i++) {
      for(j = 0; j < srcWidth; j++) {      
        pixelIndex = i + j * srcHeight;
        
        /* GET INTERFEROGRAM FOR BOLOMETER AT LOCATION (i, j) IN SUBARRAY */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) { 
            interferogram[k] = *((float*)srcCube + index);
          }
          else if(srcData->dtype == SMF__DOUBLE) { 
            interferogram[k] = *((double*)srcCube + index);
          }          
        }
        
        /* REMOVE GLITCHES */
        fts2_deglitch(  
            interferogram, 
            srcN, 
            coreClusterSize, 
            tailClusterSize,
            tcSigma, 
            tcSigmaMul,
            zpdIndex, 
            dsHalfLength, 
            mode, 
            SMF__DEGLITCH_THRESHOLD);
            
        /* UPDATE BOLOMETER INTERFEROGRAM AT LOCATION (i, j) */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;
          if(srcData->dtype == SMF__FLOAT) { 
            *((float*)srcCube + index) = (float) interferogram[k];
          } else if(srcData->dtype == SMF__DOUBLE) { 
            *((double*)srcCube + index) = (double) interferogram[k];
          }
        }
      }
    }      
    astFree(interferogram);
    
    /* CLOSE THE DATA FILE */
    smf_close_file(&srcData, status);
  }
  ndfEnd(status);  
    
  grpDelet(&igrp, status);
  grpDelet(&ogrp, status);
}
