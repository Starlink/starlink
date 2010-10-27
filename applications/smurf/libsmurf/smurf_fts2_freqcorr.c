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
  if( *status != SAI__OK ) { return; }

  int fIndex         = 0; /* File loop counter */
  int i              = 0; /*Loop counter */
  int index          = 0; /* Index */
  int j              = 0; /*Loop counter */
  int k              = 0; /*Loop counter */
  int pixelCount     = 0; /* Number of bolometers */
  int pixelIndex     = 0; /* Bolometer index */
  int srcHeight      = 0; /* Height of the source subarray */
  int srcN           = 0; /* Sample count */
  int srcWidth       = 0; /* Width of the source subarray */
  int thetaHeight    = 0; /* Height of the theta */
  int thetaWidth     = 0;  /* Width of the theta */
  double* specIm     = NULL; /* Spectrum (imaginary component) */
  double* specImNew  = NULL; /* New spectrum (imaginary component) */
  double* specRe     = NULL; /* Spectrum (real component) */
  double* specReNew  = NULL; /* New spectrum (real component) */
  double* wn         = NULL; /* Wavenumbers */
  double* wnNew      = NULL; /* New wave numbers */
  Grp* igrp          = NULL; /* Input group */
  Grp* ogrp          = NULL; /* Output group */
  Grp* thetagrp      = NULL; /* Theta group */
  size_t outSize     = 0;    /* Output group size */
  size_t inSize      = 0;    /* Input group size */
  size_t thetaSize   = 0;    /* Theta group size */
  smfData* srcData   = NULL; /* Pointer to source data */
  smfData* thetaData = NULL; /* Pointer to theta data */
  void* src4D        = NULL; /* Pointer to the input data (4D data array) */
  void* theta2D      = NULL; /* Pointer to the THETA data */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &inSize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf( "OUT", ogrp, inSize, inSize,
             "Equal number of input and output files expected!",
             &ogrp, &outSize, status);
  /* GET THETA GROUP */
  kpg1Gtgrp("THETA", &thetagrp, &thetaSize, status);

  ndfBegin();

  /* OPEN THETA */
  smf_open_file( thetagrp, 1, "READ",
                 SMF__NOCREATE_HEAD |
                 SMF__NOCREATE_FILE |
                 SMF__NOCREATE_DA |
                 SMF__NOCREATE_FTS,
                 &thetaData, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to open the THETA file!", status);
    goto CLEANUP;
  }
  theta2D     = thetaData->pntr[0];
  thetaWidth  = thetaData->dims[0];
  thetaHeight = thetaData->dims[1];

  /* LOOP THROUGH EACH NDF FILE IN THE GROUP */
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_and_flatfield(igrp, ogrp, fIndex, NULL, NULL, &srcData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    src4D      = srcData->pntr[0];
    srcWidth   = srcData->dims[0];
    srcHeight  = srcData->dims[1];
    srcN       = srcData->dims[2];
    pixelCount = srcWidth * srcHeight;

    /* VERIFY THAT THE SOURCE & THETA HAVE COMPATIBLE DIMENSIONS */
    if(srcWidth != thetaWidth || srcHeight != thetaHeight) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible Theta file!", status);
      smf_close_file(&srcData, status);
      break;
    }

    /* APPLY FREQUENCY CORRECTION */
    specRe    = astMalloc(srcN * sizeof(*specRe));
    specIm    = astMalloc(srcN * sizeof(*specIm));
    specReNew = astMalloc(srcN * sizeof(*specReNew));
    specImNew = astMalloc(srcN * sizeof(*specImNew));
    wn        = astMalloc(srcN * sizeof(*wn));
    wnNew     = astMalloc(srcN * sizeof(*wnNew));
    for(i = 0; i < srcHeight; i++) {
      for(j = 0; j < srcWidth; j++) {
        pixelIndex = i + j * srcHeight;

        /* GET SPECTRUM & WAVENUMBERS */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;

          if(srcData->dtype == SMF__FLOAT) {
            specRe[k] = *((float*)src4D + index);
            specIm[k] = *((float*)src4D + index + 1);
          } else {
            specRe[k] = *((double*)src4D + index);
            specIm[k] = *((double*)src4D + index + 1);
          }

          wn[k] = k;

          /* NOTES:
           * If the theta file contains the cosine values already,
           * remove the cosine below and multiply k with the argument of cosine.
           * i.e, wnNew[k] = k * (*((double*)theta2D + pixelIndex));
           */
          wnNew[k] = k * cos(*((double*)theta2D + pixelIndex));
        }

        /* FREQUENCY(REAL & IMAGINARY) SHIFT BY CUBIC SPLINE */
        fts2_naturalcubicsplineinterpolator( wn, specRe, srcN,
                                             wnNew, specReNew, srcN);
        fts2_naturalcubicsplineinterpolator( wn, specIm, srcN,
                                             wnNew, specImNew, srcN);

        /* APPLY FREQUENCY CORRECTION */
        for(k = 0; k < srcN; k++) {
          index = pixelIndex + pixelCount * k;

          if(srcData->dtype == SMF__FLOAT) {
            *((float*)src4D + index) = (float) specReNew[k];
            *((float*)src4D + index + 1) = (float) specImNew[k];
          } else {
            *((double*)src4D + index) = specReNew[k];
            *((double*)src4D + index + 1) = specImNew[k];
          }
        }
      }
    }
    astFree(specRe);
    astFree(specIm);
    astFree(wn);
    astFree(specReNew);
    astFree(specImNew);
    astFree(wnNew);
    smf_close_file(&srcData, status);
  }
  smf_close_file(&thetaData, status);

  CLEANUP:
    ndfEnd(status);
    grpDelet(&igrp, status);
    grpDelet(&ogrp, status);
    grpDelet(&thetagrp, status);
}
