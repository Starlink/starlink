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

*  ADAM Parameters:
*      IN = NDF (Read)
*          Input data files.
*      OUT = NDF (Write)
*          Output data files.
*      THETA = NDF (Read)
*          Theta file storing the off-axis angles.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     15-JUL-2010 (COBA):
*        Original version.
*     04-NOV-2010 (COBA):
*        Modified to work with 3D spectrum cubes.

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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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

  int bolCount       = 0;    /* Number of bolometers */
  int bolIndex       = 0;    /* Bolometer index */
  int nbolX          = 0;    /* Width of the source subarray */
  int nbolY          = 0;    /* Height of the source subarray */
  int N              = 0;    /* Sample count */
  double* calPntr    = NULL; /* Pointer to the THETA data */
  double* inPntr     = NULL; /* Pointer to the input data (4D data array) */
  double* outPntr    = NULL; /* Pointer to the output data (4D data array) */
  double* spec       = NULL; /* Spectrum (real component) */
  double* specNew    = NULL; /* New spectrum (real component) */
  double* wn         = NULL; /* Wavenumbers */
  double* wnNew      = NULL; /* New wave numbers */
  Grp* calgrp        = NULL; /* Theta group */
  Grp* igrp          = NULL; /* Input group */
  Grp* ogrp          = NULL; /* Output group */
  size_t calSize     = 0;    /* Theta group size */
  size_t outSize     = 0;    /* Output group size */
  size_t inSize      = 0;    /* Input group size */
  size_t fIndex      = 0;    /* File loop counter */
  smfData* calData   = NULL; /* Pointer to theta data */
  smfData* inData    = NULL; /* Pointer to input data */
  smfData* outData   = NULL; /* Pointer to output data */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &inSize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf( "OUT", ogrp, inSize, inSize,
             "Equal number of input and output files expected!",
             &ogrp, &outSize, status);
  /* GET THETA GROUP */
  kpg1Gtgrp("THETA", &calgrp, &calSize, status);

  ndfBegin();

  // OPEN THETA
  smf_open_file( NULL, calgrp, 1, "READ",
                 SMF__NOCREATE_DA | SMF__NOCREATE_FTS,
                 &calData, status);
  if(*status != SAI__OK) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unable to open the THETA file!", status);
    goto CLEANUP;
  }

  calPntr = calData->pntr[0];

  // LOOP THROUGH EACH NDF FILE IN THE GROUP
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_file( NULL, igrp, fIndex, "READ", 0, &inData, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }

    // VERIFY THAT THE SOURCE & THETA HAVE COMPATIBLE DIMENSIONS
    if( (inData->dims[0] != calData->dims[0]) ||
        (inData->dims[1] != calData->dims[1]) ) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Incompatible Theta file!", status);
      smf_close_file( NULL,&inData, status);
      break;
    }

    outData = smf_deepcopy_smfData(NULL, inData, 0, SMF__NOCREATE_DATA, 0, 0, status);

    if(*status == SAI__OK) {
      inPntr   = inData->pntr[0];
      nbolX    = (int) inData->dims[0];
      nbolY    = (int) inData->dims[1];
      N        = (int) inData->dims[2];
      bolCount = nbolX * nbolY;

      outData->dtype   = SMF__DOUBLE;
      outData->ndims   = 3;
      outData->dims[0] = inData->dims[0];
      outData->dims[1] = inData->dims[1];
      outData->dims[2] = inData->dims[2];
      outData->lbnd[0] = outData->lbnd[0];
      outData->lbnd[1] = outData->lbnd[1];
      outData->lbnd[2] = outData->lbnd[2];
      outData->pntr[0] = (double*) astMalloc( (N * bolCount)*sizeof(double) );
      outPntr          = outData->pntr[0];

      // APPLY FREQUENCY CORRECTION
      wn      = astMalloc(N * sizeof(*wn));
      spec    = astMalloc(N * sizeof(*spec));
      wnNew   = astMalloc(N * sizeof(*wnNew));
      specNew = astMalloc(N * sizeof(*specNew));

      int i = 0, j = 0, k = 0;
      for(i = 0; i < nbolY; i++) {
        for(j = 0; j < nbolX; j++) {
          bolIndex = i + j * nbolY;

          // GET SPECTRUM & WAVENUMBERS
          for(k = 0; k < N; k++) {
            wn[k] = k;
            spec[k] = inPntr[bolIndex + k * bolCount];

            wnNew[k] = k * calPntr[bolIndex];
          }

          // FREQUENCY SHIFT BY CUBIC SPLINE
          fts2_naturalcubicsplineinterpolator(wn, spec, N, wnNew, specNew, N);

          // APPLY FREQUENCY CORRECTION
          for(k = 0; k < N; k++) {
            outPntr[bolIndex + k * bolCount] = specNew[k];
          }
        }
      }

      astFree(wn);
      astFree(wnNew);
      astFree(spec);
      astFree(specNew);

      smf_write_smfData(NULL, outData, NULL, NULL, ogrp, fIndex, 0, MSG__VERB,
                        0, NULL, NULL, status);
      smf_close_file( NULL,&outData, status);

      smf_close_file( NULL,&inData, status);
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to deep copy!", status);
      smf_close_file( NULL,&inData, status);
      break;
    }
  }
  smf_close_file( NULL,&calData, status);

  CLEANUP:
    ndfEnd(status);
    grpDelet(&igrp, status);
    grpDelet(&ogrp, status);
    grpDelet(&calgrp, status);
}
