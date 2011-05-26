/*
*+
*  Name:
*     FTS2SPECRE

*  Purpose:
*     Reduces a 4D data set to a 3D data cube with real part of the spectrum.
*     The format of 4D data set must be [N x NBOLX x NBOLY x 2] where N is the
*     sample size. The new format of the 3D data cube is same as the time series.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_specre(status)

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Reduces a 4D data set to a 3D data cube with real part of the spectrum.
*     The format of 4D data set must be [N x NBOLX x NBOLY x 2] where N is the
*     sample size. The new format of the 3D data cube is same as the time series.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     29-OCT-2010 (COBA):
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

#define FUNC_NAME "smurf_fts2_specre"
#define TASK_NAME "FTS2SPECRE"

void smurf_fts2_specre(int* status)
{
  if( *status != SAI__OK ) { return; }

  int bolCount       = 0;    /* Number of bolometers in the subarray */
  int component      = 0;    /* Frequency component */
  int dataLength     = 0;    /* Number of data points */
  int fIndex         = 0;    /* File loop counter */
  int nbolX          = 0;    /* Number of horizontal bolometers in subarray */
  int nbolY          = 0;    /* Number of vertical bolometers in subarray */
  int N              = 0;    /* Sample size */
  Grp* igrp          = NULL; /* Input group */
  Grp* ogrp          = NULL; /* Output group */
  size_t inSize      = 0;    /* Input group size */
  size_t outSize     = 0;    /* Output group size */
  smfData* src4D     = NULL; /* Pointer to input data */
  smfData* src3D     = NULL; /* Pointer to output data */

  /* GET INPUT GROUP */
  kpg1Rgndf("IN", 0, 1, "", &igrp, &inSize, status);
  /* GET OUTPUT GROUP */
  kpg1Wgndf( "OUT", ogrp, inSize, inSize,
             "Equal number of input and output files expected!",
             &ogrp, &outSize, status);

  ndfBegin();
  for(fIndex = 1; fIndex <= inSize; fIndex++) {
    smf_open_file(igrp, fIndex, "READ", 0, &src4D, status);
    if(*status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }

    if( !smf_isfft(src4D, NULL, NULL, NULL, status) ||
        *status != SAI__OK) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to verify the spectrum!", status);
      break;
    }

    N          = src4D->dims[0];
    nbolX      = src4D->dims[1];
    nbolY      = src4D->dims[2];
    bolCount   = nbolX * nbolY;

    src3D = smf_deepcopy_smfData(src4D, 0, SMF__NOCREATE_DATA, 0, 0, status);
    if(*status == SAI__OK) {
      int ndata = N * bolCount;

      src3D->dtype   = SMF__DOUBLE;
      src3D->ndims   = 3;
      src3D->dims[0] = nbolX;
      src3D->dims[1] = nbolY;
      src3D->dims[2] = N;
      src3D->dims[3] = 0;
      src3D->lbnd[0] = 0;
      src3D->lbnd[1] = 0;
      src3D->lbnd[2] = 1;
      src3D->pntr[0] = (double*) astMalloc( ndata*sizeof(double) );

      int i = 0;
      int j = 0;
      int k = 0;
      int index = 0;
      int index3D = 0;
      int index4D = 0;

      double freq = 0.0;
      for(i = 0; i < nbolY; i++) {
        for(j = 0; j < nbolX; j++) {
          index = j + i * nbolX;
          for(k = 0; k < N; k++) {
            index3D = index + k * bolCount;
            index4D = index * N + k;
            freq = *((double*) (src4D->pntr[0]) + index4D);
            *((double*) (src3D->pntr[0]) + index3D) = freq;
          }
        }
      }
    } else {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Unable to deep copy 4D data set!", status);
      break;
    }

    smf_close_file(&src4D, status);
    smf_write_smfData(src3D, NULL, NULL, ogrp, fIndex, 0, status);
    smf_close_file(&src3D, status);
  }
  ndfEnd(status);

  grpDelet(&igrp, status);
  grpDelet(&ogrp, status);
}
