/*
*+
*  Name:
*     smurf_fts2_transcorr.c

*  Purpose:
*     Corrects for atmospheric transmission across the spectral dimension.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_transcorr(int *status)

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Corrects for atmospheric transmission across the spectral dimension,
*     given the current PWV and elevation. Transmission data is stored in the
*     form of a wet and dry Tau vs frequency table in the calibration database.

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
#include "par.h"

// SMURF includes
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "smurflib.h"
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "smurf_fts2_transcorr"
#define TASK_NAME "FTS2_TRANSCORR"

void smurf_fts2_transcorr(int *status) 
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

  // Get TAU group
  Grp* taugrp = NULL;
  kpg1Gtgrp("TAU", &taugrp, &size, status);

  // GET AIR MASS
  double airMass = 0;
  parGet0d( "AM", &airMass, status );

  // GET PWV
  double PWV = 0;
  parGet0d( "PWV", &PWV, status );

  ndfBegin();

  // OPEN TAU
  smfData* tauData;
  smf_open_file(taugrp, 1, "READ", SMF__NOCREATE_QUALITY, &tauData, status);
  if(*status != SAI__OK)
  {
    errRep(FUNC_NAME, "Unable to open the TAU file!", status);
    return;
  }
  
  //
  // READ IN TAU INFO
  //
  HDSLoc* hdsLocMore = smf_get_xloc(tauData, "MORE", "EXT", "READ", 0, 0, status);
  // FACTOR
  double tauFactor;
  HDSLoc* hdsLoc = NULL;
  datFind(hdsLocMore, "FACTOR", &hdsLoc, status);
  datGet0D(hdsLoc, &tauFactor, status);
  // DRY DATA
  double* dryData = NULL;
  size_t dryCount;
  datFind(hdsLocMore, "DRY", &hdsLoc, status);
  datSize(hdsLoc, &dryCount, status);
  dryData = (double*) astMalloc(dryCount * sizeof(double));
  datGetVD(hdsLoc, dryCount, dryData, &dryCount, status);  
  // WET DATA
  double* wetData = NULL;
  size_t wetCount;
  datFind(hdsLocMore, "WET", &hdsLoc, status);
  datSize(hdsLoc, &wetCount, status);
  wetData = (double*) astMalloc(wetCount * sizeof(double));
  datGetVD(hdsLoc, wetCount, wetData, &wetCount, status);

  // COMPUTE e^(-TAU)
  double* tau = NULL;
  tau = (double*) astMalloc(dryCount * sizeof(double));
  for(int i = 0; i < dryCount; i++)
  {
    tau[i] = exp(-(airMass * PWV * wetData[i] + dryData[i]));
  }
  // FREE RESOURCES
  astFree(dryData);
  astFree(wetData);
  datAnnul(&hdsLocMore, status);
  datAnnul(&hdsLoc, status);

  // APPLY TRANSMISSION CORRECTION FOR EACH SPECTRUM FILE
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

    double wnFactor = fts2_getWaveNumberFactor(srcData, status);

    int nwn = srcData->dims[2];
    double* tsm = NULL;
    tsm = (double*) astMalloc(nwn * sizeof(double));
    double* ftsWNRel = NULL;
    ftsWNRel = (double*) astMalloc(nwn * sizeof(double));
    for(int i = 0; i < nwn; i++)
    {
      ftsWNRel[i] = i * wnFactor / tauFactor;
    }

    // TODO Replace with fts2_naturalCubicSplineInterpolator
    // csi_simplified(tau, dryCount, ftsWNRel, nwn, tsm);

    int width = srcData->dims[0];
    int height = srcData->dims[1];
    int pixelCount = width * height;

    int index, pixelIndex;
    for(int i = 0; i < height; i++)
    {
      for(int j = 0; j < width; j++)
      {
        pixelIndex = i + j * height;
        for(int k = 0; k < nwn; k++)
        {
          index = pixelIndex + pixelCount * k;
          src[index] /= tsm[k];
        }
      }
    }

    // FREE RESOURCES
    astFree(tsm);
    astFree(ftsWNRel);
  }

  // FREE RESOURCES
  smf_close_file(&tauData, status);

  // END
  ndfEnd(status);
}
