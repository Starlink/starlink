/*
*+
*  Name:
*     FTS2TRANSCORR

*  Purpose:
*     Corrects for atmospheric transmission across the spectral dimension.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     ADAM TASK

*  Invocation:
*     smurf_fts2_transcorr(status);

*  Arguments:
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Corrects for atmospheric transmission across the spectral dimension,
*     given the current PWV and elevation. Transmission data is stored in the
*     form of a wet and dry Tau vs frequency table in the calibration database.

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
#define TASK_NAME "FTS2TRANSCORR"

void smurf_fts2_transcorr(int* status)
{
  printf("FTS2TRANSCORR...: NOT IMPLEMENTED YET!\n");

  if( *status != SAI__OK ) { return; }

/*
  int i = 0;
  int j = 0;
  int k = 0;
  int index = 0;
  int pixelIndex = 0;
  int width = 0;
  int height = 0;
  int pixelCount = 0;
  double airMass = 0;
  double PWV = 0;
  double tauFactor = 0.0;
  double* dryData = NULL;
  size_t dryCount = 0;
  size_t fIndex = 0;

  double* src = NULL;
  double wnFactor = 0.0;
  int nwn = 0;
  double* tsm = NULL;
  double* ftsWNRel = NULL;

  double* wetData = NULL;
  size_t wetCount = 0;

  double* tau = NULL;

  HDSLoc* hdsLoc        = NULL;
  HDSLoc* hdsLocFactor  = NULL;
  HDSLoc* hdsLocMore    = NULL;

  Grp* igrp = NULL;
  Grp* ogrp = NULL;
  Grp* taugrp = NULL;
  size_t size;
  size_t outsize;

  smfData* tauData = NULL;
  smfData* srcData = NULL;

  // Get input group
  kpg1Rgndf("IN", 0, 1, "", &igrp, &size, status);
  // Get output group
  kpg1Wgndf("OUT", ogrp, size, size, "Equal number of input and output files expected!", &ogrp, &outsize, status);
  // Get TAU group
  kpg1Gtgrp("TAU", &taugrp, &size, status);

  parGet0d( "AM", &airMass, status ); // GET AIR MASS
  parGet0d( "PWV", &PWV, status ); // GET PWV

  ndfBegin();

  // OPEN TAU
  smf_open_file(taugrp, 1, "READ", SMF__NOCREATE_QUALITY, &tauData, status);
  if(*status != SAI__OK)
  {
    errRep(FUNC_NAME, "Unable to open the TAU file!", status);
    return;
  }

  //
  // READ IN TAU INFO
  //
  hdsLocMore = smf_get_xloc(tauData, "MORE", "EXT", "READ", 0, 0, status);
  // FACTOR
  datFind(hdsLocMore, "FACTOR", &hdsLoc, status);
  datGet0D(hdsLoc, &tauFactor, status);
  // DRY DATA
  datFind(hdsLocMore, "DRY", &hdsLoc, status);
  datSize(hdsLoc, &dryCount, status);
  dryData = (double*) astMalloc(dryCount * sizeof(double));
  datGetVD(hdsLoc, dryCount, dryData, &dryCount, status);
  // WET DATA
  datFind(hdsLocMore, "WET", &hdsLoc, status);
  datSize(hdsLoc, &wetCount, status);
  wetData = (double*) astMalloc(wetCount * sizeof(double));
  datGetVD(hdsLoc, wetCount, wetData, &wetCount, status);

  // COMPUTE e^(-TAU)
  tau = (double*) astMalloc(dryCount * sizeof(double));
  for(i = 0; i < dryCount; i++)
  {
    tau[i] = exp(-(airMass * PWV * wetData[i] + dryData[i]));
  }
  // FREE RESOURCES
  astFree(dryData);
  astFree(wetData);
  datAnnul(&hdsLocMore, status);
  datAnnul(&hdsLoc, status);

  // APPLY TRANSMISSION CORRECTION FOR EACH SPECTRUM FILE
  for(fIndex = 1; fIndex <= size; fIndex++)
  {
    // OPEN SOURCE
    smf_open_file(ogrp, fIndex, "UPDATE", SMF__NOCREATE_QUALITY, &srcData, status);
    if(*status != SAI__OK)
    {
      errRep(FUNC_NAME, "Unable to open source file!", status);
      break;
    }
    src = (double*) (srcData->pntr[0]);

    hdsLoc = smf_get_xloc(srcData, "FTS2DR", "EXT", "READ", 0, 0, status);
    datFind(hdsLoc, "FTS_WN_FACTOR", &hdsLocFactor, status);
    datGet0D(hdsLocFactor, &wnFactor, status);
    datAnnul(&hdsLocFactor, status);
    datAnnul(&hdsLoc, status);

    nwn = srcData->dims[2];
    tsm = (double*) astMalloc(nwn * sizeof(double));
    ftsWNRel = (double*) astMalloc(nwn * sizeof(double));
    for(i = 0; i < nwn; i++)
    {
      ftsWNRel[i] = i * wnFactor / tauFactor;
    }

    // TODO Replace with fts2_naturalcubicsplineinterpolator
    // csi_simplified(tau, dryCount, ftsWNRel, nwn, tsm);

    width = srcData->dims[0];
    height = srcData->dims[1];
    pixelCount = width * height;

    for(i = 0; i < height; i++)
    {
      for(j = 0; j < width; j++)
      {
        pixelIndex = i + j * height;
        for(k = 0; k < nwn; k++)
        {
          index = pixelIndex + pixelCount * k;
          src[index] /= tsm[k];
        }
      }
    }

    // FREE RESOURCES
    astFree(tsm);
    astFree(ftsWNRel);
    smf_close_file(&srcData, status);
  }

  // FREE RESOURCES
  smf_close_file(&tauData, status);

  // END
  ndfEnd(status);
  */
}
