/*
*+
*  Name:
*     fts2.c

*  Purpose:
*     Implementation of general purpose FTS2 methods.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Implementation of general purpose FTS2 methods.

*  Authors:
*     Coskun (Josh) OBA (UoL)

*  History :
*     Created: July 9, 2010

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

#include <string.h>
#include <stdio.h>
#include <ctype.h>

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
#include "libsmf/smf_err.h"
#include "sc2da/sc2store.h"
#include "libsmurf/smurflib.h"
#include "libsc2fts/fts2.h"

/* Determines whether the FTS-2 is in the beam. */
bool fts2_isInBeam(smfData* data, int* status)
{
  char inbeam[SZFITSCARD+1];
  smf_fits_getS(data->hdr, "INBEAM", inbeam, sizeof(inbeam), status);

  char* upperCase = NULL;
  upperCase = inbeam;
  for(upperCase = inbeam; *upperCase; upperCase++)
  {
    *upperCase = toupper(*upperCase);
  }

  if(strncmp(upperCase, "FTS", 3) == 0 )
  {
    return TRUE;
  }
  else
  {
    return FALSE;
  }
}

/* Gets the FTS-2 scanning mode. */
FTS2Mode fts2_getScanMode(smfData* data, int* status)
{
  int mode;
  char ftsMode[SZFITSCARD+1];
  smf_fits_getS(data->hdr, "FTS_MODE", ftsMode, sizeof(ftsMode), status);
  if(strncmp(ftsMode, "FSCAN", 5) == 0 )
  {
	  mode = FSCAN;
  }
  else if(strncmp(ftsMode, "STEPINT", 7) == 0)
  {
	  mode = STEPINT;
  }
  else
  {
    mode = UNKNOWN;
  }
  return mode;
}

/* Gets the mirror position when in STEPINT mode, [mm]. */
double fts2_getMirrorPosition(smfData* data, int* status)
{
  double position = 0.0;
  smf_fits_getD(data->hdr, "MIRPOS", &position, status);
  return position;
}

/* Gets the mirror scan rate in FSCAN mode, [mm/s]. */
double fts2_getScanVelocity(smfData* data, int* status)
{
  double scanVel = 0.0;
  smf_fits_getD(data->hdr, "SCANVEL", &scanVel, status);
  return scanVel;
}

/*
 * Gets the FTS-2 positions.
 * Caller is responsible for freeing the returned positions.
 * Example: smf_free(positions, status);
 */
double* fts2_getPositions(smfData* data, int* status)
{
  size_t count;

  HDSLoc* hdsLoc = smf_get_xloc(data, "JCMTSTATE", "EXT", "READ", 0, 0, status);
  HDSLoc* hdsLocPosition = NULL;
  datFind(hdsLoc, "FTS_POS", &hdsLocPosition, status);
  datSize(hdsLocPosition, &count, status);
  double* positions = (double*) malloc(count * sizeof(double));
  float* tmp = (float*) malloc(count * sizeof(float));
  datGetVR(hdsLocPosition, count, tmp, &count, status);
  if(*status == SAI__OK)
  {
    for(int i = 0; i < (int) count; i++)
    {
	  positions[i] = (double) tmp[i];
    }
  }
  // FREE RESOURCES
  free(tmp);
  datAnnul(&hdsLoc, status);
  datAnnul(&hdsLocPosition, status);

  return positions;
}

/* Gets the wave number factor. */
double fts2_getWaveNumberFactor(smfData* data, int* status)
{
  double wnFactor = 0.0;
  HDSLoc* hdsLoc = smf_get_xloc(data, "FTS2DR", "EXT", "READ", 0, 0, status);
  HDSLoc* hdsLocFactor = NULL;
  datFind(hdsLoc, "FTS_WN_FACTOR", &hdsLocFactor, status);
  datGet0D(hdsLocFactor, &wnFactor, status);
  // FREE RESOURCES
  datAnnul(&hdsLocFactor, status);
  datAnnul(&hdsLoc, status);
  return wnFactor;
}
