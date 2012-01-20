/*
*+
*  Name:
*     fts2_getmirrorpositions.c

*  Purpose:
*     Gets the FTS2 mirror stage positions.

*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Gets the FTS2 mirror stage positions.

*  Authors:
*     COBA: Coskun Oba (UoL)

*  History :
*     2011-05-04 (COBA):
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
#include <ctype.h>

/* STARLINK INCLUDES */
#include "ast.h"
#include "star/ndg.h"
#include "ndf.h"
#include "mers.h"
#include "prm_par.h"
#include "sae_par.h"
#include "msg_par.h"
#include "par_err.h"

/* SMURF INCLUDES */
#include "smurf_par.h"
#include "libsmf/smf.h"
#include "libsmf/smf_err.h"
#include "libsmurf/smurflib.h"
#include "sc2da/sc2store.h"
#include "libsc2fts/fts2.h"

#define FUNC_NAME "fts2_getmirrorpositions"

void fts2_getmirrorpositions(smfData* data, double* positions, int* size, int* status)
{
  if(*status != SAI__OK) { return; }

  char ftsMode[SZFITSTR];
  size_t count      = 0;    /* Size */
  HDSLoc* hdsLoc    = NULL; /* Pointer to HDS location */
  HDSLoc* hdsLocPos = NULL; /* Pointer to mirror positions */

  smf_fits_getS(data->hdr, "FTS_MODE", ftsMode, sizeof(ftsMode), status);
  if(strncmp(ftsMode, "FSCAN", 5) == 0 ) {
    hdsLoc = smf_get_xloc(data, "JCMTSTATE", "EXT", "READ", 0, 0, status);
    datFind(hdsLoc, "FTS_POS", &hdsLocPos, status);
    datSize(hdsLocPos, &count, status);
    datGetVD(hdsLocPos, count, positions, &count, status);
    *size = (int) count;
  } else if(strncmp(ftsMode, "STEPINT", 7) == 0 ) {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "STEPINT mode is NOT supported!", status);
  } else {
    *status = SAI__ERROR;
    errRep(FUNC_NAME, "Unknown Scan Mode is found!", status);
  }

  if(hdsLoc) { datAnnul(&hdsLoc, status); }
  if(hdsLocPos) { datAnnul(&hdsLocPos, status); }
}
