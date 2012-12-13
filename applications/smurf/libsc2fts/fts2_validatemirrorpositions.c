/*
*+
*  Name:
*     fts2_validatemirrorpositions.c

*  Purpose:
*     Validate the FTS2 mirror stage positions.
*
*  Language:
*     Starlink ANSI C

*  Type of Module:

*  Invocation:

*  Description:
*     Validate the FTS2 mirror stage positions.
*
*     It is possible that there may be repeating mirror positions.
*       If there are repeating mirror positions at the beginning of the signal,
*       determine where the last index where repeating stops and record it to return.
*       If there are repeating mirror positions at the end of the signal,
*       determine where the first index where repeating starys and record it to return.
*       Return failure if repeating is found anywehere else in the mirror positions.

*  Authors:
*     COBA: Coskun Oba (UoL)
*     MSHERWOOD: Matt Sherwood (UofL)

*  History :
*     2012-05-24 (COBA):
*        Original version.
*     2012-12-12 (MSHERWOOD):
*     	  Removed temporary testing code.

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

#define FUNC_NAME "fts2_validatemirrorpositions"

void fts2_validatemirrorpositions(double* positions, int count, int* ni, int* nf, int* status)
{
  if(*status != SAI__OK) { return; }

  const double EPSILON = 0.001;

  int i = 0;

  // CREATE SHIFTED MIRROR POSITIONS
  double* shifted = (double*) astCalloc(count, sizeof(double));
  for(i = 1; i < count; i++) {
    shifted[i] = positions[i - 1];
  }
  shifted[0] = positions[count - 1];

  // COMPUTE DELTA MIRROR POSITIONS
  double* delta = (double*) astCalloc(count, sizeof(double));
  for(i = 0; i < count; i++) {
    delta[i] = positions[i] - shifted[i];
  }

  // FIND THE START INDEX
  for(i = 0; i < count; i++) {
    if(fabs(delta[i]) >= EPSILON) {
      *ni = i;
      break;
    }
  }

  // FIND THE END INDEX
  for(i = count - 1; i > -1; i--) {
    if(fabs(delta[i]) >= EPSILON) {
      *nf = i;
      break;
    }
  }

/*
  // CHECK TO SEE IF THE POSITIONS HAVE REPEATING VALUES IN BETWEEN
  for(i = *ni + 1; i < *nf; i++) {
    if(fabs(delta[i]) <= EPSILON) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Repeating mirror position values found!", status);
    }
  }
  */
}
