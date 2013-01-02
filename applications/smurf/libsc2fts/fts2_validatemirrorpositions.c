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
*     2012-12-21 (MSHERWOOD)
*         Changed validation logic to trim non-uniform data from ends
*         while adapting to mirror speed.
*         Also reverse mirror position array in case of opposite scan direction.

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

void fts2_validatemirrorpositions(double* positions, int count, int* ni, int* nf, smfData* inData, int* status)
{
  if(*status != SAI__OK) { return; }

  printf("fts2_validatemirrorpositions: BEGIN\n",ni);

  // Compute EPSILON as a fraction of the expected mirror position step size (s)
  // calculated from the SCANVEL (v) and the STEPTIME (t), where:
  // s = vt
  // and EPSILON should be reasonably large to ignore jitter, but small enough not to miss valid movement, 
  // let's say at least half way to the next expected mirror position, or:
  // EPSILON = s/2
  
  double s = 0.0;
  double t = 0.0;
  smf_fits_getD(inData->hdr, "STEPTIME", &t, status);
  double v = 0.0;
  smf_fits_getD(inData->hdr, "SCANVEL", &v, status);
  s = v*t;
  double EPSILON = s/2;

  printf("fts2_validatemirrorpositions: StepSize(s): %f = STEPTIME(t): %f * SCANVEL(v): %f\n",s,t,v);
  printf("fts2_validatemirrorpositions: EPSILON: %f = StepSize(s): %f / 2\n",EPSILON,s);
  
  int i,j = 0;
  double positive = 0.0;
  double negative = 0.0;
  double direction = 0.0;
  
  
  
  // Determine scan direction
  /* Mirror scans are supposed to be unidirectional (monotonically increasing or decreasing)
	 but can have slow starts or trailing ends where there is little to no significant change.
	 Determine the majority direction of this scan: positive or negative.
  */
  for(i=0; i<count-1; i++) {
	if(positions[i] < positions[i+1])
	  positive += (positions[i+1] - positions[i]);
	else if(positions[i] > positions[i+1])
	  negative += (positions[i] - positions[i+1]);
  }
  direction = positive - negative;
  printf("fts2_validatemirrorpositions: direction: %f = positive: %f - negative: %f\n",direction,positive,negative);
  
  // Invert negative scan
  double* inverted = NULL;
  if(direction < 0) {
	printf("fts2_validatemirrorpositions: Inverting mirror positions\n");
	inverted = (double*) astCalloc(count, sizeof(double));
    for(i=0,j=count-1; i < count; i++,j--) {
	  inverted[i] = positions[j];
    }
	// Copy inverted values back to positions
    for(i=0; i < count; i++) {
		positions[i] = inverted[i];
    }
  }

/*
  // CREATE SHIFTED MIRROR POSITIONS
  double* shifted = (double*) astCalloc(count, sizeof(double));
  for(i = 1; i < count; i++) {
    shifted[i] = positions[i - 1];
  }
  shifted[0] = positions[count - 1];
*/
  
  // COMPUTE DELTA MIRROR POSITIONS
  double* delta = (double*) astCalloc(count, sizeof(double));
  for(i = 0; i < count-1; i++) {
    delta[i] = positions[i+1] - positions[i];
  }

  // FIND THE START INDEX
  for(i = 0; i < count-1; i++) {
    if(delta[i] >= EPSILON) {
      *ni = i;
	  printf("fts2_validatemirrorpositions: ni: %i\n",*ni);
      break;
    }
  }

  // FIND THE END INDEX
  for(i = count - 1; i > -1; i--) {
    if(delta[i] >= EPSILON) {
      *nf = i+1;
	  printf("fts2_validatemirrorpositions: nf: %i\n",*nf);
      break;
    }
  }

/*
  // CHECK TO SEE IF THE POSITIONS HAVE REPEATING VALUES IN BETWEEN
  for(i = *ni + 1; i < *nf; i++) {
    if(abs(delta[i]) <= EPSILON) {
      *status = SAI__ERROR;
      errRep(FUNC_NAME, "Repeating mirror position values found!", status);
    }
  }
  */
  
// CLEANUP:
  if(delta) {astFree(delta); delta = NULL;}
//if(shifted) {astFree(shifted); shifted = NULL;}
  if(direction < 0 && inverted) {astFree(inverted); inverted = NULL;}

  printf("fts2_validatemirrorpositions: END\n",ni);

}
