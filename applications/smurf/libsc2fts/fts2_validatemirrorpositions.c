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
*     MS: Matt Sherwood (UofL)

*  History :
*     2012-05-24 (COBA):
*        Original version.
*     2012-12-12 (MS):
*           Removed temporary testing code.
*     2012-12-21 (MS)
*         Changed validation logic to trim non-uniform data from ends
*         while adapting to mirror speed.
*         Also reverse mirror position array in case of opposite scan direction.
*     2013-04-05 (MS)
*         Reverse data also with mirror position array in case of opposite scan direction.
*     2013-11-25 (MS)
*         Also treat RTS timing values.
*     2014-01-21 (MS)
*         Rewrite RTS values for reverse scans to be increasing in time
*         - The GSL interpolator requires monotonically increasing values for the X dimension
*           so the first reordered RTS timestamp is set to the time of the first original timestamp
*           and subsequent timestamps are incremented by the original difference between successive values
*     2014-04-28 (MS)
*         Detect and override inconsistent SCANDIR
*         - Some historical data indicates a SCANDIR that differs from the actual positions array values.
*           This is now being detected and corrected where necessary.

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

void fts2_validatemirrorpositions(double* positions, double* times, int count, int* ni, int* nf, smfData* inData, int* status)
{
  if(*status != SAI__OK) { return; }

  /* Compute EPSILON as a fraction of the expected mirror position step size (s)
  *  calculated from the SCANVEL (v) and the STEPTIME (t), where:
  *  s = vt
  *  and EPSILON should be reasonably large to ignore jitter, but small enough not to miss valid movement,
  *  let's say at least half way to the next expected mirror position, or:
  *  EPSILON = s/2
  */

  double s = 0.0;
  double t = 0.0;
  smf_fits_getD(inData->hdr, "STEPTIME", &t, status);
  double v = 0.0;
  smf_fits_getD(inData->hdr, "SCANVEL", &v, status);
  s = v*t;
  double EPSILON = s/2.0;

  int i,j,x,y = 0;
  int scanDir      = 0;             /* Mirror scan direction forward==1, reverse==-1 */

  double * copyData;                /* Array of pointers to DATA/VARIANCE/QUALITY */
  size_t nWidth             = 0;
  size_t nHeight            = 0;
  size_t nFrames            = 0;
  size_t nPixels            = 0;
  int bolIndex              = 0;

  smf_fits_getI(inData->hdr, "SCANDIR", &scanDir, status);
  /* Some historical data has a mismatched SCANDIR
     so check for this condition and override as necessary */
  if(scanDir < 0 && positions[count-1] > positions[0]) {
    /*printf("fts2_validatemirrorpositions: Correcting for mismatched SCANDIR of %d!\n", scanDir);*/
      scanDir = 1;
  } else if(scanDir > 0 && positions[0] > positions[count-1]) {
    /*printf("fts2_validatemirrorpositions: Correcting for mismatched SCANDIR of %d!\n", scanDir);*/
      scanDir = -1;
  }

  /* Invert negative scan */
  double* invertedP = NULL;         /* Inverted positions */
  double* invertedT = NULL;         /* Inverted times */
  if(scanDir < 0) {
    /* Copy input data into output data for inversion */
    nWidth  = inData->dims[0];
    nHeight = inData->dims[1];
    nFrames = inData->dims[2];
    nPixels = nWidth * nHeight;
    copyData = (double*) astMalloc((nPixels * nFrames) * sizeof(*copyData));

    invertedP = (double*) astCalloc(count, sizeof(*invertedP));
    invertedT = (double*) astCalloc(count, sizeof(*invertedT));
    for(i=0,j=count-1; i < count; i++,j--) {
      /* Do a simple reversal on the positions */
      invertedP[i] = positions[j];
      if (i==0) {
        /* Set the first inverted time to the first original time of
           the reverse scan */
        invertedT[i] = times[j];
      }
      else {
        /* Set subsequent inverted times to be equal to the previous value
           plus the original difference to the next value */
        invertedT[i] = invertedT[i-1] + (times[j+1] - times[j]);
      }
      for(x = 0; x < nWidth; x++) {
        for(y = 0; y < nHeight; y++) {
          bolIndex = x + y * nWidth;
          copyData[i*nPixels+bolIndex] = *((double*) inData->pntr[0] + j*nPixels+bolIndex);
        }
      }
    }
    /* Copy inverted values back to positions and times */
    for(i=0; i < count; i++) {
      positions[i] = invertedP[i];
      times[i] = invertedT[i];
      for(x = 0; x < nWidth; x++) {
        for(y = 0; y < nHeight; y++) {
          bolIndex = x + y * nWidth;
          *((double*) (inData->pntr[0]) + i*nPixels+bolIndex) = copyData[i*nPixels+bolIndex];
        }
      }
    }
  }

  /* COMPUTE DELTA MIRROR POSITIONS */
  double* delta = (double*) astCalloc(count, sizeof(double));
  for(i = 0; i < count-1; i++) {
    delta[i] = positions[i+1] - positions[i];
  }
/*printf("fts2_validatemirrorpositions: count=%d, EPSILON=%f\n", count, EPSILON);*/

  /* FIND THE START INDEX */
  for(i = 0; i < count-1; i++) {
  /*printf("fts2_validatemirrorpositions: delta[%d]=%f\n", i, delta[i]);*/
    if(delta[i] >= EPSILON) {
      *ni = i;
    /*printf("fts2_validatemirrorpositions: delta[%d]=%f, ni=%d\n", i, delta[i], *ni);*/
      break;
    }
  }

  /* FIND THE END INDEX */
  for(i = count - 1; i > -1; i--) {
  /*printf("fts2_validatemirrorpositions: delta[%d]=%f\n", i, delta[i]);*/
    if(delta[i] >= EPSILON) {
      *nf = i+1;
    /*printf("fts2_validatemirrorpositions: delta[%d]=%f, nf=%d\n", i, delta[i], *nf);*/
      break;
    }
  }

/* CLEANUP: */
  if(delta) {astFree(delta); delta = NULL;}
/*if(shifted) {astFree(shifted); shifted = NULL;}*/
  if(scanDir < 0) {
    if(invertedP) {
        astFree(invertedP);
        invertedP = NULL;
    }
    if(invertedT) {
        astFree(invertedT);
        invertedT = NULL;
    }
    if(copyData) {
      astFree(copyData);
      copyData = NULL;
    }
  }

}
