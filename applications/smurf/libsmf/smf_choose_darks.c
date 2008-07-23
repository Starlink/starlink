/*
*+
*  Name:
*     smf_choose_darks

*  Purpose:
*     Decide which darks are relevant

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_choose_darks( const smfArray *darks, const smfData *indata,
*                            size_t *dark1, size_t *dark2, int * status );

*  Arguments:
*     darks = const smfArray* (Given)
*        Set of dark observations.
*     indata = const smfData * (Given)
*        Reference science observation to choose darks.
*     dark1 = size_t * (Returned)
*        Index in smfArray for the closest previous dark. -1 if none
*        can be found.
*     dark2 = size_t * (Returned)
*        Index in smfArray for the closest following dark. -1 if none
*        can be found.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Search through the supplied darks to find the closest observations
*     to the supplied reference science observation.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-18 (TIMJ):
*        Initial version.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     All Rights Reserved.

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

#include <stdio.h>

/* Starlink includes */
#include "mers.h"
#include "sae_par.h"
#include "prm_par.h"

/* SMURF includes */
#include "libsmf/smf.h"

typedef struct {
  size_t index;
  double diff;
} smf_timediff;

void smf_choose_darks( const smfArray *darks, const smfData *indata,
                       size_t *dark1, size_t *dark2, int * status ) {
  size_t i;          /* loop counter */
  const double maxdiff = 15.0; /* maximum gap between dark and science */
  double reftime;    /* MJD of input science data */
  int refsubnum;     /* Subarray number of science data */

  smf_timediff prev; /* information on closest previous */
  smf_timediff next; /* information on closest next */

  *dark1 = (size_t)-1;
  *dark2 = (size_t)-1;

  if (*status  != SAI__OK) return;

  /* get reference MJD and subarray number */
  reftime = (indata->hdr->allState)[0].rts_end;
  smf_find_subarray( indata->hdr, NULL, 0, &refsubnum, status );

  /* initialise the diff structs */
  prev.diff = VAL__MAXD;
  prev.index = -1;
  next.diff = VAL__MAXD;
  next.index = -1;

  /* loop through all the darks finding the ones closest in time
     with the correct subarray number */
  for (i=0; i< darks->ndat; i++) {
    smfData *thisdark = (darks->sdata)[i];
    int thissubnum;
    smf_find_subarray( thisdark->hdr, NULL, 0, &thissubnum, status );

    /* see if we even need to look at the time */
    if (thissubnum == refsubnum) {
      double thistime = (thisdark->hdr->allState)[0].rts_end;
      double diff = reftime - thistime;
      if (diff > 0) {
        if (prev.diff > diff) {
          prev.diff = diff;
          prev.index = i;
        }
      } else if (diff < 0) {
        diff = fabs(diff);
        if (next.diff > diff) {
          next.diff = diff;
          next.index = i;
        }
      } else {
        /* should not be possible */
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          errRep(" ","Should not be possible for dark and science "
                 "observation to have identical MJD times", status );
          return;
        }
      }
    }
  }

  /* if we found a previous, see how close it really was
      - reduced darks still have all JCMTState if processed by
      SMURF internally.
  */
  if (prev.index != (size_t)-1) {
    smfData *thisdark = (darks->sdata)[prev.index];
    size_t endframe = thisdark->hdr->nframes - 1;
    double endtime = (thisdark->hdr->allState)[endframe].rts_end;
    double difftime = reftime - endtime;

    /* convert to seconds */
    difftime *= SPD;
    if ( difftime > maxdiff ) {
      /* dark is no good */
      prev.index = -1;
    }

  }

  if (next.index != (size_t)-1) {
    smfData *thisdark = (darks->sdata)[next.index];
    size_t endframe = indata->hdr->nframes - 1;
    double starttime = (thisdark->hdr->allState)[0].rts_end;
    double refendtime = (indata->hdr->allState)[endframe].rts_end;
    double difftime = starttime - refendtime;

    /* convert to seconds */
    difftime *= SPD;
    if ( difftime > maxdiff ) {
      /* dark is no good */
      next.index = -1;
    }

  }

  if (prev.index == (size_t)-1) {
    msgOutif( MSG__VERB, " ","Unable to find any prior dark", status );
  }
  if (next.index == (size_t)-1) {
    msgOutif( MSG__VERB, " ","Unable to find any following dark", status );
  }

  /* store return values */
  *dark1 = prev.index;
  *dark2 = next.index;

}
