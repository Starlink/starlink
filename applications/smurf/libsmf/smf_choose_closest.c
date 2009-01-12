/*
*+
*  Name:
*     smf_choose_closest

*  Purpose:
*     Decide which datasets are closest to a reference in time.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_choose_closest( const smfArray *alldata, const smfData *indata,
*                            size_t *previdx, size_t *nextidx, int * status );

*  Arguments:
*     alldata = const smfArray* (Given)
*        Set of observations to search.
*     indata = const smfData * (Given)
*        Reference science observation.
*     previdx = size_t * (Returned)
*        Index in smfArray for the closest previous dataset. SMF__BADIDX if none
*        can be found.
*     nextidx = size_t * (Returned)
*        Index in smfArray for the closest following dataset.
*        SMF__BADIDX if none can be found.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Search through the supplied array to find the closest observations
*     to the supplied reference science observation. There is no threshold
*     applied on a maximum time difference between reference and nearest.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-07-18 (TIMJ):
*        Initial version.
*     2008-07-25 (TIMJ):
*        Be more lenient with time gaps for scan mode. Use SMF__BADIDX
*     2008-11-25 (TIMJ):
*        Rename from smf_choose_darks:27736
*     2009-01-12 (EC):
*        Set status if invalid JCMTState in dark header

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2009 University of British Columbia
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

#define FUNC_NAME "smf_choose_closest"

void smf_choose_closest( const smfArray *alldata, const smfData *indata,
                       size_t *previdx, size_t *nextidx, int * status ) {
  size_t i;          /* loop counter */
  double reftime;    /* MJD of input science data */
  int refsubnum;     /* Subarray number of science data */

  smf_timediff prev; /* information on closest previous */
  smf_timediff next; /* information on closest next */

  *previdx = SMF__BADIDX;
  *nextidx = SMF__BADIDX;

  if (*status  != SAI__OK) return;

  /* get reference MJD and subarray number */
  reftime = (indata->hdr->allState)[0].rts_end;
  smf_find_subarray( indata->hdr, NULL, 0, &refsubnum, status );

  /* initialise the diff structs */
  prev.diff = VAL__MAXD;
  prev.index = SMF__BADIDX;
  next.diff = VAL__MAXD;
  next.index = SMF__BADIDX;

  /* loop through all the darks finding the ones closest in time
     with the correct subarray number */
  for (i=0; (*status==SAI__OK)&&(i<alldata->ndat); i++) {
    smfData *thisdark = (alldata->sdata)[i];
    int thissubnum;
    smf_find_subarray( thisdark->hdr, NULL, 0, &thissubnum, status );

    /* Is there a valid state? */
    if( !thisdark->hdr->allState ) {
      *status = SAI__ERROR;
      errRep( "", FUNC_NAME ": dark does not contain a valid JCMT State", 
              status );
    }

    /* see if we even need to look at the time */
    if ( (*status==SAI__OK) && (thissubnum == refsubnum) ) {
      double thistime = (thisdark->hdr->allState)[0].rts_end;
      double diff = reftime - thistime;
      if (diff >= 0) {
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
      } //else {
        /* should not be possible */
        //if (*status == SAI__OK) {
        //  *status = SAI__ERROR;
        //  errRep(" ","Should not be possible for dark and science "
        //         "observation to have identical MJD times", status );
        //  return;
        //}
      //}
    }
  }

  if (prev.index == SMF__BADIDX) {
    msgOutif( MSG__VERB, " ","Unable to find any prior dataset", status );
  }
  if (next.index == SMF__BADIDX) {
    msgOutif( MSG__VERB, " ","Unable to find any following dataset", status );
  }

  /* store return values */
  *previdx = prev.index;
  *nextidx = next.index;

}
