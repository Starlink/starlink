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
*                            dim_t *previdx, dim_t *nextidx, int * status );

*  Arguments:
*     alldata = const smfArray* (Given)
*        Set of observations to search.
*     indata = const smfData * (Given)
*        Reference science observation.
*     previdx = dim_t * (Returned)
*        Index in smfArray for the closest previous dataset. SMF__BADIDX if none
*        can be found.
*     nextidx = dim_t * (Returned)
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
*        -Set status if invalid JCMTState in dark header
*        -Don't set bad status if dark time stamp same as data
*     2009-01-13 (TIMJ):
*        use smf_find_dateobs

*  Copyright:
*     Copyright (C) 2008, 2009 Science and Technology Facilities Council.
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
*     Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
*     MA 02110-1301, USA

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
  dim_t index;
  double diff;
} smf_timediff;

#define FUNC_NAME "smf_choose_closest"

void smf_choose_closest( const smfArray *alldata, const smfData *indata,
                       dim_t *previdx, dim_t *nextidx, int * status ) {
  dim_t i;          /* loop counter */
  double reftime;    /* MJD of input science data */
  sc2ast_subarray_t refsubnum; /* Subarray number of science data */

  smf_timediff prev; /* information on closest previous */
  smf_timediff next; /* information on closest next */

  *previdx = SMF__BADIDX;
  *nextidx = SMF__BADIDX;

  if (*status  != SAI__OK) return;
  if (!indata) return;

  /* get reference MJD and subarray number */
  smf_find_dateobs( indata->hdr, &reftime, NULL, status );
  smf_find_subarray( indata->hdr, NULL, 0, &refsubnum, status );

  /* initialise the diff structs */
  prev.diff = VAL__MAXD;
  prev.index = SMF__BADIDX;
  next.diff = VAL__MAXD;
  next.index = SMF__BADIDX;

  /* loop through all the files finding the ones closest in time
     with the correct subarray number */
  for (i=0; (*status==SAI__OK)&&(i<alldata->ndat); i++) {
    smfData *thisfile = (alldata->sdata)[i];
    sc2ast_subarray_t thissubnum;
    smf_find_subarray( thisfile->hdr, NULL, 0, &thissubnum, status );

    /* see if we even need to look at the time */
    if ( (*status==SAI__OK) && (thissubnum == refsubnum) ) {
      double thistime;
      double diff;
      smf_find_dateobs( thisfile->hdr, &thistime, NULL, status );
      diff = reftime - thistime;
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
      }
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
