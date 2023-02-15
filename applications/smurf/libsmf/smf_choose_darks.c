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
*                            dim_t *dark1, dim_t *dark2, int * status );

*  Arguments:
*     darks = const smfArray* (Given)
*        Set of dark observations.
*     indata = const smfData * (Given)
*        Reference science observation to choose darks.
*     dark1 = dim_t * (Returned)
*        Index in smfArray for the previous dark associated with this sequence.
*        SMF__BADIDX if none can be found.
*     dark2 = dim_t * (Returned)
*        Index in smfArray for the dark following the sequence of indata.
*        SMF__BADIDX if none can be found.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Search through the supplied darks to find the darks taken either
*     size of the supplied reference science observation.

*  Notes:
*     Uses the SEQCOUNT and SUBARRAY information to determine related
*     darks.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-07-18 (TIMJ):
*        Initial version.
*     2008-07-25 (TIMJ):
*        Be more lenient with time gaps for scan mode. Use SMF__BADIDX
*     2008-11-14 (TIMJ):
*        Use SEQCOUNT to decide relatedness rather than time.
*     2010-03-15 (TIMJ):
*        Ensure that darks come from the same observation as indata.

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

void smf_choose_darks( const smfArray *darks, const smfData *indata,
                       dim_t *dark1, dim_t *dark2, int * status ) {
  dim_t i;          /* loop counter */
  int refseq;        /* Sequence count of input science data */
  sc2ast_subarray_t refsubnum;     /* Subarray number of science data */

  *dark1 = SMF__BADIDX;
  *dark2 = SMF__BADIDX;

  if (*status  != SAI__OK) return;
  if (!darks) return;
  if (!smf_validate_smfData( indata, 1, 0, status ) ) return;

  /* get reference sequence counter and subarray number */
  smf_find_seqcount( indata->hdr, &refseq, status );
  smf_find_subarray( indata->hdr, NULL, (dim_t)0, &refsubnum, status );

  /* Loop through all the darks looking for ones that only differ
     from the reference sequence counter by 1 */
  for (i=0; i< darks->ndat; i++) {
    smfData *thisdark = (darks->sdata)[i];
    sc2ast_subarray_t thissubnum;
    smf_find_subarray( thisdark->hdr, NULL, (dim_t)0, &thissubnum, status );

    /* see if we even need to look at the sequence counter */
    if (thissubnum == refsubnum &&
        strcmp( indata->hdr->obsidss, thisdark->hdr->obsidss ) == 0 ) {
      int thisseq;
      int seqdiff;
      smf_find_seqcount( thisdark->hdr, &thisseq, status );
      seqdiff = refseq - thisseq;

      if ( seqdiff == 1 ) {
        /* Valid previous dark */
        *dark1 = i;
      } else if (seqdiff == -1 ) {
        /* Valid next dark */
        *dark2 = i;
      } else if (seqdiff == 0) {
        /* should not be possible */
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          errRep(" ","Should not be possible for dark and science "
                 "observation to have identical sequence counter.", status );
          return;
        }
      }
    }

    /* finish if we have everything */
    if (*dark1 != SMF__BADIDX && *dark2 != SMF__BADIDX) break;

  }

  if (*dark1 == SMF__BADIDX) {
    msgOutif( MSG__VERB, " ","Unable to find any prior dark", status );
  }
  if (*dark2 == SMF__BADIDX) {
    msgOutif( MSG__VERB, " ","Unable to find any following dark", status );
  }

}
