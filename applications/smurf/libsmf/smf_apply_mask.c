/*
*+
*  Name:
*     smf_apply_dark

*  Purpose:
*     Given a data set and a collection of darks, subtract the relevant dark.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_apply_dark( smfData *indata, const smfArray *bpms,
*                          int *status);

*  Arguments:
*     indata = const smfData * (Given)
*        Observation to be masked.
*     darks = const smfArray* (Given)
*        Set of masks to search. Can be NULL to ignore masks.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Search through the supplied masks looking for relevant ones, then
*     apply the best to the supplied data set. No error
*     if no suitable mask can be found.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-11-26 (TIMJ):
*        Initial version.

*  Notes:
*     - Currently calls smf_subtract_dark to apply the mask. This has
*       the disadvantage that every element in the input time series
*       must be visited. May be more efficient to simply use quality
*       to set the mask in the first time slice.

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


void smf_apply_mask( smfData *indata, const smfArray *bpms,
                     int *status) {

  size_t previdx;
  size_t nextidx;
  smfData * bpm1 = NULL;
  smfData * bpm2 = NULL;
  smfData * bpm = NULL;
  smfFile * file = NULL;

  if (*status != SAI__OK) return;
  if (!bpms) return;

  /* work out which masks are suitable */
  smf_choose_closest( bpms, indata, &previdx, &nextidx, status );

  /* get the file struct and create a token */
  file = indata->file;
  if (file) {
    msgSetc( "FILE", file->name );
  } else {
    msgSetc( "FILE", "<no file>" );
  }

  /* and correct for dark */
  if (previdx != SMF__BADIDX) bpm1 = bpms->sdata[previdx];
  if (nextidx != SMF__BADIDX) bpm2 = bpms->sdata[nextidx];
  if (bpm1 || bpm2) {
    smfData * thisbpm = NULL;
    if (bpm1) {
      msgSetc("PRIOR", "yes");
      thisbpm = bpm1;
    } else {
      msgSetc("PRIOR", "no");
    }
    if (bpm2) {
      if (!thisbpm) thisbpm = bpm2;
      msgSetc("POST", "yes");
    } else {
      msgSetc("POST", "no");
    }
    msgOutif(MSG__VERB," ", "Applying mask to ^FILE."
             " Prior mask: ^PRIOR  Following mask: ^POST", status);

    /* Use smf_subtract_dark and create a 2d mask by replacing
       all data with 0 and leaving bads inplace. To start with
       we always use either previous or next but do not AND them to get
       a global mask. */
    bpm = smf_create_smfData( SMF__NOCREATE_FILE | SMF__NOCREATE_DA |
                              SMF__NOCREATE_HEAD, status );
    if (*status == SAI__OK) {
      size_t nelem;
      size_t i;
      double *ddata = NULL;
      int *idata = NULL;
      int *odata = NULL;

      bpm->dtype = SMF__INTEGER;
      bpm->ndims = 2;
      bpm->dims[0] = thisbpm->dims[0];
      bpm->dims[1] = thisbpm->dims[1];

      nelem = bpm->dims[0] * bpm->dims[1];
      odata = smf_malloc( nelem, smf_dtype_sz(bpm->dtype, status ),
                          0, status);
      bpm->pntr[0] = odata;

      /* now copy in the mask */
      smf_select_pntr( thisbpm->pntr, thisbpm->dtype, &ddata, NULL,
                       &idata, NULL, status);
      if (ddata) {
        for (i = 0; i < nelem; i++) {
          if ( ddata[i] != VAL__BADD) {
            odata[i] = 0;
          } else {
            odata[i] = VAL__BADI;
          }
        }
      } else if (idata) {
        for (i = 0; i < nelem; i++) {
          if ( idata[i] != VAL__BADI) {
            odata[i] = 0;
          } else {
            odata[i] = VAL__BADI;
          }
        }
      } else {
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          errRep(" ", "Unexpectedly failed to find data of correct type",
                 status);
        }
      }
    }

    smf_subtract_dark( indata, bpm, NULL, SMF__DKSUB_PREV, status );
    smf_close_file(&bpm, status );
  } else {
    msgOutif(MSG__QUIET, " ",
             "Warning: File ^FILE has no suitable dark frame",
             status);
  }


}
