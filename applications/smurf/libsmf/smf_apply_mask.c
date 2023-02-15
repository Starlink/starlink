/*
*+
*  Name:
*     smf_apply_mask

*  Purpose:
*     Apply bad bolometer masks to data.

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     void smf_apply_mask( ThrWorkForce *wf, smfData *indata,
*                          const smfArray *bbms, smf_bbm_meth method,
*                          smf_qual_t addqual, int *status)

*  Arguments:
*     wf = ThrWorkForce * (Given)
*        Pointer to a pool of worker threads
*     indata = smfData * (Given)
*        Observation to be masked.
*     bbms = smfArray * (Given)
*        Masks for each subarray (e.g. returned by smf_reqest_mask call)
*     method = smf_bbm_meth (Given)
*        Bit mask indicating how the mask should be applied.
*        Can be used to control whether the data are modified, quality
*        is modified or both. If quality is to be modified it must exist
*        if only quality is to be modified. ie, if data are also to be
*        modified quality can be optional.
*     addqual = smf_qual_t (Given)
*        Additional quality that can be applied when masking a bolometer.
*        Only used for SMF__BBM_QUAL and SMF__BBM_QQUAL.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Search through the supplied masks looking for relevant ones, then
*     apply the best to the supplied data set. No error if no suitable mask
*     can be found. Available methods (which can be combined) are
*       SMF__BBM_DATA - Mask the data array with VAL__BADx
*       SMF__BBM_QUAL - Mask the quality array with SMF__Q_BADB|addqual
*       SMF__BBM_QQUAL- Mask the first slice of the quality array with
*                       SMF__Q_BADB|addqual. If both QUAL and QQUAL are
*                       specified QUAL takes precedence.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     EC: Ed Chapin (UBC)
*     {enter_new_authors_here}

*  History:
*     2008-11-26 (TIMJ):
*        Initial version.
*     2008-12-01 (TIMJ):
*        Add QUALITY option
*     2008-12-11 (TIMJ):
*        Mask data before masking quality.
*     2009-01-06 (EC):
*        Added optional external quality array
*     2010-01-08 (AGG):
*        Change BPM to BBM.
*     2010-03-16 (TIMJ):
*        Use smf_smfFile_msg
*     2010-07-06 (TIMJ):
*        Add addqual parameter to allow
*        Allow QUAL mode to work with any time order

*  Notes:
*      - for efficiency use SMF__BBM_QQUAL alone. All other methods
*       touch every element in the time series.
*      - SMF__BBM_QUAL is the only option that works on both time ordered
*       and bolometer ordered data.

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2009-2010 University of British Columbia.
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
*     MA 02110-1301, USA.

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

#define FUNC_NAME "smf_apply_mask"

void smf_apply_mask( ThrWorkForce *wf, smfData *indata,
                     const smfArray *bbms, smf_bbm_meth method,
                     smf_qual_t addqual, int *status) {

  dim_t previdx;
  dim_t nextidx;
  smfData * bbm1 = NULL;
  smfData * bbm2 = NULL;
  smfData * bbm = NULL;
  int masked = 0;
  dim_t nbolo = 0;
  smf_qual_t *qua=NULL;

  if (*status != SAI__OK) return;
  if (!bbms) return;
  if (!indata) {
    *status = SAI__ERROR;
    errRep("", "Supplied smfData for masking is a NULL pointer."
           " (possible programming error)", status );
    return;
  }

  /* work out which masks are suitable */
  smf_choose_closest( bbms, indata, &previdx, &nextidx, status );

  /* Only handles time-ordered at the moment */
  if( indata->isTordered != 1 && method != SMF__BBM_QUAL ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": bolo-ordered data not currently supported",
            status );
    return;
  }

  /* Internal or external quality? */
  qua = smf_select_qualpntr( indata, NULL, status );

  /* Get some properties of the input smfData */
  smf_get_dims( indata, NULL, NULL, &nbolo, NULL, NULL, NULL, NULL, status );

  /* get the file struct and create a token */
  smf_smfFile_msg( indata->file, "FILE", 1, "<no file>");

  /* and correct for dark */
  if (previdx != SMF__BADIDX) bbm1 = bbms->sdata[previdx];
  if (nextidx != SMF__BADIDX) bbm2 = bbms->sdata[nextidx];
  if (bbm1 || bbm2) {
    smfData * thisbbm = NULL;
    if (bbm1) {
      msgSetc("PRIOR", "yes");
      thisbbm = bbm1;
    } else {
      msgSetc("PRIOR", "no");
    }
    if (bbm2) {
      if (!thisbbm) thisbbm = bbm2;
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
    bbm = smf_create_smfData( SMF__NOCREATE_FILE | SMF__NOCREATE_DA |
                              SMF__NOCREATE_HEAD, status );
    if (*status == SAI__OK) {
      dim_t nelem;
      dim_t i;
      double *ddata = NULL;
      int *idata = NULL;
      int *odata = NULL;

      bbm->dtype = SMF__INTEGER;
      bbm->ndims = 2;
      bbm->dims[0] = thisbbm->dims[0];
      bbm->dims[1] = thisbbm->dims[1];

      nelem = bbm->dims[0] * bbm->dims[1];

      /* sanity check */
      if (nelem != nbolo ) {
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          msgSetk( "B1", bbm->dims[0]);
          msgSetk( "B2", bbm->dims[1]);
          msgSetk( "TB", bbm->dims[0] * bbm->dims[1] );
          msgSetk( "NB", nbolo );
          errRep( " ", FUNC_NAME ": the selected bad bolometer mask has a "
                  "different number of elements (^B1 x ^B2 = ^TB) than the data "
                  "it is masking (^NB)",
                  status);
        }
      }

      odata = astMalloc( nelem*smf_dtype_sz(bbm->dtype, status ) );
      bbm->pntr[0] = odata;

      /* now copy in the mask */
      smf_select_pntr( thisbbm->pntr, thisbbm->dtype, &ddata, NULL,
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

    /* mask the data array */
    if (method & SMF__BBM_DATA) {
      masked = 1;
      smf_subtract_dark( indata, bbm, NULL, SMF__DKSUB_PREV, status );
    }

    /* mask the quality array after masking the data array. This results
       in the bad values from the mask being set to SMF__Q_BADDA as well
       as SMF__Q_BADB if data array masking is enabled. This may or may
       not be a good idea.
    */
    if ( method & (SMF__BBM_QUAL | SMF__BBM_QQUAL) ) {
      masked = 1;
      if (qua) {
        if (method & SMF__BBM_QUAL) {
          smf_update_quality( wf, indata, 1, bbm->pntr[0], addqual, 0, status);
        } else {
          /* just mask the first nelem items */
          smf_qual_t maskqual = SMF__Q_BADB | addqual;
          int *mask = bbm->pntr[0];
          dim_t nelem = bbm->dims[0] * bbm->dims[1];
          dim_t i;

          for (i=0; i<nelem; i++) {
            if (mask[i] == VAL__BADI) {
              qua[i] |= maskqual;
            }
          }

        }
      } else if ( ! (method & SMF__BBM_DATA) ) {
        /* request for quality masking when quality array is missing
           and data array is not to be masked */
        if (*status == SAI__OK) {
          *status = SAI__ERROR;
          errRep( " ",
                  FUNC_NAME " quality array is not present so can not mask it",
                  status);
        }
      }
    }

    /* clean up resources */
    smf_close_file(NULL, &bbm, status );

    /* we had a valid mask but did not use it */
    if (!masked && *status == SAI__OK) {
      *status = SAI__ERROR;
      errRep( " ", FUNC_NAME " we had a mask but no valid method was supplied",
              status );
    }

  } else {
    msgOutif(MSG__QUIET, " ",
             "Warning: File ^FILE has no suitable bad bolometer mask",
             status);
  }


}
