/*
*+
*  Name:
*     smf_addto_smfArray

*  Purpose:
*     Add smfDatas to a smfArray structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_addto_smfArray( smfArray *ary, smfData *data, int * status );

*  Arguments:
*     ary = smfArray* (Given and Returned)
*        Pointer to smfArray to populate. Should be freed using
*        smf_close_related.
*     data = smfData* (Given)
*        Pointer to smfData to be added
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function adds a given smfData to a smfArray structure. The structure
*     will resize if required but is efficient in the normal usage pattern
*     of storing data from a shared time slice.

*  Notes:
*     For the normal case where the smfArray contains related data
*     from the same time slice but different subarrays, a pre-sized
*     is used. If more than SMF__MXSMF (usually 8) data files are added
*     to the array a dynamically allocated buffer is used. It will be resized
*     on demand.

*  Authors:
*     Andy Gibb (UBC)
*     Ed Chapin (UBC)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-06-02 (AGG):
*        Initial version
*     2007-07-10 (EC):
*        Changed smfArray.sdata to static array and altered behaviour
*        so that smfArray.ndat reflects true number of allocated smfData.
*     2008-07-14 (TIMJ):
*        Use dynamically resizing buffer.
*     2008-08-27 (TIMJ):
*        Dynamically resizing may change the pointer value.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2008 Science and Technology Facilities Council.
*     Copyright (C) 2006 Particle Physics and Astronomy Research
*     Council. Copyright (C) 2006, 2007 University of British Columbia.
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

/* System includes */
#include <stdlib.h>
#include <string.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"
#include "ast.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_addto_smfArray"

void smf_addto_smfArray( smfArray *ary, smfData *data, int *status ) {

  dim_t i;            /* loop counter */
  dim_t ndat;         /* Number of smfDatas that have been added */
  dim_t newsize;      /* Required number of smfDatas */

  if (*status != SAI__OK) return;

  /* Retrieve current number of smfDatas */
  ndat = ary->ndat;
  newsize = ndat + 1;

  /* we need to switch from static to dynamic buffer */
  if (ndat == SMF__MXSMF && ary->dyndata == NULL) {
    ary->dyndata = astCalloc( 2 * ndat, sizeof(*(ary->dyndata)) );

    /* copy from the static to dynamic buffer */
    for (i=0; i<ndat; i++) {
      (ary->dyndata)[i] = (ary->stdata)[i];
      (ary->stdata)[i] = NULL; /* remove confusion */
    }

    /* repoint the main data pointer */
    ary->sdata = ary->dyndata;
  }

  /* Make sure we have enough space in dynamic buffer */
  if (ary->dyndata && ary->dynsize < newsize) {
    smfData **newbuf = NULL;
    dim_t nbins = 1;

    /* double size of buffer each time rather than increment by one
       each time*/
    nbins = 2 * ndat;
    newbuf = astRealloc( ary->dyndata, nbins * sizeof(*newbuf) );
    if (*status == SAI__OK && newbuf ) {
      ary->dyndata = newbuf;
      ary->dynsize = nbins;
      ary->sdata = ary->dyndata;
    }
  }

  /* add pointer to sdata  */
  if (*status == SAI__OK) {
    (ary->sdata)[ndat] = data;
    (ary->ndat)++;
  }

  return;
}
