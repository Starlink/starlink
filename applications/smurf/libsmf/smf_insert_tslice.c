/*
*+
*  Name:
*     smf_insert_tslice

*  Purpose:
*     Inserts a given time slice into a time series

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_insert_tslice( smfData **idata, smfData *tdata, int index, int * status);

*  Arguments:
*     idata = smfData** (Given and Returned)
*        Data structure containing time series data.
*        The smfHead item in the structure will be updated to receive
*        the updated FrameSet. In addition, if sc2head is non-null, the
*        contents of hdr->sc2head will be updated for this time slice.
*     tdata = smfData* (Given)
*        Data structure containing the data for a given time slice
*     index = int (Given)
*        Index of time slice
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This routine is used to reinsert a 2-D array for a given
*     timeslice back into the original timeseries cube. This allows
*     modified data to be stored. The pointer to the 2-D array is
*     freed in this routine.

*  Authors:
*     Andy Gibb (UBC)
*     {enter_new_authors_here}

*  History:
*     2006-01-09 (AGG):
*        Initial version.
*     2007-12-18 (AGG):
*        Update to use new smf_free behaviour
*     {enter_further_changes_here}

*  Notes:
*

*  Copyright:
*     Copyright (C) 2006 University of British Columbia.
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

/* Starlink includes */
#include "ast.h"
#include "sae_par.h"
#include "mers.h"
#include "star/hds_types.h"

/* Standard includes */
#include <string.h>
#include <stdio.h>

/* SMURF includes */
#include "smf.h"
#include "smf_typ.h"

/* Simple default string for errRep */
#define FUNC_NAME "smf_insert_tslice"

void smf_insert_tslice (smfData **idata, smfData *tdata, int index, int *status ) {

  int i;                 /* Loop counter */
  int npts;              /* Number of points in the timeslice */
  int offset;

  void *ipntr[3];
  void *tpntr[3];

  double *indataArr;
  double *tdataArr;

  /* Number of points in time slice */
  npts = (tdata->dims)[0] * (tdata->dims)[1];

  /* Should check that npts in the given timeslice is the same as the
     no of points as a single timeslice in the input */

  ipntr[0] = ((*idata)->pntr)[0];
  indataArr = ipntr[0];

  tpntr[0] = (tdata->pntr)[0];
  tdataArr = tpntr[0];

  /* Offset into the 3-d timeseries */
  offset = npts * index;

  /* Store new values in timeseries */
  for (i=0; i<npts; i++) {
    indataArr[offset + i] = tdataArr[i];
  }

  /* Free tdata so we don't leak memory */
  tdata = astFree( tdata );
}
