/*
 *+
 *  Name:
 *     smf_open_raw_asdouble

 *  Purpose:
 *     Open raw files, uncompress and store in double smfData

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SMURF subroutine

 *  Invocation:
 *     smf_open_raw_as_double( ThrWorkForce *wf, const Grp *igrp,
 *                             dim_t index, const smfArray* darks,
 *                             smfData **data, int *status );

 *  Arguments:
 *     wf = ThrWorkForce * (Given)
 *        Pointer to a pool of worker threads
 *     igrp = const Grp* (Given)
 *        Pointer to an input group
 *     index = dim_t (Given)
 *        Index into the group
 *     darks = const smfArray* (Given)
 *        Set of darks that could be applied. Can be NULL.
 *     data = smfData** (Returned)
 *        Pointer to a pointer to smfData struct containing _DOUBLE data.
 *        Will be created by this routine, or NULL on error.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Purpose:
 *     A small wrapper around smf_open_file that ensures that we are
 *     reading raw data and forces the returned smfData to be a malloced
 *     _DOUBLE array. Similar to smf_open_and_flatfield except that it
 *     does not flatfield and does not propagate data to an output file.
 *     The flatfield information will be included in the smfData and
 *     dark subtraction can be applied if necessary.
 *
 *     It will be an error to attempt to open a file that has already
 *     been flatfielded.

 *  Authors:
 *     TIMJ: Tim Jenness (JAC, Hawaii)

 *  History:
 *     02-Oct-2009 (TIMJ):
 *        First version.
 *     10-Jan-2014 (DSB):
 *        Added argument wf.

 *  Notes:
 *     - No file will be associated with the returned smfData.

 *  Copyright:
 *     Copyright (C) 2009,2014 Science & Technology Facilities Council.
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

 *-
 */

#include "sae_par.h"
#include "mers.h"
#include "smf_err.h"
#include "prm_par.h"
#include "star/thr.h"

#include "smf.h"

#define FUNC_NAME "smf_open_raw_asdouble"

void
smf_open_raw_asdouble( ThrWorkForce *wf, const Grp *igrp, dim_t index,
                       const smfArray* darks, smfData **data, int *status ) {

  int flags = 0;            /* Flags for creating smfData clone */
  smfData * indata = NULL;  /* initial smfData associated with file */

  if (*status != SAI__OK) return;

  smf_open_file( wf, igrp, index, "READ", 0, &indata, status );

  if (*status != SAI__OK) goto CLEANUP;

  if (! indata->file->isSc2store ) {
    *status = SAI__ERROR;
    errRep( "", FUNC_NAME ": Must be given a raw data file to open. Aborting",
            status);
    goto CLEANUP;
  }

  /* Make sure we have not flatfielded it */
  smf_check_flat( indata, status );
  if (*status == SMF__FLATN) {
    errRep( " ", "This function can only be run on unflatfielded data", status );
    goto CLEANUP;
  }
  if (*status != SAI__OK) goto CLEANUP;


  /* So the data are not flatfielded - good */

  /* Create a new smfData and copy the data across, converting to _DOUBLE. There shouldn't
     be any variance. */
  flags |= SMF__NOCREATE_FILE;
  *data = smf_deepcopy_smfData( wf, indata, 1, flags, 0, 0, status );

  if ( *status != SAI__OK) {
    errRep( " ", "Unable to allocate enough memory to create _DOUBLE version of smfData",
            status );
    goto CLEANUP;
  }

  /* In principal we could mask out bad bolometers here by looking
     at the time stream and seeing that it is filled with zero. We
     can't convert all 0 to bad though since 0 is a valid reading */

  /* optionally apply dark subtraction */
  smf_apply_dark( *data, darks, status );

 CLEANUP:
  if (indata) smf_close_file( wf, &indata, status );

}
