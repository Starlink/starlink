/*
 *+
 *  Name:
 *     smf_open_asdouble

 *  Purpose:
 *     A wrapper routine to open_and_flatfield, or open_raw_asdouble

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     SMURF subroutine

 *  Invocation:
 *     smf_open_asdouble( const Grp *igrp, size_t index, const smfArray* darks,
 *                        const smfArray* flatramps, int ensureflat,
 *                        smfData **data, int *status );

 *  Arguments:
 *     igrp = const Grp* (Given)
 *        Pointer to an input group
 *     index = size_t (Given)
 *        Index into the group
 *     darks = const smfArray* (Given)
 *        Set of darks that could be applied. Can be NULL.
 *     flatramps = const smfArray * (Given)
 *        Set of flatfield ramps to be assigned to any relevant data files.
 *        Can be NULL.
 *     ensureflat = int (Given)
 *        If true, ensure that the flatfield is applied when opening the data
 *        files, else, if they are already flatfielded read them as is, or if
 *        they are raw files read them as raw files converted to double.
 *     data = smfData** (Returned)
 *        Pointer to a pointer to smfData struct containing _DOUBLE data.
 *        Will be created by this routine, or NULL on error.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Purpose:
 *     In a couple of places we are interested in working with data
 *     that needs to be double-precision, but flatfielding the data if
 *     it is raw is optional. This routine wraps the calls to
 *     smf_open_and_flatfield or smf_open_raw_asdouble as needed.

 *  Authors:
 *     EC: Ed Chapin (UBC)

 *  History:
 *     18-Mar-2011 (EC):
 *        First version factored out of smf_concat_smfGroup

 *  Notes:

 *  Copyright:
 *     Copyright (C) 2011 University of British Columbia.
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

 *-
 */

#include "sae_par.h"
#include "mers.h"
#include "smf_err.h"
#include "prm_par.h"

#include "smf.h"

#define FUNC_NAME "smf_open_asdouble"

void smf_open_asdouble( const Grp *igrp, size_t index, const smfArray* darks,
                        const smfArray* flatramps, int ensureflat,
                        smfData **data, int *status ) {

  if( *status != SAI__OK ) return;

  /* Load data, flatfielding and/or opening raw as double as necessary */
  if( ensureflat ) {
    smf_open_and_flatfield( igrp, NULL, index, darks, flatramps, data,
                            status );
  } else {
    /* open as raw if raw else just open as whatever we have */
    smfData *tmpdata = NULL;
    smf_open_file( igrp, index, "READ", SMF__NOCREATE_DATA, &tmpdata, status );
    if (tmpdata && tmpdata->file && tmpdata->file->isSc2store) {
      smf_open_raw_asdouble( igrp, index, darks, data, status );
    } else {
      smf_open_and_flatfield( igrp, NULL, index, darks, flatramps, data,
                              status );
    }
    smf_close_file( &tmpdata, status );
  }

}
