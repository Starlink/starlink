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
*     int smf_apply_dark( smfData *indata, const smfArray *darks,
*                          int *status);

*  Arguments:
*     indata = const smfData * (Given)
*        Observation to be dark subtracted.
*     darks = const smfArray* (Given)
*        Set of dark observations to search. Can be NULL to ignore darks.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     Returns true if a dark was subtracted, returns false if the data
*     are unchanged.

*  Description:
*     Search through the supplied darks looking for relevant ones, then
*     subtract the dark (or darks) from the supplied data set. No error
*     if no suitable dark can be found.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-11-26 (TIMJ):
*        Initial version. Copied from smf_open_and_flatfield.
*     2010-03-12 (TIMJ):
*        Add return value to indicate whether a dark was subtracted or not.
*     2010-03-16 (TIMJ):
*        Use smf_smfFile_msg

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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


int smf_apply_dark( smfData *indata, const smfArray *darks,
                     int *status) {

  dim_t dark1;
  dim_t dark2;
  smfData * dkdata1 = NULL;
  smfData * dkdata2 = NULL;
  int retval = 0;

  if (*status != SAI__OK) return retval;
  if (!darks) return retval;

  /* work out which darks are suitable */
  smf_choose_darks( darks, indata, &dark1, &dark2, status );

  /* get the file struct and create a token */
  smf_smfFile_msg( indata->file, "FILE", 1, "<no file>" );

  /* and correct for dark */
  if (dark1 != SMF__BADIDX) dkdata1 = darks->sdata[dark1];
  if (dark2 != SMF__BADIDX) dkdata2 = darks->sdata[dark2];
  if (dkdata1 || dkdata2) {

    if (dkdata1) {
      msgSetc("PRIOR", "yes");
    } else {
      msgSetc("PRIOR", "no");
    }
    if (dkdata2) {
      msgSetc("POST", "yes");
    } else {
      msgSetc("POST", "no");
    }
    msgOutif(MSG__VERB," ", "Dark subtracting ^FILE."
             " Prior dark: ^PRIOR  Following dark: ^POST", status);
    smf_subtract_dark( indata, dkdata1, dkdata2, SMF__DKSUB_CHOOSE, status );
    retval = 1;
  } else {
    msgOutif(MSG__QUIET, " ",
             "Warning: File ^FILE has no suitable dark frame",
             status);
  }

  return retval;
}
