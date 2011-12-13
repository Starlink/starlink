/*
*+
*  Name:
*     smf_check_smfFts

*  Purpose:
*     Check (and set) all elements of a smfFts structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_check_smfFts(const smfData* idata, smfData* odata, int* status);

*  Arguments:
*     idata = const smfData* (Given)
*        Pointer to input smfData
*     odata = smfData * (Given)
*        Pointer to output smfData
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function checks all elements of a smfFts structure and
*     copies values from the input structure if necessary

*  Authors:
*     COBA: Coskun Oba (UoL)
*     {enter_new_authors_here}

*  History:
*     2010-10-18 (COBA):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science and Technology Facilities Council.
*     Copyright (C) 2006 University of British Columbia. All Rights
*     Reserved.

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
#include <stdio.h>

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_check_smfFts"

void smf_check_smfFts(const smfData* idata, smfData* odata, int* status)
{
  if (*status != SAI__OK) return;

  smfFts* ifts = idata->fts;
  smfFts* ofts = odata->fts;

  if(ofts == NULL) {
    if(ifts != NULL) {
      ofts = smf_deepcopy_smfFts(idata, status);
    } else {
	    *status = SAI__ERROR;
	    errRep( FUNC_NAME,
	            "Input smfFts struct is null. Possible programming error.",
	            status);
    }
    if(ofts == NULL) {
      *status = SAI__ERROR;
      errRep( FUNC_NAME,
              "Unable to allocate memory for new smfFts structure!",
              status);
    } else {
      odata->fts = ofts;
    }
  }
}
