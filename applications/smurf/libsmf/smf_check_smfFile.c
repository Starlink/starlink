/*
*+
*  Name:
*     smf_check_smfFile

*  Purpose:
*     Check (and set) all elements of a smfFile structure

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Subroutine

*  Invocation:
*     smf_check_smfFile( const smfData *idata, smfData *odata, int * status );

*  Arguments:
*     idata = const smfData* (Given)
*        Pointer to input smfData
*     odata = smfData * (Given)
*        Pointer to output smfData
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     This function checks all elements of a smfFile structure and
*     copies values from the input structure if necessary

*  Authors:
*     Andy Gibb (UBC)
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2006-04-03 (AGG):
*        Initial version.
*     2010-03-16 (TIMJ):
*        ofile->name is non-NULL by definition because it's part of the struct.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010 Science & Technology Facilities Council.
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

/* Starlink includes */
#include "sae_par.h"
#include "mers.h"
#include "ndf.h"

/* SMURF routines */
#include "smf.h"
#include "smf_typ.h"
#include "smf_err.h"

#define FUNC_NAME "smf_check_smfFile"

void smf_check_smfFile( const smfData *idata, smfData *odata, int * status ) {

  smfFile *ofile = NULL;   /* New smfFile */
  smfFile *ifile = NULL;   /* New smfFile */

  if (*status != SAI__OK) return;

  /* All the checks are of the type: does it exist? If no, copy from
     input. If yes check it's either self-consistent or the same as
     the input. Set status to bad and report and error if there are any
     errors */

  /* This is a little trickier than the others */

  ofile = odata->file;
  ifile = idata->file;

  /* Check NDF ID & filename simultaneously */
  /* If both exist then we have to assume that everything's OK */
  if ( ofile->ndfid == 0 ) {
    if ( ofile->name == NULL || strlen(ofile->name) == 0) {
      msgOutif(MSG__VERB," ", "NDF ID is zero, and filename is NULL. Assuming that is correct...", status);
    } else {
      if ( *status == SAI__OK ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "NDF ID is zero, but filename is set.", status);
      }
    }
  } else {
    if ( ofile->name == NULL || strlen(ofile->name) == 0) {
      if ( *status == SAI__OK ) {
        *status = SAI__ERROR;
        errRep(FUNC_NAME, "NDF ID is not zero, but filename is NULL.", status);
      }
    }
  }

  /* Is it time series data? */
  if ( !(ofile->isTstream) ) {
    if ( odata->ndims == 3 ) {
      ofile->isTstream = 1;
    }
  } else {
    if ( odata->ndims != 3 ) {
      msgOutif(MSG__VERB," ", "2D data has isTstream = 1: overriding and setting to zero", status);
      ofile->isTstream = 0;
    }
  }

  /* Were the data opened by the sc2store lib? */
  if ( !(ofile->isSc2store) ) {
    /* Check input value */
    if ( ifile->isSc2store == 1 ) {

    }
  } else {

  }

}
