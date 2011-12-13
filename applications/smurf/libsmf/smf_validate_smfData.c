/*
*+
*  Name:
*     smf_validate_smfData

*  Purpose:
*     Ensure that certain components of a smfData are present

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     int smf_validate_smfData( const smfData * data, int hashdr,
*                               int hasfile, int *status );

*  Arguments:
*     data = const smfData* (Given)
*        Header struct to be validated.
*     hashdr = int (Given)
*        If true, the header must be present.
*     hassfile = int (Given)
*        If true, smfFile must be present.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Description:
*     Performs simple check on the smfData struct and returns false
*     and sets status to bad if a check fails. The extent of the checks
*     is controlled by the flag arguments.

*  Returned Value:
*     int = true if the smfData is okay, false otherwise.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2008-12-18 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Notes:
*     "data" must be non-null to pass even the basic check.

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

#include "sae_par.h"
#include "mers.h"

#include "smf.h"

#define FUNC_NAME "smf_validate_smfData"

int smf_validate_smfData( const smfData * data, int hashdr, int hasfile,
                          int *status ) {

  if (*status != SAI__OK) return 0;

  if ( data == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
            "Supplied smfData is a NULL pointer. Possible programming error.",
            status);
    return 0;
  }

  if ( hasfile && data->file == NULL ) {
    *status = SAI__ERROR;
    errRep( FUNC_NAME,
            "No file associated with supplied smfData. "
            "Possible programming error.", status );
    return 0;
  }

  if (hashdr) return smf_validate_smfHead( data->hdr, 0, 0, status );

  return 1;

}
