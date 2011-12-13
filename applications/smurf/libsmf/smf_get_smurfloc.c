/*
*+
*  Name:
*     smf_get_smurfloc

*  Purpose:
*     Get a locator to the SMURF extension, creating it if necessary (and if possible)

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     HDSLoc * smf_get_smurfloc( const smfData *data, const char *  accmode,
*                                int *status );

*  Arguments:
*     data = const smfData* (Given)
*         Data file in which to locate the SMURF extension.
*     accmode = const char * (Given)
*         Access mode to use to open the SMURF extension.
*     status = int * (Given and Returned)
*         Pointer to inherited status.

*  Description:
*     Returns a locator to the SMURF extension. The extension will be opened
*     with the same mode as the primary NDF. If the extension is missing and
*     the requested access mode is UPDATE or WRITE mode the extension will be
*     created. If the extension is missing and READ access is required
*     then a NULL pointer will be returned.

*  Returned Value:
*     HDSLoc * = smf_get_smurfloc()
*        HDS locator of the SMURF extension. Will return NULL on error, including
*        if the SMURF extension is missing and the file is opened for READ access.

*  See Also:
*      smf_get_xloc

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2009-SEP-18 (TIMJ):
*        Original version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
#include "sae_par.h"
#include "star/hds.h"

/* SMURF includes */
#include "libsmf/smf.h"
#include "libsmf/smf_typ.h"

HDSLoc * smf_get_smurfloc( const smfData * data, const char * accmode, int * status ) {
  if (*status != SAI__OK) return NULL;
  return smf_get_xloc( data, SMURF__EXTNAME, SMURF__EXTTYPE, accmode, 0, 0, status );
}
