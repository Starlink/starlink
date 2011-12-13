/*
 *+
 *  Name:
 *     smf_write_clabels

 *  Purpose:
 *     Write units, label and title to file.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     smf_write_clabels( const smfData *data, int * status );

 *  Arguments:
 *     data = const smfData* (Given)
 *        Data struct. If no smfFile or smfHead component are present
 *        returns without action.
 *     status = int (Given & Returned)
 *        inherited status.

 *  Description:
 *     Copies the title, label and units from a smfHead into the file
 *     associated with a smfFile.

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2008-04-30 (TIMJ):
 *        Original version.

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

 *-
 */

/* System includes */
#include <string.h>

/* Starlink includes */
#include "ndf.h"
#include "sae_par.h"
#include "smf.h"
#include "smf_typ.h"

void smf_write_clabels( const smfData* data, int * status ) {
  smfHead* hdr  = NULL;
  smfFile* file = NULL;

  if (*status != SAI__OK) return;

  hdr = data->hdr;
  file = data->file;

  if (hdr == NULL || file == NULL) return;
  if (file->ndfid == NDF__NOID) return;

  if (strlen(hdr->units)) {
    ndfCput( hdr->units, file->ndfid, "UNITS", status);
  }

  if (strlen(hdr->dlabel)) {
    ndfCput( hdr->dlabel, file->ndfid, "LABEL", status);
  }

  if (strlen(hdr->title)) {
    ndfCput( hdr->title, file->ndfid, "TITLE", status);
  }

  return;
}
