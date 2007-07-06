/*
*+
*  Name:
*     smf_accumulate_prov

*  Purpose:
*     Store file provenance information into keymap

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     smf_accumulate_prov( astKeyMap * keymap, const smfFile * file,
*                    const Grp * igrp, int index, int * status );

*  Arguments:
*     obsidmap = AstKeyMap * (Given & Returned)
*        Keymap for tracking provenance information. Must have been
*        created beforehand.
*     file = const smfFile * (Given)
*        smfFile containing the file name. Can be NULL if a group is being used.
*     igrp = const Grp * (Given)
*        NDG group identifier. If "file" is present and contains a filename in the
*        struct that takes precedence over the Grp.
*     index = int (Given)
*        Index corresponding to required file in group
*     status = int* (Given and Returned)
*        Pointer to inherited status.

*  Description:
*     This function is used to build up provenance information into a
*     keymap suitable for use in a call to smf_fits_add_prov. The filename
*     is determined from the input parameters.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2007-07-05 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Notes:
*     - See smf_fits_add_prov for a routine on adding provenance information
*     to the output FITS header.

*  Copyright:
*     Copyright (C) 2007 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
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

*  Bugs:
*     {note_any_bugs_here}

*-
*/
#include "smf.h"
#include "star/grp.h"
#include "sae_par.h"
#include "ast.h"
#include "merswrap.h"

void
smf_accumulate_prov( AstKeyMap * prvmap, const smfFile * file, const Grp* igrp,
                int index, int * status ) {

  char filename[GRP__SZNAM + 1];    /* name of file from Grp */
  char *pname = NULL;         /* pointer to start of name */

  if ( *status != SAI__OK ) return;

  if ( file ) {
    /* Look at the smfFile struct */
    if (file->name) {
      /* Look for name inside smfFile */
      pname = file->name;
    }
  }

  if (pname == NULL) {
    /* Look in the group */
    if (igrp) {
      pname = filename;
      grpGet( igrp, index, 1, &pname, SMF_PATH_MAX, status);
    }
  }

  if (pname == NULL && *status == SAI__OK) {
    *status = SAI__ERROR;
    errRep( "", "Unable to determine filename for provenance tracking", status );
  }

  /* Need to strip directory information and any file suffix that may have crept
     in */

  /* Store it in the keymap */
  astMapPut0I( prvmap, pname, 1, NULL );

  return;
}
