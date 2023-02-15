/*
*+
*  Name:
*     smf_subinst_keymap

*  Purpose:
*     Create keymap with all relevant subinstruments and flag indicate current one

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     Library routine

*  Invocation:
*     keymap = smf_subinst_keymap( smf_subinst_t subinst, const smfData * indata,
*                                  const Grp * igrp, int idx, int * status );

*  Arguments:
*     subinst = smf_subinst_t (Given)
*        The current sub-instrument. I the supplied, value is SMF__SUBINST_NONE,
*        the current sub-instrument will be determined from "indata" or "igrp/idx".
*     indata = const smfData * (Given)
*        If non-null and if subinst is SMF__SUBINST_NONE, the header of this
*        smfData will be used to define the current sub-instrument.
*     igrp = const Grp * (Given)
*        If non-null and if indata is non-null and if subinst is
*        SMF__SUBINST_NONE, the group (along with idx) will be used to
*        determine which file should be opened.
*     idx = int (Given)
*        index into igrp.
*     status = int* (Given and Returned)
*        Pointer to global status.

*  Returned Value:
*     keymap = AstKeyMap *
*        Keymap created by this routine. Should be annulled when no longer required.
*        Returns NULL pointer on error.

*  Description:
*     Create a keymap and put an entry in it for ever sub-instrument that is
*     known about by SMURF. Open the relevant file and extract it's sub-instrument
*     name and indicate that this is the active sub-instrument by setting
*     the relevant value in the keymap to true.

*  Authors:
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2010-06-08 (TIMJ):
*        Initial version
*     2011-04-20 (DSB):
*        Added argument subinst.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2010-2011 Science and Technology Facilities Council.
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

#include "smf.h"

#include "mers.h"
#include "sae_par.h"
#include "ast.h"

AstKeyMap *smf_subinst_keymap( smf_subinst_t subinst, const smfData * indata,
                               const Grp * igrp, int idx, int * status ) {

  const smfHead * hdr = NULL;        /* Header of file to be examined */
  dim_t i;
  smfData * sub_data = NULL;         /* File to be examined */
  AstKeyMap * sub_instruments;       /* Keymap to be filled */

  if (*status != SAI__OK) return NULL;

  if (subinst == SMF__SUBINST_NONE && !indata && !igrp) {
    *status = SAI__ERROR;
    errRep( "", "Must supply either a subinst, a smfData or a Grp"
            " (possible programming error)", status );
    return NULL;
  }

  /* Create new keymap */
  sub_instruments = astKeyMap( " " );

  /* prefill with the list of known sub-instruments. */
  for (i = 0; i < SMF__SUBINST_NSUBINST; i++ ) {
    const char * substr = smf_subinst_str( i, status );
    if (substr) astMapPut0I( sub_instruments, substr, 0, NULL );
  }

  /* If the current sub-instrument has not been supplied, get it from the file.
     Use indata in preference to the group */
  if( subinst == SMF__SUBINST_NONE ) {
    if (indata) {
      hdr = indata->hdr;
    } else {
      smf_open_file( NULL, igrp, idx, "READ", SMF__NOCREATE_DATA, &sub_data,
                     status );
      if (sub_data) {
        hdr = sub_data->hdr;
      }
    }
    if (hdr) subinst = smf_calc_subinst( hdr, status );
  }

  /* flag this as being the relevant sub-instrument */
  if (subinst != SMF__SUBINST_NSUBINST ) {
    const char * substr = smf_subinst_str( subinst, status );
    if (substr) {
      astMapPut0I( sub_instruments, substr, 1, NULL );
    }
  }

  if (sub_data) smf_close_file( NULL, &sub_data, status );

  /* Free the keymap if we have bad status */
  if (*status != SAI__OK && sub_instruments) {
    sub_instruments = astAnnul( sub_instruments );
  }

  return sub_instruments;
}
