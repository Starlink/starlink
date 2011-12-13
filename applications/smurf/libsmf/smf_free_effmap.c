/*
*+
*  Name:
*     smf_free_effmap

*  Purpose:
*     Free the AstKeyMap associated with efficiency data

*  Language:
*     Starlink ANSI C

*  Type of Module:
*     C function

*  Invocation:
*     effmap = smf_free_effmap( AstKeyMap * effmap, int * status );

*  Arguments:
*     effmap = AstKeyMap * (Given)
*        Key map to be emptied and annulled.
*     status = int * (Given and Returned)
*        Pointer to inherited status.

*  Returned Value:
*     Returns a NULL pointer.

*  Description:
*     The efficiency data returned by smf_find_science() is an AST
*     KeyMap with values corresponding to a smfData. Each smfData needs
*     to be freed before the keymap itself can be freed and this routine
*     does this.

*  Authors:
*     Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     2011-09-07 (TIMJ):
*        Initial version.
*     {enter_further_changes_here}

*  Copyright:
*     Copyright (C) 2011 Science & Technology Facilities Council.
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
#include "ast.h"
#include "mers.h"
#include "smf.h"
#include "smf_err.h"

AstKeyMap * smf_free_effmap ( AstKeyMap * effmap, int * status ) {

  int i;
  int keysize;

  keysize = astMapSize( effmap );

  for (i=0;i<keysize;i++) {
    void * tmp = NULL;
    const char * key = astMapKey( effmap, i );
    astMapGet0P( effmap, key, &tmp );
    if (tmp) {
      smfData * thisdata = tmp;
      smf_close_file( &thisdata, status );
    }
  }
  return astAnnul( effmap );
}
