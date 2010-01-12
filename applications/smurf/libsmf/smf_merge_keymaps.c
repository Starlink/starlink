/*
 *+
 *  Name:
 *     smf_merge_keymaps

 *  Purpose:
 *     Merge keymaps with the same root.

 *  Language:
 *     Starlink ANSI C

 *  Type of Module:
 *     Library routine

 *  Invocation:
 *     smf_merge_keymaps( AstKeyMap * keymap, const char * key,
 *                        const char * suffix, int * status );

 *  Arguments:
 *     keymap = AstKeyMap * (Given and Returned)
 *        Keymap containing "key" and "key_suffix".
 *     key = const char * (Given)
 *        Name of primary key to use for merging.
 *     suffix = const chat * (Given)
 *        Name of suffix to apply to "key" for merging.
 *     status = int* (Given and Returned)
 *        Pointer to global status.

 *  Description:
 *     Given a keymap merge the item indicated by the supplied key with
 *     any item of that name but with the supplied suffix. This allows
 *     keymaps with key "xxx" to be merged with "xxx_yyy". The values
 *     associated with the specific "_yyy" key take precedence over the
 *     generic values.

 *  Authors:
 *     Tim Jenness (JAC, Hawaii)
 *     {enter_new_authors_here}

 *  History:
 *     2010-01-11 (TIMJ):
 *        Initial Version

 *  Notes:
 *     Does nothing if neither key nor key_suffix are present.

 *  Copyright:
 *     Copyright (C) 2010 Science and Technology Facilities Council.
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

 *  Bugs:
 *     {note_any_bugs_here}
 *-
 */

#include "smf.h"

#include "ast.h"
#include "star/one.h"
#include "sae_par.h"
#include "star/atl.h"
#include "mers.h"

void smf_merge_keymaps ( AstKeyMap * keymap, const char * key, const char * suffix,
                         int * status ) {

  char altkey[30];
  AstKeyMap * altkeymap = NULL;
  AstKeyMap * sinkkeymap = NULL;

  if (*status != SAI__OK) return;

  /* form the search key */
  one_strlcpy( altkey, key, sizeof(altkey), status );
  one_strlcat( altkey, suffix, sizeof(altkey), status );

  /* and see if it is present */
  if (! astMapHasKey( keymap, altkey ) ) return;

  /* we will need it so retrieve it and assume it is a keymap */
  astMapGet0A( keymap, altkey, &altkeymap );

  /* then see if the primary key is present */
  if ( ! astMapHasKey( keymap, key ) ) {

    /* if it is not present but we had the alt key then
       we need to move it over */
    msgOutiff( MSG__VERB, "", "Did not find keymap %s but copied values from %s",
               status, key, altkey );
    astMapPut0A( keymap, key, altkeymap, "Copied from alternate entry" );
    altkeymap = astAnnul( altkeymap );
    return;
  }

  /* Get the keymap that will receive the value (assume it is a keymap itself for now) */
  astMapGet0A( keymap, key, &sinkkeymap );

  msgOutiff( MSG__VERB, "", "Merging keymap with name %s into keymap %s",
             status, altkey, key );

  /* Now merge them */
  atlMapCopy( sinkkeymap, altkeymap, status );

  (void)astAnnul( altkeymap );
  (void)astAnnul( sinkkeymap );

}
