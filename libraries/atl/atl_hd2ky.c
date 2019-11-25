#include "star/hds.h"
#include "star/hds_fortran.h"
#include "f77.h"
#include "ast.h"
#include "atl.h"
#include "sae_par.h"

F77_SUBROUTINE(atl_hd2ky)( CHARACTER(LOC), INTEGER(KEYMAP), INTEGER(STATUS)
                           TRAIL(LOC) ) {
/*
*+
*  Name:
*     ATL_KY2HD

*  Purpose:
*     Converts an HDS structure into an AST KeyMap.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL ATL_KY2HD( KEYMAP, LOC, STATUS )

*  Description:
*     This routine copies the contents of an HDS structure into a supplied
*     AST KeyMap. Each entry added to the KeyMap has a key that is equal
*     to the name of the corresponding HDS component. Any existing entry
*     in the KeyMap with the same name is replaced.

*  Arguments:
*     LOC = CHARACTER * (DAT__SZLOC) (Given)
*        A locator for the HDS object.
*     KEYMAP = INTEGER (Given)
*        An AST pointer to the KeyMap into which the HDS contents
*        are to be copied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2012 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2012 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(KEYMAP)
   GENPTR_INTEGER(STATUS)

   HDSLoc *locator_c = NULL;
   AstKeyMap *keymap;
   int cstatus;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   keymap = astI2P( *KEYMAP );
   datImportFloc( LOC, LOC_length, &locator_c, &cstatus );

   atlHd2ky( locator_c, keymap, &cstatus );

   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
