#include "star/hds.h"
#include "star/hds_fortran.h"
#include "f77.h"
#include "ast.h"
#include "sae_par.h"
#include "atl.h"

F77_SUBROUTINE(atl_ky2hd)( INTEGER(KEYMAP), CHARACTER(LOC), INTEGER(STATUS)
                           TRAIL(LOC) ) {
/*
*+
*  Name:
*     ATL_KY2HD

*  Purpose:
*     Converts an AST KeyMap into an HDS structure.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL ATL_KY2HD( KEYMAP, LOC, STATUS )

*  Description:
*     This routine copies the contents of an AST KeyMap into a supplied
*     HDS structure.

*  Arguments:
*     KEYMAP = INTEGER (Given)
*        The AST KeyMap identifier.
*     LOC = CHARACTER * (DAT__SZLOC) (Given)
*        A locator for the HDS object into which the KeyMap contents
*        are to be copied.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2008,2012 Science & Technology Facilities Council.
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
*     29-APR-2008 (DSB):
*        Original version.
*     14-SEP-2012 (DSB):
*        Moved from KAPLIBS to ATL.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*/
   GENPTR_INTEGER(KEYMAP)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)

   HDSLoc *locator_c = NULL;
   AstKeyMap *keymap = NULL;
   int cstatus;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   keymap = astI2P( *KEYMAP );
   datImportFloc( LOC, LOC_length, &locator_c, &cstatus );

   atlKy2hd( keymap,  locator_c, &cstatus );

   F77_EXPORT_INTEGER( cstatus, *STATUS );
}
