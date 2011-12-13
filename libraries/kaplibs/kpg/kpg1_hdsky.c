#include "star/hds.h"
#include "star/hds_fortran.h"
#include "f77.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_hdsky)( CHARACTER(LOC), INTEGER(KEYMAP),
                            INTEGER(OLD), INTEGER(NEW), INTEGER(STATUS)
                            TRAIL(LOC) ) {
/*
*+
*  Name:
*     KPG1_HDSKY

*  Purpose:
*     Appends a primitive HDS object to an AST KeyMap.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_HDSKY( LOC, KEYMAP, OLD, NEW, STATUS )

*  Description:
*     This function stores the vectorised data values in the supplied HDS
*     object (which must be primitive) in the supplied KeyMap. The key for
*     the KeyMap entry is the name of the HDS object. If the KeyMap already
*     contains an entry with this name, then what happens is specified
*     by OLD. Likewise, if the KeyMap does not already contain an entry
*     with this name, then what happens is specified by NEW.

*  Arguments:
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for a primitive scalar or array object.
*     KEYMAP = INTEGER (Given)
*        An AST pointer to an existing KeyMap.
*     OLD = INTEGER (Given)
*        Specifies what happens if the supplied KeyMap already contains
*        an entry with the name of the supplied HDS object.
*
*        1 - Append the new vectorised array values read from the HDS
*        object to the end of the values already in the KeyMap. The HDS
*        values will be converted to the data type of the values already
*        in the KeyMap (an error will be reported if this is not possible).
*
*        2 - Replace the existing KeyMap entry with a new entry holding
*        the vectorised array values read from the HDS object.
*
*        3 - Do nothing. The KeyMap is returned unchanged, and no error
*        is reported.
*
*        4 - Report an error. The KeyMap is returned unchanged, and an error
*        is reported.
*     NEW = INTEGER (Given)
*        Specifies what happens if the supplied KeyMap does not already
*        contain an entry with the name of the supplied HDS object.
*
*        1 - Create a new entry holding the vectorised array values read from
*        the HDS object.
*
*        2 - Do nothing. The KeyMap is returned unchanged, and no error
*        is reported.
*
*        3 - Report an error. The KeyMap is returned unchanged, and an error
*        is reported.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     - An error is reported if the supplied HDS object is a structure.

*  Copyright:
*     Copyright (C) 2008 Science & Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry
*     {enter_new_authors_here}

*  History:
*     10-MAR-2008 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}
*-
*/
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(KEYMAP)
   GENPTR_INTEGER(OLD)
   GENPTR_INTEGER(NEW)
   GENPTR_INTEGER(STATUS)

   HDSLoc *locator_c = NULL;
   AstKeyMap *keymap;
   int cstatus, old, new;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   F77_IMPORT_INTEGER( *OLD, old );
   F77_IMPORT_INTEGER( *NEW, new );
   keymap = astI2P( *KEYMAP );
   datImportFloc( LOC, LOC_length, &locator_c, &cstatus );

   kpg1Hdsky( locator_c, keymap, old, new, &cstatus );

   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
