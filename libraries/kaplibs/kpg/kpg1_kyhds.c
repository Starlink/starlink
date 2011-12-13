#include "star/hds.h"
#include "star/hds_fortran.h"
#include "f77.h"
#include "kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_kyhds)( INTEGER(KEYMAP), INTEGER_ARRAY(MAP), INTEGER(AXIS),
                            INTEGER(MODE), CHARACTER(LOC), INTEGER(STATUS)
                            TRAIL(LOC) ) {
/*
*+
*  Name:
*     KPG1_KYHDS

*  Purpose:
*     Copies values from an AST KeyMap to a primitive HDS object.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_KYHDS( KEYMAP, MAP, AXIS, MODE, LOC, STATUS )

*  Description:
*     This function fills a specified HDS object with primitive values
*     read from a vector entry in an AST KeyMap. It is the inverse of
*     KPG1_HDSKY. The HDS object must already exist and must be a
*     primitive array or scalar. The values to store in the HDS object
*     are read from the KeyMap entry that has a key equal to the name
*     of the HDS object. The vector read from the KeyMap is interpreted
*     as an N-dimension array, where N is the number of dimensions in the
*     HDS object. Array slices can be re-arranged as they are copied from
*     KeyMap to HDS object. The AXIS argument specifies which axis is
*     being re-arranged. Each array slice is perpendicular to this axis.
*     The KeyMap array and the HDS array are assumed to have the same
*     dimensions on all other axes.

*  Arguments:
*     KEYMAP = INTEGER (Given)
*        An AST pointer to the KeyMap.
*     MAP( * ) = INTEGER (Given)
*        An array which indicates how to map slices in the KeyMap array
*        onto slices in the HDS array. The length of the supplied array
*        should be equal to the HDS array dimension specified by AXIS.
*        Element J of this array says where the data for the J'th slice
*        of the HDS array should come from, where J is the index along
*        the axis specified by AXIS. The value of element J is a
*        zero-based index along axis AXIS of the array read from the
*        KeyMap.
*     AXIS = INTEGER (Given)
*        The index of the axis to be re-arranged. The first axis is axis 1.
*     MODE = INTEGER (Given)
*        Specifies what happens if the supplied KeyMap does not contain
*        an entry with the name of the supplied HDS object.
*
*        1 - Report an error.
*
*        2 - Do nothing
*     LOC = CHARACTER * ( DAT__SZLOC ) (Given)
*        An HDS locator for a primitive scalar or array object.
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
   GENPTR_INTEGER(KEYMAP)
   GENPTR_INTEGER_ARRAY(MAP)
   GENPTR_INTEGER(AXIS)
   GENPTR_INTEGER(MODE)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)

   HDSLoc *locator_c = NULL;
   AstKeyMap *keymap;
   int cstatus, axis, mode;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   F77_IMPORT_INTEGER( *AXIS, axis );
   F77_IMPORT_INTEGER( *MODE, mode );
   keymap = astI2P( *KEYMAP );
   datImportFloc( LOC, LOC_length, &locator_c, &cstatus );

   kpg1Kyhds( keymap, MAP, axis, mode, locator_c, &cstatus );

   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
