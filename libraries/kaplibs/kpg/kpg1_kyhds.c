#include "star/hds.h"
#include "star/hds_fortran.h"
#include "f77.h"
#include "star/kaplibs.h"
#include "ast.h"
#include "sae_par.h"

F77_SUBROUTINE(kpg1_kyhds)( INTEGER(KEYMAP),INTEGER(START), INTEGER(MODE), 
                            CHARACTER(LOC), INTEGER(STATUS) TRAIL(LOC) ) {
/*
*+
*  Name:
*     KPG1_KYHDS

*  Purpose:
*     Copy values from an AST KeyMap to a primitive HDS object.

*  Language:
*     C, designed to be called from Fortran.

*  Invocation:
*     CALL KPG1_KYHDS( KEYMAP, START, MODE, LOC, STATUS )

*  Description:
*     This function fills a specified HDS object with primitive values
*     read from a vector entry in an AST KeyMap. It is the inverse of 
*     KPG1_HDSKY. The HDS object must already exist and must be a 
*     primitive array or scalar. The values to store in the HDS object
*     are read sequentially from the vector entry in the specified KeyMap
*     which has a key equal to the name of the HDS object. The length of
*     the vector in the KeyMap can be greater than the number of elements
*     in the HDS object, in which case the index of the first entry to 
*     transfer is given by START.

*  Arguments:
*     KEYMAP = INTEGER (Given)
*        An AST pointer to the KeyMap.
*     START = INTEGER (Given)
*        The index of the first element to use in the vector entry in the 
*        KeyMap. The first element has index 1 in the Fortran interface,
*        and zero in the C interface (kpg1Kyhds).
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

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
   GENPTR_INTEGER(START)
   GENPTR_INTEGER(MODE)
   GENPTR_CHARACTER(LOC)
   GENPTR_INTEGER(STATUS)

   HDSLoc *locator_c = NULL;
   AstKeyMap *keymap;
   int cstatus, start, mode;

   F77_IMPORT_INTEGER( *STATUS, cstatus );
   F77_IMPORT_INTEGER( *START, start );
   F77_IMPORT_INTEGER( *MODE, mode );
   keymap = astI2P( *KEYMAP );
   datImportFloc( LOC, LOC_length, &locator_c, &cstatus );

   kpg1Kyhds( keymap, start - 1, mode, locator_c, &cstatus );

   F77_EXPORT_INTEGER( cstatus, *STATUS );

}
