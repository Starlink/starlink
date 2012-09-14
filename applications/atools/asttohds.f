      SUBROUTINE ASTTOHDS( STATUS )
*+
*  Name:
*     ASTTOHDS

*  Purpose:
*     Copy the contents of a KeyMap to a new HDS object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTTOHDS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new HDS object containing data copied from
*     a supplied AST KeyMap.

*  Usage:
*     asttohds keymap object

*  ADAM Parameters:
*     KEYMAP = LITERAL (Read)
*        An text file holding the KeyMap.
*     OBJECT = UNIV (Write)
*        The output HDS object.

*  Copyright:
*     Copyright (C) 2012 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-SEP-2012 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAKEYMAP

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER KEYMAP
      CHARACTER LOC*(DAT__SZLOC)
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the KeyMap.
      CALL KPG1_GTOBJ( 'KEYMAP', 'KeyMap', AST_ISAKEYMAP, KEYMAP,
     :                 STATUS )

*  Create the HDS object.
      CALL DAT_CREAT( 'OBJECT', 'KEYMAP', 0, 0, STATUS )

* Get a locator for it.
      CALL DAT_ASSOC ( 'OBJECT', 'WRITE', LOC, STATUS )

*  Copy the KeyMap into the HDS object.
      CALL ATL_KY2HD( KEYMAP, LOC, STATUS )

* Annul the locator and cancel the parameter.
      CALL DAT_ANNUL( LOC, STATUS )
      CALL DAT_CANCL( 'OBJECT', STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTTOHDS_ERR', 'Error copying a KeyMap to an '//
     :                 'HDS object.', STATUS )
      END IF

      END
