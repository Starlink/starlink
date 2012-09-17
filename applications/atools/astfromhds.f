      SUBROUTINE ASTFROMHDS( STATUS )
*+
*  Name:
*     ASTFROMHDS

*  Purpose:
*     Create a KeyMap containing the contents of an HDS object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTFROMHDS( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new KeyMap containing data copied from
*     a supplied HDS object. Note, multi-dimensional arrays are not
*     copied.

*  Usage:
*     astfromhds object keymap

*  ADAM Parameters:
*     KEYMAP = LITERAL (Write)
*        A text file to receive the new KeyMap.
*     OBJECT = UNIV (Read)
*        The input HDS object.

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

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER ENTRY
      INTEGER KEYMAP
      CHARACTER KEY*(AST__SZCHR)
      CHARACTER LOC*(DAT__SZLOC)
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the HDS object.
      CALL DAT_ASSOC( 'OBJECT', 'READ', LOC, STATUS )

*  Create the KeyMap.
      KEYMAP = AST_KEYMAP( ' ', STATUS )

*  Copy the HDS object to the KeyMap.
      CALL ATL_HD2KY( LOC, KEYMAP, STATUS )

*  If the KeyMap contains a single scalar KeyMap pointer, then use the
*  nested KeyMap in place of the top level keymap (this means that a round
*  trip of astfromhds and asttohds does not cause the final HDS object to
*  descend by one level).
      IF( AST_MAPSIZE( KEYMAP, STATUS ) .EQ. 1 ) THEN
         KEY = AST_MAPKEY( KEYMAP, 1, STATUS )
         IF( AST_MAPLENGTH( KEYMAP, KEY, STATUS ) .EQ. 1 .AND.
     :       AST_MAPTYPE( KEYMAP, KEY, STATUS ) .EQ.
     :                                           AST__OBJECTTYPE ) THEN
            IF(  AST_MAPGET0A( KEYMAP, KEY, ENTRY, STATUS ) ) THEN
               IF( AST_ISAKEYMAP( ENTRY, STATUS ) ) THEN
                  CALL AST_ANNUL( KEYMAP, STATUS )
                  KEYMAP = ENTRY
               ELSE
                  CALL AST_ANNUL( ENTRY, STATUS )
               END IF
            END IF
         END IF
      END IF

*  Write the KeyMap out to a text file.
      CALL ATL1_PTOBJ( 'KEYMAP', ' ', KEYMAP, STATUS )

* Annul the locator and cancel the parameter.
      CALL DAT_ANNUL( LOC, STATUS )
      CALL DAT_CANCL( 'OBJECT', STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTFROMHDS_ERR', 'Error copying a an '//
     :                 'HDS object to a KeyMap.', STATUS )
      END IF

      END
