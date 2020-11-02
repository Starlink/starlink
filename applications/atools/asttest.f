      SUBROUTINE ASTTEST( STATUS )
*+
*  Name:
*     ASTTEST

*  Purpose:
*     Test if an Object attribute value is set.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTTEST( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application displays a logical value indicating whether a
*     value has been explicitly set for one of an Object's attributes.
*     This logical value is also written to an output parameter.

*  Usage:
*     asttest this attrib

*  ADAM Parameters:
*     ATTRIB = LITERAL (Read)
*        A string containing the name of the attribute.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Object. If an NDF is supplied,
*        the WCS FrameSet will be used.
*     VALUE = _LOGICAL (Write)
*        On exit, this holds a boolean value indicating if the attribute
*        was set or not.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
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
*     12-JAN-2001 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Status:
      INTEGER STATUS

*  External References:
      INTEGER CHR_LEN

*  Local Variables:
      CHARACTER ATTRIB*30
      INTEGER THIS
      LOGICAL VAL
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get an Object.
      CALL KPG1_GTOBJ( 'THIS', ' ', AST_NULL, THIS, STATUS )

*  Get the name of the attribute.
      CALL PAR_GET0C( 'ATTRIB', ATTRIB, STATUS )

*  Get the set/clear flag.
      VAL = AST_TEST( THIS, ATTRIB, STATUS )

*  Display it.
      CALL MSG_SETL( 'V', VAL )
      CALL MSG_OUT( 'ASTTEST_MSG1', '^V', STATUS )

*  Store it in an output parameter.
      CALL PAR_PUT0L( 'VALUE', VAL, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTTEST_ERR', 'Error testing an attribute '//
     :                 'in an AST Object.', STATUS )
      END IF

      END
