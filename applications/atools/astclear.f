      SUBROUTINE ASTCLEAR( STATUS )
*+
*  Name:
*     ASTCLEAR

*  Purpose:
*     Clear attribute values for an Object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCLEAR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application clears the values of a specified set of attributes
*     for an Object. Clearing an attribute cancels any value that has
*     previously been explicitly set for it, so that the standard default
*     attribute value will subsequently be used instead. This also
*     causes the ASTTEST application to return the value FALSE for the
*     attribute, indicating that no value has been set.

*  Usage:
*     astclear this attrib result

*  ADAM Parameters:
*     ATTRIB = LITERAL (Read)
*        A string containing a comma-separated list of the names of the
*        attributes to be cleared.
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the modified Object. If an NDF
*        is supplied, the WCS FrameSet within the NDF will be replaced by
*        the new Object if possible (if it is a FrameSet in which the base
*        Frame has Domain GRID and has 1 axis for each NDF dimension).
*     THIS = LITERAL (Read)
*        An NDF or text file holding the original Object. If an NDF is
*        supplied, the WCS FrameSet will be used.

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

*  Local Variables:
      CHARACTER ATTRIB*30
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get an Object.
      CALL KPG1_GTOBJ( 'THIS', ' ', AST_NULL, THIS, STATUS )

*  Get the name of the attribute.
      CALL PAR_GET0C( 'ATTRIB', ATTRIB, STATUS )

*  Clear the attribute value.
      CALL AST_CLEAR( THIS, ATTRIB, STATUS )

*  Write the modified Object out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCLEAR_ERR', 'Error clearing an attribute '//
     :                 'value in an AST Object.', STATUS )
      END IF

      END
