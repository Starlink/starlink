      SUBROUTINE ASTCOPY( STATUS )
*+
*  Name:
*     ASTCOPY

*  Purpose:
*     Copy an AST Object.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCOPY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application reads an AST Object from a file and creates a copy
*     of it stored either in a new file or in an existing NDF. For example,
*     you can read a FrameSet from a set of FITS headers, and store it as
*     the WCS FrameSet in an NDF.

*  Usage:
*     astcopy this result

*  ADAM Parameters:
*     CLASS = LITERAL (Read)
*        Specifies the class of the required result. Currently supported
*        values are: "Frame", "Mapping", "FrameSet", "Region", "Object".
*        ["Object"]
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     RESULT = LITERAL (Read)
*        A text file or NDF to receive the Object.
*     THIS = LITERAL (Read)
*        A text file or NDF containing the Object to be copied.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     Copyright (C) 2013 Science & Technology Facilities Council.
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
*     3-DEC-2013 (DSB):
*        Added CLASS parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAOBJECT
      EXTERNAL AST_ISAFRAME
      EXTERNAL AST_ISAFRAMESET
      EXTERNAL AST_ISAMAPPING
      EXTERNAL AST_ISAREGION

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER RESULT
      INTEGER THIS
      CHARACTER CLASS*20
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the required class.
      CALL PAR_CHOIC( 'CLASS', 'OBJECT', 'OBJECT,FRAME,FRAMESET,'//
     :                'MAPPING,REGION', .FALSE., CLASS, STATUS )

*  Get the object to copy.
      IF( CLASS .EQ. 'FRAME' ) THEN
         CALL KPG1_GTOBJ( 'THIS', 'Frame', AST_ISAFRAME, THIS,
     :                    STATUS )
      ELSE IF( CLASS .EQ. 'FRAMESET' ) THEN
         CALL KPG1_GTOBJ( 'THIS', 'FrameSet', AST_ISAFRAMESET, THIS,
     :                    STATUS )
      ELSE IF( CLASS .EQ. 'MAPPING' ) THEN
         CALL KPG1_GTOBJ( 'THIS', 'Mapping', AST_ISAMAPPING, THIS,
     :                    STATUS )
      ELSE IF( CLASS .EQ. 'REGION' ) THEN
         CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS,
     :                    STATUS )
      ELSE
         CALL KPG1_GTOBJ( 'THIS', 'Object', AST_ISAOBJECT, THIS,
     :                    STATUS )
      END IF

*  Write the FrameSet out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCOPY_ERR', 'Error copying an AST Object.',
     :                 STATUS )
      END IF

      END
