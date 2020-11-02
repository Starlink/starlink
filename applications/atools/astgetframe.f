      SUBROUTINE ASTGETFRAME( STATUS )
*+
*  Name:
*     ASTGETFRAME

*  Purpose:
*     Obtain a specified Frame in a FrameSet

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns a copy of a specified Frame in a FrameSet.

*  Usage:
*     astgetframe this iframe result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     IFRAME = LITERAL (Read)
*        The integer index or Domain name of the required Frame within the
*        FrameSet (the strings AST__BASE and AST__CURRENT may also be
*        supplied).
*     RESULT = LITERAL (Read)
*        An text file to receive the Frame.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the FrameSet. If an NDF is supplied,
*        the WCS FrameSet will be used.

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
      EXTERNAL AST_ISAFRAMESET

*  Local Variables:
      INTEGER IAST
      INTEGER IFRM
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get an AST Object.
      CALL KPG1_GTOBJ( 'THIS', 'FrameSet', AST_ISAFRAMESET, IAST,
     :                 STATUS )

*  Get the index of the required Frame.
      CALL ATL1_GTFRM( 'IFRAME', IAST, IFRM, STATUS )

*  Get the required Frame.
      RESULT = AST_GETFRAME( IAST, IFRM, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTGETFRAME_ERR', 'Error extracting a Frame '//
     :                 'from a FrameSet.', STATUS )
      END IF

      END
