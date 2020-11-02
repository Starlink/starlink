      SUBROUTINE ASTCMPFRAME( STATUS )
*+
*  Name:
*     ASTCMPFRAME

*  Purpose:
*     Create a CmpFrame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCMPFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new CmpFrame and optionally initialises
*     its attributes. A CmpFrame is a compound Frame which allows two
*     component Frames (of any class) to be merged together to form a
*     more complex Frame. The axes of the two component Frames then
*     appear together in the resulting CmpFrame (those of the first Frame,
*     followed by those of the second Frame).
*
*     Since a CmpFrame is itself a Frame, it can be used as a component in
*     forming further CmpFrames. Frames of arbitrary complexity may be
*     built from simple individual Frames in this way.

*  Usage:
*     astcmpframe frame1 frame2 options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FRAME1 = LITERAL (Read)
*        An NDF or text file holding the first component Frame. If an NDF
*        is supplied, the current Frame in its WCS FrameSet will be used.
*     FRAME2 = LITERAL (Read)
*        An NDF or text file holding the second component Frame. If an NDF
*        is supplied, the current Frame in its WCS FrameSet will be used.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new CmpFrame.
*     RESULT = LITERAL (Read)
*        A text file to receive the new CmpFrame.

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
      EXTERNAL AST_ISAFRAME

*  Local Variables:
      INTEGER RESULT
      INTEGER FRAME1
      INTEGER FRAME2
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Frame.
      CALL KPG1_GTOBJ( 'FRAME1', 'Frame', AST_ISAFRAME, FRAME1, STATUS )

*  Get the second Frame.
      CALL KPG1_GTOBJ( 'FRAME2', 'Frame', AST_ISAFRAME, FRAME2, STATUS )

*  Create the required Frame.
      RESULT = AST_CMPFRAME( FRAME1, FRAME2, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCMPFRAME_ERR', 'Error creating a new '//
     :                 'CmpFrame.', STATUS )
      END IF

      END
