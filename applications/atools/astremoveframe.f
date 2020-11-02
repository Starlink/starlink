      SUBROUTINE ASTREMOVEFRAME( STATUS )
*+
*  Name:
*     ASTREMOVEFRAME

*  Purpose:
*     Remove a Frame from a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTREMOVEFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application  removes a Frame from a FrameSet. All other Frames
*     in the FrameSet have their indices re-numbered from one (if necessary),
*     but are otherwise unchanged.

*  Usage:
*     astremoveframe this iframe result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     IFRAME = LITERAL (Read)
*        The integer index or Domain name of the Frame within the
*        FrameSet which is to be removed (the strings AST__BASE and
*        AST__CURRENT may also be supplied).
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the modified FrameSet. If an NDF
*        is supplied, the WCS FrameSet within the NDF will be replaced by
*        the new FrameSet, if possible.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the original FrameSet from which a
*        Frame is to be removed. If an NDF is supplied, the WCS
*        FrameSet will be used.

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
      INTEGER IFRAME
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a FrameSet.
      CALL KPG1_GTOBJ( 'THIS', 'FrameSet', AST_ISAFRAMESET, THIS,
     :                 STATUS )

*  Get the index of the Frame.
      CALL ATL1_GTFRM( 'IFRAME', THIS, IFRAME, STATUS )

*  Remove the Frame from the FrameSet.
      CALL AST_REMOVEFRAME( THIS, IFRAME, STATUS )

*  Write the modified FrameSet out.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTREMOVEFRAME_ERR', 'Error removing a Frame '//
     :                 'from a FrameSet.', STATUS )
      END IF

      END
