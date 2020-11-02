      SUBROUTINE ASTADDFRAME( STATUS )
*+
*  Name:
*     ASTADDFRAME

*  Purpose:
*     Add a Frame to a FrameSet to define a new coordinate system.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTADDFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application adds a new Frame and an associated Mapping to a
*     FrameSet so as to define a new coordinate system, derived from one
*     which already exists within the FrameSet. The new Frame then becomes
*     the FrameSet's current Frame.

*  Usage:
*     astaddframe this iframe map frame result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FRAME = LITERAL (Read)
*        An NDF or text file holding the Frame that describes the new
*        coordinate system. This application may also be used to merge
*        two FrameSets by supplying a second FrameSet for this argument
*        (see SUN/210 for details). If an NDF is supplied, its WCS
*        FrameSet will be used.
*     IFRAME = LITERAL (Read)
*        The integer index or Domain name of the Frame within the
*        FrameSet which describes the coordinate system upon which the
*        new one is to be based (the strings AST__BASE and AST__CURRENT
*        may also be supplied).
*     MAP = LITERAL (Read)
*        An NDF or text file holding the Mapping which describes how to
*        convert coordinates from the old coordinate system (described by the
*        Frame with index IFRAME) into coordinates in the new system. The
*        Mapping's forward transformation should perform this conversion,
*        and its inverse transformation should convert in the opposite
*        direction. If an NDF is supplied, the Mapping from the Base
*        Frame to the Current Frame of its WCS FrameSet will be used.
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the modified FrameSet. If an NDF
*        is supplied, the WCS FrameSet within the NDF will be replaced by
*        the new FrameSet, if possible.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the original FrameSet to which a
*        new Frame is to be added. If an NDF is supplied, the WCS
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
      EXTERNAL AST_ISAFRAME
      EXTERNAL AST_ISAMAPPING

*  Local Variables:
      INTEGER FRAME
      INTEGER IFRAME
      INTEGER MAP
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a FrameSet.
      CALL KPG1_GTOBJ( 'THIS', 'FrameSet', AST_ISAFRAMESET, THIS,
     :                 STATUS )

*  Get the index of the basis Frame.
      CALL ATL1_GTFRM( 'IFRAME', THIS, IFRAME, STATUS )

*  Get a Mapping.
      CALL KPG1_GTOBJ( 'MAP', 'Mapping', AST_ISAMAPPING, MAP,
     :                 STATUS )

*  Get a Frame.
      CALL KPG1_GTOBJ( 'FRAME', ' ', AST_ISAFRAME, FRAME,
     :                 STATUS )

*  Add the Frame into the FrameSet.
      CALL AST_ADDFRAME( THIS, IFRAME, MAP, FRAME, STATUS )

*  Write the modified FrameSet out.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTADDFRAME_ERR', 'Error adding a Frame into '//
     :                 'a FrameSet.', STATUS )
      END IF

      END
