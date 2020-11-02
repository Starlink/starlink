      SUBROUTINE ASTGETMAPPING( STATUS )
*+
*  Name:
*     ASTGETMAPPING

*  Purpose:
*     Obtain a Mapping that converts between two Frames in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETMAPPING( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns a Mapping that will convert coordinates
*     between the coordinate systems represented by two Frames in a
*     FrameSet.

*  Usage:
*     astgetmapping this frame1 frame2 result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     IFRAME1 = LITERAL (Read)
*        The integer index or Domain name of the first Frame within the
*        FrameSet (the strings AST__BASE and AST__CURRENT may also be
*        supplied). This Frame describes the coordinate system for the
*        "input" end of the Mapping.
*     IFRAME2 = LITERAL (Read)
*        The integer index or Domain name of the second Frame within the
*        FrameSet (the strings AST__BASE and AST__CURRENT may also be
*        supplied). This Frame describes the coordinate system for the
*        "output" end of the Mapping.
*     RESULT = LITERAL (Read)
*        An text file to receive the Mapping.
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

*  External References:
      EXTERNAL AST_ISAFRAMESET

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER IAST
      INTEGER IFRM1
      INTEGER IFRM2
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get an AST Object.
      CALL KPG1_GTOBJ( 'THIS', 'FrameSet', AST_ISAFRAMESET, IAST,
     :                 STATUS )

*  Get the index of the first Frame.
      CALL ATL1_GTFRM( 'IFRAME1', IAST, IFRM1, STATUS )

*  Get the index of the second Frame.
      CALL ATL1_GTFRM( 'IFRAME2', IAST, IFRM2, STATUS )

*  Get the required Mapping.
      RESULT = AST_GETMAPPING( IAST, IFRM1, IFRM2, STATUS )

*  Write this Mapping out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTGETMAPPING_ERR', 'Error extracting a '//
     :                 'Mapping from a FrameSet.', STATUS )
      END IF

      END
