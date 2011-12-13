      SUBROUTINE ASTOVERLAP( STATUS )
*+
*  Name:
*     ASTOVERLAP

*  Purpose:
*     Test if two regions overlap each other.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTOVERLAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application determines if the two supplied Regions overlap. The
*     two Regions are converted to a commnon coordinate system before
*     performing the check. If this conversion is not possible (for instance
*     because the two Regions represent areas in different domains), then
*     the check cannot be performed.

*  Usage:
*     astoverlap this that

*  ADAM Parameters:
*     THIS = LITERAL (Read)
*        An NDF or text file holding the first region. If an NDF is
*        supplied, the current Frame in the WCS FrameSet will be used.
*     THAT = LITERAL (Read)
*        An NDF or text file holding the second region. If an NDF is
*        supplied, the current Frame in the WCS FrameSet will be used.
*     RESULT = _INTEGER (Write)
*        On exit, this holds an integer indicating if there is any overlap
*        between the two Regions. Possible values are:
*
*        0 - The check could not be performed because the second Region
*            could not be mapped into the coordinate system of the first
*            Region.
*
*        1 - There is no overlap between the two Regions.
*
*        2 - The first Region is completely inside the second Region.
*
*        3 - The second Region is completely inside the first Region.
*
*        4 - There is partial overlap between the two Regions.
*
*        5 - The Regions are identical to within their uncertainties.
*
*        6 - The second Region is the exact negation of the first Region
*            to within their uncertainties.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     30-JUN-2009 (DSB):
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
      EXTERNAL AST_ISAREGION

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER THAT
      INTEGER THIS
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Region.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS, STATUS )

*  Get the second Region.
      CALL KPG1_GTOBJ( 'THAT', 'Region', AST_ISAREGION, THAT, STATUS )

*  Perform the test.
      RESULT = AST_OVERLAP( THIS, THAT, STATUS )

*  Display the result.
      CALL MSG_BLANK( STATUS )

      IF( RESULT .EQ. 0 ) THEN
         CALL MSG_OUT( ' ', 'The check could not be performed '//
     :                 'because the second Region could not be '//
     :                 'mapped into the coordinate system of the '//
     :                 'first Region.', STATUS )

      ELSE IF( RESULT .EQ. 1 ) THEN
         CALL MSG_OUT( ' ', 'There is no overlap between the two '//
     :                 'Regions.', STATUS )

      ELSE IF( RESULT .EQ. 2 ) THEN
         CALL MSG_OUT( ' ', 'The first Region is completely inside '//
     :                 'the second Region.', STATUS )

      ELSE IF( RESULT .EQ. 3 ) THEN
         CALL MSG_OUT( ' ', 'The second Region is completely inside '//
     :                 'the first Region.', STATUS )

      ELSE IF( RESULT .EQ. 4 ) THEN
         CALL MSG_OUT( ' ', 'There is partial overlap between the two'//
     :                 ' Regions.', STATUS )

      ELSE IF( RESULT .EQ. 5 ) THEN
         CALL MSG_OUT( ' ', 'The Regions are identical to within '//
     :                 'their uncertainties.', STATUS )

      ELSE IF( RESULT .EQ. 6 ) THEN
         CALL MSG_OUT( ' ', 'The second Region is the exact negation '//
     :                 'of the first Region to within their '//
     :                 'uncertainties.', STATUS )
      END IF

      CALL MSG_BLANK( STATUS )

*  Write the result out to the output parameter.
      CALL PAR_PUT0I( 'RESULT', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTOVERLAP_ERR', 'Error testing if two '//
     :                 'Regions overlap.', STATUS )
      END IF

      END
