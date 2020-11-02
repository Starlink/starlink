      SUBROUTINE ASTNEGATE( STATUS )
*+
*  Name:
*     ASTNEGATE

*  Purpose:
*     Negate the area represented by a Region.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTNEGATE( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application negates the area represented by a Region. That is,
*     points which were previously inside the region will then be
*     outside, and points which were outside will be inside. This is
*     acomplished by toggling the state of the Negated attribute for
*     the supplied region.

*  Usage:
*     astnegate this result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     RESULT = LITERAL (Read)
*        A text file to receive the negated Region.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the Region. If an NDF is supplied,
*        the current Frame of the WCS FrameSet will be used (if it is a
*        Region).

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
*     18-MAY-2009 (DSB):
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
      EXTERNAL AST_ISAREGION

*  Local Variables:
      INTEGER THIS
      INTEGER RESULT
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Region.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS, STATUS )

*  Negate the Region.
      CALL AST_NEGATE( THIS, STATUS )

*  Write this Region out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTNEGATE_ERR', 'Error negating a '//
     :                 'Region.', STATUS )
      END IF

      END
