      SUBROUTINE ASTGETUNC( STATUS )
*+
*  Name:
*     ASTGETUNC

*  Purpose:
*     Obtain uncertainty information from a Region.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTGETUNC( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns a Region which represents the uncertainty
*     associated with positions within the supplied Region. See astSetUnc
*     for more information about Region uncertainties and their use.

*  Usage:
*     astgetunc this def result

*  ADAM Parameters:
*     DEF = _LOGICAL (Read)
*        Controls what is returned if no uncertainty information has been
*        associated explicitly with the supplied Region. If a TRUE value
*        is supplied, then the default uncertainty Region used internally
*        within AST is returned. If FALSE is supplied, then an error is
*        reported. [TRUE]
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the new Region describing the
*        uncertainty in the supplied Region.
*     THIS = LITERAL (Read)
*        A text file holding the Region.

*  Copyright:
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
*     28-MAY-3007 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
*  Type Definitions:
      IMPLICIT NONE              ! no default typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  External References:
      EXTERNAL AST_ISAREGION

*  Status:
      INTEGER STATUS

*  Local Variables:
      INTEGER RESULT
      INTEGER THIS
      LOGICAL DEF
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the Region.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS,
     :                 STATUS )

*  Get the required parameters.
      CALL PAR_GET0L( 'DEF', DEF, STATUS )

*  Find the uncertainty region.
      RESULT = AST_GETUNC( THIS, DEF, STATUS )

*  If required, report an error.
      IF( RESULT .EQ. AST__NULL ) THEN
         IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( ' ', 'Unable to find default uncertainty '//
     :                    'information', status )
         END IF

*  Otherwise, write the new Region out.
      ELSE
         CALL ATL1_PTOBJ( 'RESULT', 'THIS', RESULT, STATUS )
      END IF

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( ' ', 'Error finding the uncertainty in a '//
     :                 'Region.', STATUS )
      END IF

      END
