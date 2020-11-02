      SUBROUTINE ASTMAPREGION( STATUS )
*+
*  Name:
*     ASTMAPREGION

*  Purpose:
*     Transform a Region into a new Frame using a given Mapping

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMAPREGION( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application returns a pointer to a new Region which
*     corresponds to supplied Region described by some other specified
*     coordinate system. A Mapping is supplied which transforms positions
*     between the old and new coordinate systems. The new Region may not be of
*     the same class as the original region.

*  Usage:
*     astmapregion this map frame result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FRAME = LITERAL (Read)
*        An NDF or text file holding the Frame that describes the
*        coordinate system in which the new Region is required. If an NDF
*        is supplied, the current Frame will be used.
*     MAP = LITERAL (Read)
*        An NDF or text file holding the Mapping which transforms
*        positions from the coordinate system represented by the supplied
*	 Region to the coordinate system specified by FRAME. The supplied
*	 Mapping should define both forward and inverse transformations,
*	 and these transformations should form a genuine inverse pair.
*	 That is, transforming a position using the forward
*	 transformation and then using the inverse transformation should
*	 produce the original input position. Some Mapping classes (such
*	 as PermMap, MathMap, SphMap) can result in Mappings for which
*	 this is not true.
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the new Region.
*     THIS = LITERAL (Read)
*        A text file holding the original Region.

*  Copyright:
*     Copyright (C) 2007 Science & Technology Facilities Council.
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
*     28-MAY-2007 (DSB):
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
      EXTERNAL AST_ISAFRAME
      EXTERNAL AST_ISAMAPPING

*  Local Variables:
      INTEGER FRAME
      INTEGER MAP
      INTEGER RESULT
      INTEGER THIS
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get a Region.
      CALL KPG1_GTOBJ( 'THIS', 'Region', AST_ISAREGION, THIS,
     :                 STATUS )

*  Get a Mapping.
      CALL KPG1_GTOBJ( 'MAP', 'Mapping', AST_ISAMAPPING, MAP,
     :                 STATUS )

*  Get a Frame.
      CALL KPG1_GTOBJ( 'FRAME', ' ', AST_ISAFRAME, FRAME,
     :                 STATUS )

*  Map the Region.
      RESULT = AST_MAPREGION( THIS, MAP, FRAME, STATUS )

*  Write the new Region out.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', RESULT, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMAPREGION_ERR', 'Error mapping a Region '//
     :                 'into a new Frame.', STATUS )
      END IF

      END
