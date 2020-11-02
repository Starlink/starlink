      SUBROUTINE ASTPRISM( STATUS )
*+
*  Name:
*     ASTPRISM

*  Purpose:
*     Create a Prism.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTPRISM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new Prism and optionally initialises
*     its attributes.
*
*     A Prism is a Region that represents an extrusion of an existing Region
*     into one or more orthogonal dimensions (specified by another Region).
*     If the Region to be extruded has N axes, and the Region defining the
*     extrusion has M axes, then the resulting Prism will have (M+N) axes.
*     A point is inside the Prism if the first N axis values correspond to
*     a point inside the Region being extruded, and the remaining M axis
*     values correspond to a point inside the Region defining the extrusion.
*
*     As an example, a cylinder can be represented by extruding an existing
*     Circle, using an Interval to define the extrusion. Ih this case, the
*     Interval would have a single axis and would specify the upper and
*     lower limits of the cylinder along its length.

*  Usage:
*     astprism region1 region2 options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new Prism.
*     REGION1 = LITERAL (Read)
*        A text file holding the Region to be extruded.
*     REGION2 = LITERAL (Read)
*        A text file holding the Region defining the extent of the extrusion.
*     RESULT = LITERAL (Read)
*        A text file to receive the new Prism.

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
*     5-OCT-2013 (DSB):
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
      CHARACTER TEXT*10
      INTEGER RESULT
      INTEGER REGION1
      INTEGER REGION2
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Region.
      CALL KPG1_GTOBJ( 'REGION1', 'Region', AST_ISAREGION, REGION1,
     :                 STATUS )

*  Get the second Region.
      CALL KPG1_GTOBJ( 'REGION2', 'Region', AST_ISAREGION, REGION2,
     :                 STATUS )

*  Create the required Prism.
      RESULT = AST_PRISM( REGION1, REGION2, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTPRISM_ERR', 'Error creating a new '//
     :                 'Prism.', STATUS )
      END IF

      END
