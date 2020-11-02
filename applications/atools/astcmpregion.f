      SUBROUTINE ASTCMPREGION( STATUS )
*+
*  Name:
*     ASTCMPREGION

*  Purpose:
*     Create a CmpRegion.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTCMPREGION( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new CmpMap and optionally initialises
*     its attributes. A CmpRegion is a Region which allows two component
*     Regions (of any class) to be combined to form a more complex
*     Region. This combination may be performed a boolean AND, OR
*     or XOR (exclusive OR) operator. If the AND operator is
*     used, then a position is inside the CmpRegion only if it is
*     inside both of its two component Regions. If the OR operator is
*     used, then a position is inside the CmpRegion if it is inside
*     either (or both) of its two component Regions. If the XOR operator
*     is used, then a position is inside the CmpRegion if it is inside
*     one but not both of its two component Regions. Other operators can
*     be formed by negating one or both component Regions before using
*     them to construct a new CmpRegion.
*
*     The two component Region need not refer to the same coordinate
*     Frame, but it must be possible for the AST_CONVERT
*     function to determine a Mapping between them (an error will be
*     reported otherwise when the CmpRegion is created). For instance,
*     a CmpRegion may combine a Region defined within an ICRS SkyFrame
*     with a Region defined within a Galactic SkyFrame. This is
*     acceptable because the SkyFrame class knows how to convert between
*     these two systems, and consequently the AST_CONVERT
*     function will also be able to convert between them. In such cases,
*     the second component Region will be mapped into the coordinate Frame
*     of the first component Region, and the Frame represented by the
*     CmpRegion as a whole will be the Frame of the first component Region.
*
*     Since a CmpRegion is itself a Region, it can be used as a
*     component in forming further CmpRegions. Regions of arbitrary
*     complexity may be built from simple individual Regions in this
*     way.

*  Usage:
*     astcmpregion region1 region2 oper options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     OPER = LITERAL (Read)
*        The boolean operator with which to combine the two Regions. This
*        must be one of "AND", "OR" or "XOR".
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new CmpMap.
*     REGION1 = LITERAL (Read)
*        A text file holding the first component Region.
*     REGION2 = LITERAL (Read)
*        A text file holding the second component Region.
*     RESULT = LITERAL (Read)
*        A text file to receive the new CmpMap.

*  Copyright:
*     Copyright (C) 2007-2009 Science & Technology Facilities Council.
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
*     9-SEP-2009 (DSB):
*        Added XOR option.
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
      INTEGER OPER
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

*  Get the boolean operator to use.
      CALL PAR_CHOIC( 'OPER', 'OR', 'AND,OR,XOR', .FALSE., TEXT,
     :                STATUS )
      IF( TEXT .EQ. 'AND' ) THEN
         OPER = AST__AND
      ELSE IF( TEXT .EQ. 'OR' ) THEN
         OPER = AST__OR
      ELSE
         OPER = AST__XOR
      END IF

*  Create the required CmpRegion.
      RESULT = AST_CMPREGION( REGION1, REGION2, OPER, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTCMPREGION_ERR', 'Error creating a new '//
     :                 'CmpRegion.', STATUS )
      END IF

      END
