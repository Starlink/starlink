      SUBROUTINE ASTTRANMAP( STATUS )
*+
*  Name:
*     ASTTRANMAP

*  Purpose:
*     Create a TranMap.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTTRANMAP( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new TranMap and optionally initialises its
*     attributes.
*
*     A TranMap is a Mapping which combines the forward transformation of
*     a supplied Mapping with the inverse transformation of another
*     supplied Mapping, ignoring the un-used transformation in each
*     Mapping (indeed the un-used transformation need not exist).
*
*     When the forward transformation of the TranMap is referred to, the
*     transformation actually used is the forward transformation of the
*     first Mapping supplied when the TranMap was constructed. Likewise,
*     when the inverse transformation of the TranMap is referred to, the
*     transformation actually used is the inverse transformation of the
*     second Mapping supplied when the TranMap was constructed.

*  Usage:
*     asttranmap map1 map2 options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     MAP1 = LITERAL (Read)
*        An NDF or text file holding the first component Mapping, which
*        defines the forward transformation. If an NDF is supplied, the
*        Mapping from the Base Frame to the Current Frame of its WCS FrameSet
*        will be used.
*     MAP2 = LITERAL (Read)
*        An NDF or text file holding the second component Mapping, which
*        defines the inverse transformation. If an NDF is supplied, the
*        Mapping from the Base Frame to the Current Frame of its WCS FrameSet
*        will be used.
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new TranMap.
*     RESULT = LITERAL (Read)
*        A text file to receive the new TranMap.

*  Copyright:
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     24-FEB-2004 (DSB):
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
      EXTERNAL AST_ISAMAPPING

*  Local Variables:
      INTEGER RESULT
      INTEGER MAP1
      INTEGER MAP2
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Mapping.
      CALL KPG1_GTOBJ( 'MAP1', 'Mapping', AST_ISAMAPPING, MAP1, STATUS )

*  Get the second Frame.
      CALL KPG1_GTOBJ( 'MAP2', 'Mapping', AST_ISAMAPPING, MAP2, STATUS )

*  Create the required TranMap.
      RESULT = AST_TRANMAP( MAP1, MAP2, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTTRANMAP_ERR', 'Error creating a new '//
     :                 'TranMap.', STATUS )
      END IF

      END
