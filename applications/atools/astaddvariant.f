      SUBROUTINE ASTADDVARIANT( STATUS )
*+
*  Name:
*     ASTADDVARIANT

*  Purpose:
*     Store a new variant Mapping for the current Frame in a FrameSet.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTADDVARIANT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows a new variant Mapping to be stored with the
*     current Frame in a FrameSet. See the "Variant" attribute for more
*     details. It can also be used to rename the currently selected variant
*     Mapping.

*  Usage:
*     astaddvariant this map name result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     MAP = LITERAL (Read)
*        An NDF or text file holding the Mapping which describes how to
*        convert coordinates from the current Frame to the new variant of
*        the current Frame. If null (!) is supplied, then the name
*        associated with the currently selected variant of the current
*        Frame is set to the value supplied for NAME, but no new variant
*        is added. If an NDF is supplied, the Mapping from the Base
*        Frame to the Current Frame of its WCS FrameSet will be used.
*     NAME = LITERAL (Read)
*        The name to associate with the new variant Mapping (or the
*        currently selected variant Mapping if a null value is supplied
*        for MAP).
*     RESULT = LITERAL (Read)
*        An NDF or text file to receive the modified FrameSet. If an NDF
*        is supplied, the WCS FrameSet within the NDF will be replaced by
*        the new FrameSet, if possible.
*     THIS = LITERAL (Read)
*        An NDF or text file holding the original FrameSet to which a
*        new variant Mapping is to be added. If an NDF is supplied, the WCS
*        FrameSet will be used.

*  Notes:
*     - The newly added Variant becomes the current variant on exit (this
*     is equivalent to setting the Variant attribute to the value supplied
*     for NAME).
*     - An error is reported if a variant with the supplied name already
*     exists in the current Frame.

*  Copyright:
*     Copyright (C) 2013 Central Laboratory of the Research Councils.
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
*     29-APR-2013 (DSB):
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
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS

*  External References:
      EXTERNAL AST_ISAFRAMESET
      EXTERNAL AST_ISAMAPPING

*  Local Variables:
      CHARACTER NAME*80
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

*  Get a Mapping. Allow null (!) to be supplied.
      IF( STATUS .EQ. SAI__OK ) THEN
         CALL KPG1_GTOBJ( 'MAP', 'Mapping', AST_ISAMAPPING, MAP,
     :                    STATUS )
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            MAP = AST__NULL
         END IF
      END IF

*  Get the name for the new variant Mapping.
      CALL PAR_GET0C( 'NAME', NAME, STATUS )

*  Add the variant Mapping into the FrameSet.
      CALL AST_ADDVARIANT( THIS, MAP, NAME, STATUS )

*  Write the modified FrameSet out.
      CALL ATL1_PTOBJ( 'RESULT', 'THIS', THIS, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTADDVARIANT_ERR', 'Error adding a variant '//
     :                 'Mapping into a FrameSet.', STATUS )
      END IF

      END
