      SUBROUTINE ASTSFLUXFRAME( STATUS )
*+
*  Name:
*     ASTSFLUXFRAME

*  Purpose:
*     Create a SpecFluxFrame.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTSFLUXFRAME( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application creates a new SpecFluxFrame and optionally initialises
*     its attributes.
*
*     A SpecFluxFrame combines a SpecFrame and a FluxFrame into a single
*     2-dimensional compound Frame. Such a Frame can for instance be used
*     to describe a Plot of a spectrum in which the first axis represents
*     spectral position and the second axis represents flux.

*  Usage:
*     astspecfluxframe frame1 frame2 options result

*  ADAM Parameters:
*     FMT = LITERAL (Read)
*        The format in which to store output objects. For allowed values,
*        see the top level help for the ATOOLS package using command
*        'atlhelp'. Only used if the output object is written to a text
*        file. An error is reported if the output object cannot be written
*        using the requested format. ["AST"]
*     FRAME1 = LITERAL (Read)
*        An NDF or text file holding the SpecFrame. This will form the first
*        axis in the new SpecFluxFrame. If an NDF is supplied, the current
*        Frame in its WCS FrameSet will be used (which must be a SpecFrame).
*     FRAME2 = LITERAL (Read)
*        An NDF or text file holding the FluxFrame. This will form the second
*        axis in the new SpecFluxFrame. The "SpecVal" attribute of this
*        FluxFrame is not used by the SpecFluxFrame class and so may be set
*        null when the FluxFrame is created. If an NDF is supplied, the
*        current Frame in its WCS FrameSet will be used (which must be a
*        FluxFrame).
*     OPTIONS = LITERAL (Read)
*        A string containing an optional comma-separated list of attribute
*        assignments to be used for initialising the new SpecFluxFrame.
*     RESULT = LITERAL (Read)
*        A text file to receive the new SpecFluxFrame.

*  Copyright:
*     Copyright (C) 2005 Particle Physics & Astronomy Research Council.
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
*     6-JAN-2005 (DSB):
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
      EXTERNAL AST_ISAFLUXFRAME
      EXTERNAL AST_ISASPECFRAME

*  Local Variables:
      INTEGER RESULT
      INTEGER FRAME1
      INTEGER FRAME2
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Frame.
      CALL KPG1_GTOBJ( 'FRAME1', 'SpecFrame', AST_ISASPECFRAME, FRAME1,
     :                 STATUS )

*  Get the second Frame.
      CALL KPG1_GTOBJ( 'FRAME2', 'FluxFrame', AST_ISAFLUXFRAME, FRAME2,
     :                 STATUS )

*  Create the required SpecFluxFrame.
      RESULT = AST_SPECFLUXFRAME( FRAME1, FRAME2, ' ', STATUS )

*  Store the required attribute values.
      CALL ATL1_SETOP( 'OPTIONS', RESULT, STATUS )

*  Write the results out to a text file.
      CALL ATL1_PTOBJ( 'RESULT', ' ', RESULT, STATUS )

 999  CONTINUE

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTSFLUXFRAME_ERR', 'Error creating a new '//
     :                 'SpecFluxFrame.', STATUS )
      END IF

      END
