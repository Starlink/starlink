      SUBROUTINE ASTMATCHAXES( STATUS )
*+
*  Name:
*     ASTMATCHAXES

*  Purpose:
*     Find any corresponding axes in two Frames.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL ASTMATCHAXES( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application looks for corresponding axes within two supplied
*     Frames. A list array of integers is displayed with one element
*     for each axis in the second supplied Frame. An element in this list
*     will be set to zero if the associated axis within the second Frame
*     has no corresponding axis within the first Frame. Otherwise, it
*     will be set to the index (a non-zero positive integer) of the
*     corresponding axis within the first supplied Frame.

*  Usage:
*     astmatchaxes frm1 frm2

*  ADAM Parameters:
*     AXES() = _INTEGER (Write)
*        An output parameter to which is written an integer array holding
*        the indices of the axes (within the first Frame) that correspond to
*        each axis within the second Frame. Axis indices start at 1. A value
*        of zero will be stored in the returned array for each axis in the
*        second Frame that has no corresponding axis in the first Frame.
*     FRM1 = LITERAL (Read)
*        An NDF or text file holding the first Frame or FrameSet. If an NDF
*        is supplied, the WCS FrameSet will be used.
*     FRM2 = LITERAL (Read)
*        An NDF or text file holding the second Frame or FrameSet. If an NDF
*        is supplied, the WCS FrameSet will be used.

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
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     28-SEP-2009 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF constants

*  External References:
      EXTERNAL AST_ISAFRAME

*  Status:
      INTEGER STATUS

*  Local Constants:
      INTEGER COL1               ! Index of start of first displayed column
      PARAMETER ( COL1 = 3 )

      INTEGER COL2               ! Index of start of second displayed column
      PARAMETER ( COL2 = 35 )

*  Local Variables:
      CHARACTER ATTR*15
      CHARACTER LAB1*50
      CHARACTER LAB2*50
      CHARACTER TEXT*79
      INTEGER AXES( NDF__MXDIM )
      INTEGER FRM1
      INTEGER FRM2
      INTEGER I
      INTEGER IAT
      INTEGER NAX
*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the first Frame.
      CALL KPG1_GTOBJ( 'FRM1', 'Frame or FrameSet', AST_ISAFRAME, FRM1,
     :                 STATUS )

*  Get the second Frame.
      CALL KPG1_GTOBJ( 'FRM2', 'Frame or FrameSet', AST_ISAFRAME, FRM2,
     :                 STATUS )

*  Get the required axis indices.
      CALL AST_MATCHAXES( FRM1, FRM2, AXES, STATUS )

*  Display the results.
      CALL MSG_BLANK( STATUS )
      CALL MSG_OUT( ' ', 'Corresponding axis indices:', STATUS)
      CALL MSG_BLANK( STATUS )

      TEXT = ' '
      TEXT( COL1: ) = 'FRM2'
      TEXT( COL2: ) = 'FRM1'
      CALL MSG_OUT( ' ', TEXT, STATUS)

      TEXT = ' '
      TEXT( COL1: ) = '----'
      TEXT( COL2: ) = '----'
      CALL MSG_OUT( ' ', TEXT, STATUS)

      ATTR = 'Label('

      NAX = AST_GETI( FRM2, 'Naxes', STATUS )
      DO I = 1, NAX

         IAT = 6
         CALL CHR_PUTI( I, ATTR, IAT )
         CALL CHR_APPND( ')', ATTR, IAT )
         LAB1 = AST_GETC( FRM2, ATTR( : IAT ), STATUS )

         IF( AXES( I ) .GT. 0 ) THEN
            IAT = 6
            CALL CHR_PUTI( AXES( I ), ATTR, IAT )
            CALL CHR_APPND( ')', ATTR, IAT )
            LAB2 = AST_GETC( FRM1, ATTR( : IAT ), STATUS )
         ELSE
            LAB2 = ' '
         END IF

         TEXT = ' '
         IAT = COL1
         CALL CHR_PUTI( I, TEXT, IAT )
         IAT = IAT + 1
         CALL CHR_APPND( '(', TEXT, IAT )
         CALL CHR_APPND( LAB1, TEXT, IAT )
         CALL CHR_APPND( ')', TEXT, IAT )

         IAT = COL2

         IF( AXES( I ) .GT. 0 ) THEN
            CALL CHR_PUTI( AXES( I ), TEXT, IAT )
            IAT = IAT + 1
            CALL CHR_APPND( '(', TEXT, IAT )
            CALL CHR_APPND( LAB2, TEXT, IAT )
            CALL CHR_APPND( ')', TEXT, IAT )
         ELSE
            CALL CHR_APPND( 'none', TEXT, IAT )
         END IF

         CALL MSG_OUT( ' ', TEXT, STATUS)
      END DO

      CALL MSG_BLANK( STATUS )

*  Write the axis associations out to an output parameter.
      CALL PAR_PUT1I( 'AXES', NAX, AXES, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a context message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ASTMATCHAXES_ERR', 'Error finding '//
     :                 'corresponding axes in two Frames.', STATUS )
      END IF

      END
