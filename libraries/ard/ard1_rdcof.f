      SUBROUTINE ARD1_RDCOF( NWCS, IGRP, AWCS, UWCS, STATUS )
*+
*  Name:
*     ARD1_RDCOF

*  Purpose:
*     Creates an AST Frame from a COFRAME statment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_RDCOF( NWCS, IGRP, AWCS, UWCS, STATUS )

*  Description:
*     This routine reads the supplied arguments to the COFRAME statement
*     and creates a FrameSet containing the single Frame decribed by the
*     COFRAME statement.

*  Arguments:
*     NWCS = INTEGER (Given)
*        The default number of axes in the user coord system.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the supplied group.
*     AWCS = INTEGER (Given)
*        An AST pointer to the application WCS FrameSet.
*     UWCS = INTEGER (Returned)
*        An AST pointer to the returned Object. AST__NULL is returned if
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-2001 (DSB):
*        Original version.
*     10-MAY-2007 (DSB):
*        - Extend the range of AST Frame classes that are supported.
*        - Correct interpretation of extra attribute settings.
*        - Aded argument AWCS.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations
      INCLUDE 'ARD_ERR'          ! ARD error constants
      INCLUDE 'GRP_PAR'          ! GRP constants

*  Arguments Given:
      INTEGER NWCS
      INTEGER IGRP
      INTEGER AWCS

*  Arguments Returned:
      INTEGER UWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOMAIN*30       ! Frame Domain
      CHARACTER TEXT*(GRP__SZNAM)! Group element
      INTEGER COMMA             ! Index of comma within group element
      INTEGER FR                ! Frame or Skyframe
      INTEGER I                 ! Loop count
      INTEGER ICURR             ! Original index of current Frame
      INTEGER RESULT            ! FrameSet returned by AST_FINDFRAME
      INTEGER SIZE              ! Number of elements in supplied group
*.

*  Initialise returned pointer.
      UWCS = AST__NULL

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the number of elements in the supplied group.
      CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
      IF( SIZE .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = ARD__BADAR
         CALL ERR_REP( 'ARD1_RDCOF_ERR1', 'No arguments supplied.',
     :                 STATUS )
      END IF

*  Get the first elment from the group.
      CALL ARD1_GET( IGRP, 1, 1, TEXT, STATUS )

*  If there is a comma in it, the Frame Domain is the text before the
*  first comma.
      DOMAIN = ' '
      COMMA = INDEX( TEXT, ',' )
      IF( COMMA .GT. 0 ) THEN
         IF( COMMA .GT. 1 ) THEN
            DOMAIN = TEXT( : COMMA - 1 )
         END IF

*  If there is no comma, the Domain name is the entire string.
      ELSE
         DOMAIN = TEXT
      END IF

*  Remove spaces, can convert to upper case.
      CALL CHR_RMBLK( DOMAIN )
      CALL CHR_UCASE( DOMAIN )

*  If the Domain is SKY, create SkyFrame.
      IF( DOMAIN .EQ. 'SKY' ) THEN
         FR = AST_SKYFRAME( ' ', STATUS )

*  If the Domain is TIME, create TimeFrame,
      ELSE IF( DOMAIN .EQ. 'TIME' ) THEN
         FR = AST_TIMEFRAME( ' ', STATUS )

*  If the Domain is SPECTRUM, create
*  SpecFrame.
      ELSE IF( DOMAIN .EQ. 'SPECTRUM' ) THEN
         FR = AST_SPECFRAME( ' ', STATUS )

*  If the Domain is DSBSPECTRUM, create
*  a DSBSpecFrame.
      ELSE IF( DOMAIN .EQ. 'DSBSPECTRUM' ) THEN
         FR = AST_DSBSPECFRAME( ' ', STATUS )

*  Otherwise create a Frame.
      ELSE
         FR = AST_FRAME( NWCS, ' ', STATUS )
         CALL AST_SETC( FR, 'DOMAIN', DOMAIN, STATUS )
      END IF

*  Use any remaining text in the statement to assign values to Frame
*  attributes.
      IF( COMMA .GT. 0 ) THEN
         CALL AST_SET( FR, TEXT( COMMA + 1 : ), STATUS )
      END IF

*  Use any other elements in the group.
      DO I = 2, SIZE
         CALL ARD1_GET( IGRP, I, 1, TEXT, STATUS )
         CALL AST_SET( FR, TEXT, STATUS )
      END DO

*  Find a Frame in the application FrameSet that looks like the supplied
*  Frame. We use the user-supplied Frame as the template so that any
*  unset attributes are inherited form the application FrameSet.
      CALL AST_SETI( FR, 'MaxAxes', AST_GETI( AWCS, 'Nout', STATUS ),
     :               STATUS )
      ICURR = AST_GETI( AWCS, 'Current', STATUS )
      RESULT = AST_FINDFRAME( AWCS, FR, ' ', STATUS )
      CALL AST_SETI( AWCS, 'Current', ICURR, STATUS )

*  Use the Frame found above rather than the original Frame as it
*  will have fewer unset attribute values.
      IF( RESULT .NE. AST__NULL ) THEN
         CALL AST_ANNUL( FR, STATUS )
         FR = AST_GETFRAME( RESULT, AST__CURRENT, STATUS )
         CALL AST_ANNUL( RESULT, STATUS )
      END IF

*  Create the FrameSet.
      UWCS = AST_FRAMESET( FR, ' ', STATUS )

*  Add a context message if an error has been reported.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARD1_RDCOF_ERR2', 'Unable to interpret a '//
     :                 'COFRAME statement in an ARD expression.',
     :                 STATUS )
      END IF

      END
