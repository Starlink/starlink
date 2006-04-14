      SUBROUTINE ARD1_RDCOF( NDIM, IGRP, UWCS, STATUS )
*+
*  Name:
*     ARD1_RDCOF

*  Purpose:
*     Creates an AST Frame from a COFRAME statment.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_RDCOF( NDIM, IGRP, UWCS, STATUS )

*  Description:
*     This routine reads the supplied arguments to the COFRAME statement 
*     and creates a FrameSet containing the single Frame or SkyFrame 
*     decribed by the COFRAME statement. 

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of axes required in the Current Frame.
*     IGRP = INTEGER (Given)
*        A GRP identifier for the supplied group.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     17-JUL-2001 (DSB):
*        Original version.
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
      INTEGER NDIM
      INTEGER IGRP

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

*  If the Domain is SKY and the ARD expression is 2D, create SkyFrame,
*  otherwise create a Frame.
      IF( DOMAIN .EQ. 'SKY' .AND. NDIM .EQ. 2 ) THEN
         FR = AST_SKYFRAME( ' ', STATUS )
      ELSE
         FR = AST_FRAME( NDIM, ' ', STATUS )
         CALL AST_SETC( FR, 'DOMAIN', DOMAIN, STATUS )
      END IF

*  Loop round any remaining elements in the group, using them to assign
*  values to Frame attributes
      DO I = 2, SIZE
         CALL ARD1_GET( IGRP, I, 1, TEXT, STATUS ) 
         CALL AST_SET( FR, TEXT, STATUS )
      END DO

*  Create the FrameSet.
      UWCS = AST_FRAMESET( FR, ' ', STATUS )

*  Add a context message if an error has been reported.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'ARD1_RDCOF_ERR2', 'Unable to interpret a '//
     :                 'COFRAME statement in an ARD expression.', 
     :                 STATUS )
      END IF

      END
