      SUBROUTINE ARD1_SCWCS( NDIM, PAR, UWCS, STATUS )
*+
*  Name:
*     ARD1_SCWCS

*  Purpose:
*     Create a new user FrameSet from a SCALE statement.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_SCWCS( NDIM, PAR, UWCS, STATUS )

*  Description:
*     This routine creates a new user FrameSet (UWCS) from the 
*     supplied parameters.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of axes.
*     PAR( * ) = DOUBLE PRECISION (Given)
*        The statement parameters.
*     UWCS = INTEGER (Given)
*        An AST pointer to the User FrameSet. The Current Frame
*        in this FrameSet is user coords.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     12-JUL-2001 (DSB):
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

*  Arguments Given:
      INTEGER NDIM
      DOUBLE PRECISION PAR( * )

*  Arguments Returned:
      INTEGER UWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER DOM*30           ! Frame Domain
      INTEGER FR                 ! Pointer to a Frame
      INTEGER I                  ! Loop counter
      INTEGER IFRM               ! Index of ARDAPP Frame
      INTEGER M1                 ! ZoomMap

*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Locate a Frame in the current User FrameSet with Domain ARDAPP.
      IFRM = AST__NOFRAME
      DO I = 1, AST_GETI( UWCS, 'NFRAME', STATUS )
         FR = AST_GETFRAME( UWCS, I, STATUS )
         DOM = AST_GETC( FR, 'DOMAIN', STATUS ) 
         CALL AST_ANNUL( FR, STATUS )
         IF( DOM .EQ. 'ARDAPP' ) THEN
            IFRM = I
            GO TO 10
         END IF
      END DO
 10   CONTINUE

*  If no Frame with Domain ARDAPP was found, annull the existing User
*  FrameSet and create a new one containing user and application coordinate
*  Frames connected by a UnitMap.
      IF( IFRM .EQ. AST__NOFRAME ) THEN
         CALL AST_ANNUL( UWCS, STATUS )
         CALL ARD1_COWCS( NDIM, AST__BAD, UWCS, STATUS )
         IFRM = AST_GETI( UWCS, 'BASE', STATUS )
      END IF 

*  Create a ZoomMap from old application coords to new application coords.
      M1 = AST_ZOOMMAP( NDIM, PAR( 1 ), ' ', STATUS ) 

*  Remap the application coords Frame.
      CALL AST_REMAPFRAME( UWCS, IFRM, M1, STATUS )

*  Annull AST objects.
      CALL AST_ANNUL( M1, STATUS )

      END
