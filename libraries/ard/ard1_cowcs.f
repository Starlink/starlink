      SUBROUTINE ARD1_COWCS( NDIM, C, UWCS, STATUS )
*+
*  Name:
*     ARD1_COWCS

*  Purpose:
*     Create a new user FrameSet from a COEFFS statement.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_COWCS( NDIM, C, UWCS, STATUS )

*  Description:
*     This routine creates a new user FrameSet (UWCS) from the 
*     supplied linear Mapping coefficients.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of axes.
*     C( * ) = DOUBLE PRECISION (Given)
*        The coefficients of the user->app mapping. If element 1 is
*        AST__BAD a unit Mapping is used.
*     UWCS = INTEGER (Returned)
*        An AST pointer to the returned User FrameSet. The Current Frame
*        in this FrameSet is user coords, and the Base Frame is
*        "Application co-ordinates" (Domain ARDAPP).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-JUL-2001 (DSB):
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
      INCLUDE 'ARD_CONST'        ! ARD provate constants 

*  Arguments Given:
      INTEGER NDIM
      DOUBLE PRECISION C( * )

*  Arguments Returned:
      INTEGER UWCS

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION INA( ARD__MXDIM ) ! Position A without offset
      DOUBLE PRECISION INB( ARD__MXDIM ) ! Position B without offset
      DOUBLE PRECISION M( ARD__MXDIM*ARD__MXDIM ) ! Matrix elements
      DOUBLE PRECISION OFFV              ! Offset term
      DOUBLE PRECISION OUTA( ARD__MXDIM )! Position A with offset
      DOUBLE PRECISION OUTB( ARD__MXDIM )! Position B with offset
      INTEGER F1                 ! User coords Frame
      INTEGER F2                 ! Application coords Frame
      INTEGER I                  ! Row index
      INTEGER J                  ! Column index
      INTEGER K                  ! Index into matrix element array
      INTEGER L                  ! Index into supplied coefficient array
      INTEGER M1                 ! MatrixMap
      INTEGER M2                 ! WinMap
      INTEGER M3                 ! User to application coords Mapping

*.

      UWCS = AST__NULL

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  IF the first element is bad, use a UnitMap.
      IF( C( 1 ) .EQ. AST__BAD ) THEN
         M3 = AST_UNITMAP( NDIM, ' ', STATUS )

*  Otherwise,
      ELSE         

*  Create a MatrixMap representing the matrix part of the mapping (i.e.
*  skipping over the elements of C which represent the constant offset
*  vector.
         K = 1
         L = 1
         DO I = 1, NDIM
            L = L + 1
            DO J = 1, NDIM
               M( K ) = C( L )
               K = K + 1
               L = L + 1
            END DO
         END DO         
   
         M1 = AST_MATRIXMAP( NDIM, NDIM, 0, M, ' ', STATUS ) 

*  Now create a WinMap representing the vector offset elements.
         DO I = 1, NDIM
            OFFV = C( ( 1 + NDIM )*I - NDIM )
            INA( I ) = 0.0
            INB( I ) = OFFV
            OUTA( I ) = OFFV
            OUTB( I ) = 2*OFFV
         END DO
   
         M2 = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS ) 

*  Combine the MatrixMap and the WinMap. This is the Mapping from
*  user coords to application coords.
         M3 = AST_CMPMAP( M1, M2, .TRUE., ' ', STATUS )

*  Annull AST objects.
         CALL AST_ANNUL( M1, STATUS )
         CALL AST_ANNUL( M2, STATUS )
      }

*  Now create a Frame to represent user coords.
      F1 = AST_FRAME( NDIM, 'DOMAIN=ARDUSER,Title=ARD user coordinates',
     :                STATUS )

*  Now create a Frame to represent application coords.
      F2 = AST_FRAME( NDIM, 'DOMAIN=ARDAPP,Title=ARD application '//
     :                ' coordinates', STATUS )

*  Constrct the FrameSet, initially with user coords as Base Frame, and
*  application coords as current Frame.
      UWCS = AST_FRAMESET( F1, ' ', STATUS ) 
      CALL AST_ADDFRAME( UWCS, AST__BASE, M3, F2, STATUS ) 
      
*  Now invert the FrameSet to get the required Base and Current Frames.
      CALL AST_INVERT( UWCS, STATUS )

*  Annull AST objects.
      CALL AST_ANNUL( M3, STATUS )
      CALL AST_ANNUL( F1, STATUS )
      CALL AST_ANNUL( F2, STATUS )

      END
