      SUBROUTINE ARD1_APWCS( NDIM, TR, AWCS, STATUS )
*+
*  Name:
*     ARD1_APWCS

*  Purpose:
*     Create the Application FrameSet.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL ARD1_APWCS( NDIM, TR, AWCS, STATUS )

*  Description:
*     This routine creates a FrameSet in which the Base Frame corresponds
*     to pixel coordinates within the mask.

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of axes required in the Current Frame.
*     TR( * ) = REAL (Given)
*        The coefficients of the linear transformation from application
*        coords to pixel coords, supplied to ARD_WORK.
*     AWCS = INTEGER (Returned)
*        An AST pointer to the returned Object. AST__NULL is returned if 
*        an error occurs.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
      INCLUDE 'PRM_PAR'          ! VAL__ constants 
      INCLUDE 'ARD_CONST'        ! ARD provate constants 

*  Global Constants:
      INCLUDE 'ARD_COM'          ! ARD common blocks
*        CMN_AWCS = INTEGER (Read)
*           A pointer to the application FrameSet.

*  Arguments Given:
      INTEGER NDIM
      REAL TR( * )

*  Arguments Returned:
      INTEGER AWCS

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL ARD1_INIT         ! Initialise ARD common blocks

*  Local Variables:
      DOUBLE PRECISION INA( ARD__MXDIM ) ! Position A without offset
      DOUBLE PRECISION INB( ARD__MXDIM ) ! Position B without offset
      DOUBLE PRECISION M( ARD__MXDIM*ARD__MXDIM ) ! Matrix elements
      DOUBLE PRECISION OFFV              ! Offset term
      DOUBLE PRECISION OUTA( ARD__MXDIM )! Position A with offset
      DOUBLE PRECISION OUTB( ARD__MXDIM )! Position B with offset
      INTEGER F1                 ! Application coords Frame
      INTEGER F2                 ! Pixel coords Frame
      INTEGER I                  ! Row index
      INTEGER J                  ! Column index
      INTEGER K                  ! Index into matrix element array
      INTEGER L                  ! Index into supplied coefficient array
      INTEGER M1                 ! MatrixMap
      INTEGER M2                 ! WinMap
      INTEGER M3                 ! Pixel to application coords Mapping
*.

*  Initialise returned pointer.
      AWCS = AST__NULL

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a Frame representing ARD application coords.
      F1 = AST_FRAME( NDIM, 'DOMAIN=ARDAPP,Title=ARD application '//
     :                ' coordinates', STATUS )

*  If the first element of the supplied transformation is bad, use a 
*  UnitMap to connect this Frame to the PIXEL Frame.
      IF( TR( 1 ) .EQ. VAL__BADR ) THEN      
         M3 = AST_UNITMAP( NDIM, ' ', STATUS )

*  Otherwise, create a CmpMap representing the Mapping from this
*  Frame to the PIXEL Frame.
      ELSE     

*  First create a MatrixMap representing the matrix part of the mapping 
*  (i.e. skipping over the elements of TR which represent the constant 
*  offset vector).
         K = 1
         L = 1
         DO I = 1, NDIM
            L = L + 1
            DO J = 1, NDIM
               M( K ) = TR( L )
               K = K + 1
               L = L + 1
            END DO
         END DO         
   
         M1 = AST_MATRIXMAP( NDIM, NDIM, 0, M, ' ', STATUS ) 

*  Now create a WinMap representing the vector offset elements.
         DO I = 1, NDIM
            OFFV = TR( ( 1 + NDIM )*I - NDIM )
            INA( I ) = 0.0
            INB( I ) = OFFV
            OUTA( I ) = OFFV
            OUTB( I ) = 2*OFFV
         END DO
   
         M2 = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS ) 

*  Combine the MatrixMap and the WinMap. This is the Mapping from
*  application coords to pixel coords.
         M3 = AST_CMPMAP( M1, M2, .TRUE., ' ', STATUS )

*  Annull AST objects.
         CALL AST_ANNUL( M1, STATUS )
         CALL AST_ANNUL( M2, STATUS )

*  Invert the Mapping to get the Mapping from PIXEL to ARDAPP.
         CALL AST_INVERT( M3, STATUS )

      END IF

*  If ARD_WCS has been called, take a copy of the FrameSet supplied to
*  ARD_WCS.
      IF( CMN_AWCS .NE. AST__NULL ) THEN
         AWCS = AST_COPY( CMN_AWCS, STATUS )

*  Otherwise, create a new FrameSet containing just a PIXEL Frame.
      ELSE
         F2 = AST_FRAME( NDIM, 'DOMAIN=PIXEL,Title=Pixel coordinates',
     :                   STATUS )
         AWCS = AST_FRAMESET( F2, ' ', STATUS ) 
         CALL AST_ANNUL( F2, STATUS )
      END IF

*  Add in the ARDAPP Frame.
      CALL AST_ADDFRAME( AWCS, AST__BASE, M3, F1, STATUS ) 

*  Annull AST objects.
      CALL AST_ANNUL( M3, STATUS )
      CALL AST_ANNUL( F1, STATUS )

      END
