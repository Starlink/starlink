      SUBROUTINE KPG1_ASPRP( NDIM, INDF1, INDF2, MATRIX, OFFSET, 
     :                       STATUS )
*+
*  Name:
*     KPG1_ASPRP

*  Purpose:
*     Propagate the WCS component from one NDF to another, allowing 
*     for a linear mapping of the pixel coordinates.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_ASPRP( NDIM, INDF1, INDF2, MATRIX, OFFSET, STATUS )

*  Description:
*     This routine copies the WCS FrameSet from INDF1, and modifies the PIXEL
*     Frame by remapping it using the supplied linear transformation. The
*     modified FrameSet is stored as the WCS component in INDF2. The linear
*     transformation describes the mapping from pixel coordinates in
*     INDF1 ("PIX1") to the corresponding pixel coordinates in INDF2
*     ("PIX2"), and is:
*
*        PIX2 = MATRIX . PIX1 + OFFSET 
*
*     For instance, for NDIM = 2:
*
*        X2 = MATRIX( 1, 1 ).X1 + MATRIX( 2, 1 ).Y1 + OFFSET( 1 )
*        Y2 = MATRIX( 1, 2 ).X1 + MATRIX( 2, 2 ).Y1 + OFFSET( 2 )

*  Arguments:
*     NDIM = INTEGER (Given)
*        The number of dimensions.
*     INDF1 = INTEGER (Given)
*        An identifier for the source NDF. This should have NDIM axes.
*     INDF2 = INTEGER (Given)
*        An identifier for the destination NDF. This should have NDIM axes.
*     MATRIX( NDIM, NDIM ) = DOUBLE PRECISION (Given)
*        The matrix connecting PIX1 and PIX2.
*     OFFSET( NDIM ) = DOUBLE PRECISION (Given)
*        The offset vector for PIX2.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-JUN-1998 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'NDF_PAR'          ! NDF constants 
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER NDIM
      INTEGER INDF1
      INTEGER INDF2
      DOUBLE PRECISION MATRIX( NDIM, NDIM )
      DOUBLE PRECISION OFFSET( NDIM )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION INA( NDF__MXDIM ) ! Corner "A" of window in input Frame
      DOUBLE PRECISION INB( NDF__MXDIM ) ! Corner "B" of window in input Frame
      DOUBLE PRECISION OUTA( NDF__MXDIM )! Corner "A" of window in output Frame
      DOUBLE PRECISION OUTB( NDF__MXDIM )! Corner "B" of window in output Frame
      INTEGER I                  ! Loop count
      INTEGER ICURR              ! Index of Current Frame 
      INTEGER IWCS               ! AST pointer to WCS FrameSet
      INTEGER MAP                ! AST pointer to compound Mapping
      INTEGER MTRMAP             ! AST pointer to MatrixMap
      INTEGER WINMAP             ! AST pointer to WinMap
*.

*  Check the inherited status. 
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the WCS FrameSet from the input NDF.
      CALL NDF_GTWCS( INDF1, IWCS, STATUS )

*  Note the index of the Current Frame in the FrameSet.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Find the PIXEL Frame in the FrameSet. This results in the PIXEL Frame 
*  becoming the Current Frame. Do nothing if the PIXEL Frame is not found.
      IF( AST_FINDFRAME( IWCS, AST_FRAME( NDIM, ' ', STATUS ), 'PIXEL',
     :                   STATUS ) .NE. AST__NULL ) THEN

*  Create a MatrixMap from the supplied MATRIX array.
         MTRMAP = AST_MATRIXMAP( NDIM, NDIM, 0, MATRIX, ' ', STATUS )

*  Create a WinMap which gives the required shift of origin. 
         DO I = 1, NDIM
            INA( I ) = 0.0D0
            INB( I ) = OFFSET( I )
            OUTA( I ) = OFFSET( I )
            OUTB( I ) = 2.0*OFFSET( I )
         END DO
         WINMAP = AST_WINMAP( NDIM, INA, INB, OUTA, OUTB, ' ', STATUS )

*  Concatenate these two mappings in series, and simplify it.
         MAP = AST_SIMPLIFY( AST_CMPMAP( MTRMAP, WINMAP, .TRUE., ' ', 
     :                                   STATUS ), STATUS )

*  Remap the PIXEL Frame (now the Current Frame) using this mapping.
         CALL AST_REMAPFRAME( IWCS, AST__CURRENT, MAP, STATUS )

*  Re-instate the original Current Frame.
         CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  Store the FrameSet as the WCS component in the output NDF.
         CALL NDF_PTWCS( INDF2, IWCS, STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

*  Report an error if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL ERR_REP( 'KPG1_ASPRP_1', 'Failed to propagate WCS '//
     :                 'information to ''^NDF''.', STATUS )

      ELSE IF( IWCS .EQ. AST__NULL ) THEN
         STATUS = SAI__ERROR
         CALL NDF_MSG( 'NDF', INDF2 )
         CALL ERR_REP( 'KPG1_ASPRP_2', 'Failed to propagate WCS '//
     :                 'information to ''^NDF''.', STATUS )
      END IF

      END
