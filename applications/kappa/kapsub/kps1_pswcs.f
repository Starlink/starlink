      SUBROUTINE KPS1_PSWCS( MAP, FRM, SLBND, SUBND, LBND, UBND, INDF, 
     :                       X, Y, STATUS )
*+
*  Name:
*     KPS1_PSWCS

*  Purpose:
*     Set up a WCS component in the NDF holding the model PSF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_PSWCS( MAP, FRM, SLBND, SUBND, LBND, UBND, INDF, 
*                      X, Y, STATUS )

*  Description:
*     This routine sets up a WCS component in the NDF holding the model 
*     PSF (INDF). 

*  Arguments:
*     MAP = INTEGER (Given)
*        Mapping from input GRID Frame to FRM.
*     FRM = INTEGER (Given)
*        The current Frame from the input NDF's WCS component.
*     SLBND( 2 ) = INTEGER (Given)
*        Lower pixel index bounds of significant axes in input NDF.
*     SUBND( 2 ) = INTEGER (Given)
*        Upper pixel index bounds of significant axes in input NDF.
*     LBND( 2 ) = INTEGER (Given)
*        Lower pixel index bounds in output NDF.
*     UBND( 2 ) = INTEGER (Given)
*        Upper pixel index bounds in output NDF.
*     INDF = INTEGER (Given)
*        Identifier for output NDF.
*     X( * ) = DOUBLE PRECISION (Given and Returned)
*        Work space. Must have at least UBND( 1 ) - LBND( 1 ) + 2 elements.
*     Y( * ) = DOUBLE PRECISION (Given and Returned)
*        Work space. Must have at least UBND( 2 ) - LBND( 2 ) + 2 elements.
*     STATUS = INTEGER (Given)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-SEP-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST constants and function declarations

*  Arguments Given:
      INTEGER MAP
      INTEGER FRM
      INTEGER SLBND( 2 )
      INTEGER SUBND( 2 )
      INTEGER LBND( 2 )
      INTEGER UBND( 2 )
      INTEGER INDF

*  Arguments Given and Returned:
      DOUBLE PRECISION X( * )
      DOUBLE PRECISION Y( * )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION RTOAS     ! Radians to arc-seconds conversion factor
      PARAMETER ( RTOAS = 57.295779513082320876798*3600.0 )

*  Local Variables:
      CHARACTER DOM*20           ! DOMAIN of input current Frame
      DOUBLE PRECISION C( 2 )    ! Cur. Frame co-ords at centre of input image
      DOUBLE PRECISION CIN( 2 )  ! GRID co-ords at centre of input image
      DOUBLE PRECISION COUT( 2 ) ! GRID co-ords at centre of output image
      DOUBLE PRECISION D( 2 )    ! Cur. Frame co-ords at first pixel in LutMap
      DOUBLE PRECISION DIST0     ! Distance from first pixel to centre
      DOUBLE PRECISION E( 2 )    ! Cur. Frame co-ords at a pixel in LutMap
      DOUBLE PRECISION SHIFT( 2 )! Pixel shifts to align centres of both images
      INTEGER CMAP               ! Final combined Mapping
      INTEGER FSET               ! Temporary FrameSet
      INTEGER I                  ! Index count
      INTEGER IWCS               ! Output WCS FrameSet
      INTEGER LMAP1              ! LutMap for axis 1
      INTEGER LMAP2              ! LutMap for axis 2
      INTEGER NEWFRM             ! Current Frame for output NDF
      INTEGER NP                 ! Number of points in each LutMap
      LOGICAL SKYFRM             ! Is input Current Frame a SkyFrame?
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Get the default WCS FrameSet from the newly created output NDF.
      CALL NDF_GTWCS( INDF, IWCS, STATUS )      

*  Get the Domain of the required current Frame.
      DOM = AST_GETC( FRM, 'DOMAIN', STATUS )

*  If the required Domain is PIXEL or GRID, just set the same Frame Current
*  in the output WCS FrameSet.
      IF( DOM .EQ. 'PIXEL' .OR. DOM .EQ. 'GRID' ) THEN
         FSET = AST_FINDFRAME( IWCS, FRM, DOM, STATUS )

*  Otherwise, the required Current Frame will need to be created and
*  added to the output FrameSet.
      ELSE

*  Find the GRID co-ordinates at the centre of the input NDF.
         CIN( 1 ) = 0.5*DBLE( SUBND( 1 ) - SLBND( 1 ) + 1 ) + 0.5
         CIN( 2 ) = 0.5*DBLE( SUBND( 2 ) - SLBND( 2 ) + 1 ) + 0.5

*  Find the GRID co-ordinates at the centre of the output NDF.
         COUT( 1 ) = 0.5*DBLE( UBND( 1 ) - LBND( 1 ) + 1 ) + 0.5
         COUT( 2 ) = 0.5*DBLE( UBND( 2 ) - LBND( 2 ) + 1 ) + 0.5

*  Find the pixel shifts which align the two centres.
         SHIFT( 1 ) = COUT( 1 ) - CIN( 1 )
         SHIFT( 2 ) = COUT( 2 ) - CIN( 2 )

*  Find the corresponding Current Frame position.
         CALL AST_TRAN2( MAP, NP, CIN( 1 ), CIN( 2 ), .TRUE., 
     :                   C( 1 ), C( 2 ), STATUS ) 

*  Store the input GRID co-ordinates along a row of points passing
*  through the centre of the output NDF. Each point is at the boundary
*  between output pixels, and the points extend across the entire
*  width of the output NDF.
         NP = UBND( 1 ) - LBND( 1 ) + 2
         DO I = 1, NP
            X( I ) = DBLE( I ) - 0.5 - SHIFT( 1 )
            Y( I ) = CIN( 2 )
         END DO

*  Map these input GRID positions into the current Frame.
         CALL AST_TRAN2( MAP, NP, X, Y, .TRUE., X, Y, STATUS ) 

*  Find the geodesic distance from the centre of the first output pixel in 
*  this row to the centre of the image.
         D( 1 ) = X( 1 )
         D( 2 ) = Y( 1 )
         DIST0 = AST_DISTANCE( FRM, D, C, STATUS ) 

*  Find the geodesic distance from the centre of the output image to each
*  pixel in this row, and store it in array Y. 
         DO I = 1, NP
            E( 1 ) = X( I )
            E( 2 ) = Y( I )
            Y( I ) = AST_DISTANCE( FRM, D, E, STATUS ) - DIST0
         END DO

*  If the current Frame is a SkyFrame, the geodesic distances found above
*  will be in radians. We want them in arc-seconds so scale them.
         IF( AST_ISASKYFRAME( FRM, STATUS ) ) THEN
            SKYFRM = .TRUE.

            DO I = 1, NP
               Y( I ) = RTOAS*Y( I )
            END DO

         ELSE
            SKYFRM = .FALSE.
         END IF

*  Create a LutMap giving geodesic distance from the centre as a function of
*  output Grid position for the first axis.
         LMAP1 = AST_LUTMAP( NP, Y, 0.5D0, 1.0D0, ' ', STATUS ) 

*  Do the same for the second axis...

*  Store the input GRID co-ordinates along a column of points passing
*  through the centre of the output NDF. Each point is at the boundary
*  between output pixels, and the points extend across the entire
*  height of the output NDF.
         NP = UBND( 2 ) - LBND( 2 ) + 2
         DO I = 1, NP
            X( I ) = CIN( 1 )
            Y( I ) = DBLE( I ) - 0.5 - SHIFT( 2 )
         END DO

*  Map these input GRID positions into the current Frame.
         CALL AST_TRAN2( MAP, NP, X, Y, .TRUE., X, Y, STATUS ) 

*  Find the geodesic distance from the centre of the first output pixel in 
*  this column to the centre of the image.
         D( 1 ) = X( 1 )
         D( 2 ) = Y( 1 )
         DIST0 = AST_DISTANCE( FRM, D, C, STATUS ) 

*  Find the geodesic distance from the centre of the output image to each
*  pixel in this column, and store it in array Y. 
         DO I = 1, NP
            E( 1 ) = X( I )
            E( 2 ) = Y( I )
            Y( I ) = AST_DISTANCE( FRM, D, E, STATUS ) - DIST0
         END DO

*  If the current Frame is a SkyFrame, the geodesic distances found above
*  will be in radians. We want them in arc-seconds so scale them.
         IF( SKYFRM ) THEN
            DO I = 1, NP
               Y( I ) = RTOAS*Y( I )
            END DO
         END IF

*  Create a LutMap giving geodesic distance from the centre as a function of
*  output Grid position for the second axis.
         LMAP2 = AST_LUTMAP( NP, Y, 0.5D0, 1.0D0, ' ', STATUS ) 

*  Combine the two 1D LutMaps into a 2D Mapping, from output GRID Frame
*  to geodesic offset from the image centre within the current Frame.
         CMAP = AST_CMPMAP( LMAP1, LMAP2, .FALSE., ' ', STATUS )         

*  We now construct a suitable current Frame for the output NDF.
         NEWFRM = AST_FRAME( 2, ' ', STATUS )
         IF( SKYFRM ) THEN
            CALL AST_SETC( NEWFRM, 'UNIT(1)', 'Arc-seconds', STATUS )
            CALL AST_SETC( NEWFRM, 'UNIT(2)', 'Arc-seconds', STATUS )

         ELSE
            CALL AST_SETC( NEWFRM, 'UNIT(1)', AST_GETC( FRM, 'UNIT(1)',
     :                                                       STATUS ), 
     :                     STATUS )
            CALL AST_SETC( NEWFRM, 'UNIT(2)', AST_GETC( FRM, 'UNIT(2)',
     :                                                       STATUS ), 
     :                     STATUS )
         END IF

         CALL AST_SETC( NEWFRM, 'LABEL(1)', 'Offset on axis 1', STATUS )
         CALL AST_SETC( NEWFRM, 'LABEL(2)', 'Offset on axis 2', STATUS )
         CALL AST_SETC( NEWFRM, 'DOMAIN', 'OFFSET', STATUS )

*  Add this Frame into the default FrameSet using the Mapping found above.
         CALL AST_ADDFRAME( IWCS, AST__BASE, CMAP, NEWFRM, STATUS ) 

      END IF

*  Save the output FrameSet in the output NDF.
      CALL NDF_PTWCS( IWCS, INDF, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
