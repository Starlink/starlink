      SUBROUTINE POL1_ANGRT( IWCS, XC, YC, ANGROT, STATUS )
*+
*  Name:
*     POL1_ANGRT

*  Purpose:
*     Decides on the reference direction to use when creating Stokes
*     vectors.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_ANGRT( IWCS, XC, YC, ANGROT, STATUS )

*  Description:
*     This routine returns the anti-clockwise angle in degrees from the 
*     positive X axis (i.e. the first pixel axis) to the reference direction 
*     for the created Stokes vectors.
*
*     If a SkyFrame can be found in the supplied FrameSet, then the
*     reference direction is the direction of increasing latitude at the 
*     supplied pixel co-ordinates. If no SkyFrame can be found, the 
*     reference direction is the positive Y axis (i.e. the second pixel
*     axis). Note, the current Frame is checked first. This means that
*     the specific flavour of "north" can be selected by setting the 
*     Current Frame (e.g. to a galactic, equatorial or ecliptic Frame).

*  Arguments:
*     IWCS = INTEGER (Given)
*        An AST identifier for a FrameSet.
*     XC = REAL (Given)
*        The first pixel axis value at the centre of the field.
*     YC = REAL (Given)
*        The second pixel axis value at the centre of the field.
*     ANGROT = REAL (Returned)
*        The ACW angle from the X axis to the reference direction, in
*        degrees.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
 
*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-APR-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST__ constants

*  Arguments Given:
      INTEGER IWCS 
      REAL XC
      REAL YC

*  Arguments Returned:
      REAL ANGROT

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      DOUBLE PRECISION DTOR      ! Degrees to radians conversion factor
      PARAMETER ( DTOR = 0.0174532925)

*  Local Variables:
      DOUBLE PRECISION A         ! Sky longitude at 1 point
      DOUBLE PRECISION ARC       ! Arc-distance between 2 points in radians
      DOUBLE PRECISION B         ! Sky latitude at 1 point
      DOUBLE PRECISION P1( 2 )   ! 2D co-ordinates at 1 point
      DOUBLE PRECISION P2( 2 )   ! 2D co-ordinates at a second point
      DOUBLE PRECISION XIN( 2 )  ! X pixel co-ordinates at 2 points
      DOUBLE PRECISION XOUT( 2 ) ! Sky axis 1 co-ordinates at 2 points
      DOUBLE PRECISION YIN( 2 )  ! X pixel co-ordinates at 2 points
      DOUBLE PRECISION YOUT( 2 ) ! Sky axis 2 co-ordinates at 2 points
      INTEGER FS                 ! Pointer to a FrameSet joining the SkyFrames
      INTEGER IBASE              ! Index of original Base Frame
      INTEGER ICURR              ! Index of original Current Frame
      INTEGER ISKY               ! Index of SkyFrame
      INTEGER SKYF1              ! Pointer to the IWCS SKyFrame
      INTEGER SKYF2              ! Pointer to a new SKyFrame
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Save the index of the original Current Frame (this may be changed by
*  AST_FINDFRAME).
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )

*  Attempt to find a SkyFrame in the supplied FrameSet.
      IF( AST_FINDFRAME( IWCS, AST_SKYFRAME( ' ', STATUS ), ' ', 
     :                   STATUS ) .NE. AST__NULL ) THEN

*  If a SkyFrame was found, the Current Frame in the FrameSet will have
*  been set to identify the SkyFrame. Save its index.
         ISKY = AST_GETI( IWCS, 'CURRENT', STATUS ) 

*  We now need to set the Base Frame to the PIXEL Frame. First record the 
*  index of the original Base Frame.
         IBASE = AST_GETI( IWCS, 'BASE', STATUS )

*  Attempt to find a 2D Frame with Domain PIXEL in the supplied FrameSet.
         IF( AST_FINDFRAME( IWCS, AST_FRAME( 2, ' ', STATUS ), 'PIXEL', 
     :                      STATUS ) .NE. AST__NULL ) THEN

*  AST_FINDFRAME will have made the PIXEL Frame the Current Frame. Make
*  it the Base Frame also.
            CALL AST_SETI( IWCS, 'BASE', AST_GETI( IWCS, 'CURRENT', 
     :                                             STATUS ), STATUS )

*  Make the Current Frame the SkyFrame.
            CALL AST_SETI( IWCS, 'CURRENT', ISKY, STATUS )

*  Now transform the supplied central pixel position into the SkyFrame.
*  Also transform another point about one pixel away from the 
*  supplied position.
            XIN( 1 ) = DBLE( XC )
            XIN( 2 ) = DBLE( XC + 1.0 )
            YIN( 1 ) = DBLE( YC )
            YIN( 2 ) = DBLE( YC + 1.0 )

            CALL AST_TRAN2( IWCS, 2, XIN, YIN, .TRUE., XOUT, YOUT, 
     :                      STATUS )

*  Check that the transformed positions are defined.
            IF( XOUT( 1 ) .NE. AST__BAD .AND. 
     :          YOUT( 1 ) .NE. AST__BAD .AND.
     :          XOUT( 2 ) .NE. AST__BAD .AND.
     :          YOUT( 2 ) .NE. AST__BAD ) THEN

*  Get the arc distance between the two points.
               P1( 1 ) = XOUT( 1 )
               P1( 2 ) = YOUT( 1 )
               P2( 1 ) = XOUT( 2 )
               P2( 2 ) = YOUT( 2 )
               ARC = AST_DISTANCE( IWCS, P1, P2, STATUS )

*  If the returned value is bad, or zero, return +90 (i.e. the +ve Y axis) 
*  for the reference direction.
               IF( ARC .EQ. AST__BAD .OR. ARC .EQ. 0.0 ) THEN
                  ANGROT = 90.0

*  Otherwise, offset northwards by this arc distance from the supplied
*  central position.
               ELSE

*  Which axis is north (+latitude)? Usually, the second axis. But it is
*  possible that the axis order may have been swapped. Therefore we need
*  to check which axis is which. This is not so easy! First create a new
*  SkyFrame with the same attributes as the one in the FrameSet, but with
*  the default axis order (longitude,latitude).
                  SKYF1 = AST_GETFRAME( IWCS, ISKY, STATUS )
                  SKYF2 = AST_SKYFRAME( ' ', STATUS )
                  CALL AST_SETD( SKYF2, 'EPOCH', AST_GETD( SKYF1,
     :                                           'EPOCH', STATUS ),
     :                           STATUS )
                  CALL AST_SETD( SKYF2, 'EQUINOX', AST_GETD( SKYF1,
     :                                           'EQUINOX', STATUS ),
     :                           STATUS )
                  CALL AST_SETC( SKYF2, 'SYSTEM', AST_GETC( SKYF1,
     :                                           'SYSTEM', STATUS ),
     :                           STATUS )

*  Obtain a FrameSet representing a mapping from the IWCS SkyFrame to the 
*  new SkyFrame.
                  FS = AST_CONVERT( SKYF1, SKYF2, ' ', STATUS )

*  If not succesfull, return +90 (i.e. the +ve Y axis) for the reference 
*  direction.
                  IF( FS .EQ. AST__NULL ) THEN
                     ANGROT = 90.0

*  Otherwise, transform the central sky position using this mapping.
*  This will swap the axes if necessary to ensure that the resulting
*  position is in the SkyFrame defined by SKYF2 (i.e. with axes in the 
*  order (longitude,latitude) ).
                  ELSE
                     CALL AST_TRAN2( FS, 1, XOUT, YOUT, .TRUE., A, B, 
     :                               STATUS )

*  We now know for sure that A is the longitude and B is the latitude at
*  the central point. Increase its latitude by the arc-size of a pixel.
                     B = B + ARC

*  Transform this position back into the (potentially swapped) IWCS 
*  SkyFrame.
                     CALL AST_TRAN2( FS, 1, A, B, .FALSE., XOUT( 2 ),
     :                               YOUT( 2 ), STATUS )

*  X/YOUT( 1 ) now holds the sky coords at the central point, and 
*  X/YOUT( 2 ) holds the sky coords at a point just to the north of the
*  central point. Convert these back into pixel coordinates.
                     CALL AST_TRAN2( IWCS, 2, XOUT, YOUT, .FALSE., XIN, 
     :                               YIN, STATUS )

*  Find the anti-clockwise angle from the +ve X axis to the line joining
*  the two transformed sky positions.
                     ANGROT = ATAN2( YIN( 2 ) - YIN( 1 ), 
     :                               XIN( 2 ) - XIN( 1 ) )/DTOR

                  END IF

               END IF

*  If the sky position at the supplied central position (or its
*  neighbour) were not defined, return +90 (i.e. the +ve Y axis) for the
*  reference direction.
            ELSE
               ANGROT = 90.0
            END IF

*  If no PIXEL Frame was found, return +90 (i.e. the +ve Y axis) for the
*  reference direction.
         ELSE
            ANGROT = 90.0
         END IF

*  Re-instate the original Base Frame
         CALL AST_SETI( IWCS, 'BASE', IBASE, STATUS )

*  If no SkyFrame could be found in the supplied FrameSet, return +90 
*  (i.e. the +ve Y axis) for the reference direction.
      ELSE
         ANGROT = 90.0
      END IF

*  Re-instate the original Current Frame.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
