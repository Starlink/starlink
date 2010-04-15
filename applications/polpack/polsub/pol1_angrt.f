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
*     17-JUL-2001 (DSB):
*        Changed to find SkyFrames even if they are embedded in a CmpFrame.
*        Also changed to allow more than 2 pixel axes. All this is to
*        support spectropolarimetry data.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'AST_PAR'          ! AST__ constants
      INCLUDE 'NDF_PAR'          ! NDF__ constants

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
      DOUBLE PRECISION ARC       ! Arc-distance between 2 points in radians
      DOUBLE PRECISION IN( 2, NDF__MXDIM )  ! Pixel co-ordinates at 2 points
      DOUBLE PRECISION OUT( 2, NDF__MXDIM ) ! Sky (etc) coords at 2 points
      DOUBLE PRECISION P1( NDF__MXDIM )   ! Co-ordinates at 1 point
      DOUBLE PRECISION P2( NDF__MXDIM )   ! Co-ordinates at a second point
      INTEGER CMPT               ! The search template
      INTEGER DEFT               ! A Frame used within the search template
      INTEGER FSET               ! Pointer to a FrameSet
      INTEGER I                  ! Loop count
      INTEGER IBASE              ! Index of original Base Frame
      INTEGER ICURR              ! Index of original Current Frame
      INTEGER NIN                ! Number of pixel axes
      INTEGER NOUT               ! Number of sky (etc) axes
      INTEGER SKYT               ! SkyFrame used within the search template
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Initially, assume that no sky latitude axes is available. Set the
*  returned value to be +90 (i.e. the +ve Y axis).
      ANGROT = 90.0

*  Save the indices of the original Base and Current Frames.
      ICURR = AST_GETI( IWCS, 'CURRENT', STATUS )
      IBASE = AST_GETI( IWCS, 'BASE', STATUS )

*  Attempt to find a Frame with Domain PIXEL in the supplied FrameSet.
*  Set MaxAxes large so that we pick up PIXEL Frames with any number of axes
*  (e.g. specpol data has more than 2 pixel axes).
      IF( AST_FINDFRAME( IWCS, AST_FRAME( 2, 'MAXAXES=100', STATUS ),
     :                   'PIXEL', STATUS ) .NE. AST__NULL ) THEN

*  AST_FINDFRAME will have made the PIXEL Frame the Current Frame. Make
*  it the Base Frame also.
         CALL AST_SETI( IWCS, 'BASE', AST_GETI( IWCS, 'CURRENT',
     :                                          STATUS ), STATUS )

*  Create a template CmpFrame which will match a SkyFrame or a CmpFrame
*  containing a SkyFrame.
         SKYT = AST_SKYFRAME( ' ', STATUS )
         DEFT = AST_FRAME( 1, 'MINAXES=0,MAXAXES=100', STATUS )
         CMPT = AST_CMPFRAME( SKYT, DEFT, ' ', STATUS )

*  Attempt to find a matching Frame in the supplied FrameSet.
         FSET = AST_FINDFRAME( IWCS, CMPT, ' ', STATUS )

*  If a matching Frame was found...
         IF( FSET .NE. AST__NULL ) THEN

*  The FrameSet returned by AST_FINDFRAME will have the PIXEL Frame as the
*  Base Frame, and the Current Frame will look like the template Frame -
*  specifically, axes 1 and 2 will be sky longitude and latitude. Note the
*  number of Base and Current Frame axes.
            NIN = AST_GETI( FSET, 'NIN', STATUS )
            NOUT = AST_GETI( FSET, 'NOUT', STATUS )

*  Now transform the supplied central pixel position into the SkyFrame.
*  Also transform another point about one pixel away from the supplied
*  position. Any extra pixel axes (3 and above) are assigned the value zero.
*  The value on these extra axes should make no difference since the sky
*  axes should be independant of the others.
            IN( 1, 1 ) = DBLE( XC )
            IN( 2, 1 ) = DBLE( XC + 1.0 )
            IN( 1, 2 ) = DBLE( YC )
            IN( 2, 2 ) = DBLE( YC + 1.0 )

            DO I = 3, NIN
              IN( 1, I ) = 0.0
              IN( 2, I ) = 0.0
            END DO

            CALL AST_TRANN( FSET, 2, NIN, 2, IN, .TRUE., NOUT, 2, OUT,
     :                      STATUS )

*  Check that the transformed positions are defined.
            IF( OUT( 1, 1 ) .NE. AST__BAD .AND.
     :          OUT( 1, 2 ) .NE. AST__BAD .AND.
     :          OUT( 2, 1 ) .NE. AST__BAD .AND.
     :          OUT( 2, 2 ) .NE. AST__BAD ) THEN

*  Get the arc distance between the two points. Assign zero to any extra
*  axes.
               P1( 1 ) = OUT( 1, 1 )
               P1( 2 ) = OUT( 1, 2 )
               P2( 1 ) = OUT( 2, 1 )
               P2( 2 ) = OUT( 2, 2 )
               DO I = 3, NOUT
                  P1( I ) = 0.0
                  P2( I ) = 0.0
               END DO

               ARC = AST_DISTANCE( FSET, P1, P2, STATUS )

*  If the returned value is good, and not zero, find the long/lat of a
*  point one pixel to the north of the central point.
               IF( ARC .NE. AST__BAD .AND. ARC .GT. 0.0 ) THEN
                  OUT( 2, 1 ) = OUT( 1, 1 )
                  OUT( 2, 2 ) = OUT( 1, 2 ) + ARC

*  X/YOUT( 1 ) now holds the sky coords at the central point, and
*  X/YOUT( 2 ) holds the sky coords at a point just to the north of the
*  central point. Convert these back into pixel coordinates.
                  CALL AST_TRANN( FSET, 2, NOUT, 2, OUT, .FALSE., NIN,
     :                            2, IN, STATUS )

*  Find the anti-clockwise angle from the +ve X axis to the line joining
*  the two transformed sky positions.
                  ANGROT = ATAN2( IN( 2, 2 ) - IN( 1, 2 ),
     :                            IN( 2, 1 ) - IN( 1, 1 ) )/DTOR

               END IF

            END IF

         END IF

      END IF

*  Re-instate the original Base and Current Frames.
      CALL AST_SETI( IWCS, 'CURRENT', ICURR, STATUS )
      CALL AST_SETI( IWCS, 'BASE', IBASE, STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

      END
