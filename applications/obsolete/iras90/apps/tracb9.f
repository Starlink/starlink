      SUBROUTINE TRACB9( BSMP, ESMP, X, Y, BSEG, ESEG, XOFFST, YOFFST,
     :                   YAMPLF, YSLOPE, XLMT, YLMT, XORDER, YORDER,
     :                   PEN, STATUS )
*+
*  Name:
*     TRACB9

*  Purpose:
*     Draw an additional curve on an NCAR display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACB9( BSMP, ESMP, X, Y, BSEG, ESEG, XOFFST, YOFFST,
*                  YAMPLF, YSLOPE, XLMT, YLMT, XORDER, YORDER,
*                  PEN, STATUS )

*  Description:
*     This routine is used for drawing an additional curve segement on
*     an NCAR display. The curve in the display are the offset and
*     amplified segment of the curve given by X and Y array. In x
*     direction, the curve will have a constant offset given by XOFFST.
*     In y direction, the curve will be amplified by YAMPLF and will
*     not only have a constant offset given by YOFFST, but also have a
*     slope offset given by YSLOPE*( x distance from the centre of the
*     segment ).  It assumes that NCAR grid window has the bounds (0.0,
*     1.0, 0.0, 1.0 ) on entering and the plotting system is SGS.

*  Arguments:
*     BSMP = INTEGER (Given)
*        The begin index of samples in the curve.
*     ESMP = INTEGER (Given)
*        The end index of samples in the curve.
*     X( BSMP : ESMP ) = REAL (Given)
*        The x value of the curve at each sample.
*     Y( BSMP : ESMP ) = REAL (Given)
*        The y value of the curve at each sample.
*     BSEG = INTEGER (Given)
*        The begin sample index of the segment to be drawn.
*     ESEG = INTEGER (Given)
*        The end sample index of the segment to be drawn.
*     XOFFST = REAL (Given)
*        The offset in X direction of the curve when display.
*     YOFFST = REAL (Given)
*        The offset in Y direction of the curve when display.
*     YAMPLF = REAL (Given)
*        The  y amplifier factor of the curve.
*     YSLOPE = REAL (Given)
*        The slope in Y direction of the curve.
*     XLMT( 2 ) = REAL (Given)
*        The lower and upper X limits of the NCAR display in user's
*        coordinates. XLMT( 2 ) should always be greater than XLMT( 1 ).
*     YLMT( 2 ) = REAL (Given)
*        The lower and upper Y limits of the NCAR display in user's
*        coordinates. YLMT( 2 ) should always be greater than YLMT( 1 ).
*     XORDER = INTEGER (Given)
*        When it is 0, the x coordinates of the NCAR display increases
*        from left to right, i.e. XLMT( 1 ) is at left end of the axis
*        and XLMT( 2 ) at right end of the axis. When it is 1, the x
*        coordinates decreases from left to right.
*     YORDER = INTEGER (Given)
*        When it is 0, the y coordinates of the NCAR display increases
*        from bottom to top, i.e. YLMT( 1 ) is at the bottom end of the
*        axis and YLMT( 2 ) at top end of the axis. When it is 1, the y
*        coordinates decreases from bottom to top.
*     PEN = INTEGER (Given)
*        The pen number used to draw the curve.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1991 (WG):
*        Original version.
*     8-DEC-1993 (DSB):
*        Guard against bad detector scale factors.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Magic value definition

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      REAL X( BSMP : ESMP )
      REAL Y( BSMP : ESMP )
      INTEGER BSEG
      INTEGER ESEG
      REAL XOFFST
      REAL YOFFST
      REAL YAMPLF
      REAL YSLOPE
      REAL XLMT( 2 )
      REAL YLMT( 2 )
      INTEGER XORDER
      INTEGER YORDER
      INTEGER PEN

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER I                  ! Do loop index
      INTEGER OLDPEN             ! Previous pen number
      INTEGER OLDZON             ! Previous zone identifier
      INTEGER ZID                ! Identifier of new zone

      LOGICAL PENUP              ! Pen up flag

      REAL OLDX1                 ! Low X limit of previous zone.
      REAL OLDX2                 ! High X limit of previous zone.
      REAL OLDXM                 ! X size of previous zone
      REAL OLDY1                 ! Low Y limit of previous zone.
      REAL OLDY2                 ! High Y limit of previous zone.
      REAL OLDYM                 ! Y size of previous zone
      REAL X1                    ! X coordinate in new zone
      REAL X2                    ! X coordinate in new zone
      REAL XCENTR                ! X value of the centre of the segment
      REAL XFACT                 ! Mapping factor of X axis.
      REAL Y1                    ! Y coordinate in new zone
      REAL Y2                    ! Y coordinate in new zone
      REAL YFACT                 ! Mapping factor of Y axis.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the detector is dead, flush an error message to the user and
*  return.
      IF( YAMPLF .EQ. VAL__BADR ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACB9_ERR1','  The selected detector is '//
     :                 'dead. ', STATUS )
         CALL ERR_FLUSH( STATUS )
         GO TO 999
      END IF

*  Save old zone identifier and pen number.
      CALL SGS_IPEN( OLDPEN )
      CALL SGS_ICURZ( OLDZON )
      CALL SGS_IZONE( OLDX1, OLDX2, OLDY1, OLDY2, OLDXM, OLDYM )

*  Select the NCAR grid window as new zone.
      CALL SGS_ZONE( 0.0, 1.0, 0.0, 1.0, ZID, STATUS )

*  If the map of the X axis of original NCAR display is not increasing
*  from left to right, reverse the drawing order for X axis.
      IF ( XORDER .NE. 0 ) THEN
         X1 = -XLMT( 2 )
         X2 = -XLMT( 1 )
         XFACT = -1.0

*  Otherwis keep the drawing order.
      ELSE
         X1 = XLMT( 1 )
         X2 = XLMT( 2 )
         XFACT = 1.0
      END IF

*  If the map of the Y axis of original NCAR display is not increasing
*  from bottem to top, reverse the drawing order for Y axis.
      IF ( YORDER .NE. 0 ) THEN
         Y1 = - YLMT( 2 )
         Y2 = - YLMT( 1 )
         YFACT = -1.0

*  Otherwis keep the drawing order.
      ELSE
         Y1 = YLMT( 1 )
         Y2 = YLMT( 2 )
         YFACT = 1.0
      END IF

*  Set the x and y coordinates of the new zone as the limits of the grid
*  window in user's coordinates.
      CALL SGS_SW( X1, X2, Y1, Y2, STATUS )

*  Set the pen number for the curve.
      CALL SGS_SPEN( PEN )

*  If the segment have odd samples, the x centre will at a sample.
      IF ( MOD( ESEG - BSEG + 1, 2 ) .EQ. 1 ) THEN
         XCENTR = X( BSEG + ( ESEG - BSEG + 1 ) / 2 )

*  If the segment have even samples, the x centre will be at middle
*  of two samples.
      ELSE
         XCENTR = 0.5 * ( X( BSEG + ( ESEG - BSEG + 1 ) / 2 ) +
     :                    X(BSEG + ( ESEG - BSEG + 1 ) / 2 - 1 ) )
      END IF

*  Enter a do loop to draw the curve segment sample by sample.
      PENUP = .TRUE.
      DO I = BSEG, ESEG

*  If this sample is valid,
         IF ( Y( I ) .NE. VAL__BADR ) THEN

*  while the pen is up previously, set the pen down.
            IF ( PENUP ) THEN
               CALL SGS_BPOLY( XFACT * ( X( I ) + XOFFST ),
     :                         YFACT * ( YAMPLF * Y( I ) + YOFFST +
     :                               YSLOPE * ( X( I ) - XCENTR ) ) )
               PENUP = .FALSE.

*  or the pen is down previously, draw the line from previous point to
*  this point.
            ELSE
               CALL SGS_APOLY( XFACT * ( X( I ) + XOFFST ),
     :                         YFACT * ( YAMPLF * Y( I ) + YOFFST +
     :                               YSLOPE * ( X( I ) - XCENTR ) ) )
            END IF

*  If this sample is invalid, lift the pen up.
         ELSE
            PENUP = .TRUE.
         END IF

*  Go back to draw next point.
      END DO

*  Output the polylines drawn above.
      CALL SGS_OPOLY
      CALL SGS_FLUSH

*  Re-instate previous SGS state.
      CALL SGS_SPEN( OLDPEN )
      CALL SGS_SELZ( OLDZON, STATUS )
      CALL SGS_RELZ( ZID )
      CALL SGS_SW( OLDX1, OLDX2, OLDY1, OLDY2, STATUS )

 999  CONTINUE

      END
