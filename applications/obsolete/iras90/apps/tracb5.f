      SUBROUTINE TRACB5( BSMP, ESMP, X, Y, SCALE, SMP, ED, BG,
     :                   AEL, AXIS, SLOPE, CONST, STATUS )
*+
*  Name:
*     TRACB5

*  Purpose:
*     Calculate  background offset and slope of a peak.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL TRACB5( BSMP, ESMP, X, Y, SCALE, SMP, ED, BG, AEL, AXIS,
*                  SLOPE, CONST, STATUS )

*  Description:
*     A linear estimate of the local background is made by finding the
*     minimum data value in two segments of the displayed trace, one
*     on each side of the segment in which the point source profile is
*     to be drawn.
*
*     The line is Y = CONST + SLOPE*X where Y is a data value in the
*     displayed units (not including the vertical offsets), and X is
*     the in-scan offset in arc-mins from the centre of the source
*     (i.e. the sample specified by SMP).

*  Arguments:
*     BSMP = INTEGER (Given)
*        The begin index of the curve.
*     ESMP = INTEGER (Given)
*        The end index of the curve.
*     X( BSMP : ESMP ) = REAL (Given)
*        The X data of the curve.
*     Y( BSMP : ESMP ) = REAL (Given)
*        The unscaled Y data of the curve.
*     SCALE = REAL (Given)
*        The scale factor to produce the displayed data.
*     SMP = INTEGER (Given)
*        The position around which a peak is looked for.
*     ED = INTEGER (Given)
*        The index of the first non-zero value in the supplied point
*        source profile.
*     BG = INTEGER (Given)
*        The index of the last non-zero value in the supplied point
*        source profile.
*     AEL = INTEGER (Given)
*        No. of elements in the profile.
*     AXIS( AEL ) = REAL( Given)
*        The offset in arcmins of each element in the profile from the
*        source centre.
*     SLOPE = REAL (Returned)
*        The slope of the background line.
*     CONST = REAL (Returned)
*        The constant of the background line.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLNK)
*     {enter_new_authors_here}

*  History:
*     15-MAR-1991 (WG):
*        Original version.
*        (Based on INTERIM version of TRACB5 by MAVAD::DSB )
*     24-NOV-1992 (DSB):
*        Re-written to use simpler estimation of background.
*     8-DEC-1993 (DSB):
*        Guard against bad SCALE values.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants

*  Arguments Given:
      INTEGER BSMP
      INTEGER ESMP
      REAL X( BSMP : ESMP )
      REAL Y( BSMP : ESMP )
      REAL SCALE
      INTEGER SMP
      INTEGER ED
      INTEGER BG
      INTEGER AEL
      REAL AXIS( AEL )

*  Arguments Returned:
      REAL SLOPE
      REAL CONST

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER BSEG               ! Begin sample of the segment
      INTEGER EL                 ! Current element.
      INTEGER ESEG               ! End sample of the segment
      INTEGER NSEG               ! Number of valid segments.

      REAL END                   ! X position of end of search segment.
      REAL START                 ! X position of start of search segment.
      REAL XMIN                  ! X position at which minimum Y occurs.
      REAL XX(2)                 ! X positions of minima
      REAL YMIN                  ! Minimum y value.
      REAL YY(2)                 ! Y minima

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      XMIN = 0.0

*  Return with zero slope and offset if the detector is dead.
      IF( SCALE .EQ. VAL__BADR ) THEN
         SLOPE = 0.0
         CONST = 0.0
         GO TO 999
      END IF

*  Find the in-scan positions of the start and end of the segment
*  over which the background is estimated. The segment is three times
*  the width of the point source.
      IF( AXIS( ED ) .GT. AXIS( BG ) ) THEN
         START = X( SMP ) + 3.0*AXIS( BG )
         END = X( SMP ) +  3.0*AXIS( ED )
      ELSE
         START = X( SMP ) + 3.0*AXIS( ED )
         END = X( SMP ) +  3.0*AXIS( BG )
      END IF

*  Get the corresponding data sample numbers.
      BSEG = ESMP
      DO WHILE( X( BSEG ) .GT. START .AND. BSEG .GT. BSMP )
         BSEG = BSEG - 1
      END DO

      ESEG = BSMP
      DO WHILE( X( ESEG ) .LT. END .AND. ESEG .LT. ESMP )
         ESEG = ESEG + 1
      END DO

*  Ensure that the search segment contains the source centre.
      BSEG = MIN( BSEG, SMP )
      ESEG = MAX( ESEG, SMP )

*  Find the minimum Y value (and corresponding X value) in a segment
*  on the low sample side of the source.
      YMIN = VAL__MAXR

      DO EL = BSEG, NINT( 0.75*REAL( BSEG ) + 0.25*REAL( ESEG ) )

         IF( Y( EL ) .NE. VAL__BADR ) THEN
            IF( Y( EL ) .LT. YMIN ) THEN
               YMIN = Y( EL )
               XMIN = X( EL )
            END IF
         END IF

      END DO

*  Store the values, scaling and shifting them so that they can be
*  used directly in the calculation of the constant and slope.
      IF( YMIN .NE. VAL__MAXR ) THEN
         NSEG = 1
         YY( 1 ) = YMIN*SCALE
         XX( 1 ) = XMIN - X(SMP)
      ELSE
         NSEG = 0
      END IF

*  Find the minimum Y value (and corresponding X value) in a segment
*  on the high sample side of the source.
      YMIN = VAL__MAXR

      DO EL = NINT( 0.25*REAL( BSEG ) + 0.75*REAL( ESEG ) ), ESEG
         IF( Y( EL ) .NE. VAL__BADR ) THEN
            IF( Y( EL ) .LT. YMIN ) THEN
               YMIN = Y( EL )
               XMIN = X( EL )
            END IF
         END IF
      END DO

*  Store the values, scaling and shifting them so that they can be
*  used directly in the calculation of the constant and slope.
      IF( YMIN .NE. VAL__MAXR ) THEN
         NSEG = NSEG + 1
         YY( NSEG ) = YMIN*SCALE
         XX( NSEG ) = XMIN - X( SMP )
      END IF

*  If both segments had valid minima, calculate the slope and offset of
*  the line which joins them.
      IF( NSEG .EQ. 2 ) THEN
         SLOPE = ( YY( 2 ) - YY( 1 ) )/( XX( 2 ) - XX( 1 ) )
         CONST = YY( 1 ) - XX( 1 )*SLOPE

*  If only one of the two segments were valid, assume zero slope, and an
*  offset equal to the minimum value in the segment.
      ELSE IF( NSEG .EQ. 1 ) THEN
         SLOPE = 0.0
         CONST = YY( 1 )

*  Otherwise report an error.
      ELSE
         STATUS = SAI__ERROR
         CALL ERR_REP( 'TRACB6_ERR1',
     :       'TRACB6: Unable to fit a background to the displayed data',
     :                 STATUS )
      END IF

 999  CONTINUE

      END
