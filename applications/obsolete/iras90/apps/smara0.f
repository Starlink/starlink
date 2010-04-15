      SUBROUTINE SMARA0( NPNT, X, Y, MARKTY, MKSIZ, X1, X2, Y1, Y2,
     :                   POLY, LAST, FIRST, STATUS )
*+
*  Name:
*     SMARA0

*  Purpose:
*     Draw marks at given positions.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SMARA0( NPNT, X, Y, MARKTY, MKSIZ, X1, X2, Y1, Y2, POLY,
*                  LAST, FIRST, STATUS )

*  Description:
*     This subroutine is used by SKYMARK to draw marks or a polyline, at
*     the specified positions on the current graphic device surface.

*  Arguments:
*     NPNT = INTEGER (Given)
*        The number of positions to be marked.
*     X( NPNT ), Y( NPNT ) = DOUBLE PRECISION (Given)
*        The positions at which marks will be put.
*     MARKTY = INTEGER (Given)
*        The type of the marks.
*     MKSIZ( NPNT ) = REAL (Given)
*        The size of each mark.
*     X1, X2, Y1, Y2 = REAL (Given)
*        Bounds of the current SGS zone.
*     POLY = LOGICAL (Given)
*        True if a polyline is being drawn.
*     LAST = LOGICAL (Given)
*        If true then any partially completed polyline is closed by
*        connecting the last and first points, but nothing else is done.
*     FIRST = LOGICAL (Given and Returned)
*        True if this is the first point in a polyline. Returned false.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     25-JAN-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants
      INCLUDE 'MSG_PAR'          ! MSG_ constants

*  Arguments Given:
      INTEGER NPNT
      DOUBLE PRECISION X( NPNT ), Y( NPNT )
      INTEGER MARKTY
      REAL MKSIZ( NPNT )
      REAL X1, X2, Y1, Y2
      LOGICAL POLY
      LOGICAL LAST

*  Arguments Given and Returned:
      LOGICAL FIRST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      REAL PX                    ! X at end of current polyline.
      REAL PY                    ! Y at end of current polyline.
      DOUBLE PRECISION XX        ! X value.
      DOUBLE PRECISION YY        ! Y value.

*  Initialise PX and PY to bad values.
      DATA PX/VAL__BADR/,
     :     PY/VAL__BADR/

*  Ensure PX and PY are saved between invocations of this routine.
      SAVE PX, PY
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If this is the first or last point and if a polyline was previously
*  being built......
      IF( ( FIRST .OR. LAST ) .AND. PX .NE. VAL__BADR ) THEN

*  Close the old polyline by joining it to the original point, and
*  output it.
         CALL SGS_APOLY( PX, PY )
         CALL SGS_OPOLY

*  Reset the polyline starting coordinates.
         PX = VAL__BADR
         PY = VAL__BADR

*  Do nothing else if this is the last point.
         IF( LAST ) GO TO 999

      END IF

*  Draw the markers after setting size.
      DO I = 1, NPNT

*  Only consider those points which have a valid marker size, and which
*  have valid coordinates.
         IF ( MKSIZ( I ) .NE. VAL__BADR ) THEN
            XX = X( I )
            YY = Y( I )

            IF( XX .NE. VAL__BADD .AND. YY .NE. VAL__BADD ) THEN

*  Also check that the point is within the SGS zone.
               IF( XX .GE. X1 .AND. XX .LE. X2 .AND.
     :             YY .GE. Y1 .AND. YY .LE. Y2 ) THEN

*  If a marker is required, draw it.
                  IF( .NOT. POLY ) THEN
                     CALL GSMKSC( MKSIZ( I ) )
                     CALL SGS_MARK( REAL( XX ), REAL( YY ), MARKTY )

*  If a poly line is required...
                  ELSE

*  If this is the first point in the polyline...
                     IF( FIRST ) THEN

*  Store the starting coordinates.
                        PX = REAL( XX )
                        PY = REAL( YY )

*  Begin the polyline.
                        CALL SGS_BPOLY( PX, PY )

*  Put a marker at the first position.
                        CALL SGS_MARK( PX, PY, 1 )

*  Indicate that a polyline has been started.
                        FIRST = .FALSE.

*  If a polyline has already been started, append the next point to it.
                     ELSE
                        CALL SGS_APOLY( REAL( XX ), REAL( YY ) )

                     ENDIF

                  END IF

               ELSE
                  CALL MSG_OUTIF( MSG__NORM, 'SMARA0_MSG1',
     :     '  (supplied position is outside the bounds of the picture)',
     :                         STATUS )
               END IF

            ELSE
               CALL MSG_OUTIF( MSG__NORM, 'SMARA0_MSG2',
     :                         '  (unusable coordinates ignored)',
     :                         STATUS )
            END IF

         END IF

      END DO

*  Flush out the drawings.
 999  CONTINUE
      CALL SGS_FLUSH

      END
