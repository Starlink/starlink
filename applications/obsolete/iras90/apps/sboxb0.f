      SUBROUTINE SBOXB0( MARKER, X, Y, STATUS )

*+
*  Name:
*     SBOXB0

*  Purpose:
*     Get a cursor position, checking it is inside the zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SBOXB0( MARKER, X, Y, STATUS )

*  Description:
*     If the supplied values fo X and Y are bad, the cursor is
*     positioned at the centre of the zone. The cursor is then used to
*     get a position. If it is outside the zone the user is asked to
*     give a different position. This continues until a position is
*     obtained which is within the zone. If MARKER is true, the position
*     is marked with a dot.

*  Arguments:
*     MARKER = LOGICAL (Given)
*        If true, the final good position is marker with a dot.
*     X = REAL (Returned)
*        The X world coordinate of the cursor position
*     Y = REAL (Returned)
*        The Y world coordinate of the cursor position
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1993 (DSB):
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
      LOGICAL MARKER

*  Arguments Returned:
      REAL X
      REAL Y

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER
     :        N                  ! Choice key identifier.

      LOGICAL
     :        INSIDE             ! True if position is inside the zone

      REAL
     :        ZX1,               ! Low X bound of SGS zone
     :        ZX2,               ! High X bound of SGS zone
     :        ZY1,               ! Low Y bound of SGS zone
     :        ZY2,               ! High Y bound of SGS zone
     :        ZXM,               ! X size of SGS zone in metres
     :        ZYM                ! Y size of SGS zone in metres

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the extent of the current zone.
      CALL SGS_IZONE( ZX1, ZX2, ZY1, ZY2, ZXM, ZYM )

*  Loop round until a position is obtained which is within the zone.
      INSIDE = .FALSE.
      DO WHILE( .NOT. INSIDE .AND. STATUS .EQ. SAI__OK )

*  If the input position is bad, position the cursor at the centre of
*  the zone.
         IF( X .EQ. VAL__BADR .OR. Y .EQ. VAL__BADR ) THEN
            CALL SGS_SETCU( 0.5*( ZX1 + ZX2 ), 0.5*( ZY1 + ZY2 ) )
         END IF

*  Make the cursor visible.
         CALL SGS_CUVIS( .TRUE. )

*  Get a position from the cursor.
         CALL SGS_REQCU( X, Y, N )

*  Set a flag if this position is within the current zone.
         IF( X .GE. ZX1 .AND. X .LE. ZX2 .AND.
     :       Y .GE. ZY1 .AND. Y .LE. ZY2 ) THEN
            INSIDE = .TRUE.

*  Draw a marker at the current position if required.
            IF( MARKER ) CALL SGS_MARK( X, Y, 1 )

*  Otherwise, warn the user.
         ELSE
            CALL MSG_OUTIF( MSG__NORM, 'SBOXB0_MSG1',
     : '  The selected position is outside the current picture. '//
     : 'Please try again...', STATUS )
            X = VAL__BADR
         END IF

*  See if GKS/SGS has reported an error.
         CALL GKS_GSTAT( STATUS )

      END DO

      END
