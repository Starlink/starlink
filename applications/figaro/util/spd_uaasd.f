      SUBROUTINE SPD_UAASD( NELM, ARRAY, MONO, FIRST, STATUS )
*+
*  Name:
*     SPD_UAAS{DR}

*  Purpose:
*     Check monotonicity of an array.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UAAS{DR}( NELM, ARRAY, MONO, FIRST, STATUS )

*  Description:
*     This routine looks at a given array and works out if the elements
*     are monotonic. If not, the first violation of monotonicity is
*     returned.

*  Arguments:
*     NELM = INTEGER (Given)
*        The size of the array.
*     ARRAY( NELM ) = DOUBLE PRECISION (Given)
*        The array to be checked.
*     MONO = INTEGER (Given and Returned)
*        The given value tells for which monotonicity to check. The
*        returned value is the same or 0.
*        -  -2 if the array is strictly monotonically decreasing,
*        -  -1 if the array is monotonically decreasing,
*        -  +1 if the array is monotonically increasing,
*        -  +2 if the array is strictly monotonically increasing.
*     FIRST = INTEGER (Returned)
*        ARRAY(FIRST) is the first element that does not comply with
*        the monotonicity. If all elements comply, the value is returned
*        as 1.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     29 May 1993 (hme):
*        Original version.
*     25 Nov 1994 (hme):
*        Renamed from SPADND
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NELM
      DOUBLE PRECISION ARRAY( NELM )

*  Arguments Returned:
      INTEGER MONO
      INTEGER FIRST

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER J                  ! Loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Default first violator. The first element can of couse not be the
*  first violator. So that value is used to signal no violation.
      FIRST = 1

*  If the array should be strictly monotonically decreasing.
*  Jumping up or stagnating means it is not strictly monotonic.
      IF ( MONO .EQ. -2 ) THEN
         DO 1 J = 2, NELM
            IF ( ARRAY(J) .GE. ARRAY(J-1) ) THEN
               FIRST = J
               MONO = 0
               GO TO 500
            END IF
 1       CONTINUE

*  Else if the array should be monotonically decreasing.
*  Jumping up means it is not monotonic.
      ELSE IF ( MONO .EQ. -1 ) THEN
         DO 2 J = 2, NELM
            IF ( ARRAY(J) .GT. ARRAY(J-1) ) THEN
               FIRST = J
               MONO = 0
               GO TO 500
            END IF
 2       CONTINUE

*  Else if the should to be monotonically increasing.
*  Jumping down means it is not monotonic.
      ELSE IF ( MONO .EQ. +1 ) THEN
         DO 3 J = 2, NELM
            IF ( ARRAY(J) .LT. ARRAY(J-1) ) THEN
               FIRST = J
               MONO = 0
               GO TO 500
            END IF
 3       CONTINUE

*  Else if the array should be strictly monotonically increasing.
*  Jumping down or stagnating means it is not strictly monotonic.
      ELSE IF ( MONO .EQ. +2 ) THEN
         DO 4 J = 2, NELM
            IF ( ARRAY(J) .LE. ARRAY(J-1) ) THEN
               FIRST = J
               MONO = 0
               GO TO 500
            END IF
 4       CONTINUE

*  Else (invalid given MONO), just set it zero.
      ELSE
         MONO = 0
      END IF

*  Return.
 500  CONTINUE
      END
