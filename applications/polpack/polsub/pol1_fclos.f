      SUBROUTINE POL1_FCLOS( NEL, VALS, WANT, USE, STATUS )
*+
*  Name:
*     POL1_FCLOS

*  Purpose:
*     Find the closest value in an array to a specified value

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POL1_FCLOS( NEL, VALS, WANT, USE, STATUS )

*  Description:
*     This routine searches the supplied array and finds the closes value
*     in the array to the argument WANT. Te closest value is returned in USE.

*  Arguments:
*     NEL = INTEGER (Given)
*        The length of the array.
*     VALS( NEL ) = REAL (Given)
*        The array.
*     WANT = REAL (Given)
*        The value to search for.
*     USE = REAL (Returned)
*        The closest value within VALS to the value supplied in WANT.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2001 Central Laboratory of the Research Councils

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-FEB-2001 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NEL
      REAL VALS( NEL )
      REAL WANT

*  Arguments Returned:
      REAL USE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      REAL ERR
      REAL MINERR
*.

*  Initialise.
      USE = WANT

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initially, the first point in the array is the closest to the wanted
*  value.
      USE = VALS( 1 )
      MINERR = ABS( USE - WANT )

*  Loop round the remaining values.
      DO I = 2, NEL

*  Find the error between this value and the wanted value.
         ERR = ABS( VALS( I ) - WANT )

*  If this point is closer to the wanted value than the previous points,
*  use this point as the closest point.
         IF( ERR .LT. MINERR ) THEN
            USE = VALS( I )
            MINERR = ERR

*  If the error for this point is equal to the previous best, choose the
*  lower of the two values (e.g. this will cause an erroneously supplied
*  integer index of (say) 1 to be rounded to the pixel coordinate of 0.5
*  instead of 1.5)
         ELSE IF( ERR .EQ. MINERR ) THEN
            IF( VALS( I ) .LT. USE ) THEN
               USE = VALS( I )
               MINERR = ERR
            END IF
         END IF

      END DO

      END
