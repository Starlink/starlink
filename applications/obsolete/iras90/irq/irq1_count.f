      SUBROUTINE IRQ1_COUNT( SIZE, DATA, NGOOD, NBAD, STATUS )
*+
*  Name:
*     IRQ1_COUNT

*  Purpose:
*     Count the good and bad pixels in a vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRQ1_COUNT( SIZE, DATA, NGOOD, NBAD, STATUS )

*  Description:
*     The number of good and bad pixels in the input data are returned.
*  Arguments:
*     SIZE = INTEGER (Given)
*        Size of the data vector.
*     DATA( SIZE ) = REAL (Given)
*        Data vector.
*     NGOOD = INTEGER (Returned)
*        Number of pixels in DATA not equal to VAL__BADR.
*     NBAD = INTEGER (Returned)
*        Number of pixels in DATA equal to VAL__BADR.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-JUL-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Starlink data constants.

*  Arguments Given:
      INTEGER SIZE
      REAL DATA( SIZE )

*  Arguments Returned:
      INTEGER NGOOD
      INTEGER NBAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the counters.
      NGOOD = 0
      NBAD = 0

*  Loop round all the supplied data, counting good and bad pixels.
      DO I = 1, SIZE
         IF( DATA( I ) .EQ. VAL__BADR ) THEN
            NBAD = NBAD + 1
         ELSE
            NGOOD = NGOOD + 1
         END IF
      END DO

      END
