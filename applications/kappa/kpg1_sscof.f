      SUBROUTINE KPG1_SSCOF( EL, FACTOR, OFFSET, OUT, STATUS )
*+
*  Name:
*     KPG1_SSCOF

*  Purpose:
*     Applies a simple scaling and base-line shift to create the
*     output vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SSCOF( EL, FACTOR, OFFSET, OUT, STATUS )

*  Description:
*     Pixel indices are multiplied by the given factor and
*     the given offset is then added on, to form the output data.
*     The first pixel has value equal to the offset.

*  Arguments:
*     EL = INTEGER (Given)
*        The number elements in the returned array.
*     FACTOR = DOUBLE PRECISION (Given)
*        The factor by which the array indices are scaled.
*     OFFSET = DOUBLE PRECISION (Given)
*        The offset by which the array indices are shifted.
*     OUT( EL ) = REAL (Given)
*        The output data vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     There is no exception handler if the evaluated value exceeds the
*     machine floating-pint range.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1990 November 15 (MJC):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'NUM_CMN'          ! Numerical error flag

*  Arguments Given:
      INTEGER  EL   
      DOUBLE PRECISION FACTOR
      DOUBLE PRECISION OFFSET

*  Arguments Returned:
      REAL   OUT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER  ELEM              ! The element counter.

*.

*    Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Loop round all the elements of the vector.

      DO ELEM = 1, EL

*       Evaluate the scale and offset for each array element.

         OUT( ELEM ) = REAL( FACTOR * DBLE( ELEM - 1 ) + OFFSET )

      END DO

      END
