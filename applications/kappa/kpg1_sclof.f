      SUBROUTINE KPG1_SCLOF( IN, EL, FACTOR, OFFSET, OUT, BAD, STATUS )
*+
*  Name:
*     KPG1_SCLOF

*  Purpose:
*     Applies a simple scaling and base-line shift to the values
*     contained in the input vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SCLOF( IN, EL, FACTOR, OFFSET, OUT, NBAD, STATUS )

*  Description:
*     The input data values are multiplied by the given factor and
*     the given offset is then added on, to form the output data.

*  Arguments:
*     IN( EL ) = REAL (Given)
*        The input data vector.
*     EL = INTEGER (Given)
*        The number of elements in the input and output vectors.
*     FACTOR = DOUBLE PRECISION (Given)
*        The factor by which the input valus are scaled.
*     OFFSET = DOUBLE PRECISION (Given)
*        The offset by which the data values are shifted.
*     OUT( EL ) = REAL (Given)
*        The output data vector.
*     BAD = LOGICAL (Returned)
*        True if any bad pixels found.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-JUN-1990 (DSB):
*        Original version.
*     1990 Sep 27 (MJC):
*        Renamed from the more generic and common SCALE.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad data values.

*  Global Variables:
      INCLUDE 'NUM_CMN'          ! Numerical error flag

*  Arguments Given:
      INTEGER  EL   
      REAL   IN( EL )
      DOUBLE PRECISION FACTOR
      DOUBLE PRECISION OFFSET

*  Arguments Returned:
      REAL   OUT( EL )
      LOGICAL  BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER  ELEM              ! The element counter.

*.

*  Check inherited global status.

      IF ( STATUS .NE. SAI__OK ) RETURN

* Initialise BAD to indicate that no bad pixels have yet been found.

      BAD = .FALSE.

*  Loop round all the elements of the input vector.

      DO ELEM = 1, EL

*  If the data value is good, copy the scaled value to the output.
*  Otherwise, set the output value bad.

         IF ( IN( ELEM ) .NE. VAL__BADR ) THEN
            OUT( ELEM ) = FACTOR * IN( ELEM ) + OFFSET
         ELSE
            OUT( ELEM ) = VAL__BADR
            BAD = .TRUE.
         END IF      

      END DO

      END
