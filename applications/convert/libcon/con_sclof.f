      SUBROUTINE CON_SCLOF( EL, IN, FACTOR, OFFSET, OUT, STATUS )
*+
*  Name:
*     CON_SCLOF

*  Purpose:
*     Applies a simple scaling and base-line shift to the values
*     contained in the input vector.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CON_SCLOF( EL, IN, FACTOR, OFFSET, OUT, STATUS )

*  Description:
*     The input data values are multiplied by the given factor and
*     the given offset is then added on, to form the output data.

*  Arguments:
*     EL = INTEGER (Given)
*        The number of elements in the input and output vectors.
*     IN( EL ) = REAL (Given)
*        The input data vector.
*     FACTOR = REAL (Given)
*        The factor by which the input valus are scaled.
*     OFFSET = REAL (Given)
*        The offset by which the data values are shifted.
*     OUT( EL ) = REAL (Given)
*        The output data vector.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Implementation Deficiencies:
*     There is no protection against overflow.

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1993 Sept 2 (MJC):
*        Original version based upon KPG1_SCLOF.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! Bad data values.

*  Arguments Given:
      INTEGER  EL   
      REAL   IN( EL )
      REAL FACTOR
      REAL OFFSET

*  Arguments Returned:
      REAL   OUT( EL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local variables:
      INTEGER  ELEM              ! The element counter.

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Loop round all the elements of the input vector.
      DO ELEM = 1, EL

*  If the data value is good, copy the scaled value to the output.
*  Otherwise, set the output value bad.
         IF ( IN( ELEM ) .NE. VAL__BADR ) THEN
            OUT( ELEM ) = FACTOR * IN( ELEM ) + OFFSET
         ELSE
            OUT( ELEM ) = VAL__BADR
         END IF      

      END DO

      END
