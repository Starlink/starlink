      SUBROUTINE CDIVD( SCALAR, EL, IN, OUT, BAD, STATUS )
*+
*  Name:
*     CDIVD

*  Purpose:
*     Divide a vector by a scalar value

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CDIVD( SCALAR, EL, IN, OUT, BAD, STATUS )

*  Description:
*     The values in the given input vector are divided by the given
*     scalar and written to the given output vector. Bad values are
*     propagated from input to output. An error is reported if the
*     scalar is zero.

*  Arguments:
*     SCALAR = DOUBLE PRECISION (Given)
*        The scalar.
*     EL = INTEGER (Given)
*        The number of elements in the vector.
*     IN( EL ) = DOUBLE PRECISION (Given)
*        The input vector.
*     OUT( EL ) = DOUBLE PRECISION (Given)
*        The output vector.
*     BAD = LOGICAL (Returned)
*        True if any bad values are included in the output vector. False
*        otherwise.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     15-SEP-1993 (DSB):
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

*  Arguments Given:
      DOUBLE PRECISION SCALAR
      INTEGER EL
      DOUBLE PRECISION IN( EL )

*  Arguments Returned:
      DOUBLE PRECISION OUT( EL )
      LOGICAL BAD

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      DOUBLE PRECISION DATVAL    ! Input data value
      INTEGER I                  ! Loop count

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error if the scalar is zero.
      IF( SCALAR .EQ. 0.0D0 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'CDIVD_ERR1', 'CDIVD: Attempt to divide a '//
     :                 'vector by zero', STATUS )

*  Otherwise...
      ELSE

*  Initialise the returned bad value flag.
         BAD = .FALSE.

*  Loop round each value.
         DO I = 1, EL

*  Store the input value.
            DATVAL = IN( I )

*  If the input value is not bad, store the result.
            IF( DATVAL .NE. VAL__BADD ) THEN
               OUT( I ) = DATVAL/SCALAR

*  If the input value is bad, store a bad output value and set the
*  returned bad value flag.
            ELSE
               OUT( I ) = VAL__BADD
               BAD = .TRUE.

            END IF

         END DO

      END IF

      END
