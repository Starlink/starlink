      DOUBLE PRECISION FUNCTION PON_EVALSPLINE( X, NCAP7, COEFF, KNOT,
     :                                          IFAIL )
*+
*  Name:
*     PON_EVALSPLINE

*  Purpose:
*     Evaluate the value of a cubic spline at X.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     RESULT = PON_EVALSPLINE( X, NCAP7, COEFF, KNOT, IFAIL )

*  Description:
*     Use the NAG routine E02BBF to evaluate the value of the cubic
*     spline at the given value of X.

*  Arguments:
*     [argument_spec]...
*     [status_argument_spec]

*  Returned Value:
*     PON_EVALSPLINE = DOUBLE PRECISION
*        The value of the given spline function at X.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1992 (JBVAD::PAH):
*        Original version.
*     24-JUN-1992 (PCTR):
*        Code tidy and prologue changes.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      DOUBLE PRECISION X

      INTEGER NCAP7

      DOUBLE PRECISION COEFF( NCAP7 )
      DOUBLE PRECISION KNOT( NCAP7 )

*  Arguments Given and Returned:
      INTEGER IFAIL

*  Local Variables:
      INTEGER STATUS             ! Local status

      DOUBLE PRECISION RESULT    ! Returned value from the NAG routine

*.

*  Want a quiet error behaviour from NAG, so set the given failure flag,
*  IFAIL, to unity.
      IFAIL = 1
      CALL E02BBF( NCAP7, KNOT, COEFF, X, RESULT, IFAIL )

      IF ( IFAIL .EQ. 0 ) THEN
         PON_EVALSPLINE = RESULT
      ELSE
         PON_EVALSPLINE = 0

         IF ( IFAIL .EQ. 2 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PON_EVALSPL_NPS',
     :                    'NPOLY must be greater than 7.', STATUS )
         END IF
      END IF

      END
* $Id$
