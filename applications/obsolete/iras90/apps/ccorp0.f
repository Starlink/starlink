      SUBROUTINE CCORP0( NU, T, DERIV, B, DBBYDT )
*+
*  Name:
*     CCORP0

*  Purpose:
*     Returns a blackbody surface brightness and its first derivative
*     with temperature.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCORP0( NU, T, DERIV, B, DBBYDT )

*  Description:
*     The returned B value is a surface brightness value in ExaJanskys
*     per steradian (1E18 Jy/sr) emitted by a blackbody at temperature T
*     at frequency NU. If DERIV is supplied true, then the rate of
*     change of B with respect to temperature is also returned.

*  Arguments:
*     NU = DOUBLE PRECISION (Given)
*        The frequency at which the surface brightness is required. In
*        units of 1.0E12 Hz.
*     T = DOUBLE PRECISION (Given)
*        The temperature of the blackbody. In units of Kelvin.
*     DERIV = LOGICAL (Given)
*        TRUE if a derivative value is required.
*     B = DOUBLE PRECISION (Returned)
*        The surface brightness.
*     DBBYDT = DOUBLE PRECISION (Returned)
*        The first derivative of surface brightness with respect to
*        temperature. Only assigned a value if DERIV is true.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-MAY-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      DOUBLE PRECISION NU
      DOUBLE PRECISION T
      LOGICAL DERIV

*  Arguments Returned:
      DOUBLE PRECISION B
      DOUBLE PRECISION DBBYDT

*  Local Constants:
      DOUBLE PRECISION C1        ! Constant needed in Plank function
      PARAMETER ( C1 = 1.474508 )
      DOUBLE PRECISION C2        ! Constant needed in Plank function
      PARAMETER ( C2 = 47.979725 )

*  Local Variables:
      DOUBLE PRECISION ARG       ! The argument for the exponent in the
                                 ! Plank function.

*.

*  Calculate the argument for the exponent in the Plank function.
      ARG = C2*NU/T

*  If the argument is very large, use the Wien approximation.
      IF( ARG .GT. 20.0D0 ) THEN
         B = C1*(NU**3)*EXP( -ARG )

*  Otherwise, use the exact expression.
      ELSE IF( ARG .GT. 0.0D0 ) THEN
         B = C1*(NU**3)/(EXP( ARG ) - 1.0D0 )

*  Return zero for any bad values of ARG.
      ELSE
         B = 0.0D0

      END IF

*  If required, calculate the derivative.
      IF( DERIV ) THEN

*  If the argument is very large, use the Wien approximation.
         IF( ARG .GT. 20.0D0 ) THEN
            DBBYDT = C2*NU*B/(T**2)

*  Otherwise, use the exact expression.
         ELSE IF( ARG .GT. 0.0D0 ) THEN
            DBBYDT = ( C2*EXP( ARG )/C1 )*
     :               ( B/(NU*T) )**2

*  Return zero for any bad values of ARG.
         ELSE
            DBBYDT = 0.0D0

         END IF

      END IF

      END
