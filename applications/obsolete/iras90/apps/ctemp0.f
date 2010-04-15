      FUNCTION CTEMP0( NU, T )
*+
*  Name:
*     CTEMP0

*  Purpose:
*     Returns a blackbody surface brightness.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     = CTEMP0( NU, T )

*  Description:
*     The returned value is a surface brightness value in ExaJanskys
*     per steradian (1E18 Jy/sr) emitted by a blackbody at temperature T
*     at frequency NU.

*  Arguments:
*     NU = DOUBLE PRECISION (Given)
*        The frequency at which the surface brightness is required. In
*        units of 1.0E12 Hz.
*     T = DOUBLE PRECISION (Given)
*        The temperature of the blackbody. In units of Kelvin.
*     CTEMP0 = DOUBLE PRECISION (Returned)
*        The surface brightness.

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

*  Arguments Returned:
      DOUBLE PRECISION CTEMP0

*  Local Constants:
      DOUBLE PRECISION C1        ! Constant needed in Plank function
      PARAMETER ( C1 = 1.474508 )
      DOUBLE PRECISION C2        ! Constant needed in Plank function
      PARAMETER ( C2 = 47.979725 )

*  Local Variables:
      DOUBLE PRECISION ARG       ! The argument for the exponent in the
                                 ! Plank function.

*.

*  Return an error if temperature is zero or negative.
      IF( T .GT. 0.0D0 ) THEN

*  Calculate the argument for the exponent in the Plank function.
         ARG = C2*NU/T

*  If the argument is very large, use the Wien approximation.
         IF( ARG .GT. 20.0D0 ) THEN
            CTEMP0 = C1*(NU**3)*EXP( -ARG )

*  Otherwise, use the exact expression.
         ELSE IF( ARG .GT. 0.0D0 ) THEN
            CTEMP0 = C1*(NU**3)/(EXP( ARG ) - 1.0D0 )

*  Return zero for any bad values of ARG.
         ELSE
            CTEMP0 = 0.0D0

         END IF

      ELSE
         CTEMP0 = 0.0D0
      END IF

      END
