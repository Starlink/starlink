
***********
*  Associated routine func, all parameters passed through common. This
*  routine basically returns the value which when multplied by the
*  integrated intensity of the object gives the intensity at specified
*  radius squared
***********
      REAL FUNCTION FUNC( RADSQ )
      IMPLICIT NONE              ! no implicit typing

*  Global variables
      REAL C1, C2, CN, PARSQ, CHANGE, PARRAD
      REAL PARMN1, PARMN2, PARMNN, COMIX
      COMMON /PM    / C1, C2, CN, PARSQ, CHANGE, PARRAD
      COMMON /PMN   / PARMN1, PARMN2, PARMNN, COMIX

*  Arguments given:
      REAL RADSQ

*  Local variables
      REAL RAD, ARG

*  FUNC calculate Gauss/exp profile or Gauss/Lor model
      FUNC = 0.0
      RAD = SQRT( RADSQ )
      IF ( RAD .LT. PARRAD ) THEN
         ARG = C1 * RADSQ
         FUNC = COMIX * CN / ( 1.0 + PARMN2 * RADSQ )
         FUNC = FUNC + ( 1.0 - COMIX ) * CN * EXP( ARG )
      ELSE
         ARG = CHANGE + ( PARRAD - RAD ) * C2
         FUNC = COMIX * CN / ( 1.0 + PARMN2 * RADSQ )
         FUNC = FUNC + ( 1.0 - COMIX ) * CN * EXP( ARG )
      END IF

      END
* $Id$
