***********
*  Associated routine ccd1_tfun, all parameters passed through common.
*  This routine basically returns the value which when multiplied by the
*  integrated intensity of the object gives the intensity at specified
*  radius squared
***********
      REAL FUNCTION CCD1_TFUN( RADSQ )
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
      CCD1_TFUN = 0.0
      RAD = SQRT( RADSQ )
      IF ( RAD .LT. PARRAD ) THEN
         ARG = C1 * RADSQ
         CCD1_TFUN = COMIX * CN / ( 1.0 + PARMN2 * RADSQ )
         CCD1_TFUN = CCD1_TFUN + ( 1.0 - COMIX ) * CN * EXP( ARG )
      ELSE
         ARG = CHANGE + ( PARRAD - RAD ) * C2
         CCD1_TFUN = COMIX * CN / ( 1.0 + PARMN2 * RADSQ )
         CCD1_TFUN = CCD1_TFUN + ( 1.0 - COMIX ) * CN * EXP( ARG )
      END IF

      END
* $Id$
