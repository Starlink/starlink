*+  BR_DEXP - Simply finds EXP(x), but remembers X and result
      DOUBLE PRECISION FUNCTION BR_DEXP( X )
*    Description :
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     13 Jan 93 : Original
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Import :
*
      DOUBLE PRECISION X
*
*    Local variables :
*
      DOUBLE PRECISION LAST_X,LAST_EXP       !
*
*    Local data :
*
      DATA   LAST_X,LAST_EXP/0.0D0,1.0D0/
*
*    Preserve for next time :
*
      SAVE             LAST_X, LAST_EXP
*-

      IF ( X .NE. LAST_X ) THEN
        LAST_X = X
        LAST_EXP = DEXP(X)
      END IF

      BR_DEXP = LAST_EXP

      END
