*+  USI_CLOSE - global tidy up for USI
      SUBROUTINE USI_CLOSE()
*    Description :
*    History :
*    Type definitions :
      IMPLICIT NONE
      INCLUDE  'SAE_PAR'
      INCLUDE  'USI_CMN'
*-

      CALL USI0_EXIT()

      USI_NPS = 0
      USI_SYINIT = .FALSE.

      END
