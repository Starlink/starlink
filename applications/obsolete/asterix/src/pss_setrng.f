*+  PSS_SETRNG - Define a range for processing
      SUBROUTINE PSS_SETRNG( AX, CEN, MINV, MAXV, HWID )
*
*    Description :
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     12 Nov 90 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      INTEGER      AX,CEN,MINV,MAXV,HWID
*-

      GR_RNG_CEN(AX) = CEN
      GR_RNG_LO(AX) = MAX( MINV, CEN - HWID )
      GR_RNG_HI(AX) = MIN( MAXV, CEN + HWID )
      GR_RNG_LLO(AX) = GR_RNG_LO(AX) - CEN
      GR_RNG_LHI(AX) = GR_RNG_HI(AX) - CEN

      END
