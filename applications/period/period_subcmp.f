
C===========================================================================

      SUBROUTINE PERIOD_SUBCMP(M, SPEC, WIND, L, CPOS)

C===========================================================================
C Subtracts the complex component CPOS at array location L, and
C its complex conjugate, CNEG, at -L from the spectrum SPEC(0:M).
C The spectral window WIND(0:2M) is matched to the component and
C subtracted from the entire spectral array.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER M,I,L
      DOUBLE COMPLEX SPEC(0:M), WIND(0:2*M), CPOS, CNEG, PERIOD_CVAL

C---------------------------------------------------------------------------
C Specify the -L component.
C---------------------------------------------------------------------------

      CNEG = DCONJG(CPOS)

C---------------------------------------------------------------------------
C Remove effects of both +L and -L components.
C---------------------------------------------------------------------------

      DO 100 I = 0, M
         SPEC(I) = SPEC(I) - CPOS*PERIOD_CVAL(WIND, I-L, 2*M)
     :             - CNEG*WIND(I+L)
 100  CONTINUE               !     (From L)            (From -L)

C---------------------------------------------------------------------------
C Return to the program.
C---------------------------------------------------------------------------

      RETURN
      END
