
C===========================================================================

      FUNCTION PERIOD_ALPHA(L, M, SPEC, WIND)

C===========================================================================
C Returns an estimate for the component A, responsible for SPEC(L) through
C the relation:
C                  SPEC(L) = A + (A*)WIND(2L)
C SPEC is defined (0:M), and WIND is defined (0:2M).
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER L,M
      DOUBLE COMPLEX PERIOD_ALPHA
      DOUBLE COMPLEX SPEC(0:M), WIND(0:2*M), WIN2L, PERIOD_CVAL

      DOUBLE PRECISION ERROR,WNORM

C---------------------------------------------------------------------------
C Find the (L,-L) components which produce SPEC(L) through WIND.
C---------------------------------------------------------------------------

*   Allowed error in WNORM.
      ERROR=0.0001D0

      WIN2L = WIND(2*L)                                   ! (L,-L) interference.
      WNORM = 1.0D0 - CDABS(WIN2L)**2
      IF ( WNORM.LT.ERROR ) THEN
          PERIOD_ALPHA = 0.5D0*SPEC(L)    ! Avoid singularities.
      ELSE
          PERIOD_ALPHA = (SPEC(L)-WIN2L*
     :                   PERIOD_CVAL(SPEC,-L,M))/WNORM
      END IF

C---------------------------------------------------------------------------
C Return with the estimate of A.
C---------------------------------------------------------------------------

      RETURN
      END
