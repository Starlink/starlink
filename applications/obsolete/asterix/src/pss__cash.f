*+  PSS_CASH_EVAL - Evaluate Cash statistic at point for a given flux
      REAL FUNCTION PSS_CASH_EVAL( FLUX, BSCALE )
*
*    Description :
*
*     Evaluates Cash's C statistic at given flux and background level. The
*     spatial parameters are defined externally.
*
*    Method :
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
      INCLUDE 'DAT_PAR'
      INCLUDE 'PSS_PAR'
*
*    Global variables :
*
      INCLUDE 'PSS_CMN'
*
*    Import :
*
      REAL                     FLUX                    ! Flux value
      REAL                     BSCALE                  ! Background scaling
*
*    Local variables :
*
      REAL                     CSUM                    ! Sum of model
      REAL                     MVAL                    ! Model value at pixel

      INTEGER                  CP                      ! Loop over cache
*-

      CSUM = 0.0
      DO CP = DC_LO, DC_HI
        MVAL = FLUX*DC_PSF(CP) + BSCALE*DC_BGND(CP)
        IF ( MVAL .GT. 0.0 ) THEN
          CSUM = CSUM + MVAL - DC_IMD(CP)*LOG(MVAL)
        END IF
      END DO

*    Set return value
      PSS_CASH_EVAL = 2.0*CSUM

      END
*+  PSS_CASH_SIG - Converts delta-chi-squared to significance
      REAL FUNCTION PSS_CASH_SIG( DELCHI )
*
*    Description :
*
*     The Cash statistic is chi-squared distributed with 1 degree of freedom.
*     Given a delta chi-squared, this routine calculates a significance in
*     terms of a one-tailed normal distribution.
*
*    Method :
*
*     IF delta-chi-squared small (<100) THEN
*       Find probability from DELCHI
*       Convert to normal deviates
*     ELSE
*       Can't use above as intermediate probability too small. Instead
*       use result of series expansion
*
*           -sig^2/2 - ln sig = ln 2 - DELCHI/2 - ln DELCHI^0.5
*
*       Convergence criterion is determined by the PSS routine PSS_CONVERGED
*       which may impose absolute or fractional convergence criterion
*       depending on the accuracy required at various stages in the processing.
*
*    Authors :
*
*     David J. Allan (BHVAD::DJA)
*
*    History :
*
*     11 Mar 91 : Original (DJA)
*
*    Type definitions :
*
      IMPLICIT NONE
*
*    Global constants :
*
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
*
*    Import :
*
      REAL               DELCHI                ! Chi-squared diff
*
*    Function definitions :
*
      DOUBLE PRECISION         G01BCF, G01CEF
      LOGICAL                  PSS_CONVERGED
*
*    Local constants :
*
      INTEGER                  MAXITER                 ! Max iterations
         PARAMETER             ( MAXITER = 30 )
      REAL                     LOGTWO
         PARAMETER             ( LOGTWO = 0.69314718056 )
*
*    Local variables :
*
      DOUBLE PRECISION         PROB                    ! Probability

      REAL                     OLDSIG                  ! Previous iteration
      REAL                     SIG                     ! Working significance
      REAL                     CON                     ! Constant in iteration

      INTEGER                  IFAIL                   ! NAG status
      INTEGER                  ITER                    ! Iteration counter
*-

*    Trap silly case
      IF ( DELCHI .LT. 0.0 ) THEN
        SIG = 0.0

*    Ok to use NAG routine
      ELSE IF ( DELCHI .LT. 100.0 ) THEN
        IFAIL = 0
        PROB = G01BCF(DBLE(DELCHI),1,IFAIL)
        SIG = -REAL(G01CEF(PROB/2.0D0,IFAIL))

*    Use iteration scheme
      ELSE

*      A sensible starting value
        OLDSIG = SQRT(DELCHI)
        CON = LOGTWO - DELCHI/2.0 - LOG(SQRT(DELCHI))
        ITER = 0
 15     SIG = OLDSIG - (-OLDSIG*OLDSIG/2.0-LOG(OLDSIG)-CON) /
     :                                       (-OLDSIG-1.0/OLDSIG)
        ITER = ITER + 1

*      Trap too many iterations? Should never happen as function is
*      well-behaved for Newton-Raphson when DELCHI>100
        IF ( ITER .GT. MAXITER ) THEN
          SIG = 0.0
        ELSE IF ( .NOT. PSS_CONVERGED(SIG,OLDSIG) ) THEN
          OLDSIG = SIG
          GOTO 15
        END IF

      END IF

      PSS_CASH_SIG = SIG

      END
