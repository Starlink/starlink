*  History:
*     19 Nov 1993 (hme):
*        TABs removed.
*     15 Jan 1994 (rp):
*        Array lengths now depend on SPECX_PARS
C---------------------------------------------------------------------------

      SUBROUTINE GAUSSF (VAR, RESID, GRAD, SUMSQ, IFL)

C   This subroutine calculates values of the function and its
C   derivatives w.r.t. all parameters ( if IFL is set to 1 )
C   for the least-squares gaussian fitting routine LINFIT.
C
C     FORMAL PARAMETERS:
C        VAR    - vector of parameters to be determined
C                 ( on input contains initial guess )
C        RESID  - residuals at each point on X-axis
C        GRAD   - workspace
C        SUMSQ  - sum of squared residuals
C        IFL    - =1; calculate function and derivatives
C                 =2; calculate function values only.
C

      IMPLICIT  NONE

C     Formal parameters:

      REAL      VAR(*),RESID(*),GRAD(*)
      REAL*8    SUMSQ
      INTEGER   IFL

C     Global variables:

      INCLUDE 'SPECX_PARS'

C     Communication with main calling routine (SINFIT)

      REAL     X(LSPMAX)
      REAL     Y(LSPMAX)
      INTEGER  NDAT
      INTEGER  NPARAM
      COMMON /GFUNC/ X, Y, NDAT, NPARAM

C     Local variables:

      INTEGER  I, J
      INTEGER  L
      INTEGER  II
      INTEGER  KK
      INTEGER  K(30)
      INTEGER  NG

      REAL     AMP, DERIVA
      REAL     WID, DERIVW
      REAL     POS, DERIVP
      REAL     AEXP
      REAL     QUOT
      REAL     RESIDX
      REAL     XLOG
      REAL     YHAT

C  Ok, go...

C     initialize flags etc.

      SUMSQ = 0.D0
      XLOG  = 2.*SQRT(ALOG(2.))
      KK = 1
      NG = NPARAM/3
      DO I = 1, NPARAM
        K(I) = KK
        KK   = KK + NDAT
      END DO

C   calculate function at each point in x

      DO I = 1, NDAT
        YHAT = 0.0
        DO J = 1, NG
          AMP  = VAR(3*J-2)
          WID  = VAR(3*J-1)/XLOG
          POS  = VAR(3*J)
          AEXP = EXP(-((X(I)-POS)/WID)**2)
          YHAT = YHAT+AMP*AEXP

C   If IFL.NE.2 calculate derivatives also

          IF(IFL.NE.2)   THEN
            QUOT   = (X(I)-POS)/WID
            DERIVA = AEXP
            DERIVP = AMP*AEXP*2.*QUOT/WID
            DERIVW = DERIVP*QUOT/XLOG
            II     = 3*J-2
            GRAD(K(II))   = -DERIVA
            GRAD(K(II+1)) = -DERIVW
            GRAD(K(II+2)) = -DERIVP

          END IF
        END DO

        RESIDX = Y(I) - YHAT
        SUMSQ  = SUMSQ + RESIDX*RESIDX
        IF (IFL.NE.2)   THEN
          RESID(I) = RESIDX
          DO L = 1,NPARAM
            K(L) = K(L) + 1
          END DO
        END IF
      END DO

      RETURN
      END


