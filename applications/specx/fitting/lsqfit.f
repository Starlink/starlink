C History:
C    21-SEP-2000 (AJC):
C       Unused ISD
C-------------------------------------------------------------------------

      SUBROUTINE LSQFIT (NQ, XSCALE, BUF, IFAIL)

C   This subroutine performs a least squares fit of an N'th order
C   polynomial to the data in the x-register of 'STACK'
C
C   IFAIL  - Set to 0 on output if B-L removal successful

      IMPLICIT  NONE

C     Formal parameters

      INTEGER   NQ
      REAL      XSCALE(*)
      REAL      BUF(*)
      INTEGER   IFAIL

C     Common blocks

      INCLUDE 'FLAGCOMM'
      INCLUDE 'SPECX_PARS'
      INCLUDE 'STACKCOMM'

      REAL      XSC
      REAL      Y
      COMMON /GFUNC/    XSC(LSPMAX),Y(LSPMAX)
      COMMON /TCHEBBFT/ NPOLD,NT

C     Local variables

      INTEGER   I, J, K
      INTEGER   JDEF
      INTEGER   KOFF
      INTEGER   N
      INTEGER   NCH
      INTEGER   NPOLD
      INTEGER   NST
      INTEGER   NT

      REAL      A(11)
      REAL      SD(11,11)
      REAL      T(23)
      REAL      S(23)

      REAL      PI
      DATA      PI    /3.14157/

C     Functions

      INTEGER   NTOT

C  Ok, go...

      CALL PUSH

C     First determine pairs of points between which function is to be fitted

      CALL SETXNEW (XSCALE, IFAIL)
      IF (IFAIL.NE.0) THEN
        CALL POP
        RETURN
      END IF

      NST  = NTOT(NQ-1)+1
      NCH  = NPTS(NQ)
      CALL GETPTS (IBLP, 1, 8, NPOLD, NPR, XSCALE(NST),
     &             DATA(NST), NCH, IFAIL)
      IF (IFAIL.NE.0) THEN
        CALL POP
        RETURN
      END IF

C   Determine order of polynomial to be fitted

      CALL GEN_GETI4('Order of polynomial to be fitted?',NT,
     &               'I2',NT,JDEF)
      IF(NT.GT.10)   THEN
        PRINT *,'*** Maximum order is 10 at the moment! ***'
        IFAIL=16
        CALL POP
        RETURN
      END IF

C   Copy appropriate points to arrays for GAUSSF

      N = 0
      DO I = 1, NPR
        DO J = IBLP(2*I-1), IBLP(2*I)
          IF (DATA(NST+J-1).NE.BADPIX_VAL) THEN
            N      = N + 1
            Y(N)   = DATA(NST+J-1)
            XSC(N) = XSCALE(NST+J-1)
          END IF
        END DO
      END DO

C   Zero baselines of ALL quadrants

      DO I = 1, NTOT(NQ)
        DATA(I) = 0.0
      END DO

C   Initialize coefficients

      DO K = 1, 11
        A(K) = 0.0
      END DO

C  Calculate new sums for Least-squares fit

      DO J = 1, 2*NT+1
        S(J) = 0
        T(J) = 0
        DO I = 1,N
          S(J) = S(J) +  XSC(I)**(J-1)
          T(J) = T(J) + (XSC(I)**(J-1))*Y(I)
        END DO
      END DO

      KOFF = 0
      DO J = 1, NT+1
        DO I = 1, NT+1
          SD(J,I) = S(I+KOFF)
        END DO
        KOFF = KOFF+1
      END DO

C  Solve S * T = A for T

      CALL SOLVE4 (SD, T, A, NT+1, 11)

C   Calculate baseline

      N = NCH
      DO I = NST, NST+N-1
        DATA(I) = 0.0
        DO K = 1, NT+1
          IF (XSCALE(I).NE.0.0) THEN
            DATA(I) = DATA(I) + A(K)*XSCALE(I)**(K-1)
          END IF
        END DO
      END DO

      RETURN
      END


