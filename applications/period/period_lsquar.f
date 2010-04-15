
      SUBROUTINE PERIOD_LSQUAR(DATA, NUMBER, N, A, CHISQ, XM, NORM)

C=============================================================================
C PERIOD_LSQUAR provides a polynomial fit of specified degree to a set of
C data points. the fit is done in double precision. Uses the double precision
C matrix inversion subroutine 'PERIOD_MLSRAR'.
C
C DATA    =  ARRAY CONTAINING THE DATA. THIS SHOULD BE ARRANGED:
C            DATA(1,K): X-VALUE OF DATA POINT.
C            DATA(2,K): Y-VALUE OF DATA POINT.
C            DATA(3,K): SIGMA IN Y-VALUE OF DATA POINT.
C NUMBER  =  NUMBER OF DATA POINTS TO BE FITTED.
C N       =  NUMBER OF COEFFICIENTS IN POLYNOMIAL TO BE FITTED. THE
C            ORDER OF THE POLYNOMIAL IS (N-1).
C A       =  ARRAY HOLDING COEFFICIENTS OF FIT. THIS IS ARRANGED:
C            POLY = A(1) + A(2)*X + ... A(N)*X**(N-1).
C CHISQ   =  CONTAINS THE CHI-SQUARE VALUE OF THE FIT ON OUTPUT. IF
C            CHISQ = -1. ON OUTPUT THEN THE MATRIX WAS SINGULAR, IF
C            CHISQ = -2. ON OUTPUT THEN OVERFLOW OR DIVIDE CHECK OCCURED.
C            CHISQ = -3. ON OUTPUT THEN INVALID PARAMETERS INPUT.
C            IF YOU SET CHISQ = 0. ON INPUT ERROR MESSAGES WILL BE PRINTED.
C XM      =  WORKING STORAGE ARRAY. DIMENSION IN THE MAIN
C            PROGRAM AS XM(NMAX,2*NMAX+3) WHERE 'NMAX' IS THE MAXIMUM
C            VALUE OF 'N' YOU WISH TO CALL.
C NORM    =  SCALING PARAMETER. SINCE LARGE POWERS OF THE INPUT DATA ARE
C            TO BE TAKEN OVERFLOWS OR UNDERFLOWS CAN EASILY OCCUR. IF
C            YOU SET NORM = 1 ON INPUT DATA ARE SCALED TO REDUCE LIKELIHOOD
C            OF THIS EVENT. NORM = 0 INSTRUCTS FOR NO SCALING. INPUT DATA
C            ARE NOT DESTROYED BY THE SCALING, AND COEFFICIENTS ARE
C            AUTOMATICALLY SCALED BACK BEFORE OUTPUT.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 1-July-1992.
C
C GJP June 1995
C
C Removed unused variable PRINTR
C
C GJP March 1997
C
C Modifed test for CHISQ=ZERO. Changed REAL*8 to DOUBLE PRECISION.
C ITEST passed to another routine without having a value or type.
C
C Cosmetic Double Precision mods. (KPD), August 2001
C===============================================================================

      IMPLICIT NONE

      INCLUDE "mnmxvl.h"

      INTEGER NMAX,ITER,ITEST,N21,N22,N23,I,J,K,L,M2
      INTEGER N,NUMBER,NORM
      LOGICAL RITE
      DOUBLE PRECISION XM(N, 2*N+3), XX, RR, EPS, AVE, SIGMA
      DOUBLE PRECISION DATA(3, NUMBER), A(N), CHISQ, S, R
      DOUBLE PRECISION X1, X2, X3, X1MULT


C-------------------------------------------------------------------------------
C EPS    =  MAXIMUM ALLOWED ERROR IN ITERATIONS ON THE POLYNOMIAL
C           COEFFICIENTS. CONVERGENCE TERMINATES WHEN ERROR.LT.EPS
C NMAX   =  MAXIMUM ALLOWED NUMBER OF COEFFICIENTS (DUE TO DIMENSION
C           STATEMENTS).
C-------------------------------------------------------------------------------

      DATA EPS, NMAX/1.D-6, 50/

      RITE = .FALSE.
      ITEST=0

C-------------------------------------------------------------------------------
C Test input data.
C-------------------------------------------------------------------------------

      IF ( (NUMBER.LE.0.OR.N.GT.NUMBER) .OR. N.LE.0 ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: PERIOD_LSQUAR failed.'
         WRITE (*, *) '** ERROR: Number of points = ', NUMBER
         WRITE (*, *) '** ERROR: Number of coefficients = ', N
         WRITE (*, *) '** ERROR: Invalid values (must be positive and'
         WRITE (*, *) '** ERROR: points .ge. coefficients).'
         WRITE (*, *) ' '
         CHISQ = -3.0D0
         RETURN
      ELSE IF ( N.GT.NMAX ) THEN
         CALL PERIOD_WRITEBELL()
         WRITE (*, *) '** ERROR: PERIOD_LSQUAR failed.'
         WRITE (*, *) '** ERROR: Number of coefficients = ', N
         WRITE (*, *) '** ERROR: Maximum number of coefficients' //
     :                ' permitted = ', NMAX
         WRITE (*, *) ' '
         CHISQ = -3.0D0
         RETURN
      END IF

C-------------------------------------------------------------------------------
C If the input value of CHISQ is 0. allow printing of error messages.
C-------------------------------------------------------------------------------

      IF ( DABS(CHISQ).LT.DPMN30 ) RITE = .TRUE.
      ITER = 5
      IF ( RITE ) ITER = -ITER
      N21 = 2*N + 1
      N22 = N21 + 1
      N23 = N21 + 2

C-------------------------------------------------------------------------------
C Rescale the input data (for NORM .NE. 0).
C-------------------------------------------------------------------------------

      IF ( NORM.NE.0 .AND. N.NE.1 ) THEN
         AVE = 0.0D0
         SIGMA = 0.0D0
         DO 50 I = 1, NUMBER
            X1 = DATA(1, I)
            AVE = AVE + X1
            SIGMA = SIGMA + X1*X1
 50      CONTINUE
         AVE = AVE/DFLOAT(NUMBER)
         SIGMA = DSQRT(SIGMA/DFLOAT(NUMBER)-AVE*AVE)
      END IF

C-------------------------------------------------------------------------------
C Zero the working array.
C-------------------------------------------------------------------------------

      DO 200 I = 1, N
         DO 100 J = N21, N23
            XM(I, J) = 0.0D0
 100     CONTINUE
 200  CONTINUE
      X2 = 0.0D0
      X3 = 0.0D0

C-------------------------------------------------------------------------------
C Compute the moments of the data.
C Change by TRM @RGO 7-June-1988. Ignore point if sigma error estimate less
C than or equal to zero.
C-------------------------------------------------------------------------------

      M2 = 2*N
      DO 300 I = 1, NUMBER
         IF ( DATA(3,I).GT.0.0D0 ) THEN
            RR = (1.0D0/DATA(3,I))**2
            X2 = X2 + RR
            XX = DATA(2, I)*RR
            X3 = X3 + XX
            IF ( N.NE.1 ) THEN
               X1 = DATA(1, I)
               IF ( NORM.EQ.0 ) THEN
                  X1MULT = X1
               ELSE
                  X1MULT = (X1-AVE)/SIGMA
               ENDIF
               DO 210 J = 3, M2
                  RR = RR*X1MULT
                  IF ( J.GT.N ) THEN
                     XM(J-N, N22) = XM(J-N, N22) + RR
                  ELSE
                     XM(J, N21) = XM(J, N21) + RR
                  END IF
 210           CONTINUE
               DO 220 J = 2, N
                  XX = XX*X1MULT
                  XM(J, N23) = XM(J, N23) + XX
 220           CONTINUE
            END IF
         END IF
 300  CONTINUE
      XM(2, N21) = X2
      XM(1, N23) = X3

C-------------------------------------------------------------------------------
C Compute matrix for inversion.
C-------------------------------------------------------------------------------

      DO 400 I = 1, N
         DO 350 J = 1, N
            K = I + J
            IF ( K.GT.N ) THEN
               XM(I, J) = XM(K-N, N22)
            ELSE
               XM(I, J) = XM(K, N21)
            END IF
 350     CONTINUE
 400  CONTINUE

C-------------------------------------------------------------------------------
C Call double precision matrix inversion routine.
C-------------------------------------------------------------------------------

      IF ( N.NE.1 ) THEN
         CALL PERIOD_MLSRAR(N,XM,XM(1,N23),ITER,EPS,A,ITEST,0,
     :                      XM(1,N+1))
         IF ( ITEST.GE.5 ) THEN
            CHISQ = -2.0D0
            RETURN
         END IF
      ELSE
         XM(1, 1) = 1.0D0/XM(1, 1)
         A(1) = XM(1, 1)*XM(1, N23)
      END IF

C-------------------------------------------------------------------------------
C Compute chi-square for resulting fit.
C-------------------------------------------------------------------------------

      CHISQ = 0.0D0
      DO 500 I = 1, NUMBER
         IF ( DATA(3,I).GT.0.0D0 ) THEN
            S = A(1)
            IF ( N.NE.1 ) THEN
               R = 1.0D0
               X1 = DATA(1, I)
               IF ( NORM.EQ.0 ) THEN
                  X1MULT = X1
               ELSE
                  X1MULT = (X1-AVE)/SIGMA
               END IF
               DO 410 J = 2, N
                  R = R*X1MULT
                  S = S + A(J)*R
 410           CONTINUE
            END IF
            CHISQ = CHISQ + ((S-DATA(2,I))/DATA(3,I))**2
         END IF
 500  CONTINUE

C-------------------------------------------------------------------------------
C Error messages after inversion of the matrix XM (H in the write-up).
C-------------------------------------------------------------------------------

      IF ( NORM.EQ.0 .OR. N.EQ.1 ) RETURN

C-------------------------------------------------------------------------------
C Rescale coefficients if data scaling was requested.
C-------------------------------------------------------------------------------

      SIGMA = 1.0D0/SIGMA
      AVE = -AVE*SIGMA
      L = N - 1
      DO 600 I = 1, L
         XM(I, 1) = AVE**I
         XM(I, 2) = 0.0D0
 600  CONTINUE
      XM(1, 2) = 1.0D0
      XM(N, 2) = 0.0D0
      DO 800 I = 1, L
         K = N - I + 1
         DO 650 J = 2, K
            XM(J, 2) = XM(J, 2) + XM(J-1, 2)
 650     CONTINUE
         K = I + 1
         DO 700 J = K, N
            A(I) = A(I) + A(J)*XM(J-I+1, 2)*XM(J-I, 1)
 700     CONTINUE
         A(I) = A(I)*SIGMA**(I-1)
 800  CONTINUE
      A(N) = A(N)*SIGMA**(N-1)

      RETURN
      END
