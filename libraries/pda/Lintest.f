      PROGRAM LINTEST
*+
*  Test routine for PDA_LSQR.
*
*-
      IMPLICIT NONE
      INTEGER          MMAX, NMAX, LIWORK, LRWORK
      PARAMETER        (MMAX=100,NMAX=99,LIWORK=1,LRWORK=1)
*     .. Scalars in Common ..
      INTEGER          NCOLS, NROWS
*     .. Local Scalars ..
      DOUBLE PRECISION ACOND, ANORM, ARNORM, ATOL, BTOL, C, CONLIM,
     :                 DAMP, H, RNORM, XNORM
      INTEGER          I, I1, IFAIL, INFORM, ITN, ITNLIM, K, M, MSGLVL,
     :                 N
*     .. Local Arrays ..
      DOUBLE PRECISION B(MMAX), RWORK(LRWORK), SE(NMAX), WORK(NMAX,2),
     :                 X(NMAX), W(12), V(12)
      INTEGER          IWORK(LIWORK)
*     .. External Subroutines ..
      EXTERNAL         APROD
*     .. Common blocks ..
      COMMON           /USER/NROWS, NCOLS
*     .. Executable Statements ..
      WRITE (*,*) 'PDA_LSQR Example Program Results'
      WRITE (*,*)
      NROWS = 4
      NCOLS = 4
      H = 0.1D0
      N = NCOLS*NROWS - 4
      M = N + 1
      IF (NROWS.LT.3 .OR. NCOLS.LT.3 .OR. M.GT.MMAX) THEN
         WRITE (*,99998) 'NROWS or NCOLS is out of range: NROWS = ',
     :     NROWS, '  NCOLS = ', NCOLS
      ELSE
*
*        Initialize RHS and other quantities required by F04QAF.
*        Convergence will be sooner if we do not regard A as exact,
*        so ATOL is not set to zero.
*
         DO 20 I = 1, N
            B(I) = 0.0D0
   20    CONTINUE
         C = -H**2
         I1 = NROWS
         DO 60 K = 3, NCOLS
            DO 40 I = I1, I1 + NROWS - 3
               B(I) = C
   40       CONTINUE
            I1 = I1 + NROWS
   60    CONTINUE
         B(M) = 1.0D0/H
         DAMP = 0.0D0
         ATOL = 1.0D-5
         BTOL = 1.0D-4
         CONLIM = 1.0D0/ATOL
         ITNLIM = 100
         CALL PDA_LSQR(M, N, APROD, DAMP, LIWORK, LRWORK, IWORK, RWORK,
     :                 B, V, W, X, SE, ATOL, BTOL, CONLIM, ITNLIM,
     :                 IFAIL, ITN, ANORM, ACOND, RNORM, ARNORM,
     :                 XNORM )
*
         WRITE (*,*)
         WRITE (*,*) 'Solution returned by PDA_LSQR'
         WRITE (*,99997) (X(I),I=1,N)
         WRITE (*,*)
         WRITE (*,99996) 'Norm of the residual = ', RNORM
      END IF
      STOP
*
99999 FORMAT (1X,A,I3)
99998 FORMAT (1X,A,I6,A,I6)
99997 FORMAT (1X,5F9.3)
99996 FORMAT (1X,A,1P,D12.2)
      END
*
      SUBROUTINE APROD(MODE,M,N,X,Y,RWORK,LRWORK,IWORK,LIWORK)
*     APROD returns
*        Y = Y + A*X          when MODE = 1
*        X = X + ( A**T )*Y   when MODE = 2
*     for a given X and Y.
*     .. Scalar Arguments ..
      INTEGER          LIWORK, LRWORK, M, MODE, N
*     .. Array Arguments ..
      DOUBLE PRECISION RWORK(LRWORK), X(N), Y(M)
      INTEGER          IWORK(LIWORK)
*     .. Scalars in Common ..
      INTEGER          NCOLS, NROWS
*     .. Local Scalars ..
      INTEGER          J, J1, J2
*     .. External Subroutines ..
      EXTERNAL         ATIMES
*     .. Common blocks ..
      COMMON           /USER/NROWS, NCOLS
*     .. Executable Statements ..
      IF (MODE.NE.2) THEN
         CALL ATIMES(NROWS,NCOLS,N,X,Y)
         DO 20 J = 1, NROWS - 2
            Y(M) = Y(M) + X(J)
   20    CONTINUE
         DO 40 J = 1, NCOLS - 2
            Y(M) = Y(M) + X(J*NROWS-1) + X(J*NROWS+NROWS-2)
   40    CONTINUE
         DO 60 J = M - NROWS + 2, N
            Y(M) = Y(M) + X(J)
   60    CONTINUE
      ELSE
         CALL ATIMES(NROWS,NCOLS,N,Y,X)
         DO 80 J = 1, NROWS - 2
            X(J) = X(J) + Y(M)
   80    CONTINUE
         DO 100 J = 1, NCOLS - 2
            J1 = J*NROWS - 1
            J2 = J1 + NROWS - 1
            X(J1) = X(J1) + Y(M)
            X(J2) = X(J2) + Y(M)
  100    CONTINUE
         DO 120 J = M - NROWS + 2, N
            X(J) = X(J) + Y(M)
  120    CONTINUE
      END IF
      RETURN
      END
*
      SUBROUTINE ATIMES(NROWS,NCOLS,N,X,Y)
*     ATIMES is called by routine APROD and returns
*        Y = Y + ANN*X,
*     where ANN is the N by N symmetric part of the matrix A.
*     .. Scalar Arguments ..
      INTEGER           N, NCOLS, NROWS
*     .. Array Arguments ..
      DOUBLE PRECISION  X(N), Y(N)
*     .. Local Scalars ..
      INTEGER           I, I1, I2, I3, IL, J
*     .. Executable Statements ..
      DO 20 J = 1, NROWS - 2
         Y(J) = Y(J) + X(J) - X(J+NROWS-1)
   20 CONTINUE
      DO 60 J = 1, NCOLS - 2
         I = J*NROWS - 1
         Y(I) = Y(I) + X(I) - X(I+1)
         I1 = I + 1
         IL = I1 + NROWS - 3
         DO 40 I = I1, IL
            I2 = I - NROWS
            IF (J.EQ.1) I2 = I2 + 1
            I3 = I + NROWS
            IF (J.EQ.NCOLS-2) I3 = I3 - 1
            Y(I) = Y(I) - X(I2) - X(I-1) + 4.0D0*X(I) - X(I+1) - X(I3)
   40    CONTINUE
         I = IL + 1
         Y(I) = Y(I) - X(I-1) + X(I)
   60 CONTINUE
      DO 80 J = N - NROWS + 3, N
         Y(J) = Y(J) - X(J-NROWS+1) + X(J)
   80 CONTINUE
      RETURN
      END
