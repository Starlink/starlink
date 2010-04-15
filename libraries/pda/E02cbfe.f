*     E02CBF Example Program Text.
*     Mark 14 Revised.  NAG Copyright 1989.
*     .. Parameters ..
      INTEGER          MMAX, KMAX, NWORK, LMAX, NA
      PARAMETER        (MMAX=100,KMAX=9,NWORK=KMAX+1,LMAX=9,NA=(KMAX+1)
     +                 *(LMAX+1))
      INTEGER          NIN, NOUT
      PARAMETER        (NIN=5,NOUT=6)
*     .. Local Scalars ..
      DOUBLE PRECISION X1, XM, XMAX, XMIN, Y, YMAX, YMIN
      INTEGER          I, IFAIL, J, K, L, M, N, NCOEF
*     .. Local Arrays ..
      DOUBLE PRECISION A(NA), FF(MMAX), WORK(NWORK), X(MMAX)
*     .. External Subroutines ..
      EXTERNAL         PDA_CHE2D
*     .. Intrinsic Functions ..
      INTRINSIC        DBLE
*     .. Executable Statements ..
      WRITE (NOUT,*) 'E02CBF Example Program Results'
*     Skip heading in data file
      OPEN( UNIT=NIN, FILE='E02cbfe.dat', STATUS='OLD' )
      READ (NIN,*)
   20 READ (NIN,*,END=100) N, K, L
      IF (K.LE.KMAX .AND. L.LE.LMAX) THEN
         NCOEF = (K+1)*(L+1)
         READ (NIN,*) (A(I),I=1,NCOEF)
         READ (NIN,*) YMIN, YMAX
         DO 80 I = 1, N
            READ (NIN,*) Y, M, XMIN, XMAX, X1, XM
            IF (M.LE.MMAX) THEN
               DO 40 J = 1, M
                  X(J) = X1 + ((XM-X1)*DBLE(J-1))/DBLE(M-1)
   40          CONTINUE
               IFAIL = 0
*
               CALL PDA_CHE2D( M, XMIN, XMAX, X, YMIN, YMAX, Y, K, L,
     :                         NA, A, NWORK, WORK, FF, IFAIL )
*
               WRITE (NOUT,*)
               IF ( IFAIL .EQ. 0 ) THEN
                  WRITE (NOUT,99999) 'Y = ', Y
                  WRITE (NOUT,*)
                  WRITE (NOUT,*) '  I     X(I)      Poly(X(I),Y)'
                  DO 60 J = 1, M
                     WRITE (NOUT,99998) J, X(J), FF(J)
   60             CONTINUE
               ELSE
                  WRITE (NOUT,*) 'Failed IFAIL = ', IFAIL
               END IF
            END IF

   80    CONTINUE
         GO TO 20
      END IF
  100 STOP
*
99999 FORMAT (1X,A,D13.4)
99998 FORMAT (1X,I3,1P,2D13.4)
      END
