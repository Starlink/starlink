      SUBROUTINE PDGFIT (NPTS, X, Y, SD, ORDER, CHISQ, ES, C)
C
C  Program fits a power series polynomial of order ORDER to a set of
C NPTS data points by the method of "Least Squares".
C  The fit is weighted by the reciporical of the square of the standard
C deviation on each ordinate (Y) value.
C  The abscissal (X) errors are assumed to be zero.
C
C     The Polynomial is of the form
C      y(x) = c1 + c2*x + c3*x^2 + c4*x^3 ... + c(order+1)*x^order
C
C  Additional program segments required:  SUBROUTINE SMATIN
C
      IMPLICIT NONE
C  Imports:-
      INTEGER NPTS                        ! Number of datapairs
      DOUBLE PRECISION X(*)               ! Array of data abscissae
      DOUBLE PRECISION SD(*)              ! Array of errors on ordinates
      INTEGER ORDER                       ! Order of polynomial to be fitted
C  Import/export:-
      DOUBLE PRECISION Y(*)               ! Array of data ordinates =>
C                             contains calculated best-fit ordinates on RETURN
C  Exports:-
      DOUBLE PRECISION CHISQ              ! Chi-squared
      DOUBLE PRECISION ES                 ! Scatter of data from best-fit curve
      DOUBLE PRECISION C(ORDER+1)         ! Array of best-fit curve coefficients
C  Local:-
      DOUBLE PRECISION CALCY              ! Temporarily holds best-fit ordinates
      DOUBLE PRECISION A(20,20)           ! Matrix holding power-x sums
      DOUBLE PRECISION B(20)              ! y-times-power-x summation matrix
      DOUBLE PRECISION DELTA              ! Determinants of A
      DOUBLE PRECISION DIFF               ! Holds difference between measured
     +                         !     and calculated ordinates
      INTEGER I, J, K                     ! Do-loop increment variables
C
C  Check that no sds are zero, else fit is indeterminate.
C
      DO I = 1,NPTS
        IF (DABS(SD(I)) .LE. 1.0D-30) THEN
          WRITE(*,*)'    PDGFIT: zero error detected; cannot fit '
          RETURN
        ENDIF
      ENDDO
C
C  Initialise matrices and summed variables to zero.
C
      CHISQ = 0.0D+00
      ES = 0.0D+00
      DO 20, I=1,20
        B(I) = 0.0D+00
        DO 10, J=1,20
          A(I,J) = 0.0D+00
   10   CONTINUE
   20 CONTINUE
      DO 100, I=1,(ORDER+1)
        C(I) = 0.0D+00
  100 CONTINUE
C
C  Set matrix diagonals beyond range of sums equal to unity for inversion.
C
      DO 200, I=(ORDER+2), 20
        DO 250, J=(ORDER+2), 20
          IF (I.EQ.J) THEN
            A(I,J) = 1.0D+00
          ENDIF
  250   CONTINUE
  200 CONTINUE
C
C  Section to calculate coefficients c(1) to c(order+1)
C
      DO 450, I=1,NPTS
        DO 400, K=1,(ORDER+1)
          DO 300, J=K,(ORDER+1)
            IF ((K+J-2).NE.0) THEN
              A(K,J) = A(K,J) + (X(I)**(K+J-2))/(SD(I)**2)
              ELSE
              A(K,J) = A(K,J) + 1.0D+00/(SD(I)**2)
            ENDIF
            A(J,K) = A(K,J)
  300     CONTINUE
          IF ((K-1).NE.0) THEN
            B(K) = B(K) + (Y(I)*X(I)**(K-1))/(SD(I)**2)
            ELSE
            B(K) = B(K) + Y(I)/(SD(I)**2)
          ENDIF
  400   CONTINUE
  450 CONTINUE
      CALL SMATIN (A,20,DELTA)
      DO 600, J=1,(ORDER+1)
        DO 500, I=1,(ORDER+1)
          C(J) = C(J) + A(J,I) * B(I)
  500   CONTINUE
  600 CONTINUE
C
C  Evaluate Chi-squared and an estimate for the scatter of the data.
C  Copy best-fit ordinates into array Y.
C
      DO 750, I=1,NPTS
        CALCY = C(1)
        DO 700, J=1,ORDER
          CALCY = CALCY + C(J+1)*X(I)**J
  700   CONTINUE
        DIFF = Y(I) - CALCY
        ES = ES + DIFF**2
        CHISQ = CHISQ + DIFF**2/SD(I)**2
        Y(I) = CALCY
  750 CONTINUE
      IF ( (NPTS-ORDER) .GT. 1 ) THEN
        ES = SQRT( ES/REAL(NPTS-ORDER-1) )
      ELSE
      ENDIF
      RETURN
      END
