C
      SUBROUTINE CONTFIT(NX,XVALS,ZVALS,CX,ICST,ICONO,KORD,A,CONVALS,
     :RMS,FAULT)
C
C     Fits a least squares polynomial of order KORD-1 to the
C     continuum points in ZVALS. Array ICONO indicates the
C     continuum points to be used ( ICST is the left edge of the
C     region to be fitted ). The fitted continuum is CONVALS and
C     the r.m.s. on the fit is RMS. NAG routines E02ADF and E02AEF
C     are used for the polynomial fitting.
C
C     Parameters - (">" input, "<" output )
C
C     (>) NX      (Integer) Number of elements in XVALS and ZVALS
C     (>) XVALS   (Real array) The abscissae of the observed points
C     (>) ZVALS   (Real array) The Y values of the observed data points.
C     (>) CX      (Integer) The total number of points to be continuum
C                 fitted
C     (>) ICST    (Integer) The start of the continuum fitted region
C     (>) ICONO   (Integer array) The array indicating points to be
C                 included in the continuum fit
C     (>) KORD    (Integer) Order of polynomial fit + 1
C     (>) A       (Double Precision array) 2d array of Chebyshev
C                 polynomial coefficients ( passed for dimensioning purposes )
C     (<) CONVALS (Real array) The fitted continuum points
C     (<) RMS     (Real array) R.m.s. on continuum fit
C     (<) FAULT   (Logical) .TRUE. if an error occurs in the NAG calls
C
C                                          JRW / AAO February 1987
C
C     Modified:
C
C     23 Aug 1995  HME / UoE, Starlink.  No longer use NAG.
C                  Only the rms for the chosen order is returned correctly.
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,CX,ICONO(NX),ICST,KORD
      REAL XVALS(NX),ZVALS(NX),CONVALS(NX),RMS(8)
      DOUBLE PRECISION A(KORD,KORD)
      LOGICAL FAULT
C
C     Local variables
C
      INTEGER I,J,NP,IFAIL,IFAIL2,KPLUS1,NDEG
      DOUBLE PRECISION X(4096),Y(4096),W(4096),R(4096),A3(3*4096+3*8),
     :P,EPS,DUMMY
C
C     Form the arrays of points which are to be polynomial fitted
C     based on non-zero values of ICONO. Set weights to 1.0
C
      J=1
      DO I=1,NX
        IF (ICONO(I).EQ.1) THEN
          X(J)=DBLE(XVALS(I))
          Y(J)=DBLE(ZVALS(I))
          W(J)=1.0
          J=J+1
        END IF
      END DO
      NP=J-1

      KPLUS1=KORD
C
C     Fit the continuum by a polynomial of order KORD-1.
C
      IFAIL2=0
      EPS=0D0
      CALL PDA_DPOLFT(NP,X,Y,W,KPLUS1-1,NDEG,EPS,R,IFAIL,A3,IFAIL2)
      IF (NDEG.NE.KPLUS1-1.OR.IFAIL.NE.1.OR.IFAIL2.NE.0) THEN
         CALL PAR_WRUSER('Error in PDA_DPOLFT',IFAIL)
         FAULT=.TRUE.
         GO TO 99
      END IF
C
C     We no longer have the rms for all orders, just for the highest.
C
      DO I=1,KPLUS1
        RMS(I)=0.
      END DO
      RMS(KPLUS1)=SNGL(EPS)
C
C     Evaluate the continuum points.
C
      IFAIL=0
      DO I=ICST,ICST+CX-1,1
        CALL PDA_DP1VLU(KPLUS1-1,0,DBLE(XVALS(I)),P,DUMMY,A3,IFAIL)
         IF (IFAIL.NE.0) THEN
            CALL PAR_WRUSER('Error in PDA_DP1VLU',IFAIL)
            FAULT=.TRUE.
            GO TO 99
         END IF
        CONVALS(I)=SNGL(P)
      END DO

99    END
