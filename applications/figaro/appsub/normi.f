C
      SUBROUTINE NORMI(ORD,RMS,NFAC,RMSN)
C
C     N O R M I
C
C     Normalises the array RMS so that numbers in range 0. - 10.
C     so can be printed in F format
C
C     Parameters -  (">" input, "<" output )
C
C     (>) ORD     (Integer) Dimension of arrays RMS and RMSN
C     (>) RMS     (Real array) R.m.s. on continuum fit as
C                 function of order
C     (<) NFAC    (Real) Normalising factor for conversion
C     (<) RMSN    (Real array) Normalised r.m.s. array
C
C                                     JRW / AAO January 1987
C
C     Modified:
C        Original
C
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER ORD
      REAL RMS(ORD),NFAC,RMSN(ORD)
C
C     Local variables
C
      INTEGER I,IFAC
      REAL MAXN
C
C     Find maximum value
C
      MAXN=-1.E36
      DO I=1,ORD
        IF (RMS(I).GT.MAXN) THEN
          MAXN=RMS(I)
        ENDIF
      END DO

      IF (MAXN.LT.0.0) THEN
        MAXN=-1.*MAXN
      ENDIF

      IFAC=INT(ALOG10(MAXN)) - 1
      IF (IFAC.EQ.0) THEN
        NFAC=1.0
      ENDIF
      IF (IFAC.LT.0) THEN
        NFAC=1./10.**(REAL(IABS(IFAC)))
      ENDIF
      IF (IFAC.GT.0) THEN
        NFAC=10.**(REAL(IFAC))
      ENDIF
C
C     Normalise RMS
C
      DO I=1,ORD
        RMSN(I)=RMS(I)/NFAC
      END DO

      END
