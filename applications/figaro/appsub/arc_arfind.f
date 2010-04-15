C+
      REAL FUNCTION ARC_ARFIND(ARCN,NLARCS,VALUE)
C
C     A R F I N D
C
C     Given a wavelength, locates the nearest arc line to
C     that wavelength in a table.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARCN     (Real array ARCN(NLARCS)) Holds the table of
C                  arc wavelengths.  These should be in ascending
C                  order, and end with a 0. value or with the
C                  end of the array.
C     (>) NLARCS   (Integer) The dimension of ARCN
C     (>) VALUE    (Real) The given wavelength.
C
C     Returns -
C
C     (<) ARFIND   (Real) The nearest tabulated wavelength.
C
C                                        KS / CIT  13th Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLARCS
      REAL ARCN(NLARCS),VALUE
C
C     Local variables
C
      INTEGER I
      REAL VLAST,VTAB
C
C     Loop through table
C
      VLAST=0.
      DO I=1,NLARCS
         VTAB=ARCN(I)
         IF (VTAB.EQ.0.) THEN
            ARC_ARFIND=VLAST
            GO TO 330
         END IF
         IF (VTAB.GT.VALUE) THEN
            IF ((VTAB-VALUE).GT.(VALUE-VLAST)) THEN
               ARC_ARFIND=VLAST
            ELSE
               ARC_ARFIND=VTAB
            END IF
            GO TO 330
         END IF
         VLAST=VTAB
      END DO
C
C     If loop falls through, use last value in table
C
      ARC_ARFIND=VLAST
C
  330 CONTINUE
C
      RETURN
      END
