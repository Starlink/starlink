C+
      SUBROUTINE ARC_ARGUESS(CHANS,WAVES,NLID,CENT,VALUE)
C
C     A R G U E S S
C
C     Given that at least two lines have been identified,
C     gives a guess at the wavelength of a given pixel number
C     by linear interpolation using the two nearest lines.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) CHANS     (Real array CHANS(NLID)) The channel numbers
C                   of the identified lines.
C     (>) WAVES     (Real array WAVES(NLID)) The wavelengths of
C                   the identified lines.
C     (>) NLID      (Integer) The number of identified lines.
C     (>) CENT      (Real) The channel number.
C     (<) VALUE     (Real) The guess at the wavelength of CENT.
C
C                                         KS / CIT 13th June 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLID
      REAL CHANS(NLID),WAVES(NLID),CENT,VALUE
C
C     Local variables
C
      INTEGER I,L1,L2
      REAL DIFF,DIFF1,DIFF2
C
      VALUE=0.
      IF (NLID.GE.2) THEN
         L1=1
         L2=2
         IF (NLID.GT.2) THEN
C
C           More than two lines, so search out min 2. Note in
C           following, DIFF1 is always < DIFF2
C
            DIFF1=ABS(CENT-CHANS(1))
            DIFF2=ABS(CENT-CHANS(2))
            IF (DIFF1.GT.DIFF2) THEN
               DIFF1=DIFF2
               DIFF2=ABS(CENT-CHANS(1))
               L1=2
               L2=1
            END IF
C
C           Go through rest of lines.  Check first against DIFF1,
C           and against DIFF2 only if diff is more than DIFF1.
C
            DO I=3,NLID
               DIFF=ABS(CENT-CHANS(I))
               IF (DIFF.LT.DIFF1) THEN
                  L2=L1
                  DIFF2=DIFF1
                  L1=I
                  DIFF1=DIFF
               ELSE
                  IF (DIFF.LT.DIFF2) THEN
                     L2=I
                     DIFF2=DIFF
                  END IF
               END IF
            END DO
         END IF
C
C        Have nearest values, now interpolate.
C
         DIFF=CHANS(L2)-CHANS(L1)
         IF (DIFF.NE.0.) THEN
            VALUE=WAVES(L1)+(CENT-CHANS(L1))/DIFF
     :                                *(WAVES(L2)-WAVES(L1))
         ELSE
            VALUE=WAVES(L1)
         END IF
      END IF
C
      RETURN
      END
