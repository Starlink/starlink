C+
      INTEGER FUNCTION ARID(CHANS,NLID,CENT,VALUE)
C
C     A R I D
C
C     Locates a line at a given pixel number in the list
C     of identified lines.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) CHANS     (Real array CHANS(NLID)) The centers
C                   (in pixels) of the identified lines.
C                   These need not be any order.
C     (>) NLID      (Integer) The current number of identified
C                   lines.
C     (>) CENT      (Real) The given pixel number - assumed to
C                   be close to one of the identified lines.
C     (<) VALUE     (Real) The value of the line center that is
C                   closest to CENT.
C
C     Returns -
C
C     (<) ARID      (Integer) The element number of the array
C                   element containing VALUE.  If NLID is 0,
C                   ARID will return as 0, and VALUE as 0.
C
C                                        KS / CIT 13th Jan 1983
C+
      IMPLICIT NONE
C
C     Parameters - (CHANS is dimensioned 1 in case NLID=0)
C
      INTEGER NLID
      REAL CHANS(1),CENT,VALUE
C
C     Local variables
C
      INTEGER I
      REAL DELMIN,DELTAX
C
C     Look for closest value.
C
      IF (NLID.EQ.0) THEN
         ARID=0
         VALUE=0.
      ELSE
         ARID=1
         VALUE=CHANS(1)
         DELMIN=ABS(VALUE-CENT)
         DO I=1,NLID
            DELTAX=ABS(CHANS(I)-CENT)
            IF (DELTAX.LT.DELMIN) THEN
               DELMIN=DELTAX
               ARID=I
               VALUE=CHANS(I)
            END IF
         END DO
      END IF
C
      END
