C+
      INTEGER FUNCTION GEN_BSEARCH(ARRAY,NV,VALUE)
C
C     G E N _ B S E A R C H
C
C     Looks through a table of values and returns the number of the
C     element that is closest to a given test value, assuming the
C     elements are in either ascending or descending order (the
C     routine does not test that this condition holds).
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARRAY    (Real array ARRAY(NV)) The table of values.
C     (>) NV       (Integer) The number of values.
C     (>) VALUE    (Real) The test value.
C
C     Returns -
C
C     (<) GEN_BSEARCH (Integer) The index number (from 1 up) of
C                     the element of ARRAY which satifies the
C                     matching criterion.  If the routine finds
C                     an array element that exactly equals the
C                     search value then that element number is
C                     returned.  If two elements are found that
C                     bracket the value, the nearer element
C                     is returned.  If the value is outside the
C                     range of values in the array, 0 is returned.
C
C     Note: This routine looks for the nearest value to the
C     test value and so is unlike many binary search programs which
C     are looking for an exact match.  It is also complicated by
C     allowing either ascending or descending tables.  It also
C     assumes that there is an increased chance that the test value
C     may be one of the extreme table values and so tests these first.
C     This is a consequence of the specific problem for which it was
C     originally written.
C                                         KS / CIT  7th May 1983
C
C     Modified : SJM/MSSSO 16 June 1993.   Infinite loop on 1 element array.
C					   Put in specific check for this case.
C
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NV
      REAL ARRAY(NV),VALUE
C
C     Local variables
C
      LOGICAL ENDED
      INTEGER L,R,M
      REAL    VMAX,VMIN
C
C     Check for test value out of range of array values
C
      VMAX=MAX(ARRAY(1),ARRAY(NV))
      VMIN=MIN(ARRAY(1),ARRAY(NV))
      IF ((VALUE.GT.VMAX).OR.(VALUE.LT.VMIN)) THEN
         GEN_BSEARCH=0
      ELSE
C
C        Set up initial parameters for binary search
C
         L=1
         R=NV
         ENDED=.FALSE.

	 IF (NV .EQ. 1) THEN
	   GEN_BSEARCH = 1
	   RETURN
	 END IF
C
C        Is ARRAY in ascending or descending order ?
C
         IF (ARRAY(1).LT.ARRAY(NV)) THEN
C
C           Ascending order. Test for value at extreme ends of range
C
            IF ((ARRAY(1).LE.VALUE).AND.(ARRAY(2).GT.VALUE)) THEN
               GEN_BSEARCH=1
               IF ((VALUE-ARRAY(1)).GT.(ARRAY(2)-VALUE))
     :                                          GEN_BSEARCH=2
               ENDED=.TRUE.
            ELSE IF ((ARRAY(NV-1).LE.VALUE).AND.(ARRAY(NV).GE.VALUE))
     :                                                           THEN
               GEN_BSEARCH=NV-1
               IF ((VALUE-ARRAY(NV-1)).GT.(ARRAY(NV)-VALUE))
     :                                          GEN_BSEARCH=NV
               ENDED=.TRUE.
            END IF
C
C           If not at the ends, begin binary search.  Note: L,R and M
C           are the Left, Right, and Middle elemenys of the range the
C           search has homed in on.
C
            DO WHILE (.NOT.ENDED)
               M=(L+R)/2
               IF ((ARRAY(M).LE.VALUE).AND.(ARRAY(M+1).GT.VALUE)) THEN
                  GEN_BSEARCH=M
                  IF ((VALUE-ARRAY(M)).GT.(ARRAY(M+1)-VALUE))
     :                                          GEN_BSEARCH=M+1
                  ENDED=.TRUE.
               ELSE IF (VALUE.LT.ARRAY(M)) THEN
                  R=M
               ELSE
                  L=M
               END IF
            END DO
C
         ELSE
C
C           ARRAY is in descending order.  Following code is essentially
C           the same as for the ascending case, with the tests inverted.
C
            IF ((ARRAY(1).GE.VALUE).AND.(ARRAY(2).LT.VALUE)) THEN
               GEN_BSEARCH=1
               IF ((ARRAY(1)-VALUE).GT.(VALUE-ARRAY(2)))
     :                                          GEN_BSEARCH=2
               ENDED=.TRUE.
            ELSE IF ((ARRAY(NV-1).GE.VALUE).AND.(ARRAY(NV).LE.VALUE))
     :                                                            THEN
                  GEN_BSEARCH=NV-1
                  IF ((ARRAY(NV-1)-VALUE).GT.(VALUE-ARRAY(NV)))
     :                                          GEN_BSEARCH=NV
               ENDED=.TRUE.
            END IF
            DO WHILE (.NOT.ENDED)
               M=(L+R)/2
               IF ((ARRAY(M).GE.VALUE).AND.(ARRAY(M+1).LT.VALUE)) THEN
                  GEN_BSEARCH=M
                  IF ((VALUE-ARRAY(M)).LT.(ARRAY(M+1)-VALUE))
     :                                          GEN_BSEARCH=M+1
                  ENDED=.TRUE.
               ELSE IF (VALUE.GT.ARRAY(M)) THEN
                  R=M
               ELSE
                  L=M
               END IF
            END DO
         END IF
      END IF
C
      END
