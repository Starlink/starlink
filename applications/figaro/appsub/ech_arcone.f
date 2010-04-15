C+
      SUBROUTINE ECH_ARCONE(NLARCS,ARC1,ARC2,ARC3,NARCS,ARC)
C
C     E C H _ A R C O N E
C
C     ECHARC utility. Combines the entries from the arrays ARCn
C     (n=1,2,3) into the ONE single array ARC.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) NLARCS   (Integer) The size of each ARCn array.
C     (>) ARCn     (Real arrays ARCn(NLARCS)) The arrays containing
C                  the three separate lists of arc lines.
C     (>) NARCS    (Integer) The total number of lines that are to
C                  be found among the three ARCn frames.
C                  identified lines.
C     (<) ARC      (Real array ARC(NARCS)) The output array which
C                  will contain the sorted combination of ARCn.
C
C     Subroutines / functions used -
C
C     GEN_QFSORT   (GEN_ package) Does a quick-sort of an input array.
C
C                                             JKM / ESO 21st Nov. 1987
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NLARCS,NARCS
      REAL ARC1(NLARCS),ARC2(NLARCS),ARC3(NLARCS),ARC(NARCS)
C
C     Local variables
C
      INTEGER I,N,NSTAT
C
C     Here goes nothing ...
C
      N=0
C
      DO I=1,NLARCS,1
         IF (ARC1(I).NE.0) THEN
            N=N+1
            IF (N.LE.NARCS) THEN
               ARC(N)=ARC1(I)
            ELSE
               CALL PAR_WRUSER('Error combining ARC1',NSTAT)
               RETURN
            END IF
         ELSE
            GO TO 222
         END IF
      END DO
C
 222  DO I=1,NLARCS,1
         IF (ARC2(I).NE.0) THEN
            N=N+1
            IF (N.LE.NARCS) THEN
               ARC(N)=ARC2(I)
            ELSE
               CALL PAR_WRUSER('Error combining ARC2',NSTAT)
               RETURN
            END IF
         ELSE
            GO TO 333
         END IF
      END DO
C
 333  DO I=1,NLARCS,1
         IF (ARC3(I).NE.0) THEN
            N=N+1
            IF (N.LE.NARCS) THEN
               ARC(N)=ARC3(I)
            ELSE
               CALL PAR_WRUSER('Error combining ARC3',NSTAT)
               RETURN
            END IF
         ELSE
            GO TO 444
         END IF
      END DO
C
 444  IF (N.EQ.NARCS) THEN
         CALL GEN_QFSORT(ARC,N)
      ELSE
         CALL PAR_WRUSER('Unable to fill ARC array',NSTAT)
      END IF
C
      RETURN
      END
