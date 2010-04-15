C+
      SUBROUTINE FIG_ZAPROW (DATA,VEXIST,VARIANCE,NX,NY,LX1,LX2,LY1,
     :                       LY2,NROW,IROWS,FLAG,MAXBROW,NBADROW,
     :                       BADROWS,STATUS)
C
C     F I G _ Z A P C O L
C
C     Flags a set of rows as bad, recording their numbers in the
C     bad row table and also flagging them in the data array by
C     setting their elements to a flag value. If a variance structure
C     exists, its elements are all set to be zero. They may be changed
C     using GENVAR if required.
C
C     Parameters -  (">" input, "<" output, "!" modified)
C
C     (!) DATA      (Real array DATA(NX,NY)) The image data.
C     (!) VEXIST    (Logical) TRUE if a variance array exists
C     (!) VARIANCE  (Real array VARIANCE(NX,NY)) The variance array.
C     (>) NX        (Integer) The first dimension of DATA and VARIANCE
C     (>) NY        (Integer) The second dimension of DATA and VARIANCE.
C     (>) LX1       (Integer) Pixels outside a subset of DATA given by
C     (>) LX2       (Integer) the limits LY1,LY2 (in the 2nd dimension)
C     (>) LY1       (Integer) and LX1,LX2 (in the first dimension) will
C     (>) LY2       (Integer) not be changed by this routine.
C     (>) NROW      (Integer) The number of rows to be flagged.
C     (>) IROWS     (Integer array IROWS(NROW)) The rows to be flagged.
C     (>) FLAG      (Real) The value to be used to flag bad pixels.
C     (>) MAXBROW   (Integer) Maximum possible # of bad rows.
C     (!) NBADROW   (Integer) The total number of rows flagged.
C     (!) BADROWS   (Integer array BADROWS(NBADROW)) The rows flagged
C                   so far.
C     (<) STATUS    (Integer) Status return. 0 => OK, 1 => too many bad
C                   rows.
C
C     Common variables used - None
C
C     Subroutines / functions used -
C
C     FIG_PUNCHOUT  (FIG_ package) Flag an area of the image as invalid
C
C                                                 KS / CIT 22nd Feb 1984
C     13th May 1997 JJL/Soton, Starlink  Variance handling included.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,LX1,LY1,LX2,LY2,NROW,IROWS(NROW),NBADROW,MAXBROW
      INTEGER BADROWS(MAXBROW),STATUS
      REAL    DATA(NX,NY),VARIANCE(NX,NY),FLAG
      LOGICAL VEXIST
C
C     Local variables
C
      INTEGER ROW,IC
C
C     Loop through all rows to be flagged
C
      STATUS=0
      DO IC=1,NROW
C
C        Check that row is within Y range
C
         ROW=IROWS(IC)
         IF ((ROW.LE.LY2).AND.(ROW.GE.LY1)) THEN
C
C           Add row to tables
C
            NBADROW=NBADROW+1
            IF (NBADROW.GT.MAXBROW) THEN
               STATUS=1
               GO TO 600
            ELSE
               BADROWS(NBADROW)=ROW
C
C              Flag pixels in array as invalid
C
               CALL FIG_PUNCHOUT(DATA,NX,NY,LX1,ROW,LX2,ROW,FLAG)
C
C     If variances exist, set them all to zero.
C
               IF (VEXIST) THEN
                 CALL FIG_PUNCHOUT(VARIANCE,NX,NY,LX1,ROW,LX2,
     :                             ROW,0.0)
               ENDIF
            END IF
         END IF
      END DO
C
  600 CONTINUE
      END
