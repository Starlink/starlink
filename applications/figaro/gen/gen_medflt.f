C+
      SUBROUTINE GEN_MEDFLT (IN,NX,NY,BSIZX,BSIZY,WORK,OUT)
C
C     G E N _ M E D F L T
C
C     Median filters a 2D real array, using a box of a given size.
C     That is, each pixel of the result array is set to the median
C     value of a box of pixels centered on the corresponding pixel
C     in the input array.
C
C     Parameters -  (">" input, "<" output, "W" workspace)
C
C     (>) IN     (Real array IN(NX,NY)) The input array.
C     (>) NX     (Integer) First dimension if IN.
C     (>) NY     (Integer) Second dimension of IN.
C     (>) BSIZX  (Integer) Size of median box in X (first dimension).
C     (>) BSIZY  (Integer) Size of median box in Y (first dimension).
C                NOTE: If BSIZX or BSIZY are even, they will be treated
C                as if they were one less.
C     (W) WORK   (Real array (BSIZX*BSIZY)) Work array.
C     (<) OUT    (Real array OUT(NX,NY)) Output array.
C                NOTE: IN and OUT must NOT be the same array in the
C                calling routine.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_QFMED  (GEN_ package) Quicksort of a real array
C
C     With a fair amount of work, this routine could be made much
C     faster - it really shouldn't have to start from scratch each
C     time it finds a median, for example.
C
C                                           KS / CIT 30th Jan 1984
C     Modified:
C       17th Dec 1992.  Bug in calculation of X-limits corrected. Original
C                       version would only have worked properly if BSIZY
C                       was the same as BSIZX. KS/AAO.
C       30th Jun 1993.  Removed unused variables. KS/AAO.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,BSIZX,BSIZY
      REAL IN(NX,NY),OUT(NX,NY),WORK(BSIZX*BSIZY)
C
C     Function used
C
      REAL GEN_QFMED
C
C     Local variables
C
      INTEGER ISIDX,ISIDY,IX,IXEN,IXPT,IXST
      INTEGER IY,IYEN,IYPT,IYST,IWPT
C
C     Half sides of median box
C
      ISIDY=BSIZY/2
      ISIDX=BSIZX/2
C
C     Loop round rows of array
C
      DO IY=1,NY
         IYST=MAX(1,IY-ISIDY)
         IYEN=MIN(NY,IY+ISIDY)
C
C        Loop round columns of array
C
         DO IX=1,NX
            IXST=MAX(1,IX-ISIDX)
            IXEN=MIN(NX,IX+ISIDX)
C
C           Get data values from median box
C
            IWPT=0
            DO IYPT=IYST,IYEN
               DO IXPT=IXST,IXEN
                  IWPT=IWPT+1
                  WORK(IWPT)=IN(IXPT,IYPT)
               END DO
            END DO
C
C           Determine median value and set element of result array
C
            OUT(IX,IY)=GEN_QFMED(WORK,IWPT)
C
         END DO
      END DO
C
      END
