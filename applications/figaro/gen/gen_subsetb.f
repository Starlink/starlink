C+
      SUBROUTINE GEN_SUBSETB(IN,NX,NY,NXNEW,NYNEW,NXST,NYST,OUT)
C
C     G E N _ S U B S E T B
C
C     Returns a subset of a 2-dimensional byte array.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Byte array IN(NX,NY)) The input array.
C     (>) NX     (Integer) The first dimension of IN.
C     (>) NY     (Integer) The second dimension of IN.
C     (>) NXNEW  (Integer) The first dimension of OUT.
C     (>) NYNEW  (Integer) The second dimension of OUT.
C     (>) NXST   (Integer) The element in the x-dimension of
C                IN that is to be the first element of OUT.
C     (>) NYST   (Integer) The element in the y-direction of
C                IN that is to be the first element of OUT.
C     (<) OUT    (Byte array OUT(NXNEW,NYNEW)) The result array.
C
C     That is, OUT(1,1)=IN(NXST,NYST) and so on up to
C     OUT(NXNEW,NYNEW)=IN(NXST+NXNEW-1,NYST+NYNEW-1). If the
C     values of the parameters are such that they imply accesses
C     outside the bounds of IN, the corresponding elements of OUT
C     will be left unchanged.
C
C     IN and OUT can be the same array, but 'clever' usages
C     involving overlapping arrays with IN(1,1) not equivalent
C     to OUT(1,1) should be avoided.
C
C     Subroutines / functions called -
C
C     GEN_MOVE    (GEN_ package) Fast data copy between arrays.
C
C                                           KS / AAO 7th Dec 1989
C+
      IMPLICIT NONE
C
C     Parameters -
C
      INTEGER NX,NY,NXNEW,NYNEW,NXST,NYST
      BYTE IN(NX,NY),OUT(NXNEW,NYNEW)
C
C     Local variables
C
      INTEGER BYTES,EXCESS,IY,IYPT,NYLIM
C
C     Check ranges
C
      EXCESS=NXST+NXNEW-1-NX
      IF (EXCESS.GT.0) THEN
         BYTES=(NXNEW-EXCESS)
      ELSE
         BYTES=NXNEW
      END IF
      EXCESS=NYST+NYNEW-1-NY
      IF (EXCESS.GT.0) THEN
         NYLIM=NYNEW-EXCESS
      ELSE
         NYLIM=NYNEW
      END IF
C
C     Loop over rows of result array, copying from input rows.
C
      IYPT=NYST
      DO IY=1,NYLIM
         CALL GEN_MOVE(BYTES,IN(NXST,IYPT),OUT(1,IY))
         IYPT=IYPT+1
      END DO
C
      END
