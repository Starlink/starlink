C+
      SUBROUTINE GEN_GROWXY (ARR2D,NX,NY,NT,IT1,IT2,ARR3D)
C
C     G E N _ G R O W X Y
C
C     Copies a two-dimensional array into successive cross-
C     sections of a three-dimensional array.  The cross-sections
C     are cross-sections in the 'T' direction, ie array elements
C     for which the third array index is constant.  The arrays
C     are described here as integers, but arrays with 4-byte
C     elements (ie real arrays), can be used equally well.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARR2D    (Integer array ARR2D(NX,NY)) The 2D array.
C     (>) NX       (Integer) The number of elements in ARR2D.
C                  Also the first dimension of ARR3D.
C     (>) NY       (Integer) The second dimension of ARR3D.
C     (>) NT       (Integer) The third dimension of ARR3D.
C     (>) IT1      (Integer) The number of the first cross-section
C                  of ARR3D into which ARR2D is to be copied.
C     (>) IT2      (Integer) The number of the last cross-section
C                  of ARR3D into which ARR2D is to be copied.
C     (<) ARR3D    (Integer array ARR3D(NX,NY,NT)) The 3D array.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_MOVE     (GEN_ package) Fast mover of bytes in memory.
C
C                                             KS / AAO 15th April 1985
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,NT,IT1,IT2,ARR2D(NX,NY),ARR3D(NX,NY,NT)
C
C     Local variables
C
      INTEGER I,LIM1,LIM2
C
      LIM1=MAX(1,IT1)
      LIM2=MIN(NT,IT2)
      DO I=LIM1,LIM2
         CALL GEN_MOVE(NX*NY*4,ARR2D,ARR3D(1,1,I))
      END DO
C
      END
