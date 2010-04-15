C+
      SUBROUTINE GEN_GROWXB (ARR1D,NX,NY,IY1,IY2,ARR2D)
C
C     G E N _ G R O W X B
C
C     Copies a one dimensional byte array into successive cross-
C     sections of a two dimensional array.  The cross-sections
C     are cross-sections in the 'X' direction, ie array elements
C     for which the second array index is constant.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARR1D    (Byte array ARR1D(NX)) The 1D array.
C     (>) NX       (Integer) The number of elements in ARR1D.
C                  Also the first dimension of ARR2D.
C     (>) NY       (Integer) The second dimension of ARR2D.
C     (>) IY1      (Integer) The number of the first cross-section
C                  of ARR2D into which ARR1D is to be copied.
C     (>) IY2      (Integer) The number of the last cross-section
C                  of ARR2D into which ARR1D is to be copied.
C     (<) ARR2D    (Byte array ARR2D(NX,NY)) The 2D array.
C
C     Common variables used -  None
C
C     Subroutines / functions used -
C
C     GEN_MOVE     (GEN_ package) Fast mover of bytes in memory.
C
C                                             KS / CIT 20th Sept 1983
C                            Byte version.   SMB / ROE 6th December 1990
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,IY1,IY2
      BYTE ARR1D(NX),ARR2D(NX,NY)
C
C     Local variables
C
      INTEGER I,LIM1,LIM2
C
      LIM1=MAX(1,IY1)
      LIM2=MIN(NY,IY2)
      DO I=LIM1,LIM2
         CALL GEN_MOVE(NX,ARR1D,ARR2D(1,I))
      END DO
C
      END
