C+
      SUBROUTINE GEN_GROWYT (ARR2D,NX,NY,NT,IX1,IX2,ARR3D)
C
C     G E N _ G R O W Y T
C
C     Copies a two-dimensional array into successive cross-
C     sections of a three-dimensional array.  The cross-sections
C     are cross-sections in the 'X' direction, ie array elements
C     for which the first array index is constant.  The arrays
C     are described here as integers, but arrays with 4-byte
C     elements (ie real arrays), can be used equally well.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) ARR2D    (Integer array ARR2D(NT,NY)) The 2D array.
C     (>) NX       (Integer) The number of elements in ARR2D.
C                  Also the first dimension of ARR3D.
C     (>) NY       (Integer) The second dimension of ARR3D.
C     (>) NT       (Integer) The third dimension of ARR3D.
C     (>) IX1      (Integer) The number of the first cross-section
C                  of ARR3D into which ARR2D is to be copied.
C     (>) IX2      (Integer) The number of the last cross-section
C                  of ARR3D into which ARR2D is to be copied.
C     (<) ARR3D    (Integer array ARR3D(NX,NY,NT)) The 3D array.
C
C     Common variables used -  None
C
C     Subroutines / functions used -  None
C
C                                             KS / AAO 15th April 1985
C     Modified:
C     30th Jan 1995. KS/AAO. Corrected check on limits - was using NT
C                    as X limit instead of NX.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX,NY,NT,IX1,IX2,ARR2D(NT,NY),ARR3D(NX,NY,NT)
C
C     Local variables
C
      INTEGER IX,IT,IY,LIM1,LIM2
C
      LIM1=MAX(1,IX1)
      LIM2=MIN(NX,IX2)
      DO IT=1,NT
         DO IY=1,NY
            DO IX=LIM1,LIM2
               ARR3D(IX,IY,IT)=ARR2D(IT,IY)
            END DO
         END DO
      END DO
C
      END
