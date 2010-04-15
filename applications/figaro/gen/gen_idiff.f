C+
      SUBROUTINE GEN_IDIFF (IN,NX,NY,OUT)
C
C     G E N _ I D I F F
C
C     Given a two dimensional array, generates an output array in
C     which each element is the average absolute difference between
C     the corresponding input array element and its nearest neighbours.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(NX,NY)) Input array
C     (>) NX     (Integer) First dimension of IN
C     (>) NY     (Integer) Second dimension of IN
C     (<) OUT    (Real array OUT(NX,NY)) Result array
C
C     Common variables - None
C
C     Subroutines / functions - None
C
C                                           KS / CIT 22nd April 1984
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NX, NY
      REAL    IN(NX,NY), OUT(NX,NY)
C
C     Local variables
C
      INTEGER IX, IY, NELM
      REAL    ELEM, TOTAL
C
C     Perform the operation.  This could be coded faster by
C     treating the edges outside the main loop, but that refinement
C     can come later.
C
      DO IY=1,NY
         DO IX=1,NX
            ELEM=IN(IX,IY)
            TOTAL=0.
            NELM=0
            IF (IY.GT.1) THEN
               TOTAL=TOTAL+ABS(ELEM-IN(IX,IY-1))
               NELM=NELM+1
            END IF
            IF (IY.LT.NY) THEN
               TOTAL=TOTAL+ABS(ELEM-IN(IX,IY+1))
               NELM=NELM+1
            END IF
            IF (IX.GT.1) THEN
               TOTAL=TOTAL+ABS(ELEM-IN(IX-1,IY))
               NELM=NELM+1
            END IF
            IF (IX.LT.NX) THEN
               TOTAL=TOTAL+ABS(ELEM-IN(IX+1,IY))
               NELM=NELM+1
            END IF
            OUT(IX,IY)=TOTAL/FLOAT(NELM)
         END DO
      END DO
C
      END
