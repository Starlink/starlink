C+
      SUBROUTINE GEN_REF2D (IN,IX,IY,SAME,OUT)
C
C     G E N _ R E F 2 D
C
C     Reverses the row order of a 2D array - ie reflects
C     it about the center row.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN      (Real array IN(IX,IY)) The array to be reflected.
C     (>) IX      (Integer) The first dimension of IN
C     (>) IY      (Integer) The second dimension of IN
C     (>) SAME    (Logical) True if IN and OUT are the same array.
C                 If this is the case the algorithm used is modified
C                 to reflect the array in situ.
C     (<) OUT     (Real array OUT(IX,IY)) The result of the reflection.
C
C     Subroutines / functions used -
C
C     GEN_MOVE    (GEN_ package) Fast move of bytes in core
C
C                                     KS / CIT  7th March 1983
C
C     Modified:
C
C     17th Sep 1993  HME / UoE, Starlink.  When reversing in situ, we
C                    must not use IN and OUT, but only one of them.
C
C+
      IMPLICIT NONE
C
C     Parameters (the arrays are treated as 1D to get better
C                                            code generated.)
C
      LOGICAL SAME
      INTEGER IX,IY
      REAL IN(IX*IY),OUT(IX*IY)
C
C     Local variables
C
      INTEGER BYTES,I,IBASE,IPTR1,IPTR2,J,NREF
      REAL TEMP
C
      IF (SAME) THEN
C
C        Reflection is in situ, and is done by interchange
C        of pixels.
C
         NREF=IY/2
         IF (NREF.GT.0) THEN
            IBASE=(IY-1)*IX+1
            IPTR1=1
            DO I=1,NREF
               IPTR2=IBASE
               DO J=1,IX
                  TEMP=OUT(IPTR2)
                  OUT(IPTR2)=OUT(IPTR1)
                  OUT(IPTR1)=TEMP
                  IPTR1=IPTR1+1
                  IPTR2=IPTR2+1
               END DO
               IBASE=IBASE-IX
            END DO
         END IF
      ELSE
C
C        Reflection is not in situ, and can be done as
C        just a reflected copy.
C
         BYTES=IX*4
         IPTR1=1
         IPTR2=(IY-1)*IX+1
         DO I=1,IY
            CALL GEN_MOVE(BYTES,IN(IPTR1),OUT(IPTR2))
            IPTR1=IPTR1+IX
            IPTR2=IPTR2-IX
         END DO
      END IF
C
      END
