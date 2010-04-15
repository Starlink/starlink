C+
      SUBROUTINE GEN_REV2D (IN,IX,IY,SAME,OUT)
C
C     G E N _ R E V 2 D
C
C     Reverses the rows of a 2D array.  That is, reverses
C     the order of the data along each of the rows - the
C     data remains in the same row.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Real array IN(IX,IY)) Input array.
C     (>) IX     (Integer) Number of columns in IN.
C     (>) IY     (Integer) Number of rows in IN.
C     (>) SAME   (Logical) True if IN and OUT are the same
C                array - the algorithm used is different
C                if this is the case.
C     (<) OUT    (Real array OUT(IX,IY)) Output array.
C
C                                      KS / CIT  7th March 1983
C
C     Modified:
C
C     17th Sep 1993  HME / UoE, Starlink.  When reversing in situ, we
C                    must not use IN and OUT, but only one of them.
C
C+
      IMPLICIT NONE
C
C     Parameters  (note, the arrays are declared linear, because
C     the compiler generates better code this way.)
C
      LOGICAL SAME
      INTEGER IX,IY
      REAL IN(IX*IY),OUT(IX*IY)
C
C     Local variables
C
      INTEGER IBASE,IPTR1,IPTR2,KX,KY,NREV
      REAL TEMP
C
      IF (SAME) THEN
C
C        IN and OUT are the same.  This is an exchange of
C        pixels along each row from front to back.
C
         NREV=IX/2
         IBASE=1
         DO KY=1,IY
            IPTR1=IBASE
            IPTR2=IPTR1+IX-1
            DO KX=1,NREV
               TEMP=OUT(IPTR1)
               OUT(IPTR1)=OUT(IPTR2)
               OUT(IPTR2)=TEMP
               IPTR1=IPTR1+1
               IPTR2=IPTR2-1
            END DO
            IBASE=IBASE+IX
         END DO
      ELSE
C
C        IN and OUT are different.  This is just a copy with
C        the rows reversed.
C
         IBASE=1
         DO KY=1,IY
            IPTR1=IBASE
            IPTR2=IPTR1+IX-1
            DO KX=1,IX
               OUT(IPTR2)=IN(IPTR1)
               IPTR1=IPTR1+1
               IPTR2=IPTR2-1
            END DO
            IBASE=IBASE+IX
         END DO
      END IF
C
      END
