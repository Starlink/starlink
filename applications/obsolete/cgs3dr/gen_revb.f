C+
      SUBROUTINE GEN_REVB (IN,IX,IY,SAME,OUT)
C
C     G E N _ R E V B
C
C     Reverses the rows of a 2D array.  That is, reverses
C     the order of the data along each of the rows - the
C     data remains in the same row.  This routine is the same
C     as GEN_REV2D, except that it works on BYTE data.
C
C     Parameters -  (">" input, "<" output)
C
C     (>) IN     (Byte array IN(IX,IY)) Input array.
C     (>) IX     (Integer) Number of columns in IN.
C     (>) IY     (Integer) Number of rows in IN.
C     (>) SAME   (Logical) True if IN and OUT are the same
C                array - the algorithm used is different
C                if this is the case.
C     (<) OUT    (Byte array OUT(IX,IY)) Output array.
C
C                                      KS / CIT  11th May 1983
C     Modified :
C
C     20-Nov-1989: SMB / ROE.  Version for BYTE array (created so that
C                              data quality arrays may be reversed).
C+
      IMPLICIT NONE
C
C     Parameters  (note, the arrays are declared linear, because
C     the compiler generates better code this way.)
C
      LOGICAL SAME
      INTEGER IX,IY
      BYTE IN(IX*IY),OUT(IX*IY)
C
C     Local variables
C
      INTEGER IBASE,IPTR1,IPTR2,KX,KY,NREV
      BYTE TEMP
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
               TEMP=IN(IPTR1)
               OUT(IPTR1)=IN(IPTR2)
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
