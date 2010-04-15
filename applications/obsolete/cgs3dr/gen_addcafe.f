C+
      SUBROUTINE GEN_ADDCAFE(NELM,CONST,IN, OUT,
     :                     QULIN,QULOUT,
     :                     VARIN,VAROUT,
     :                     QUAL,FLAGS,FBAD,VARIANCES)

C
C     G E N _ A D D C A F E
C
C     Adds a real constant to a real array.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input array
C                (Note that IN may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (>) NELM      (Integer) The number of elements of IN.
C     (>) CONST  (Real) The constant to be added to all the
C                elements of IN.
C     (<) OUT    (Real array OUT(NELM)) The result of the
C                addition. Note that IN and OUT may
C                be the same array.
C     (>) QULIN  (Byte array) Quality array for input array
C     (<) QULOUT (Byte array) Quality array for output array
C     (>) VARIN  (Real array) Variance array for input array
C     (<) VAROUT (Real array) Variance array for output array
C     (>) QUAL   (Logical)    True if input has quality information
C     (>) FLAGS  (Logical)    True if input has flagged data values
C     (>) FBAD   (Real)       Flag value
C     (>) VARIANCES (Logical)    True if input array has variance array
C
C     Subroutines / functions used - None
C
C                                         KS / CIT 15th Feb 1983
C     Modified:
C
C     20th Mar  1989  JM / RAL. Modified to handle quality and errors.
C     8th Dec 1990  PRB / ROE.  Changed variable names to reflect the fact
C                               that variances, not errors, are used.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL IN(NELM),CONST,OUT(NELM)
      LOGICAL  VARIANCES, QUAL, FLAGS
      BYTE QULIN(NELM),QULOUT(NELM)
      REAL VARIN(NELM),VAROUT(NELM)
      REAL     FBAD
C
C     Local variable
C
      INTEGER I
C
C     Quality values defined symbolically
C
      BYTE GOOD, BAD
      PARAMETER (GOOD = 0, BAD=1)
C
C     Handle different quality methods separately.
C
      IF (QUAL) THEN
C
C        Image had quality data
C
         DO I=1,NELM
            IF (QULIN(I).EQ.GOOD)THEN
               OUT(I)=IN(I)+CONST
            ELSE
               QULOUT(I)=BAD
            ENDIF
         END DO
C
      ELSE IF (FLAGS) THEN
C
C        Image had flagged data values
C
         DO I=1,NELM
            IF (IN(I).NE.FBAD) THEN
               OUT(I)=IN(I)+CONST
            ELSE
               OUT(I)=FBAD
            END IF
         ENDDO
      ELSE
C
C        Image had no quality information
C
         DO I=1,NELM
            OUT(I)=IN(I)+CONST
         ENDDO

      END IF
C
C    Copy variance array
C
      IF(VARIANCES)THEN
         DO I=1,NELM
            VAROUT(I)=VARIN(I)
         ENDDO
      ENDIF
      END
