C+
      SUBROUTINE GEN_ADDCAFE(IN,NELM,CONST,OUT,QULIN,QUAL,FLAGS,FBAD)
C
C     G E N _ A D D C A F E
C
C     Adds a real constant to a real array, allowing for quality
C     or bad pixel information.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input array
C                (Note that IN may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (>) NELM   (Integer) The number of elements of IN.
C     (>) CONST  (Real) The constant to be added to all the
C                elements of IN.
C     (<) OUT    (Real array OUT(NELM)) The result of the
C                addition. Note that IN and OUT may
C                be the same array.
C     (>) QULIN  (Byte array) Quality array for input array
C     (>) QUAL   (Logical)    True if input has quality information
C     (>) FLAGS  (Logical)    True if input has flagged data values
C     (>) FBAD   (Real)       Flag value

C
C     Subroutines / functions used - None
C
C                                         KS / CIT 15th Feb 1983
C     Modified:
C
C     20th Mar  1989  JM / RAL.  Modified to handle quality and errors.
C     13th Dec  1989  KS / AAO.  Routine simplified slightly.  Errors
C                     no longer processed, only an input quality array
C                     used.  (Assumes these are copied elsewhere.) 'E'
C                     added to end of routine name.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL IN(NELM),CONST,OUT(NELM),FBAD
      LOGICAL  QUAL, FLAGS
      BYTE QULIN(NELM)
C
C     Local variable
C
      INTEGER I
C
C     Quality values defined symbolically
C
      INTEGER  GOOD, BAD
      PARAMETER (BAD = 1, GOOD = 0)
C
C     Handle different quality methods separately.
C
      IF (QUAL) THEN
C
C        Image had quality data
C
         DO I=1,NELM
            IF (QULIN(I).EQ.GOOD) OUT(I)=IN(I)+CONST
         END DO
C
      ELSE IF (FLAGS) THEN
C
C        Image had flagged data values
C
         DO I=1,NELM
            IF (IN(I).NE.FBAD) OUT(I)=IN(I)+CONST
         ENDDO
C
      ELSE
C
C        Image had no quality information
C
         DO I=1,NELM
            OUT(I)=IN(I)+CONST
         ENDDO
C
      END IF
C
      END
