C+
      SUBROUTINE GEN_MULCAFE(IN,NELM,CONST,OUT,
     :                      QULIN,QULOUT,ERRIN,ERROUT,
     :                      QUAL,FLAGS,FBAD,ERRORS)
C
C     G E N _ M U L C A F E
C
C     Multiplies a real array by a real constant, allowing for quality
C     information or flagged data values, and propagating errors.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) IN     (Real array IN(NELM)) The input array
C                (Note that IN may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (>) NELM   (Integer) The number of elements of IN.
C     (>) CONST  (Real) The constant by which all the
C                elements of IN are to be multiplied.
C     (<) OUT    (Real array OUT(NELM)) The result of the
C                multiplication.  Note that IN and OUT may
C                be the same array.
C     (>) QULIN  (Byte array) Quality array for input array
C     (<) QULOUT (Byte array) Quality array for output array
C     (>) ERRIN  (Real array) Error array for input array
C     (<) ERROUT (Real array) Error array for output array
C     (>) QUAL   (Logical)    True if input has quality information
C     (>) FLAGS  (Logical)    True if input has flagged data values
C     (>) FBAD   (Real)       Flag value
C     (>) ERRORS (Logical)    True if input array has error array
C
C     Subroutines / functions used - None
C
C                                         KS / CIT 1st March 1983
C     Modified:
C
C     20th Mar  1989  JM / RAL.  Modified to handle quality and errors.
C                     This routine assumes errors are standard deviation.
C     13th Dec  1989  KS / AAO.  'E' added to the end of the routine name.
C     14th Aug  1996  MJCL / Starlink, UCL.  Modified to give correct
C                     errors even when CONST is negative.
C     24th Mar  1997  JJL / Starlink, Southampton. Modified to give a new
C                     variance vlue rather than a new standard deviation.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      REAL IN(NELM),CONST,OUT(NELM)
      LOGICAL  ERRORS, QUAL, FLAGS
      BYTE QULIN(NELM),QULOUT(NELM)
      REAL ERRIN(NELM),ERROUT(NELM)
      REAL     FBAD
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
            IF (QULIN(I).EQ.GOOD)THEN
               OUT(I)=IN(I)*CONST
            ELSE
               QULOUT(I)=BAD
            ENDIF
         END DO

      ELSE IF (FLAGS) THEN
C
C        Image had flagged data values
C
         DO I=1,NELM
            IF (IN(I).NE.FBAD) THEN
               OUT(I)=IN(I)*CONST
            ELSE
               OUT(I)=FBAD
            END IF
         ENDDO
      ELSE
C
C        Image had no quality information
C
         DO I=1,NELM
            OUT(I)=IN(I)*CONST
         ENDDO
C
      END IF
C
C     Work out output error array if appropriate. The constatnt is squared
C     because it is the variance used rather than the standard deviation.
C
      IF(ERRORS)THEN
         DO I=1,NELM
            ERROUT(I)=ERRIN(I)*(CONST**2.)
         ENDDO
      ENDIF
C
      END
