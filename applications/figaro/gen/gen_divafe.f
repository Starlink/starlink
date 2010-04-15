C+
      SUBROUTINE GEN_DIVAFE(NELM,ARRAY1,ARRAY2,ARRAY3,
     :                     Q1DATA,Q2DATA,Q3DATA,
     :                     E1DATA,E2DATA,E3DATA,
     :                     QUAL,FLAGS,FBAD,VARIANCE)
C
C     G E N _ D I V A F E
C
C     Divides two floating point arrays.  The arrays
C     may have any dimensions; they are treated here as
C     linear in order to generate more efficient code.
C     If any divisor element is zero, the result is set to 0
C     or the flagged value if the data in question has an associated
C     bad value flag.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array
C     (>) ARRAY1   (Real array) Input array
C     (>) ARRAY2   (Real array) Second input array.
C     (<) ARRAY3   (Real array) Result array.  ARRAY3=ARRAY1/ARRAY2
C     (>) Q1DATA   (Byte array) Quality array for first input array
C     (>) Q2DATA   (Byte array) Quality array for second input array
C     (<) Q3DATA   (Byte array) Quality array for output array
C     (>) E1DATA   (Real array) Variance array for input array
C     (>) E2DATA   (Real array) Variance array for second input array
C     (<) E3DATA   (Real array) Variance array for output array
C     (>) QUAL     (Logical)    True if input has quality information
C     (>) FLAGS    (Logical)    True if input has flagged data values
C     (>) FBAD     (Real)       Flag value
C     (>) VARIANCE (Logical)    True if both input arrays have variance arrays
C
C     Note that any of the arrays may be the same.
C
C     Modified:
C
C     20th Mar  1989  JM / RAL.  Modified to handle quality and errors.
C     12th Dec  1989  KS / AAO.  Now uses variance instead of standard
C                     deviation for errors.
C     30th July 1991  HME / UoE. Typo: must divide by ARRAY2**4.
C     30th Jan  1992  HME / UoE, Starlink. Calculate variance before
C                     data. This is vital if the output array is one of
C                     the inputs.
C+
      IMPLICIT NONE
      LOGICAL  VARIANCE, QUAL, FLAGS
      INTEGER  NELM
      BYTE Q1DATA(NELM),Q2DATA(NELM),Q3DATA(NELM)
      REAL E1DATA(NELM),E2DATA(NELM),E3DATA(NELM)
      REAL ARRAY1(NELM),ARRAY2(NELM),ARRAY3(NELM)
      REAL     FBAD
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
            IF ((Q1DATA(I).EQ.GOOD).AND.(Q2DATA(I).EQ.GOOD)) THEN
               IF (ARRAY2(I).EQ.0.) THEN
                  Q3DATA(I)=BAD
                  ARRAY3(I)=0.
               ELSE
                  IF(VARIANCE)THEN
                     E3DATA(I)=E1DATA(I)/(ARRAY2(I)*ARRAY2(I)) +
     :                  (E2DATA(I)*ARRAY1(I)*ARRAY1(I))/
     :                  (ARRAY2(I)*ARRAY2(I)*ARRAY2(I)*ARRAY2(I))
                  END IF
                  ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
               END IF
            ELSE
               Q3DATA(I)=BAD
            ENDIF
         END DO

      ELSE IF (FLAGS) THEN
C
C        Image had flagged data values
C
         DO I=1,NELM
            IF ((ARRAY1(I).NE.FBAD).AND.(ARRAY2(I).NE.FBAD)) THEN
               IF (ARRAY2(I).EQ.0.) THEN
                  ARRAY3(I)=FBAD
               ELSE
                  IF(VARIANCE)THEN
                     E3DATA(I)=E1DATA(I)/(ARRAY2(I)*ARRAY2(I)) +
     :                  (E2DATA(I)*ARRAY1(I)*ARRAY1(I))/
     :                  (ARRAY2(I)*ARRAY2(I)*ARRAY2(I)*ARRAY2(I))
                  END IF
                  ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
               END IF
            ELSE
               ARRAY3(I)=FBAD
            END IF
         ENDDO
      ELSE
C
C        Image had no quality information
C
         IF (VARIANCE) THEN
            DO I=1,NELM
               IF (ARRAY2(I).EQ.0.) THEN
                  ARRAY3(I)=0.
               ELSE
                  E3DATA(I)=E1DATA(I)/(ARRAY2(I)*ARRAY2(I)) +
     :                  (E2DATA(I)*ARRAY1(I)*ARRAY1(I))/
     :                  (ARRAY2(I)*ARRAY2(I)*ARRAY2(I)*ARRAY2(I))
                  ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
               ENDIF
            ENDDO
         ELSE
            DO I=1,NELM
               IF (ARRAY2(I).EQ.0.) THEN
                  ARRAY3(I)=0.
               ELSE
                  ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
               END IF
            ENDDO
         END IF
C
      END IF
C
      END
