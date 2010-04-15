C+
      SUBROUTINE GEN_SCALE (NELM,ARRAY1,ARRAY3,Q1DATA,Q3DATA,
     :                       V1DATA,V3DATA,QUAL,FLAGS,FBAD,VARIANCES,
     :                       FACTOR,CONSTANT)
C
C     G E N _ S C A L E
C
C     Scales a floating point array. The input array may have
C     any dimensions; it is treated here as linear in order to
C     generate more efficient code.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array
C     (>) ARRAY1   (Real array) Input array
C     (<) ARRAY3   (Real array) Result array.  ARRAY3=ARRAY1*ARRAY2
C     (>) Q1DATA  (Byte array) Quality of first input
C     (<) Q3DATA  (Byte array) Quality of result
C     (>) V1DATA  (Real array) Variances of first input
C     (<) V3DATA  (Real array) Variances of output
C     (>) QUAL    (Logical)    T if quality to be used
C     (>) FLAGS   (Logical)    T if flags to be used
C     (>) FBAD    (Real)       Value of bad pixel flag
C     (>) VARIANCES (Logical)    T if variances are to be propagated (Gaussian)
C     (>) FACTOR  (Real) Factor to multiply data by
C     (>) CONSTANT (Real) Constant to add to data
C
C     Note that any of the arrays may be the same.
C
C     Modified:
C
C     24th Sep  1990  AB / JAC   Original (from GEN_ADDAFE)
C+
      IMPLICIT NONE
      LOGICAL  VARIANCES, QUAL, FLAGS
      INTEGER  NELM
      BYTE Q1DATA(NELM),Q3DATA(NELM)
      REAL V1DATA(NELM),V3DATA(NELM)
      REAL ARRAY1(NELM),ARRAY3(NELM)
      REAL     FBAD
      REAL FACTOR, CONSTANT
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
         IF (VARIANCES) THEN
C
C           Image had error information
C
            DO I=1,NELM
               IF (Q1DATA(I).EQ.GOOD) THEN

C                Do variances first before ARRAY3 is set, this may be
C                the same as ARRAY1 remember in which case the
C                variances must be calculated before the input data
C                is messed up.

                  V3DATA(I) = V1DATA(I) * FACTOR * FACTOR
                  ARRAY3(I) = ARRAY1(I) * FACTOR + CONSTANT
               ELSE
                  ARRAY3(I) = 0.0
                  V3DATA(I) = 0.0
                  Q3DATA(I) = BAD
               ENDIF
            END DO

         ELSE

            DO I=1,NELM
               IF (Q1DATA(I).EQ.GOOD) THEN
                  ARRAY3(I) = ARRAY1(I) * FACTOR + CONSTANT
               ELSE
                  ARRAY3(I) = 0.0
                  Q3DATA(I) = BAD
               ENDIF
            END DO

         ENDIF

      ELSE IF (FLAGS) THEN
C
C        Image had flagged data values
C
         IF (VARIANCES) THEN
C
C           Image had error information
C
            DO I=1,NELM
               IF (ARRAY1(I).NE.FBAD) THEN
                  V3DATA(I) = V1DATA(I) * FACTOR * FACTOR
                  ARRAY3(I) = ARRAY1(I) * FACTOR + CONSTANT
               ELSE
                  V3DATA(I) = 0.0
                  ARRAY3(I) = FBAD
               END IF
            ENDDO

         ELSE

            DO I=1,NELM
               IF (ARRAY1(I).NE.FBAD) THEN
                  ARRAY3(I) = ARRAY1(I) * FACTOR + CONSTANT
               ELSE
                  ARRAY3(I) = FBAD
               ENDIF
            END DO

         ENDIF

      ELSE
C
C        Image had no quality information
C
         IF (VARIANCES) THEN
C
C           Image had error info
            DO I=1,NELM
               V3DATA(I) = V1DATA(I) * FACTOR * FACTOR
               ARRAY3(I) = ARRAY1(I) * FACTOR + CONSTANT
            ENDDO

         ELSE

            DO I=1,NELM
               ARRAY3(I) = ARRAY1(I) * FACTOR + CONSTANT
            ENDDO

         ENDIF

      END IF

      END
