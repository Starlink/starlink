C+
      SUBROUTINE GEN_SUBAFV (NELM, ARRAY1, ARRAY2, ARRAY3,
     :                       Q1DATA, Q2DATA, Q3DATA,
     :                       V1DATA, V2DATA, V3DATA,
     :                       QUAL, FLAGS, FBAD, VARIANCES)
C
C     G E N _ S U B A F V
C
C     Subtracts array2 from array1 into array3. The arrays are
C     floating point and may have any dimensions; they are treated here as
C     linear in order to generate more efficient code.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array
C     (>) ARRAY1   (Real array) Input array
C     (>) ARRAY2   (Real array) Second input array.
C     (<) ARRAY3   (Real array) Result array.  ARRAY3=ARRAY1+ARRAY2
C     (>) Q1DATA  (Byte array) Quality of first input
C     (>) Q2DATA  (Byte array) Quality of second input
C     (<) Q3DATA  (Byte array) Quality of result
C     (>) V1DATA  (Real array) Variances of first input
C     (>) V2DATA  (Real array) Variances of second input
C     (<) V3DATA  (Real array) Variances of output
C     (>) QUAL    (Logical)    T if quality to be used
C     (>) FLAGS   (Logical)    T if flags to be used
C     (>) FBAD    (Real)       Value of bad pixel flag
C     (>) VARIANCES (Logical)  T if variances are to be propagated (Gaussian)
C
C     Note that any of the arrays may be the same.
C
C                                     KS / CIT  18th Feb 1983
C
C     Modified:
C
C     20th Mar  1989  JM / RAL.  Modified to handle quality and errors.
C     31st Oct  1989  JFL/ ROE.  Modified to handle variances, name changed
C                                to distinguish it from basic GEN_SUBAF
C
C
C+
      IMPLICIT NONE
      LOGICAL  VARIANCES, QUAL, FLAGS
      INTEGER  NELM
      BYTE Q1DATA(NELM),Q2DATA(NELM),Q3DATA(NELM)
      REAL V1DATA(NELM),V2DATA(NELM),V3DATA(NELM)
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
         IF (VARIANCES) THEN

C          Image had error data
            DO I=1,NELM
               IF ((Q1DATA(I).EQ.GOOD).AND.(Q2DATA(I).EQ.GOOD))THEN
                  ARRAY3(I) = ARRAY1(I) - ARRAY2(I)
                  V3DATA(I) = V1DATA(I) + V2DATA(I)
                  Q3DATA(I) = GOOD
               ELSE
                  ARRAY3(I) = 0.0
                  V3DATA(I) = 0.0
                  Q3DATA(I) = BAD
               ENDIF
            END DO

         ELSE

            DO I=1,NELM
               IF ((Q1DATA(I).EQ.GOOD).AND.(Q2DATA(I).EQ.GOOD))THEN
                  ARRAY3(I) = ARRAY1(I) - ARRAY2(I)
                  Q3DATA(I) = GOOD
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

            DO I=1,NELM
               IF ((ARRAY1(I).NE.FBAD).AND.(ARRAY2(I).NE.FBAD)) THEN
                  ARRAY3(I) = ARRAY1(I) - ARRAY2(I)
                  V3DATA(I) = V1DATA(I) + V2DATA(I)
               ELSE
                  ARRAY3(I)=BAD
                  V3DATA(I) = 0.0
               END IF
            ENDDO

         ELSE

            DO I=1,NELM
               IF ((ARRAY1(I).NE.FBAD).AND.(ARRAY2(I).NE.FBAD)) THEN
                  ARRAY3(I) = ARRAY1(I) - ARRAY2(I)
               ELSE
                  ARRAY3(I)=BAD
               END IF
            ENDDO

         ENDIF

      ELSE
C
C        Image had no quality information
C
         IF (VARIANCES) THEN

            DO I=1,NELM
               ARRAY3(I) = ARRAY1(I) - ARRAY2(I)
               V3DATA(I) = V1DATA(I) + V2DATA(I)
            ENDDO

         ELSE

            DO I=1,NELM
               ARRAY3(I) = ARRAY1(I) - ARRAY2(I)
            ENDDO

         ENDIF

      END IF

      END
