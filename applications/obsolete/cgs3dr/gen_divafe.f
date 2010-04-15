C+
      SUBROUTINE GEN_DIVAFE (NELM,ARRAY1,ARRAY2,ARRAY3,
     :                       Q1DATA,Q2DATA,Q3DATA,
     :                       V1DATA,V2DATA,V3DATA,
     :                       QUAL,FLAGS,FBAD,VARIANCES)

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
C     (>) Q1DATA  (Byte array) Quality of first input
C     (>) Q2DATA  (Byte array) Quality of second input
C     (<) Q3DATA  (Byte array) Quality of result
C     (>) E1DATA  (Real array) Variances of first input
C     (>) E2DATA  (Real array) Variances of second input
C     (<) E3DATA  (Real array) Variances of output
C     (>) QUAL    (Logical)    T if quality to be used
C     (>) FLAGS   (Logical)    T if flags to be used
C     (>) FBAD    (Real)       Value of bad pixel flag
C     (>) VARIANCES (Logical)    T if variances are to be propagated (Gaussian)
C
C     Note that any of the arrays may be the same.
C
C     Modified:
C
C     20th Mar  1989  JM / RAL.  Modified to handle quality and errors.
C     4th  Nov  1989  JFL / ROE. Name changed to GEN_DIVAFE from GEN_DIVAF.
C     20th Nov  1989  SMB / ROE. History updated.
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
C
C           Image had error info
C
            DO I=1,NELM
               IF ((Q1DATA(I).EQ.GOOD).AND.(Q2DATA(I).EQ.GOOD)) THEN
                  IF (ARRAY2(I).EQ.0.) THEN
                     Q3DATA(I)=BAD
                     ARRAY3(I)=0.
                     V3DATA(I) = 0.0
                  ELSE
*                   do variances first before ARRAY3 is set, this may be
*                   the same as ARRAY1 or 2 remember in which case the
*                   variances must be calculated before the input data
*                   is messed up
                     V3DATA(I) = V1DATA(I)/(ARRAY2(I)*ARRAY2(I)) +
     :                           V2DATA(I)*ARRAY1(I)*ARRAY1(I)/
     :                           ARRAY2(I)**4
                     ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
                  END IF
               ELSE
                  ARRAY3(I) = 0.0
                  V3DATA(I) = 0.0
                  Q3DATA(I)=BAD
               ENDIF
            END DO

         ELSE

            DO I=1,NELM
               IF ((Q1DATA(I).EQ.GOOD).AND.(Q2DATA(I).EQ.GOOD)) THEN
                  IF (ARRAY2(I).EQ.0.) THEN
                     Q3DATA(I)=BAD
                     ARRAY3(I)=0.
                  ELSE
                     ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
                  END IF
               ELSE
                  ARRAY3(I) = 0.0
                  Q3DATA(I)=BAD
               ENDIF
            END DO

         ENDIF

      ELSE IF (FLAGS) THEN
C
C        Image had flagged data values
C
         IF (VARIANCES) THEN
C
C           Image had error info
C
            DO I=1,NELM
               IF ((ARRAY1(I).NE.FBAD).AND.(ARRAY2(I).NE.FBAD)) THEN
                  IF (ARRAY2(I).EQ.0.) THEN
                     ARRAY3(I)=FBAD
                     V3DATA(I) = 0.0
                  ELSE
                     V3DATA(I) = V1DATA(I)/(ARRAY2(I)*ARRAY2(I)) +
     :                           V2DATA(I)*ARRAY1(I)*ARRAY1(I)/
     :                           ARRAY2(I)**4
                     ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
                  END IF
               ELSE
                  ARRAY3(I)=FBAD
                  V3DATA(I) = 0.0
               END IF
            ENDDO

         ELSE

            DO I=1,NELM
               IF ((ARRAY1(I).NE.FBAD).AND.(ARRAY2(I).NE.FBAD)) THEN
                  IF (ARRAY2(I).EQ.0.) THEN
                     ARRAY3(I)=FBAD
                  ELSE
                     ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
                  END IF
               ELSE
                  ARRAY3(I)=FBAD
               END IF
            ENDDO

         ENDIF

      ELSE
C
C        Image had no quality information
C
         IF (VARIANCES) THEN
C
C           Image had error info
C
            DO I=1,NELM
               IF (ARRAY2(I).EQ.0.) THEN
                  ARRAY3(I)=0.
                  V3DATA(I) = 0.0
               ELSE
                  V3DATA(I) = V1DATA(I)/(ARRAY2(I)*ARRAY2(I)) +
     :                        V2DATA(I)*ARRAY1(I)*ARRAY1(I)/
     :                        ARRAY2(I)**4
                  ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
               ENDIF
            ENDDO

         ELSE

            DO I=1,NELM
               IF (ARRAY2(I).EQ.0.) THEN
                  ARRAY3(I)=0.
               ELSE
                  ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
               ENDIF
            ENDDO

         ENDIF

      END IF

      END

