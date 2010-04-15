C+
      SUBROUTINE GEN_DIVAFV(NELM,ARRAY1,ARRAY2,ARRAY3,
     :                     Q1DATA,Q2DATA,Q3DATA,
     :                     V1DATA,V2DATA,V3DATA,
     :                     QUAL,FLAGS,FBAD,VARIANCE)
C
C     G E N _ D I V A F V
C
C     Divides two floating point arrays, propagating VARIANCE.
C     The arrays may have any dimensions; they are treated here as
C     linear in order to generate more efficient code.
C     If any divisor element is zero, the result is set to 0
C     or the flagged value if the data in question has an associated
C     bad value flag. If we exceed the machine precision in the
C     variance array then that value is set to zero.
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
C     (>) V1DATA   (Real array) Variance array for input array
C     (>) V2DATA   (Real array) Variance array for second input array
C     (<) V3DATA   (Real array) Variance array for output array
C     (>) QUAL     (Logical)    True if input has quality information
C     (>) FLAGS    (Logical)    True if input has flagged data values
C     (>) FBAD     (Real)       Flag value
C     (>) VARIANCE (Logical)    True if both input arrays have variance arrays
C
C     Note that any of the arrays may be the same.
C
C     THIS ROUTINE _MUST_ BE COMPILED WITH THE /G_FLOAT OPTION ENABLED
C
C     Modified:
C
C     20th Mar  1989  JM / RAL.  Modified to handle quality and errors.
C     12th Dec  1989  KS / AAO.  Now uses variance instead of standard
C                     deviation for errors.
C     30th July 1991  HME / UoE. Typo: must divide by ARRAY2**4.
C     13th Aug  1991  SMB / ROE. Variance calculated first, to guard against
C                     cases where ARRAY1 and ARRAY3 are the same.
C     18th Sept 1991  PND / JAC. Changes division from X*X*X*X to X**4
C      8th Oct  1991  PND / JAC. Dummy variables in double precision.
C      9th Oct  1991  PND / JAC. Added NAG routines X02ALF, X02AKF to
C                                find maximum and minimum real values.
C      9th Oct  1991  PND / JAC. Check array to see if out of machine
C                                range and if so set variance to zero.
C                                Note that this subroutine _must_ be
C                                compiled with the /G_FLOAT qualifier.
C      9th Jan 1996 PND / JAC.   Remove NAG for VAL__ constants.
C+
      IMPLICIT NONE
      INCLUDE 'PRM_PAR'
      LOGICAL  VARIANCE, QUAL, FLAGS
      INTEGER  NELM
      BYTE     Q1DATA(NELM), Q2DATA(NELM), Q3DATA(NELM)
      REAL     V1DATA(NELM), V2DATA(NELM), V3DATA(NELM)
      REAL     ARRAY1(NELM), ARRAY2(NELM), ARRAY3(NELM)
      REAL     FBAD
      INTEGER  I
      DOUBLE PRECISION A1SQ, A2SQ, A2QU, V1V, V2V, V3V
C
C     Quality values defined symbolically
C
      INTEGER   GOOD, BAD
      PARAMETER ( BAD = 1, GOOD = 0 )
C
C     Maximum and minimum range of values from NAG routines
C
      A1SQ = 0D0                    ! Holds ARRAY1**2 in double precision
      A2SQ = 0D0                    ! Holds ARRAY2**2 in double precision
      A2QU = 0D0                    ! Holds ARRAY2**4 in double precision
      V1V  = 0D0                    ! Initial V1DATA as double precision VALUE
      V2V  = 0D0                    ! Initial V2DATA as double precision VALUE
      V3V  = 0D0                    ! Initial V3DATA as double precision VALUE
C
C     Handle different quality methods separately.
C
      IF ( QUAL ) THEN
C
C        Image had quality data
C
         DO I = 1, NELM
            IF ( (Q1DATA(I).EQ.GOOD) .AND. (Q2DATA(I).EQ.GOOD) ) THEN
               IF ( ARRAY2(I).EQ.0. ) THEN
                  Q3DATA(I)=BAD
                  ARRAY3(I)=0.
               ELSE
                  IF ( VARIANCE ) THEN
                     A1SQ = DBLE( ARRAY1(I) ) * DBLE( ARRAY1(I) )
                     A2SQ = DBLE( ARRAY2(I) ) * DBLE( ARRAY2(I) )
                     A2QU = A2SQ * A2SQ
                     V1V = DBLE( V1DATA(I) )
                     V2V = DBLE( V2DATA(I) )
                     V3V = ( ( V1V/A2SQ ) + ( (V2V*A1SQ)/A2QU) )
                     IF ( V3V.GT.(DBLE(VAL__MAXR)) ) THEN
                        V3DATA(I)=0.0
                     ELSE IF ( V3V.LT.(DBLE(VAL__MINR)) ) THEN
                        V3DATA(I)=0.0
                     ELSE
                        V3DATA(I)=REAL(V3V)
                     ENDIF
                  END IF
                  ARRAY3(I)=ARRAY1(I)/ARRAY2(I)
               END IF
            ELSE
               Q3DATA(I)=BAD
            ENDIF
         END DO

      ELSE IF ( FLAGS ) THEN
C
C        Image had flagged data values
C
         DO I=1,NELM
            IF ( (ARRAY1(I).NE.FBAD) .AND. (ARRAY2(I).NE.FBAD) ) THEN
               IF (ARRAY2(I).EQ.0.) THEN
                  ARRAY3(I)=FBAD
               ELSE
                  IF ( VARIANCE ) THEN
                     A1SQ = DBLE( ARRAY1(I) ) * DBLE( ARRAY1(I) )
                     A2SQ = DBLE( ARRAY2(I) ) * DBLE( ARRAY2(I) )
                     A2QU = A2SQ * A2SQ
                     V1V = DBLE( V1DATA(I) )
                     V2V = DBLE( V2DATA(I) )
                     V3V = ( (V1V/A2SQ) + ((V2V*A1SQ)/A2QU) )
                     IF ( V3V.GT.(DBLE(VAL__MAXR)) ) THEN
                        V3DATA(I)=0.0
                     ELSE IF ( V3V.LT.(DBLE(VAL__MINR)) ) THEN
                        V3DATA(I)=0.0
                     ELSE
                        V3DATA(I)=REAL(V3V)
                     ENDIF
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
         IF ( VARIANCE ) THEN
            DO I=1,NELM
               IF (ARRAY2(I).EQ.0.) THEN
                  ARRAY3(I)=0.
               ELSE
                  A1SQ = DBLE( ARRAY1(I) ) * DBLE( ARRAY1(I) )
                  A2SQ = DBLE( ARRAY2(I) ) * DBLE( ARRAY2(I) )
                  A2QU = A2SQ * A2SQ
                  V1V = DBLE( V1DATA(I) )
                  V2V = DBLE( V2DATA(I) )
                  V3V = ( (V1V/A2SQ) + ((V2V*A1SQ)/A2QU) )
                  IF ( V3V.GT.(DBLE(VAL__MAXR)) ) THEN
                     V3DATA(I)=0.0
                  ELSE IF ( V3V.LT.(DBLE(VAL__MINR)) ) THEN
                     V3DATA(I)=0.0
                  ELSE
                     V3DATA(I)=REAL(V3V)
                  ENDIF
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
