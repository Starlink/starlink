C+
      SUBROUTINE GEN_POWEREQ(ARRAY1,NELM,POWER,QUAL,Q1DATA,
     :                       FLAGS,FBAD,ERRORS,E1DATA,
     :                       E2DATA,Q2DATA,ARRAY2,FAILURES)
C
C  Routine name:
C     GEN_POWEREQ
C
C  Function:
C     Raises a real array to a specified power.
C
C  Description:
C     GEN_POWEREQ raises each value of a real array to a specified power.
C     This is an extension of the simpler routine GEN_POWER, modified to
C     support data quality arrays, flagged data values, and error arrays.
C     It checks for the three possible cases where:- 1) a quality array
C     exists, 2) flagged data values are used, and 3) there is no quality
C     information supplied at all.  In all three cases, it checks whether
C     error arrays exist, and if so, it calculates the error value for each
C     new array element.  If a data element cannot be raised to the desired
C     power (either because it is negative and the power is not an integer,
C     or because the result would be too large or too small) it is set to
C     a default value (usually 0.0) or flagged. In these cases, the variance
C     is set to the same value as the pixel value (so it's obvious that
C     particular variance is bad).
C
C  Language:
C     FORTRAN
C
C  Call:
C     CALL GEN_POWEREQ(ARRAY1,NELM,POWER,QUAL,Q1DATA,FLAGS,FBAD,
C                          ERRORS,E1DATA,E2DATA,Q2DATA,ARRAY2,FAILURES)
C
C  Parameters: (">" input, "!" modified, "W" workspace, "<" ouput)
C
C     (>) ARRAY1    (Real array,ref) The input data array.
C     (>) NELM      (Integer,ref) The number of elements in ARRAY1 & ARRAY2
C     (>) POWER     (Real,ref) The power to which the data is to be raised
C     (>) QUAL      (Logical,ref) True if quality data exists
C     (>) Q1DATA    (Byte array,ref) Original quality data array (good=0,bad=1)
C     (>) FLAGS     (Logical,ref) True if flagged data exist
C     (>) FBAD      (Real,ref) Value of flagged data
C     (>) ERRORS    (Logical,ref) True if error values exist for data
C     (>) E1DATA    (Real array,ref) Original error values
C     (<) E2DATA    (Real array,ref) Error values for modified data
C     (<) Q2DATA    (Byte array,ref) Quality array for modified data
C     (<) ARRAY2    (Real array,ref) Data raised to desired power
C     (<) FAILURES  (Integer,ref) # of times data could not be raised to power
C
C  External subroutines / functions used:  GEN_SIMILAR
C
C  Support:  Keith Shortridge, AAO
C
C  Version Date: 8th Jan. 1991
C-
C  History:
C     8th Jan  1991.  JMS/AAO. Original version, closely based on a routine
C                     by JM/RAL.
C     24th Mar 1997.  JJL / Starlink, Southampton. Fixed a bug in the
C                     error calculation and made it appropriate for
C                     a variance rather than an uncertainty.
C+
C
      IMPLICIT NONE
C
C     Functions
C
      LOGICAL GEN_SIMILAR
C
C     Local variables
C
      LOGICAL  ERRORS, QUAL, FLAGS, DECPOWER, SQROOT
      INTEGER  NELM
      BYTE Q1DATA(NELM),Q2DATA(NELM)
      REAL E1DATA(NELM),E2DATA(NELM)
      REAL ARRAY1(NELM),ARRAY2(NELM)
      REAL FBAD
      REAL POWER
      REAL MAX
      REAL VALUE
      REAL SIGN
      INTEGER FAILURES
      INTEGER I
C
C     Set maximum and minimum number sizes (arbitrarily, we choose to
C     use the VAX floating point limits)
C
      REAL FMAX,FMIN
      PARAMETER (FMAX = 1.7E+38)
      PARAMETER (FMIN = 1.7E-38)
C
C     Quality values defined symbolically
C
      INTEGER  GOOD, BAD
      PARAMETER (BAD = 1, GOOD = 0)
C
C     Initialise number of failures and the max size of the number to
C     be raised to the required power.
C
      FAILURES = 0
C
      IF (POWER.GT.1.0) THEN
         MAX=FMAX**(1.0/POWER)
      ELSE IF (POWER.LT.-1.0) THEN
         MAX=(1/FMIN)**(1/ABS(POWER))
      ELSE
         MAX=FMAX
      END IF
C
C     Test whether POWER has a decimal value
C
      DECPOWER=(POWER.NE.FLOAT(IFIX(POWER)))
C
C     If power is an integer then determine whether it is odd or even.
C
      IF (.NOT.DECPOWER) THEN
         IF (AMOD(POWER,2.0).NE.0.0) THEN
            SIGN = -1.0
         ELSE
            SIGN = 1.0
         END IF
      END IF
C
C     If POWER=0.5 then use SQRT for efficiency. Determine if necessary.
C
      SQROOT=(GEN_SIMILAR(POWER,0.5))
C
C     Handle different quality methods separately.  In all cases, for
C     good elements only, sum elements for data, sum squares of errors
C     and finally take root of sum as final error.  If any pixels are
C     bad, flag the final pixel - either using a flag value or the quality
C     array.  If any pixels cannot be raised to the required power (-ve
C     values to a non-integer power, or values too large) increment
C     failure count and set to a suitable value.
C
      IF (QUAL) THEN
C
C        Image had quality data.  In this case, process all element values
C        irrespective of their quality (since some value has to be put into
C        the output arrays, we might as well use the value we'd get if
C        the data were not flagged as bad). Copy the input quality array
C        to the output quality array.
C
         DO I=1,NELM
C
C           Check that element can be raised to required power
C
            IF (((ARRAY1(I).LT.0.0).AND.(DECPOWER))
     :         .OR.(ARRAY1(I).GT.MAX)) THEN
C
C              It can't, so set to a suitable value and increment failure count
C              and set quality array element to bad as well.
C
               FAILURES = FAILURES + 1
               Q2DATA(I)=BAD
               IF ((ARRAY1(I).GT.MAX).AND.(POWER.GT.0.0)) THEN
                  ARRAY2(I)=FMAX
                  IF (ERRORS) E2DATA(I)=FMAX
               ELSE
                  ARRAY1(I)=0.0
                  IF (ERRORS) E2DATA(I)=0.0
               END IF
            ELSE
C
C              Value can be processed.  Note optimisation of using SQRT for
C              a power of 0.5.
C
               VALUE=ARRAY1(I)
               IF (VALUE.LT.0.0) THEN
                  ARRAY2(I)=SIGN*(ABS(VALUE)**POWER)
               ELSE
                  IF (SQROOT) THEN
                     ARRAY2(I)=SQRT(VALUE)
                  ELSE
                     ARRAY2(I)= VALUE**POWER
                  END IF
               END IF
               IF(ERRORS)THEN
C        Don't have to sqare E1DATA as it is a variance.
                  E2DATA(I)=E1DATA(I)*((ARRAY2(I)*POWER/VALUE)**2.0)
               ENDIF
               Q2DATA(I)=Q1DATA(I)
            END IF
         END DO

      ELSE IF (FLAGS) THEN
C
C        Image had flagged data values.  Treat similarly to quality case,
C        but if value cannot be raised to power then set output element
C        to the flagged value.
C
         DO I=1,NELM
            IF (((ARRAY1(I).LT.0.0).AND.(DECPOWER))
     :         .OR.(ARRAY1(I).GT.MAX)) THEN
               FAILURES = FAILURES + 1
               IF (ARRAY1(I).GT.MAX) THEN
                  IF (POWER.LT.0.0) THEN
                     ARRAY2(I)=0.0
                     IF (ERRORS) E2DATA(I)=0.0
                  ELSE
                     ARRAY2(I)=FMAX
                     IF (ERRORS) E2DATA(I)=FMAX
                  END IF
               ELSE
                  ARRAY1(I)=FBAD
                     IF (ERRORS) E2DATA(I)=FBAD
               END IF
            ELSE
               IF (ARRAY1(I).NE.FBAD)THEN
                  VALUE=ARRAY1(I)
                  IF (VALUE.LT.0.0) THEN
                     ARRAY2(I)=SIGN*(ABS(VALUE)**POWER)
                  ELSE
                     IF (SQROOT) THEN
                        ARRAY2(I)=SQRT(VALUE)
                     ELSE
                        ARRAY2(I)= VALUE**POWER
                     END IF
                  END IF
                  IF (ERRORS) THEN
C     Make sure that VALUE is not zero. This leads to a divide by
C     zero problem and a badly defined error propagation. If it
C     is zero, flag the data as BAD.
                    IF (VALUE.EQ.0.0) THEN
                       Q2DATA(I) = BAD
                       ARRAY2(I) = 0.0
                       E2DATA(I) = 0.0
                    ELSE
                       E2DATA(I)=E1DATA(I)*((ARRAY2(I)*POWER/
     :                                       VALUE)**2.0)
                    ENDIF
                  END IF
               ELSE
                  ARRAY2(I)=FBAD
                  IF (ERRORS) E2DATA(I)=FBAD
               ENDIF
            END IF
         END DO
C
      ELSE
C
C        Image had neither quality information nor flagged data values
C
         DO I=1,NELM
            IF (((ARRAY1(I).LT.0.0).AND.(DECPOWER))
     :         .OR.(ARRAY1(I).GT.MAX)) THEN
             FAILURES = FAILURES + 1
             IF ((ARRAY1(I).GT.MAX).AND.(POWER.GT.0.0)) THEN
                ARRAY2(I)=FMAX
                IF (ERRORS) E2DATA(I)=FMAX
             ELSE
                ARRAY1(I)=0.0
                IF (ERRORS) E2DATA(I)=0.0
             END IF
          ELSE
             VALUE=ARRAY1(I)
                 IF (VALUE.LT.0.0) THEN
                    ARRAY2(I)=SIGN*(ABS(VALUE)**POWER)
                 ELSE
                    IF (SQROOT) THEN
                       ARRAY2(I)=SQRT(VALUE)
                    ELSE
                       ARRAY2(I)= VALUE**POWER
                    END IF
                 END IF
             IF (ERRORS) THEN
C     Make sure that VALUE is not zero. This leads to a divide by
C     zero problem and a badly defined error propagation. If it
C     is zero, flag the data as BAD.
                    IF (VALUE.EQ.0.0) THEN
                       Q2DATA(I) = BAD
                       ARRAY2(I) = 0.0
                       E2DATA(I) = 0.0
                    ELSE
                       E2DATA(I)=E1DATA(I)*
     :                         ((ARRAY2(I)*POWER/VALUE)**2.0)
                    ENDIF
             END IF
          END IF
         ENDDO

      END IF
C
      END
