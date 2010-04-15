C+
      SUBROUTINE GEN_CLEANE (NELM, DATA, VARIANCE, QUALITY, CUT,
     :   LIMIT, QUAL, FLAGS, FBAD)
C
C     G E N _ C L E A N E
C
C     Searches through the data and variance arrays, setting quality
C     or magic values to bad whenever the signal to noise falls below CUT
C     or whenever the absolute data value falls below LIMIT.
C
C     Parameters -    (">" input, "<" output)
C
C     (>) NELM     (Integer) Number of elements in each array
C     (>) DATA     (Real array) Input data array
C     (>) VARIANCE (Real array) Input variance array
C     (<) QUALITY  (Byte array) Quality array
C     (>) CUT      (Real) Signal to noise below which quality is to be set bad
C     (>) LIMIT    (Real) Data value below which quality is to be set bad.
C     (>) QUAL     (Logical) True if using quality array
C     (>) FLAGS    (Logical) True if using magic values
C     (>) FBAD     (Real) Magic value to be used
C
C      17th Oct  1989  JFL / ROE. Original version.
C      20th Nov  1989  SMB / ROE. History added. Bug, in which BAD and
C                                 FBAD mixed up, fixed. CUTSQ introduced
C                                 for efficiency. Kludge removed by
C                                 introducing a minimum data value, LIMIT.
C      7th  Nov  1990  SMB / ROE. Testing real values for equality
C                                 removed, as this is bad practise.
C
C+
      IMPLICIT NONE
      INTEGER  NELM
      INTEGER I
      LOGICAL QUAL, FLAGS
      BYTE QUALITY (NELM)
      REAL CUT, LIMIT
      REAL FBAD
      REAL DATA (NELM)
      REAL VARIANCE (NELM)
      REAL SNRSQ, CUTSQ
C
C     Quality values defined symbolically
C
      INTEGER  GOOD, BAD
      PARAMETER (BAD = 1, GOOD = 0)
C
C     Initialise CUTSQ
C
      CUTSQ = CUT * CUT
C
C     Handle different quality methods separately.
C
      IF (QUAL) THEN
C
C        Data has a quality array
C
         DO I = 1, NELM
            IF (QUALITY(I) .EQ. GOOD) THEN
C
C              Set the data to "bad" if the Signal to noise < CUT
C
               IF (VARIANCE(I) .GT. 0.0) THEN
                  SNRSQ = DATA(I)*DATA(I) / VARIANCE(I)
                  IF (SNRSQ .LT. CUTSQ) THEN
                     QUALITY (I) = BAD
                  ENDIF
               ENDIF
C
C              Set the data to "bad" if the Data value < LIMIT
C
               IF (DATA(I) .LT. LIMIT) THEN
                  QUALITY (I) = BAD
               ENDIF
            ENDIF
         END DO

      ELSE IF (FLAGS) THEN
C
C        Data has magic values
C
         DO I = 1, NELM
            IF (DATA(I) .NE. FBAD) THEN
C
C              Set the data to "bad" if the Signal to noise < CUT
C
               IF (VARIANCE(I) .GT. 0.0) THEN
                  SNRSQ = DATA(I)*DATA(I) / VARIANCE(I)
                  IF (SNRSQ .LT. CUTSQ) THEN
                     DATA (I) = FBAD
                  ENDIF
               ENDIF
C
C              Set the data to "bad" if the Data value < LIMIT
C
               IF (DATA(I) .LT. LIMIT) THEN
                  DATA (I) = FBAD
               ENDIF
            ENDIF
         END DO

      ENDIF

      END
