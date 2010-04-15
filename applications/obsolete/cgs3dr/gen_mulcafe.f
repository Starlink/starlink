C+
      SUBROUTINE GEN_MULCAFE (NELM, CONST, INDATA, OUTDATA, INQUAL,
     :   OUTQUAL, INVAR, OUTVAR, QUAL, FLAGS, FBAD, VARIANCES)
C
C     G E N _ M U L C A F E
C
C     Multiplies a real array by a real constant.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) NELM   (Integer) The number of elements of INDATA.
C     (>) CONST  (Real) The constant by which all the
C                elements of INDATA are to be multiplied.
C     (>) INDATA (Real array INDATA(NELM)) The input array
C                (Note that INDATA may be multiply dimensioned
C                in the calling program.  It is treated as
C                1D here for efficiency and generality.)
C     (<) OUTDATA(Real array OUTDATA(NELM)) The result of the
C                multiplication.  Note that any of the IN-OUT
C                pairs may be the same array
C     (>) INQUAL (Byte array INQUAL(NELM)). Quality array
C                associated with INDATA
C     (<) OUTQUAL Same for OUTDATA
C     (>) INVAR  (Real array INVAR(NELM)) Variance array
C                associated with INDATA
C     (<) OUTVAR Same for OUTVAR
C     (>) QUAL   (Logical) T if quality is to be used
C     (>) FLAGS  (Logical) T if bad pixels are flagged
C     (>) FBAD   (Real) The value used to flag a bad pixel
C     (>) VARIANCES (Logical) T if variances are to be treated
C
C     Subroutines / functions used - None
C
C                                         KS / CIT 1st March 1983
C     Modified:
C
C     4th  Nov 1989  JFL / ROE. Modified to handle quality and variances.
C+
      IMPLICIT NONE
C
C     Parameters
C
      INTEGER NELM
      LOGICAL QUAL, FLAGS, VARIANCES
      BYTE INQUAL(NELM), OUTQUAL(NELM)
      REAL CONST, INDATA(NELM), OUTDATA(NELM), INVAR(NELM),
     :   OUTVAR(NELM)
      REAL FBAD

C
C     Local variable
C
      INTEGER I
      INTEGER GOOD, BAD
      PARAMETER (BAD = 1, GOOD = 0)
C
C     Perform operation
C
      IF (QUAL) THEN
C
C        quality array in use

         IF (VARIANCES) THEN
C
C           error array to be treated as well
C
            DO I = 1, NELM
               OUTDATA(I) = CONST * INDATA(I)
               OUTVAR(I) = CONST * CONST * INVAR(I)
               OUTQUAL(I) = INQUAL(I)
            END DO

         ELSE

            DO I = 1, NELM
               OUTDATA(I) = CONST * INDATA(I)
               OUTQUAL(I) = INQUAL(I)
            END DO

         ENDIF

      ELSE IF (FLAGS) THEN
C
C        bad pixels are flagged
C
         IF (VARIANCES) THEN
C
C           error array to be treated as well
C
            DO I = 1, NELM
               IF (INDATA(I) .NE. FBAD) THEN
                  OUTDATA(I) = CONST * INDATA(I)
                  OUTVAR(I) = CONST * CONST * INVAR(I)
               ELSE
                  OUTDATA(I) = FBAD
                  OUTVAR(I) = CONST * CONST * INVAR(I)
               ENDIF
            END DO

         ELSE

            DO I = 1, NELM
               IF (INDATA(I) .NE. FBAD) THEN
                  OUTDATA(I) = CONST * INDATA(I)
               ELSE
                  OUTDATA(I) = FBAD
               ENDIF
            END DO

         ENDIF

      ELSE
C
C       No quality info at all
C
         IF (VARIANCES) THEN
C
C           error array to be treated as well
C
            DO I = 1, NELM
               OUTDATA(I) = CONST * INDATA(I)
               OUTVAR(I) = CONST * CONST * INVAR(I)
            END DO

         ELSE

            DO I = 1, NELM
               OUTDATA(I) = CONST * INDATA(I)
            END DO

         ENDIF

      ENDIF
C
      END
