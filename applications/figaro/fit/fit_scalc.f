C+
      SUBROUTINE FIT_SCALC (DATA,NELM,CHECK,VMIN,VMAX,SCALES,
     :                                             ZEROS,ERRORS)
C
C     F I T _ S C A L C
C
C     Given a real array, this routine calculates the scale
C     and zero values needed to write it out as a) 8 bit
C     unsigned integers, b) 16 bit signed integers, c) 32 bit
C     signed integers.  The user has to choose which of these
C     representations should be used, trading off precision
C     against compactness of data.
C
C     Parameters -   (">" input, "<" output)
C
C     (>) DATA     (Real array DATA(NELM)) The data array.  This
C                  can have any number of dimensions, but is
C                  treated here as 1-dimensional for generality.
C     (>) NELM     (Integer) The number of elements in DATA.
C     (>) CHECK    (Logical) If true, the routine will pass through
C                  the data applying the scaling for each integer
C                  representaion and then applying the inverse
C                  operation to the result.  The maximum errors
C                  found will be returned in ERRORS.
C     (<) VMIN     (Real) Minimum value in DATA.
C     (<) VMAX     (Real) Maximum value in DATA.
C     (<) SCALES   (Real array SCALES(3)) Gives the
C                  scale factors for the three integer types.
C                  SCALES(1) is for 8 bit, (2) for 16 bit, and
C                  (3) for 32 bit.
C     (<) ZEROS    (Real array ZEROS(3)) Gives the zero values for
C                  the three integer types.
C     (<) ERRORS   (Real array ERRORS(3)) If CHECK is specified,
C                  ERRORS will be returned with the maximum errors
C                  for each of the three representaions.  This may
C                  help in deciding that the loss in precision that
C                  results from an 8 or 16 bit representation will
C                  be acceptable.
C
C     SCALES and ZEROS should be applied as -
C
C     Integer = ( real - ZEROS(N) ) / SCALES(N) + .5
C
C     Common variables used - None
C
C     Subroutines / functions used - None
C
C                                     KS / CIT 10th Oct 1983
C+
      IMPLICIT NONE
C
C     Parameters
C
      LOGICAL CHECK
      INTEGER NELM
      REAL DATA(NELM),VMIN,VMAX,SCALES(3),ZEROS(3),ERRORS(3)
C
C     Local variables
C
      INTEGER I,INTVAL,J
      REAL RANGE,RSCALE(3),RVALUE,VALUE
C
C     Find range of data array
C
      VMIN=DATA(1)
      VMAX=VMIN
      DO I=1,NELM
         IF (DATA(I).GT.VMAX) VMAX=DATA(I)
         IF (DATA(I).LT.VMIN) VMIN=DATA(I)
      END DO
C
C     Now get the three scales and zeros.  Note that we don't
C     try to make use of the VERY ends of the integer ranges, to
C     allow a little slop for rounding errors etc.
C
      RANGE=VMAX-VMIN
      IF (RANGE.LT.1.E-35) THEN
         SCALES(1)=1.
      ELSE
         SCALES(1)=RANGE/252.
      END IF
      IF (RANGE.LT.1.E-30) THEN
         SCALES(2)=1.
      ELSE
         SCALES(2)=RANGE/65530.
      END IF
      IF (RANGE.LT.1.E-25) THEN
         SCALES(3)=1.
      ELSE
         SCALES(3)=RANGE/4.E9
      END IF
      ZEROS(1)=VMIN
      ZEROS(2)=(VMAX+VMIN)*.5
      ZEROS(3)=ZEROS(2)
C
C     Are we to check on the errors?
C
      IF (CHECK) THEN
         DO J=1,3
            ERRORS(J)=0.
            RSCALE(J)=1./SCALES(J)
         END DO
         DO I=1,NELM
            VALUE=DATA(I)
            DO J=1,3
               INTVAL=(VALUE-ZEROS(J))*RSCALE(J)+.5
               RVALUE=(FLOAT(INTVAL)*SCALES(J))+ZEROS(J)
               ERRORS(J)=MAX(ERRORS(J),ABS(VALUE-RVALUE))
            END DO
         END DO
      END IF
C
      END
