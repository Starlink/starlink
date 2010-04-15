C+
      SUBROUTINE FIT_SCALD (DATA,NELM,CHECK,VMIN,VMAX,SCALES,
     :                                             ZEROS,ERRORS)
C
C     F I T _ S C A L D
C
C     Given a double precision array, this routine calculates
C     the scale and zero values needed to write it out as a) 8 bit
C     unsigned integers, b) 16 bit signed integers, c) 32 bit
C     signed integers.  The user has to choose which of these
C     representations should be used, trading off precision
C     against compactness of data.
C
C     This is a double precision version of FIT_SCALC
C
C     Parameters -   (">" input, "<" output)
C
C     (>) DATA     (Double precision array DATA(NELM)) The data
C                  array.  This can have any number of dimensions,
C                  but is treated here as 1-dimensional for generality.
C     (>) NELM     (Integer) The number of elements in DATA.
C     (>) CHECK    (Logical) If true, the routine will pass through
C                  the data applying the scaling for each integer
C                  representaion and then applying the inverse
C                  operation to the result.  The maximum errors
C                  found will be returned in ERRORS.
C     (<) VMIN     (Double precision) Minimum value in DATA.
C     (<) VMAX     (Double precision) Maximum value in DATA.
C     (<) SCALES   (Double precision array SCALES(3)) Gives the
C                  scale factors for the three integer types.
C                  SCALES(1) is for 8 bit, (2) for 16 bit, and
C                  (3) for 32 bit.
C     (<) ZEROS    (Double precision array ZEROS(3)) Gives the zero
C                  values for the three integer types.
C     (<) ERRORS   (Double precision array ERRORS(3)) If CHECK is
C                  specified, ERRORS will be returned with the maximum
C                  errors for each of the three representaions.  This
C                  may help in deciding that the loss in precision that
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
      DOUBLE PRECISION DATA(NELM),VMIN,VMAX,SCALES(3),ZEROS(3)
      DOUBLE PRECISION ERRORS(3)
C
C     Local variables
C
      INTEGER I,INTVAL,J
      DOUBLE PRECISION RANGE,RSCALE(3),RVALUE,VALUE
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
