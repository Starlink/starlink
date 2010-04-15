
C===========================================================================

      FUNCTION PERIOD_CVAL(ARRAY, I, MARR)

C===========================================================================
C Returns the "value" of the complex ARRAY at the "location" I.
C If I > 0 then it is equivalent to ARRAY(I), but if I < 0,
C it returns the complex conjugate of ARRAY(-I).  ARRAY must be
C indexed from 0 to at least ABS(I) and DOUBLE COMPLEX.
C If ABS(I) > MARR (maximum element in array), PERIOD_CVAL=0.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER I,MARR,LOC
      DOUBLE COMPLEX PERIOD_CVAL
      DOUBLE COMPLEX ARRAY(0:MARR)

C---------------------------------------------------------------------------
C Set value to conjugate of absolute location if necessary.
C---------------------------------------------------------------------------

      LOC = IABS(I)
      IF ( LOC.GT.MARR ) THEN
         PERIOD_CVAL = (0.0D0, 0.0D0)
         RETURN
      END IF
      IF ( I.GE.0 ) PERIOD_CVAL = ARRAY(LOC)
      IF ( I.LT.0 ) PERIOD_CVAL = DCONJG(ARRAY(LOC))

      RETURN
      END
