
C===========================================================================

      FUNCTION PERIOD_HWHM(M, ARRAY)

C===========================================================================
C Finds the half-width, half-maximum of the CABS of ARRAY(0:M).
C This is done by linear interpolation between successive elements of ARRAY.
C Returns the HWHM in the units of the array elements.
C
C Adapted for PERIOD by Vikram Singh Dhillon @Sussex 24-June-1992.
C
C Unused variable BELL removed - GJP June 1995
C
C Converted to Double Precision (KPD), August 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER M,I
      DOUBLE COMPLEX ARRAY(0:M)
      DOUBLE PRECISION PERIOD_HWHM
      DOUBLE PRECISION HALFMX, HALFWD, LAST, CURRENT, XLAST, SLOPE

C---------------------------------------------------------------------------
C Initial conditions for search.
C---------------------------------------------------------------------------

      HALFMX = CDABS(ARRAY(0))/2.0D0 ! Half-maximum (assume maximum at I=0).
      HALFWD = 0.0D0                 ! Half-width not yet found.

C---------------------------------------------------------------------------
C Loop through until less than HALFMX.
C---------------------------------------------------------------------------

      DO 100 I = 1, M
         CURRENT = CDABS(ARRAY(I))               ! Current array value.
         IF ( CURRENT.LT.HALFMX ) THEN
            LAST = CDABS(ARRAY(I-1))             ! Last array value.
            XLAST = DFLOAT(I-1)                  ! Last "x" co-ordinate.
            SLOPE = 1.0D0/(CURRENT-LAST)         ! Inverse slope between them.
            HALFWD = XLAST + SLOPE*(HALFMX-LAST) ! Interpolate to HALFMX.
            GO TO 200                            ! Pop out of loop.
         END IF
 100  CONTINUE

C---------------------------------------------------------------------------
C Return with the result.
C---------------------------------------------------------------------------

 200  CONTINUE
      PERIOD_HWHM = HALFWD

      RETURN
      END
