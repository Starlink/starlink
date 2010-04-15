
C===========================================================================

      SUBROUTINE PERIOD_PUTYMEAN(IPARRAY, NUMROWS, MXCOL, AVE,
     :                           SDEV, YERROR, OPARRAY)

C===========================================================================
C Outputs mean data from DETREND. Both arrays can correspond to slices
C of dynamically-allocated memory, provided that the appropriate "calling"
C arguments are memory pointers being passed via the %VAL() intrinsic
C function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMROWS, MXCOL, I
      DOUBLE PRECISION IPARRAY(NUMROWS,MXCOL)
      DOUBLE PRECISION OPARRAY(NUMROWS,MXCOL)
      DOUBLE PRECISION AVE, SDEV
      LOGICAL YERROR


      DO 10 I = 1, NUMROWS
         OPARRAY(I, 1) = IPARRAY(I, 1)
         OPARRAY(I, 2) = (IPARRAY(I, 2)-AVE)/SDEV
         IF ( YERROR ) THEN
            OPARRAY(I, 3) = DSQRT((IPARRAY(I, 3)**2)/(SDEV**2))
         ELSE
            OPARRAY(I, 3) = 0.0D0
         END IF
  10  CONTINUE

      RETURN
      END
