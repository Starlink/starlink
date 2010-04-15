
C===========================================================================

      SUBROUTINE PERIOD_FAKECHAOS(IPARRAY, NUMPTS, MXCOL, COEFF,
     :                             INITVAL, STARTPT, ENDPT)

C===========================================================================
C Calculates chaotic data for FAKE. All arrays can correspond to slices
C of dynamically-allocated memory, provided that the appropriate "calling"
C arguments are memory pointers being passed via the %VAL() intrinsic
C function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INTEGER NUMPTS, MXCOL, K
      DOUBLE PRECISION IPARRAY(NUMPTS,MXCOL)
      DOUBLE PRECISION INITVAL, STARTPT, ENDPT
      DOUBLE PRECISION COEFF, INTERVAL, INITIAL


      INTERVAL = (ENDPT-STARTPT)/(DFLOAT(NUMPTS)-1.0D0)

      INITIAL = INITVAL

      DO 10 K = 1, NUMPTS
         IPARRAY(K, 1) = STARTPT + (INTERVAL*DFLOAT(K-1))
         IPARRAY(K, 2) = COEFF*INITIAL*(1.0D0-INITIAL)
         INITIAL = IPARRAY(K, 2)
  10  CONTINUE

      RETURN
      END
