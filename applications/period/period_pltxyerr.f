
      SUBROUTINE PERIOD_PLTXYERR(XR, YR, ER1, ER2,
     :                           IPARRAY, NPTS, MXCOL, YERROR,
     :                           DMIX, DMIY, DMXX, DMXY)

C=============================================================================
C Sets-up single-precision data arrays for PLT
C
C Written by Kevin P Duffey @RAL, October 2001
C=============================================================================

      IMPLICIT NONE

C-----------------------------------------------------------------------------
C PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER MXCOL, NPTS

      REAL XR(NPTS), YR(NPTS), ER1(NPTS), ER2(NPTS)

C-----------------------------------------------------------------------------
C PERIOD_PLT declarations.
C-----------------------------------------------------------------------------

      INTEGER K
      DOUBLE PRECISION IPARRAY(NPTS, MXCOL)
      LOGICAL YERROR
      DOUBLE PRECISION DMIX, DMIY, DMXX, DMXY

      DO 10 K = 1, NPTS

*      X co-ordinate.
         XR(K) = SNGL(IPARRAY(K, 1))
         IF( IPARRAY(K, 1) .GE. DMXX) DMXX = IPARRAY(K, 1)
         IF( IPARRAY(K, 1) .LE. DMIX) DMIX = IPARRAY(K, 1)

*      Y co-ordinate.
         YR(K) = SNGL(IPARRAY(K, 2))
         IF( IPARRAY(K, 2) .GE. DMXY) DMXY = IPARRAY(K, 2)
         IF( IPARRAY(K, 2) .LE. DMIY) DMIY = IPARRAY(K, 2)

*      Y error.
         IF (YERROR) THEN
            ER1(K) = YR(K)+SNGL(IPARRAY(K, 3))
            ER2(K) = YR(K)-SNGL(IPARRAY(K, 3))
         ELSE
            ER1(K) = 0.0
            ER2(K) = 0.0
         END IF

  10  CONTINUE

      RETURN
      END
