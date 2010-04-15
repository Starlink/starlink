
C===========================================================================

      SUBROUTINE PERIOD_FAKEPERIOD(IPARRAY, NUMPTS, MXCOL, PERIOD,
     :                             AMPLITUDE, ZEROPT, GAMMA, MAXSIN,
     :                             NUMSIN, STARTPT, ENDPT)

C===========================================================================
C Calculates periodic data for FAKE. All arrays can correspond to slices
C of dynamically-allocated memory, provided that the appropriate "calling"
C arguments are memory pointers being passed via the %VAL() intrinsic
C function.
C
C Written by Kevin P Duffey @RAL, October 2001
C===========================================================================

      IMPLICIT NONE

      INCLUDE "PIVARS"

      INTEGER NUMPTS, MXCOL, MAXSIN, NUMSIN, K, L
      DOUBLE PRECISION IPARRAY(NUMPTS,MXCOL), PERIOD(MAXSIN)
      DOUBLE PRECISION AMPLITUDE(MAXSIN), ZEROPT(MAXSIN)
      DOUBLE PRECISION GAMMA(MAXSIN)
      DOUBLE PRECISION STARTPT, ENDPT
      DOUBLE PRECISION INTERVAL


      INTERVAL = (ENDPT-STARTPT)/(DFLOAT(NUMPTS)-1.0D0)

      DO 20 K = 1, NUMPTS
         IPARRAY(K, 1) = STARTPT + (INTERVAL*DFLOAT(K-1))
         DO 10 L = 1, NUMSIN
            IPARRAY(K, 2) = IPARRAY(K, 2)
     :                            + (GAMMA(L)+(AMPLITUDE(L)
     :                            *DSIN(((TWOPI)/PERIOD(L))
     :                            *(IPARRAY(K, 1)-ZEROPT(L))))
     :                            )
  10     CONTINUE
  20  CONTINUE

      RETURN
      END
