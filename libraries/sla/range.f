      REAL FUNCTION sla_RANGE (ANGLE)
*+
*     - - - - - -
*      R A N G E
*     - - - - - -
*
*  Normalize angle into range +/- pi  (single precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the +/- pi (single
*  precision).
*
*  P.T.Wallace   Starlink   23 November 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL ANGLE

      REAL API,A2PI
      PARAMETER (API=3.141592653589793238462643)
      PARAMETER (A2PI=6.283185307179586476925287)


      sla_RANGE=MOD(ANGLE,A2PI)
      IF (ABS(sla_RANGE).GE.API)
     :          sla_RANGE=sla_RANGE-SIGN(A2PI,ANGLE)

      END
