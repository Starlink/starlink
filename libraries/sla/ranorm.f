      REAL FUNCTION sla_RANORM (ANGLE)
*+
*     - - - - - - -
*      R A N O R M
*     - - - - - - -
*
*  Normalize angle into range 0-2 pi  (single precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result is ANGLE expressed in the range 0-2 pi (single
*  precision).
*
*  P.T.Wallace   Starlink   23 November 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL ANGLE

      REAL A2PI
      PARAMETER (A2PI=6.283185307179586476925287)


      sla_RANORM=MOD(ANGLE,A2PI)
      IF (sla_RANORM.LT.0.0) sla_RANORM=sla_RANORM+A2PI

      END
