      DOUBLE PRECISION FUNCTION sla_DRANGE (ANGLE)
*+
*     - - - - - - -
*      D R A N G E
*     - - - - - - -
*
*  Normalize angle into range +/- pi  (double precision)
*
*  Given:
*     ANGLE     dp      the angle in radians
*
*  The result (double precision) is ANGLE expressed in the range +/- pi.
*
*  P.T.Wallace   Starlink   23 November 1995
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION DPI,D2PI
      PARAMETER (DPI=3.141592653589793238462643D0)
      PARAMETER (D2PI=6.283185307179586476925287D0)


      sla_DRANGE=MOD(ANGLE,D2PI)
      IF (ABS(sla_DRANGE).GE.DPI)
     :          sla_DRANGE=sla_DRANGE-SIGN(D2PI,ANGLE)

      END
