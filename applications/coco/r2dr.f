      DOUBLE PRECISION FUNCTION R2DR (NDP, ANGLE)
*+
*
*  R2DR:  function in COCO utility which takes an angle in
*         radians and returns a value in degrees suitable for
*         output to the specified number of decimal places.
*
*  Given:
*     NDP       int      number of decimal places of degrees
*     ANGLE     dp       angle in radians
*
*  Result:
*     ANGLE in degrees such that 0.LE.ANGR.LT.360.
*
*  The purpose is to prevent an output of 360.000...
*
*  P T Wallace   Starlink   18 May 1992
*-

      IMPLICIT NONE

      INTEGER NDP
      DOUBLE PRECISION ANGLE

      DOUBLE PRECISION R2D
      PARAMETER (R2D=57.29577951308232087679815D0)

      DOUBLE PRECISION U,C,V



*  Number of last decimal place units per degree
      U=DBLE(10**NDP)

*  Number of units per turn
      C=360D0*U

*  Convert to units modulo 1 turn
      V=MOD(ANINT(ANGLE*R2D*U),C)
      IF (V.LT.0D0) V=V+C

*  Convert to degrees
      R2DR=V/U

      END
