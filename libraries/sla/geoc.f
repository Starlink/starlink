      SUBROUTINE sla_GEOC (P, H, R, Z)
*+
*     - - - - -
*      G E O C
*     - - - - -
*
*  Convert geodetic position to geocentric (double precision)
*
*  Given:
*     P     dp     latitude (geodetic, radians)
*     H     dp     height above reference spheroid (geodetic, metres)
*
*  Returned:
*     R     dp     distance from Earth axis (AU)
*     Z     dp     distance from plane of Earth equator (AU)
*
*  Notes:
*     1)  Geocentric latitude can be obtained by evaluating ATAN2(Z,R).
*     2)  IAU 1976 constants are used.
*
*  Reference:
*     Green,R.M., Spherical Astronomy, CUP 1985, p98.
*
*  P.T.Wallace   Starlink   4th October 1989
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION P,H,R,Z

*  Earth equatorial radius (metres)
      DOUBLE PRECISION A0
      PARAMETER (A0=6378140D0)

*  Reference spheroid flattening factor and useful function
      DOUBLE PRECISION F,B
      PARAMETER (F=1D0/298.257D0,B=(1D0-F)**2)

*  Astronomical unit in metres
      DOUBLE PRECISION AU
      PARAMETER (AU=1.49597870D11)

      DOUBLE PRECISION SP,CP,C,S



*  Geodetic to geocentric conversion
      SP=SIN(P)
      CP=COS(P)
      C=1D0/SQRT(CP*CP+B*SP*SP)
      S=B*C
      R=(A0*C+H)*CP/AU
      Z=(A0*S+H)*SP/AU

      END
