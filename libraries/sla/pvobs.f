      SUBROUTINE sla_PVOBS (P, H, STL, PV)
*+
*     - - - - - -
*      P V O B S
*     - - - - - -
*
*  Position and velocity of an observing station (double precision)
*
*  Given:
*     P     dp     latitude (geodetic, radians)
*     H     dp     height above reference spheroid (geodetic, metres)
*     STL   dp     local apparent sidereal time (radians)
*
*  Returned:
*     PV    dp(6)  position/velocity 6-vector (AU, AU/s, true equator
*                                              and equinox of date)
*
*  Called:  sla_GEOC
*
*  IAU 1976 constants are used.
*
*  P.T.Wallace   Starlink   14 November 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION P,H,STL,PV(6)

      DOUBLE PRECISION R,Z,S,C,V

*  Mean sidereal rate (at J2000) in radians per (UT1) second
      DOUBLE PRECISION SR
      PARAMETER (SR=7.292115855306589D-5)



*  Geodetic to geocentric conversion
      CALL sla_GEOC(P,H,R,Z)

*  Functions of ST
      S=SIN(STL)
      C=COS(STL)

*  Speed
      V=SR*R

*  Position
      PV(1)=R*C
      PV(2)=R*S
      PV(3)=Z

*  Velocity
      PV(4)=-V*S
      PV(5)=V*C
      PV(6)=0D0

      END
