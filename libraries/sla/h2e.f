      SUBROUTINE sla_H2E (AZ, EL, PHI, HA, DEC)
*+
*     - - - - -
*      D E 2 H
*     - - - - -
*
*  Horizon to equatorial coordinates:  Az,El to HA,Dec
*
*  (single precision)
*
*  Given:
*     AZ      r     azimuth
*     EL      r     elevation
*     PHI     r     observatory latitude
*
*  Returned:
*     HA      r     hour angle
*     DEC     r     declination
*
*  Notes:
*
*  1)  All the arguments are angles in radians.
*
*  2)  The sign convention for azimuth is north zero, east +pi/2.
*
*  3)  HA is returned in the range +/-pi.  Declination is returned
*      in the range +/-pi/2.
*
*  4)  The latitude is (in principle) geodetic.  In critical
*      applications, corrections for polar motion should be applied.
*
*  5)  In some applications it will be important to specify the
*      correct type of elevation in order to produce the required
*      type of HA,Dec.  In particular, it may be important to
*      distinguish between the elevation as affected by refraction,
*      which will yield the "observed" HA,Dec, and the elevation
*      in vacuo, which will yield the "topocentric" HA,Dec.  If the
*      effects of diurnal aberration can be neglected, the
*      topocentric HA,Dec may be used as an approximation to the
*      "apparent" HA,Dec.
*
*  6)  No range checking of arguments is done.
*
*  7)  In applications which involve many such calculations, rather
*      than calling the present routine it will be more efficient to
*      use inline code, having previously computed fixed terms such
*      as sine and cosine of latitude.
*
*  P.T.Wallace   Starlink   21 February 1996
*
*  Copyright (C) 1996 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      REAL AZ,EL,PHI,HA,DEC

      DOUBLE PRECISION SA,CA,SE,CE,SP,CP,X,Y,Z,R


*  Useful trig functions
      SA=SIN(AZ)
      CA=COS(AZ)
      SE=SIN(EL)
      CE=COS(EL)
      SP=SIN(PHI)
      CP=COS(PHI)

*  HA,Dec as x,y,z
      X=-CA*CE*SP+SE*CP
      Y=-SA*CE
      Z=CA*CE*CP+SE*SP

*  To HA,Dec
      R=SQRT(X*X+Y*Y)
      IF (R.EQ.0.0) THEN
         HA=0.0
      ELSE
         HA=ATAN2(Y,X)
      END IF
      DEC=ATAN2(Z,R)

      END
