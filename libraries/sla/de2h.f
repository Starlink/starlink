      SUBROUTINE sla_DE2H (HA, DEC, PHI, AZ, EL)
*+
*     - - - - -
*      D E 2 H
*     - - - - -
*
*  Equatorial to horizon coordinates:  HA,Dec to Az,El
*
*  (double precision)
*
*  Given:
*     HA      d     hour angle
*     DEC     d     declination
*     PHI     d     observatory latitude
*
*  Returned:
*     AZ      d     azimuth
*     EL      d     elevation
*
*  Notes:
*
*  1)  All the arguments are angles in radians.
*
*  2)  Azimuth is returned in the range 0-2pi;  north is zero,
*      and east is +pi/2.  Elevation is returned in the range
*      +/-pi/2.
*
*  3)  The latitude must be geodetic.  In critical applications,
*      corrections for polar motion should be applied.
*
*  4)  In some applications it will be important to specify the
*      correct type of hour angle and declination in order to
*      produce the required type of azimuth and elevation.  In
*      particular, it may be important to distinguish between
*      elevation as affected by refraction, which would
*      require the "observed" HA,Dec, and the elevation
*      in vacuo, which would require the "topocentric" HA,Dec.
*      If the effects of diurnal aberration can be neglected, the
*      "apparent" HA,Dec may be used instead of the topocentric
*      HA,Dec.
*
*  5)  No range checking of arguments is carried out.
*
*  6)  In applications which involve many such calculations, rather
*      than calling the present routine it will be more efficient to
*      use inline code, having previously computed fixed terms such
*      as sine and cosine of latitude, and (for tracking a star)
*      sine and cosine of declination.
*
*  P.T.Wallace   Starlink   9 July 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION HA,DEC,PHI,AZ,EL

      DOUBLE PRECISION D2PI
      PARAMETER (D2PI=6.283185307179586476925286766559D0)

      DOUBLE PRECISION SH,CH,SD,CD,SP,CP,X,Y,Z,R,A


*  Useful trig functions
      SH=SIN(HA)
      CH=COS(HA)
      SD=SIN(DEC)
      CD=COS(DEC)
      SP=SIN(PHI)
      CP=COS(PHI)

*  Az,El as x,y,z
      X=-CH*CD*SP+SD*CP
      Y=-SH*CD
      Z=CH*CD*CP+SD*SP

*  To spherical
      R=SQRT(X*X+Y*Y)
      IF (R.EQ.0D0) THEN
         A=0D0
      ELSE
         A=ATAN2(Y,X)
      END IF
      IF (A.LT.0D0) A=A+D2PI
      AZ=A
      EL=ATAN2(Z,R)

      END
