      DOUBLE PRECISION FUNCTION sla_ZD (HA, DEC, PHI)
*+
*     - - -
*      Z D
*     - - -
*
*  HA, Dec to Zenith Distance (double precision)
*
*  Given:
*     HA     d     Hour Angle in radians
*     DEC    d     declination in radians
*     PHI    d     observatory latitude in radians
*
*  The result is in the range 0 to pi.
*
*  Notes:
*
*  1)  The latitude must be geodetic.  In critical applications,
*      corrections for polar motion should be applied.
*
*  2)  In some applications it will be important to specify the
*      correct type of hour angle and declination in order to
*      produce the required type of zenith distance.  In particular,
*      it may be important to distinguish between the zenith distance
*      as affected by refraction, which would require the "observed"
*      HA,Dec, and the zenith distance in vacuo, which would require
*      the "topocentric" HA,Dec.  If the effects of diurnal aberration
*      can be neglected, the "apparent" HA,Dec may be used instead of
*      the topocentric HA,Dec.
*
*  3)  No range checking of arguments is done.
*
*  4)  In applications which involve many zenith distance calculations,
*      rather than calling the present routine it will be more efficient
*      to use inline code, having previously computed fixed terms such
*      as sine and cosine of latitude, and perhaps sine and cosine of
*      declination.
*
*  P.T.Wallace   Starlink   3 April 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION HA,DEC,PHI

      DOUBLE PRECISION SH,CH,SD,CD,SP,CP,X,Y,Z


      SH=SIN(HA)
      CH=COS(HA)
      SD=SIN(DEC)
      CD=COS(DEC)
      SP=SIN(PHI)
      CP=COS(PHI)
      X=CH*CD*SP-SD*CP
      Y=SH*CD
      Z=CH*CD*CP+SD*SP
      sla_ZD=ATAN2(SQRT(X*X+Y*Y),Z)

      END
