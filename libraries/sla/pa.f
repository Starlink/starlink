      DOUBLE PRECISION FUNCTION sla_PA (HA, DEC, PHI)
*+
*     - - -
*      P A
*     - - -
*
*  HA, Dec to Parallactic Angle (double precision)
*
*  Given:
*     HA     d     hour angle in radians (geocentric apparent)
*     DEC    d     declination in radians (geocentric apparent)
*     PHI    d     observatory latitude in radians (geodetic)
*
*  The result is in the range -pi to +pi
*
*  Notes:
*
*  1)  The parallactic angle at a point in the sky is the position
*      angle of the vertical, i.e. the angle between the direction to
*      the pole and to the zenith.  In precise applications care must
*      be taken only to use geocentric apparent HA,Dec and to consider
*      separately the effects of atmospheric refraction and telescope
*      mount errors.
*
*  2)  At the pole a zero result is returned.
*
*  P.T.Wallace   Starlink   16 August 1994
*
*  Copyright (C) 1995 Rutherford Appleton Laboratory
*-

      IMPLICIT NONE

      DOUBLE PRECISION HA,DEC,PHI

      DOUBLE PRECISION CP,SQSZ,CQSZ



      CP=COS(PHI)
      SQSZ=CP*SIN(HA)
      CQSZ=SIN(PHI)*COS(DEC)-CP*SIN(DEC)*COS(HA)
      IF (SQSZ.EQ.0D0.AND.CQSZ.EQ.0D0) CQSZ=1D0
      sla_PA=ATAN2(SQSZ,CQSZ)

      END
