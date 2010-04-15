*-----------------------------------------------------------------------

      SUBROUTINE LOSTIM2 (ALONG, UTY, UTD, XLST)

C   The epoch is 1900 January 0.5UT = Julian day 2415020.0

      IMPLICIT   NONE

*     Formal parameters

      DOUBLE PRECISION ALONG     !input, longitude
      INTEGER          UTY       !input, UT year
      DOUBLE PRECISION UTD       !input, UT days
      DOUBLE PRECISION XLST      !output, sidereal time (days)

*     Local variables

      DOUBLE PRECISION DU
      DOUBLE PRECISION TU
      DOUBLE PRECISION START
      DOUBLE PRECISION GST
      DOUBLE PRECISION C1
      DOUBLE PRECISION WLONG

*     Functions

      INTEGER  JULDA

*   Ok, go...

*     DU is the GMT from Jan 0.0 to the present (days)

      DU   = DFLOAT(JULDA(UTY)) - 0.5D0
      TU   = DU/36525.D0

*     START is the Greenwich mean sidereal time on Jan 0.0 (days)
*     (the extra 129.1794 secs corresponds to the 0.7 century
*     subtracted from TU. The precision is thereby improved.

      START=(6.D0 +38.D0/60.D0 +(45.836D0+129.1794D0+8640184.542D0*
     &      (TU-0.7D0)  +.0929D0*TU**2)/3600.D0)/24.D0

*     C1 is the conversion factor from sidereal time to solar time

      C1 = 0.997269566414D0

*     GST is the Greenwich mean sidereal time (days)
*     XLST is the local mean sidereal time (from Jan 0) (days)

      GST   = START + UTD/C1
      WLONG = ALONG/360.D0
      XLST  = GST - WLONG
      XLST  = XLST - IDINT(XLST)

      RETURN
      END

*-----------------------------------------------------------------------


