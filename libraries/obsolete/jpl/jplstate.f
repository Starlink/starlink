      PROGRAM JPLSTATE
*+
*  - - - - - - - - -
*   J P L S T A T E
*  - - - - - - - - -
*
*  Example program using the subroutine STATE, in the JPL Ephemeris
*  package, to interpolate the planetary ephemeris.  The
*  heliocentric position and velocity of the Earth-Moon
*  barycentre for a given epoch near the vernal equinox of
*  2000AD are determined.
*
*  Input is from the binary direct-access form of the JPL ephemeris
*  (as output by the EPHDSK program).
*
*  P.T.Wallace   Starlink   25 April 1994
*-

      IMPLICIT NONE

*  Julian date for ephemeris interpolation (see STATE source)
      DOUBLE PRECISION DJ(2)

*  Flags indicating which values are required
      INTEGER LIST(12)

*  Array to receive ephemeris data (1,2 and 4-11 not used)
      DOUBLE PRECISION PV(6,11)

*  Array to receive nutation (not used)
      DOUBLE PRECISION DNUT(4)

      INTEGER I
      LOGICAL OK



*  Set up LIST array to interpolate for Earth only
      DO I=1,12
         LIST(I)=0
      END DO
      LIST(3)=2

*  Put Julian Date 2451624.5 in DJ(1) and DJ(2).
      DJ(1)=2451624.5D0
      DJ(2)=0D0

*  Interpolate the ephemeris and print results
      CALL STATE(DJ,LIST,PV,DNUT,OK)
      IF (OK) WRITE (*,'(6(1X,E24.16/))') (PV(I,3),I=1,6)

      END
