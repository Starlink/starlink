      PROGRAM JPLPLEPH
*+
*  - - - - - - - - -
*   J P L P L E P H
*  - - - - - - - - -
*
*  An example program using the Starlink subroutine PLEPH to
*  interpolate the JPL planetary ephemeris.  The heliocentric
*  Earth position and velocity for an instant near the vernal
*  equinox in 2000AD are determined.
*
*  Input is from the binary direct-access form of the JPL ephemeris
*  (as output by the EPHDSK program).  The file is called JPLEPH.
*
*  P.T.Wallace   Starlink   22 April 1994
*-

      IMPLICIT NONE

*  PLEPH arguments
      DOUBLE PRECISION TDB
      INTEGER NP,NC
      DOUBLE PRECISION PV(6)
      LOGICAL OK



*  TDB = Julian Date 2451624.5
      TDB=51624D0

*  Body is the Earth
      NP=3

*  Origin is the Sun
      NC=11

*  Read and interpolate the ephemeris
      CALL PLEPH(TDB,NP,NC,PV,OK)

*  List the resulting position and velocity
      IF (OK) WRITE (*,'(6(1X,E24.16/))') PV

      END
