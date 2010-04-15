*  History:
*      1 Aug 2000 (ajc):
*        Change TYPE * to PRINT *
*     18 Sep 2000 (ajc):
*        Change IUTFLG LOGICAL to BYTE
*-----------------------------------------------------------------------

      SUBROUTINE ASTRO_TIMES (TIME, DATE, ALONG, TIMCOR, IUTFLG,
     &                        UTD, LST, JULIAN_DATE)

*  Routine to work out relevant astronomical times from time and
*  date expressed in VAX character format.

      IMPLICIT  NONE

*     Formal parameters

      CHARACTER         TIME*8        ! input time hh:mm:ss
      CHARACTER         DATE*9        ! input date dd-mon-yy
      DOUBLE PRECISION  ALONG         ! input longitude of observation
      DOUBLE PRECISION  TIMCOR        ! input time correction local -> UT (hrs)
      BYTE IUTFLG                     ! input flag to indicate data in UT already
      DOUBLE PRECISION  UTD           ! output UT, days
      DOUBLE PRECISION  LST           ! output sidereal time, hours
      DOUBLE PRECISION  JULIAN_DATE   ! output julian date, days

*     Local variables

      INTEGER          UTY
      INTEGER          JTIME(3)
      INTEGER          JDATE(3)
      DOUBLE PRECISION XLST

*     Functions

      INTEGER  JULDA

*  Ok, go...

      CALL DECTIM (TIME, JTIME)
      CALL DECDAT (DATE, JDATE)

*     Calculate U.T. and Julian date

      CALL UTCALC2 (JDATE, JTIME, IUTFLG, TIMCOR, UTY, UTD)
      JULIAN_DATE = DFLOAT (JULDA(UTY)) - 0.5D0 + UTD

CD    print *, '     jdate ', jdate
CD    print *, '     jtime ', jtime
CD    print *, '     uty   ', uty
CD    print *, '     utd   ', utd

*     Calculate LST

      CALL LOSTIM2 (ALONG, UTY, UTD, XLST)
      LST = XLST*24.D0

      RETURN
      END

*-----------------------------------------------------------------------

