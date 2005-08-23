* History:
*    21 Sep 2000 (ajc):
*       Unused NYR
*-----------------------------------------------------------------------

      SUBROUTINE UTCALC2 (JDAT, JTIM, IUTFLG, TIMCOR, UTY, UTD)

*   Routine to calculate universal time from local time and observatory
*   constants (i.e. standard time correction)

      IMPLICIT NONE

*     Formal parameters

      INTEGER          JDAT(3)      ! input, local date (y,m,d)
      INTEGER          JTIM(3)      ! input, local time (h,m,s)
      BYTE             IUTFLG       ! input, UT flag
      DOUBLE PRECISION TIMCOR       ! input, correction to local time, hrs
      INTEGER          UTY          ! output, UT year
      DOUBLE PRECISION UTD          ! output, UT days

*     Local variables

      DOUBLE PRECISION UTH
      DOUBLE PRECISION TIMLOC

      INTEGER          I
      INTEGER          NDAY
      INTEGER          MON
      INTEGER          NLEAP

      INTEGER  LMON(12)
      DATA LMON     /31,28,31,30,31,30,31,31,30,31,30,31/
*  Ok, go...

*     Work out local time (in hours)

      TIMLOC = DFLOAT(JTIM(1)) + DFLOAT(JTIM(2))/60.D0
     &                         + DFLOAT(JTIM(3))/3600.D0

      NDAY  = JDAT(1)
      MON   = JDAT(2)
      UTY   = JDAT(3)

*     Work out day number in year

      DO I = 1,11
        IF (I.GE.MON)   GO TO 20
        NDAY = NDAY + LMON(I)
      END DO

*     Add on a day if after Feb in a leap year

   20 CONTINUE
      IF ((UTY - 4*(UTY/4)).EQ.0 .AND. MON.GE.3) NDAY = NDAY+1

*     Make local time correction

      UTH = TIMLOC
      IF (IUTFLG.EQ.0)   UTH = UTH + TIMCOR
      IF (UTH.GE.0.0)    GO TO 30

*     If time correction takes into previous UT day correct day (and year)

      NDAY = NDAY - 1
      UTH  = UTH + 24.D0
      IF (NDAY.GE.0)   GO TO 40
      UTY  = UTY-1
      NDAY = 364
      IF ((UTY-4*(UTY/4)).EQ.0)   NDAY = NDAY+1
      GO TO 40

*     If time correction takes into next UT day correct day and year likewise

   30 IF (UTH.LE.24.0D0)   GO TO 40
      UTH   = UTH - 24.0D0
      NDAY  = NDAY+1
      NLEAP = 365
      IF ((UTY-4*(UTY/4)).EQ.0)   NLEAP = 366
      IF (NDAY.NE.NLEAP)   GO TO 40
      NDAY  = 0
      UTY   = UTY+1

*     Date is now correct: work out fractional days

   40 UTD   = DFLOAT(NDAY) + UTH/24.D0

      RETURN
      END

*-----------------------------------------------------------------------

