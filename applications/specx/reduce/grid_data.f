*-------------------------------------------------------------------------

      SUBROUTINE GRID_DATA (NDAT, XSCALE, DATA, WORK, NOUT,
     &                      GRIDN, XMID, NBOT, NTOP, BAD, IERR)

C  Routine to regrid data in array DATA, with X-locations for each point
C  described by XSCALE, onto new X-grid defined by centre point XMID and
C  gridding interval GRIDN.
C  On return NBOT and NTOP contain first and last channels of DATA which
C  contain useful data (can be used to drop channels if necessary).

C  History:
C     6-JUN-2000 (AJC):
C       Replace 'type *' with 'PRINT *'

      IMPLICIT  NONE

C     Formal parameters:

      INTEGER   NDAT            ! Length of data array.
      REAL      XSCALE(*)       ! Array with X-values of current data
      REAL      DATA(*)         ! Input data array
      REAL      WORK(*)         ! Work array
      INTEGER   NOUT            ! Number of channels in output array
      REAL      GRIDN           ! New gridding interval
      REAL      XMID            ! Origin (centre) of output grid
      INTEGER   NBOT            ! First useful channel in input array
      INTEGER   NTOP            ! Last useful channel in input array
      REAL      BAD             ! Bad channel value
      INTEGER   IERR            ! Error status return

C     Local variables:

      INTEGER   IERR1
      INTEGER   IERR2
      INTEGER   J, K
      REAL      DXIN
      REAL      BL, BR
      REAL      PL, PR
      REAL      SUM
      REAL      TWID
      REAL      WEIGHT

C     Functions

      REAL      XTRANS
      REAL      XSNART

C  Ok, go..

      IERR  = 0

CD    PRINT *, ' -- grid_data --'
CD    PRINT *, '    ndat, nout  = ', NDAT, NOUT
CD    PRINT *, '    gridn, xmid = ', GRIDN, XMID
CD    PRINT *, '    current xscale array:'
CD    PRINT *,      (XSCALE(J),J=1,NDAT)

C     Find the centre channel and initialize

      NBOT  = NDAT  ! First correct channel in output spectrum
      NTOP  = 1     ! Last correct channel in output spectrum

      DO J = 1, NDAT
        WORK(J) = BAD
      END DO

C     Loop over bins of new spectrum, checking to see if the edges of the new
C     bins lie within the boundaries of the original grid. Integrate between
C     edges of new channels if so - otherwise set new data to zero.
C     In this revised form of this routine, the integration is done as well
C     as it can be from a non-linear input grid, but always *onto* a regular
C     grid.

      DO J = 1, NOUT

C       Calculate the output bin edges (too much numerical error from just
C       incrementing bin after bin)

        BL = XMID + 0.5*FLOAT(2*(J-1)-NOUT)*GRIDN
        BR = BL + GRIDN

C       Locate output bin-edges in the input spectrum in units of points.
C       Note that in SPECX convention the spectrum starts at 0.5 points and
C       finishes at N+0.5 points.

        PL = XSNART (XSCALE, BL, NDAT, IERR1)
        PR = XSNART (XSCALE, BR, NDAT, IERR2)

C       Don't do any more if bin does not lie within original spectrum...

        IF (      PL.GT.0.5 .AND. PL.LT.FLOAT(NDAT)+0.5
     &      .AND. PR.GT.0.5 .AND. PR.LT.FLOAT(NDAT)+0.5) THEN

C       Reorder so that PR is always greater than PL

          IF (PR.LT.PL) CALL SWAP2 (PR, PL)

          NBOT = MIN (NBOT, NINT(PL))
          NTOP = MAX (NTOP, NINT(PR))

C         Check that both edges of the bin lie within the current spectrum;
C         if so integrate over affected input channels to find new values,
C         otherwise ignore this bin. Average so that new spectrum will also
C         be correct on temperature scale (not integrated intensity).
C         Width of *input* bin is taken as difference of x-values at
C         plus or minus half a channel from nominal channel number.
C         Don't need to test errors, since result is guaranteed by earlier
C         doing reverse operation successfully.

          TWID = 0.0
          SUM  = 0.0

          DO K = NINT(PL), NINT(PR)
            DXIN   = XTRANS (XSCALE, FLOAT(K)+0.5, NDAT, IERR1)
     &                - XTRANS (XSCALE, FLOAT(K)-0.5, NDAT, IERR2)
            WEIGHT = 1.0
            IF (K.EQ.NINT(PL)) WEIGHT = WEIGHT - (PL-FLOAT(K)+0.5)
            IF (K.EQ.NINT(PR)) WEIGHT = WEIGHT - (FLOAT(K)+0.5-PR)
            IF (DATA(K).NE.BAD) THEN
              SUM  = SUM  + WEIGHT*DATA(K)
              TWID = TWID + ABS(WEIGHT)
            END IF
          END DO

          IF (TWID.GT.0.0) THEN
            SUM = SUM / TWID
          ELSE
            SUM = BAD
          END IF

C         Write new value into current output point.
          WORK(J) = SUM
        END IF

      END DO

C     Regridded array is complete: copy data back from work area
C     into original data array

      DO J = 1, NDAT
        DATA(J) = WORK(J)
      END DO

      PRINT *,'First and last useful channels in input:',NBOT,NTOP

      RETURN
      END

*-------------------------------------------------------------------------

