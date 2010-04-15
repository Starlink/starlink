*-------------------------------------------------------------------------

      SUBROUTINE GRID_DATA8 (NDAT, XSCALE, DATA, GDAT, NOUT,
     &                       GRIDN, XMID, NBOT, NTOP, BAD, IERR)

C  Routine to regrid data in array DATA, with X-locations for each point
C  described by XSCALE, onto new X-grid defined by centre point XMID and
C  gridding interval GRIDN.
C  On return NBOT and NTOP contain first and last channels of DATA which
C  contain useful data (can be used to drop channels if necessary).

C  History:
C     6-JUN-2000 (AJC):
C       Replace 'TYPE *' with 'PRINT *'
C       Unused XTRANS8
C-

      IMPLICIT  NONE

C     Formal parameters:

      INTEGER   NDAT            ! Length of data array.
      REAL*8    XSCALE(*)       ! Array with X-values of current data
      REAL      DATA(*)         ! Input data array
      REAL      GDAT(*)         ! Output array
      INTEGER   NOUT            ! Number of channels in output array
      REAL*8    GRIDN           ! New gridding interval
      REAL*8    XMID            ! Origin (centre) of output grid
      INTEGER   NBOT            ! First useful channel in input array
      INTEGER   NTOP            ! Last useful channel in input array
      REAL      BAD             ! Bad channel value
      INTEGER   IERR            ! Error status return

C     Local variables:

      INTEGER   IERR1
      INTEGER   IERR2
      INTEGER   J, K
      REAL*8    BL, BR
      REAL*8    PL, PR
      REAL      SUM
      REAL      TWID
      REAL      WEIGHT

C     Functions

      REAL*8    XSNART8

C  Ok, go..

      IERR  = 0

CD    PRINT *, ' -- grid_data8 --'
CD    PRINT *, '    ndat, nout  = ', NDAT, NOUT
CD    PRINT *, '    gridn, xmid = ', GRIDN, XMID
C     PRINT *, '    current x-scale array ='
C     PRINT *,      (XSCALE(J),J=1,NDAT)

C     Find the centre channel and initialize

      NBOT  = NOUT  ! First useful channel in input spectrum
      NTOP  = 1     ! Last useful channel in input spectrum

      DO J = 1, NOUT
        GDAT(J) = BAD
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

        PL = XSNART8 (XSCALE, BL, NDAT, IERR1)
        PR = XSNART8 (XSCALE, BR, NDAT, IERR2)

C       Don't do any more if bin does not lie wholly within original spectrum...

        IF ( IERR1 .eq. 0  .and.  IERR2 .eq. 0) THEN

C       Reorder so that PR is always greater than PL

          IF (PR.LT.PL) CALL SWAP8 (PR, PL)

          NBOT = MIN (NBOT, NINT(PL))
          NTOP = MAX (NTOP, NINT(PR))

C         Integrate over affected input channels to find new values,
C         Average so that new spectrum will also
C         be correct on temperature scale (not integrated intensity).
C         Width of *input* bin is taken as difference of x-values at
C         plus or minus half a channel from nominal channel number.

          TWID = 0.0
          SUM  = 0.0

          DO K = NINT(PL), NINT(PR)
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

          GDAT(J) = SUM

        END IF
      END DO

C     Regridded array is complete:

      PRINT *,'First and last useful channels in input:',NBOT,NTOP

      RETURN
      END

*-------------------------------------------------------------------------

