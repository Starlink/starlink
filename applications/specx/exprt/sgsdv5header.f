C-----------------------------------------------------------------------

      SUBROUTINE SPECX_GSD_V5_HEADER (IERR)

C   Routine to read header from open GSD version 5 file containing a
C   generalized observation.

C   RMP 30-July-1991  NRC is got from C3NRS not C3NRC
C   JFL 22-NOV-1991   Version brought back from visit to Dwingeloo, unchanged
C                     I think.
C   RMP 18-Nov-1991   Put V4 integration time code into V5.
C   RPT 30-jan-1997   Correct integration time for on-the-fly maps.
C   AJC  8-may-2000   Port to Linux
C                     Replace TYPE with PRINT
C                     IXN undefined
C                     Unused I, J, ACTDIMS
C-

      IMPLICIT  NONE

      INTEGER*4 IERR

      INTEGER*4 ADAM__OK
      PARAMETER (ADAM__OK=0)

      LOGICAL   ARRAY
      INTEGER*4 LVAL
      INTEGER*4 NO
      INTEGER*4 STATUS
      CHARACTER TYPE*1
      CHARACTER UNITS*15

*  GSD_FIND

      CHARACTER*20 DIMNAMES(2)
      CHARACTER*20 DIMUNITS(2)
      INTEGER*4    DIMVALS (2)
      INTEGER*4    ACTVALS

*  GSD_INQ_SIZE

      INTEGER*4    SIZE

*  GSD_GET

      INTEGER*4    START(2)
      INTEGER*4    END(2)

      INCLUDE  'FLAGCOMM'
      INCLUDE  'STACKCOMM'
      INCLUDE  'GSD_INDEX.INC'
      INCLUDE  'GSD_FILE.INC'
      INCLUDE  'GSD_VAR.INC'

C  Others

      REAL*4       FRAC_INT
      REAL*8       VRGEO, VRHEL, VRLSR

      IF (IERR.NE.0) RETURN
      STATUS = ADAM__OK

C  Find and read the necessary items of header information.

C  Backend

      CALL GSD_FIND  (IFD, 'C1BKE',
     &                NO, UNITS, TYPE, ARRAY, IND_BKE, STATUS)
      CALL GSD_GET0C (IND_BKE, BACKEND, STATUS)

C  Scan identification

C     ...Observer
      CALL GSD_FIND  (IFD, 'C1OBS',
     &                NO, UNITS, TYPE, ARRAY, IND_OBS, STATUS)
      CALL GSD_GET0C (IND_OBS, OBSERVER, STATUS)

C     ...Telescope
      CALL GSD_FIND  (IFD, 'C1TEL',
     &                NO, UNITS, TYPE, ARRAY, IND_TEL, STATUS)
      CALL GSD_GET0C (IND_TEL, TELESCOPE, STATUS)

C     ...Source name
      CALL GSD_FIND (IFD, 'C1SNA1',
     &               NO, UNITS, TYPE, ARRAY, IND_SNAME1, STATUS)
      CALL GSD_FIND (IFD, 'C1SNA2',
     &               NO, UNITS, TYPE, ARRAY, IND_SNAME2, STATUS)
      CALL GSD_GET0C (IND_SNAME1, SOURCE_NAME(:16), STATUS)
      CALL GSD_GET0C (IND_SNAME2, SOURCE_NAME(17:), STATUS)
      IF (STATUS.NE.ADAM__OK) GO TO 99

C     ...scan number
      CALL GSD_FIND  (IFD, 'C1SNO',
     &                NO, UNITS, TYPE, ARRAY, IND_SCAN, STATUS)
      CALL GSD_GET0I (IND_SCAN, SCAN_NO, STATUS)

C  Date and time of observation

C     ...Date
      CALL GSD_FIND  (IFD, 'C3DAT',
     &                NO, UNITS, TYPE, ARRAY, IND_DAT, STATUS)
      CALL GSD_GET0D (IND_DAT, DDATE, STATUS)

C     ...Time (UT)
      CALL GSD_FIND  (IFD, 'C3UT',
     &                NO, UNITS, TYPE, ARRAY, IND_UT, STATUS)
      CALL GSD_GET0D (IND_UT, DUT, STATUS)
CD    WRITE (ILOUT,*) ddate,dut
      IF (STATUS.NE.ADAM__OK) GO TO 99

C  Source position (map centre)

C     ...R.A.
      CALL GSD_FIND  (IFD, 'C4ERA',
     &                NO, UNITS, TYPE, ARRAY, IND_RA, STATUS)
      CALL GSD_GET0D (IND_RA, RADG, STATUS)

C     ...Declination
      CALL GSD_FIND  (IFD, 'C4EDEC',
     &                NO, UNITS, TYPE, ARRAY, IND_DEC, STATUS)
      CALL GSD_GET0D (IND_DEC, DECDG, STATUS)
CD    WRITE (ILOUT,*) radg,decdg
      IF (STATUS.NE.ADAM__OK) GO TO 99

C  Mapping parameters

C     ...scan direction  (c6sd, Char)
      CALL GSD_FIND  (IFD, 'c6sd',
     &                NO, UNITS, TYPE, ARRAY, IND_SD, STATUS)
      CALL GSD_GET0C (IND_SD, SCAN_DIR, STATUS)

C     ...scan type  (c6st, Char)
      CALL GSD_FIND  (IFD, 'c6st',
     &                NO, UNITS, TYPE, ARRAY, IND_ST, STATUS)
      CALL GSD_GET0C (IND_ST, SCAN_TYPE, STATUS)

C     ...on the fly  (c3fly, I*4)
      CALL GSD_FIND  (IFD, 'c3fly',
     &                NO, UNITS, TYPE, ARRAY, IND_FLY, STATUS)
      CALL GSD_GET0I (IND_FLY, IFLY, STATUS)

C     ...map cell x-size (C6DX, R*8)
      CALL GSD_FIND  (IFD, 'C6DX',
     &                NO, UNITS, TYPE, ARRAY, IND_DX, STATUS)
      CALL GSD_GET0D (IND_DX, DX, STATUS)

C     ...map cell y-size (C6DY, R*8)
      CALL GSD_FIND  (IFD, 'C6DY',
     &                NO, UNITS, TYPE, ARRAY, IND_DY, STATUS)
      CALL GSD_GET0D (IND_DY, DY, STATUS)
CD    WRITE (ILOUT,*) 'map cell size dx,dy ', dx, dy
      IF (STATUS.NE.ADAM__OK) GO TO 99

C     ...# map x-points  (C6XNP, I*4)
      CALL GSD_FIND  (IFD, 'C6XNP',
     &                NO, UNITS, TYPE, ARRAY, IND_XNP, STATUS)
      CALL GSD_GET0I (IND_XNP, IXNP, STATUS)

C     ...# map y-points  (C6YNP, I*4)
      CALL GSD_FIND  (IFD, 'C6YNP',
     &                NO, UNITS, TYPE, ARRAY, IND_YNP, STATUS)
      CALL GSD_GET0I (IND_YNP, IYNP, STATUS)
CD    WRITE (ILOUT,*) 'Number of map x & y points: ', ixnp, iynp
      IF (STATUS.NE.ADAM__OK) GO TO 99

C     ...Start x  (C6XGC, R*4)
      CALL GSD_FIND  (IFD, 'C6XGC',
     &                NO, UNITS, TYPE, ARRAY, IND_XGC, STATUS)
      CALL GSD_GET0R (IND_XGC, XGC, STATUS)

C     ...Start y  (C6YGC, R*4)
      CALL GSD_FIND  (IFD, 'C6YGC',
     &                NO, UNITS, TYPE, ARRAY, IND_YGC, STATUS)
      CALL GSD_GET0R (IND_YGC, YGC, STATUS)
CD    WRITE (ILOUT,*) 'Map start position, x & y cells', xgc, ygc
      IF (STATUS.NE.ADAM__OK) GO TO 99

C     ...dX positive? (c6xpos, L)
      CALL GSD_FIND  (IFD, 'c6xpos',
     &                NO, UNITS, TYPE, ARRAY, IND_XPOS, STATUS)
      CALL GSD_GET0L (IND_XPOS, XPOS, STATUS)

C     ...dY positive? (c6ypos, L)
      CALL GSD_FIND  (IFD, 'c6ypos',
     &                NO, UNITS, TYPE, ARRAY, IND_YPOS, STATUS)
      CALL GSD_GET0L (IND_YPOS, YPOS, STATUS)
CD    WRITE (ILOUT,*) 'Increments in x & y positive?: ',xpos, ypos

C     ...P.A. of map y direction (from N) (Index already knows if it exists, but
C        get it again anyway to make subsequent changes easier)
      CALL GSD_FIND  (IFD, 'CELL_V2Y',
     &                NO, UNITS, TYPE, ARRAY, IND_V2Y, STATUS)
      CALL GSD_GET0D (IND_V2Y, V2Y, STATUS)
C     ...P.A. of map x-axis from map y-axis
      CALL GSD_FIND  (IFD, 'c4axy',
     &                NO, UNITS, TYPE, ARRAY, IND_X2Y, STATUS)
      CALL GSD_GET0D (IND_X2Y, X2Y, STATUS)
C     ...Offsets of map centre
      CALL GSD_FIND  (IFD, 'c4sx',
     &                NO, UNITS, TYPE, ARRAY, IND_XMAP_OFF, STATUS)
      CALL GSD_GET0D (IND_XMAP_OFF, XMAP_OFF, STATUS)
      CALL GSD_FIND  (IFD, 'c4sy',
     &                NO, UNITS, TYPE, ARRAY, IND_YMAP_OFF, STATUS)
      CALL GSD_GET0D (IND_YMAP_OFF, YMAP_OFF, STATUS)
C     ...Local system coordinate code
      CALL GSD_FIND  (IFD, 'c4lsc',
     &                NO, UNITS, TYPE, ARRAY, IND_LSC, STATUS)
      CALL GSD_GET0C (IND_LSC, LOCAL_COSYS, STATUS)

CD    WRITE (ILOUT,*) 'v2y & x2y', v2y, x2y
CD    WRITE (ILOUT,*) 'map offsets (x,y) ', xmap_off, ymap_off
CD    WRITE (ILOUT,*) 'local coordinate system', '   ', LOCAL_COSYS(1:2)

CD    WRITE (ILOUT,*) scan_dir,' ',scan_type,' ',dx,dy
CD    WRITE (ILOUT,*) scan_angle,ixnp,iynp,xgc,ygc,xpos,ypos

C  Pointing History table

      CALL GSD_FIND     (IFD, 'c14phist',
     &                   NO, UNITS, TYPE, ARRAY, IND_PHIST, STATUS)
      CALL GSD_INQ_SIZE (IFD, NO, 2, DIMNAMES, DIMUNITS, DIMVALS,
     &                   PH_DIMVALS, SIZE, STATUS)

      DIMVALS(1) = 2
      START(1)      = 1
      START(2)      = 1
      END(1)        = DIMVALS(1)
      END(2)        = DIMVALS(2)

      CALL GSD_GET1R    (IND_PHIST, 2, DIMVALS, START, END,
     &                   PHIST, ACTVALS, STATUS)
      IF (STATUS.NE.ADAM__OK) GO TO 99
CD    WRITE (ILOUT,*) 'Pointing history table'
CD    WRITE (ILOUT,'((1X,I4,2(1X,F8.2)))')
CD   &      (J,(PHIST(I,J),I=1,2),J=1,END(2))

C  Miscellaneous pars

C     ...radial velocity
      CALL GSD_FIND  (IFD, 'C7VR',
     &                NO, UNITS, TYPE, ARRAY, IND_VRAD, STATUS)
      CALL GSD_GET0D (IND_VRAD, VRAD, STATUS)

      CALL GSD_FIND  (IFD, 'C12VREF',
     &                NO, UNITS, TYPE, ARRAY, IND_VREF, STATUS)
      CALL GSD_GET0C (IND_VREF, VREF, STATUS)

      CALL GSD_FIND  (IFD, 'C12VDEF',
     &                NO, UNITS, TYPE, ARRAY, IND_VDEF, STATUS)
      CALL GSD_GET0C (IND_VDEF, VDEF, STATUS)

      IF (STATUS.NE.ADAM__OK) THEN
        VDEF   = 'RADIO'
        VREF   = 'LSR'
        STATUS =  ADAM__OK
      END IF

C     ...radial velocities of telescope, earth and sun

      CALL GSD_FIND  (IFD, 'C7VRADIAL',
     &                NO, UNITS, TYPE, ARRAY, IND_VRADTEL, STATUS)
      CALL GSD_GET1D (IND_VRADTEL, 1, 48, 13, 13, VRGEO, LVAL, STATUS)
      CALL GSD_GET1D (IND_VRADTEL, 1, 48, 25, 25, VRHEL, LVAL, STATUS)
      CALL GSD_GET1D (IND_VRADTEL, 1, 48,  1,  1, VRLSR, LVAL, STATUS)
CD    WRITE (ILOUT,*), 'vrgeo, vrhel, vrlsr: ', vrgeo, vrhel, vrlsr

      IF (STATUS.NE.ADAM__OK) THEN
        VSL    = 0.0
        VES    = 0.0
        VTE    = 0.0
        STATUS =  ADAM__OK
      ELSE
        VSL    = - (VRLSR - VRHEL)
        VES    = - (VRHEL - VRGEO)
        VTE    = - (VRGEO)
CD      WRITE (ILOUT,*), 'vsl, ves, vte: ', vsl, ves, vte
      END IF

C     ...azimuth
      CALL GSD_FIND  (IFD, 'C4AZ',
     &                NO, UNITS, TYPE, ARRAY, IND_AZ, STATUS)
      CALL GSD_GET0D (IND_AZ, AZIMUTH, STATUS)
      IF (STATUS.NE.ADAM__OK) GO TO 99

C     ...elevation
      CALL GSD_FIND  (IFD, 'C4EL',
     &                NO, UNITS, TYPE, ARRAY, IND_ELEV, STATUS)
      CALL GSD_GET0D (IND_ELEV, ELEVATION, STATUS)
CD    WRITE (ILOUT,*) 'vrad, azimuth, elevation ',
CD   &                 vrad, azimuth, elevation
      IF (STATUS.NE.ADAM__OK) GO TO 99

C  Integration parameters

C     ...integration time
C     Look for an actual integration time (as stored by RXC for example,
C     but if we can't find it, use the old parameter)

      CALL GSD_FIND  (IFD, 'C3AIT',
     &                NO, UNITS, TYPE, ARRAY, IND_INTIME, STATUS)
      IF (STATUS .NE. ADAM__OK) THEN
        STATUS = ADAM__OK
        CALL GSD_FIND  (IFD, 'C3SRT',
     &                  NO, UNITS, TYPE, ARRAY, IND_INTIME, STATUS)
      END IF
      CALL GSD_GET0D (IND_INTIME, INT_TIME, STATUS)
CD    WRITE (ILOUT,*) 'int_time ', int_time
      IF (STATUS.NE.ADAM__OK) GO TO 99

C     Now correct the integration time to comply with the definition
C     of being the "on+off" time for on-the-fly.
C     For on-the-fly rasters, note that sqrt(#/row)*t_on  time
C     is observed at the "off" position, but that C3SRT will be
C     the total "on" for the complete row.
C
      IF ( IFLY .NE. 0 ) THEN
        INT_TIME =
     &    4.D0 / (1.D0 + 1.D0/SQRT(1.D0*IXNP)) * INT_TIME / IXNP
      END IF

C     ...# phases per cycle
      CALL GSD_FIND  (IFD, 'C3PPC',
     &                NO, UNITS, TYPE, ARRAY, IND_PPC, STATUS)
      CALL GSD_GET0I (IND_PPC, PPC, STATUS)

C     ...# cycles per scan
      CALL GSD_FIND  (IFD, 'c3nci',
     &                NO, UNITS, TYPE, ARRAY, IND_NCI, STATUS)
      CALL GSD_GET0I (IND_NCI, NCI, STATUS)

C     ...# phases measured
      CALL GSD_FIND  (IFD, 'C6NP',
     &                NO, UNITS, TYPE, ARRAY, IND_NP, STATUS)
      CALL GSD_GET0I (IND_NP, NP, STATUS)
CD     WRITE (ILOUT,*) 'ppc,nci,np ', ppc,nci,np

      IF (STATUS.NE.ADAM__OK) GO TO 99

*  the following logic only works for discretely sampled maps, I think (JFL).
*  It may well need changing for `on the fly' spectra.

C     ...Number of spectra in scan file (calculated)
      NGSDSPEC = (NP-1)/(PPC*NCI) + 1
      IF (NP.EQ.0) NGSDSPEC = 0

C     ...See if there are any scans incomplete, if so calculate
C     integration time of last integration in file

      NSPEC_FULL = NP/(PPC*NCI)

      IF (NSPEC_FULL .EQ. NGSDSPEC .AND. NGSDSPEC.NE.0) THEN
        INT_TIME_LAST = INT_TIME
      ELSE IF (NGSDSPEC.NE.0) THEN
        FRAC_INT      = FLOAT(NP) / FLOAT(PPC*NCI) - NSPEC_FULL
        INT_TIME_LAST = FRAC_INT * INT_TIME
      END IF

      IF (NGSDSPEC.NE.NSPEC_FULL) THEN
        WRITE (ILOUT,*) 'Last scan finished prematurely -'
        WRITE (ILOUT,*) '   # complete spectra in file  ', NSPEC_FULL,
     &                  ' - Integ''n time ', SNGL(INT_TIME)
        WRITE (ILOUT,*) '   # last spectrum  - Integ''n time ',
     &                  SNGL(INT_TIME_LAST)
      END IF

C  Generate standard SPECX header items from GSD data

      CALL TRAN_DATE (DDATE, IDATE)
      CALL TRAN_TIME (DUT,   ITIME)
      RA   = RADG
      DEC  = DECDG
      CALL VELENCODE (VREF, VDEF, LSRFLG)

      LSCAN  = SCAN_NO
      INTT   = NINT (1000 * INT_TIME)  ! Total integration time (msec)
      VLSR   = VRAD                    ! Radial velocity (km/s)
      AZ     = AZIMUTH             ! Azimuth
      EL     = ELEVATION           ! Elevation
      IMODE  = 2                   ! Switched, ON-OFF
      ICALZD = 0                   ! Calibration zenith distance (0 deg)
      IUTFLG = 1                   ! Time is in U.T.
      IQCEN  = 0                   ! All quadrants have right freq data
      ITITLE = '            '//SOURCE_NAME(:8)//' '//TELESCOPE(:5)

C  Data parameters

      IF (NGSDSPEC.NE.0) THEN

C     ...Number of backend receivers (sections, quadrants)
C        change find of 'C3NRC' to 'C3NRS' here! (RMP)

         CALL GSD_FIND  (IFD, 'C3NRS',
     &                   NO, UNITS, TYPE, ARRAY, IND_NRC, STATUS)
         CALL GSD_GET0I (IND_NRC, NRC, STATUS)
         IF (STATUS.NE.ADAM__OK) GO TO 99

      ELSE
        IERR = 38  ! Report that no data
      END IF

C  Standard return

   99 IF (STATUS.NE.ADAM__OK) THEN
        PRINT '('' Status'',2X,I10,4X,''($'',Z8.8,'')'')', status,status
        IERR = 36
      END IF

      RETURN
      END
