*   History:  18/09/00  Define and use TRUBYT to pass to ASTRO_TIMES (AJC)
*                       Unused I, ISB, DECRAD, RARAD, DEC_TO_RAD
*              3/05/01  Initialise STATUS  (AJC)
*             22/08/05  TIMJ: Use DATA for init
*-----------------------------------------------------------------------

      SUBROUTINE SPECX_WRFITSSPEC (IFAIL)

*  Routine to write scan in stack X-register to a tape or disk-FITS file.
*  We assume that the FITS job is fired up elsewhere by means of the
*  FIT_INIT command, and similarly closed down elsewhere by means of the
*  FIT_END command.

      IMPLICIT  NONE

*     formal parameters:

      INTEGER   IFAIL       !  Error status, returned

*     include files

      INCLUDE  'STACKCOMM'      !  Stack (thus data!)  definition
      INCLUDE  'FLAGCOMM'       !  Environment (e.g. telescope coords etc)
      INCLUDE  'SPECX_FITS'     !  FITS file control
      INCLUDE  'DOPPLER'        !  LO freq and doppler factor

*     externals
      DOUBLE PRECISION SPECXJD_TO_MJD

*     standard fits keywords...

      INTEGER   BITPIX
      REAL      BSCALE
      REAL      BZERO
      INTEGER   NAXIS
      INTEGER   NAXES(4)   ! NAXIS1, NAXIS2...

*    ... and non-standard keywords

      DOUBLE PRECISION    DATAMIN
      DOUBLE PRECISION    DATAMAX
      DOUBLE PRECISION    CRVAL1, CDELT1, CRPIX1
      DOUBLE PRECISION    CRVAL2, CDELT2, CRPIX2
      DOUBLE PRECISION    CRVAL3, CDELT3, CRPIX3
      DOUBLE PRECISION    AZ8, EL8
      DOUBLE PRECISION    L2,  B2
      DOUBLE PRECISION    IMAGFREQ
      DOUBLE PRECISION    DELTAV
      DOUBLE PRECISION    TAUATM
      DOUBLE PRECISION    FORWEFF
      DOUBLE PRECISION    BEAMEFF
      DOUBLE PRECISION    GAINIMAG
      DOUBLE PRECISION    RFOFF
      INTEGER             BLANK
      INTEGER             VELREF
      CHARACTER           LSTSTR*12
      CHARACTER           OBS_DATE*24
      CHARACTER           UTSTR*12
      CHARACTER           WRITE_DATE*24
      CHARACTER           LINE*12
      CHARACTER           OBJECT*12
      CHARACTER           ORIGIN*32

*     variables used to interface FIT_ routines.

      LOGICAL   CHECK
      REAL      DMIN
      REAL      DMAX
      REAL      ERRORS(3)
      INTEGER   STATUS
      REAL      SCALES(3)
      REAL      ZEROS(3)

*     Local and intermediate variables

      CHARACTER          ERROR*64
      INTEGER            NBLANK
      LOGICAL            NSVEL
      CHARACTER          WRDATE*9
      CHARACTER          WRTIME*8
      CHARACTER          VELCODE*8
      CHARACTER          VFRAME*4         ! 'TELL', 'HELI', 'GEO' or 'LSR'
      CHARACTER          VDEF*3           ! 'OPT', 'RAD', or 'REL'
      REAL               VLSR_0
      REAL               RBLANK

      DOUBLE PRECISION   DTEMP
      DOUBLE PRECISION   C
      DOUBLE PRECISION   HOUR_ANGLE
      DOUBLE PRECISION   UTD, UTHRS, JULIAN_DATE, SIDEREAL_TIME
      DOUBLE PRECISION   WR_JULIAN_DATE
      DOUBLE PRECISION   DPI

*     TRUE value to pass as IUTFLG to ASTRO_TIMES
      BYTE                TRUBYT
      DATA TRUBYT/1/

      DATA C /299792.0D3/
      DATA DPI  /3.141592654/

*  Ok, go...

      IFAIL = 0
      STATUS = 0

*     Check FITS output file open

      IF (.NOT. FITS_OPEN) THEN
        IFAIL = 111
        RETURN
      END IF

*     Substitute BAD values with RBLANK (returned). Scale RBLANK to
*     BLANK after call to FIT_SCALC.

      CALL BAD2BLANK (1, DATA, NPTS(1), RBLANK, NBLANK, IFAIL)

*     Find scaling for data using FIT_SCALC

      CHECK = .FALSE.
      CALL FIT_SCALC (DATA, NPTS(1), CHECK, DMIN, DMAX,
     &                SCALES, ZEROS, ERRORS)

*     Write a standard header using FIT_HSTAN

      BITPIX   = 32
      BSCALE   = SCALES(3)
      BZERO    = ZEROS(3)
      NAXIS    = 4

      NAXES(1) = NPTS(1)
      NAXES(2) = 1
      NAXES(3) = 1
      NAXES(4) = 1

      CALL FIT_HSTAN (BITPIX, NAXIS, NAXES, BSCALE, BZERO, STATUS)
      IF (STATUS.NE.0) GO TO 99

*     Now evaluate and write the rest of the IRAM header parameters

*     Max and min of data array...

      DATAMIN = DBLE (DMIN)
      DATAMAX = DBLE (DMAX)

*     Astronomical times...

      CALL ASTRO_TIMES (ITIME, IDATE, ALONG, TIMCOR, IUTFLG,
     &                  UTD, SIDEREAL_TIME, JULIAN_DATE)
      UTHRS   = (UTD - DFLOAT(INT(UTD))) * 24.

*     Calculate the current time in the correct FITS format
*     This requires that we find the current time and date
*     and convert it to Julian date
      CALL UGETDATE        (WRDATE,        STATUS)
      CALL UGETTIME        (WRTIME,        STATUS)
*      CALL DATE_CVT        (WRDATE,        WRITE_DATE)
      CALL ASTRO_TIMES (WRTIME, WRDATE, 0.0D0, 0.0D0, TRUBYT,
     &                  DTEMP, DTEMP, WR_JULIAN_DATE)
      CALL CVT_TO_DATE_OBS( SPECXJD_TO_MJD(WR_JULIAN_DATE), WRITE_DATE)

*     Calculate the DATE-OBS string in the correct format
*      CALL DATE_CVT        (IDATE,         OBS_DATE)
      CALL CVT_TO_DATE_OBS( SPECXJD_TO_MJD(JULIAN_DATE), OBS_DATE)
      CALL HOURS_TO_STRING (UTHRS,         UTSTR)
      CALL HOURS_TO_STRING (SIDEREAL_TIME, LSTSTR)

      HOUR_ANGLE = SIDEREAL_TIME*DPI/12. - RA*DPI/180.

      CALL HADEC_TO_AZEL   (HOUR_ANGLE,  DEC*DPI/180., DPI*ALAT/180.,
     &                      AZ8,         EL8)
      CALL RADEC_TO_L2B2   (RA*DPI/180., DEC*DPI/180., L2, B2)

      AZ8 = AZ8 * 180./DPI
      EL8 = EL8 * 180./DPI

*     Gain and frequency parameters...
*     (n.b.: second call to SETXDOPP in CALC_IMAGE also sets DOPPFAC)

      CALL CALC_IMAGE (1, LOFREQ, IFFREQ, JFCEN, JFREST, JFINC,
     &                 IQCEN, VLSR, VTE, VES, VSL, LSRFLG,
     &                 RA, DEC, ITIME, IDATE, IUTFLG,
     &                 IMAGFREQ, VLSR_0, IFAIL)

      DELTAV   = - DFLOAT (JFINC(1)) * C / (1.D3*JFREST(1)*DOPPFAC)
      RFOFF    =   1.D3 * (JFCEN(1) - JFREST(1))

      NSVEL = .FALSE.
      CALL VELDECODE (LSRFLG, VFRAME, VDEF)

      IF (VDEF.EQ.'OPT')    VELREF = 0
      IF (VDEF.EQ.'RAD')    VELREF = 256
      IF (VDEF.EQ.'REL')    VELREF = 512
      IF (VFRAME.EQ.'LSR ') VELREF = VELREF + 1
      IF (VFRAME.EQ.'HELI') VELREF = VELREF + 2
      IF (VFRAME.EQ.'TELL') VELREF = VELREF + 3
      IF (VFRAME.EQ.'GEO' ) VELREF = VELREF + 4
      IF (VFRAME.EQ.'GEO' .or. VDEF.eq.'REL') THEN
        PRINT *, '*** specx_wrfitsspec ***'
        PRINT *, '    non-standard velocity frame/law combination!'
        PRINT *, '    -- VELREF value may not be recognized; be warned.'
      END IF

      IF (VFRAME.eq.'LSR' .and. VDEF.eq.'RAD') THEN
        VELCODE = 'VLSR'
      ELSE IF (VFRAME.eq.'HELI' .and. VDEF.eq.'OPT') THEN
        VELCODE = 'VHEL'
      ELSE IF (VFRAME.eq.'GEO' .and. VDEF.eq.'RAD') THEN
        VELCODE = 'VEAR'
      ELSE IF (VFRAME.eq.'TELL' .and. VDEF.eq.'RAD') THEN
        VELCODE = 'VOBS'
      ELSE
        PRINT *, '*** specx_wrfitsspec ***'
        PRINT *, '    non-standard velocity frame/law combination!'
        PRINT *, '    -- encoded as FITS keyword VLSR; be warned.'
        VELCODE = 'VLSR'
        NSVEL   = .TRUE.
      END IF

      PRINT *, ' -- specx_wrfitsspec --'
      PRINT *, '    VFRAME  = ', VFRAME
      PRINT *, '    VDEF    = ', VDEF
      PRINT *, '    VELCODE = ', VELCODE
      PRINT *, '    VELREF  = ', VELREF
      PRINT *, '    Image frequency = ', imagfreq
      PRINT *, '    Deltav          = ', deltav


*     Calibration parameters

      TAUATM   = ALOG (293./(293.-FLOAT(ITSKY(1))))
      GAINIMAG = 1.0D0
      BEAMEFF  = 1.0D0
      FORWEFF  = 1.0D0

*     Miscellaneous

      ORIGIN   = 'Unix Specx V6.8'
      LINE     = '*'
*     ITITLE was changed to accommodate for subscanno > 999
*     assume old format if ititle(12) not blank.
      IF (ITITLE(12:12) .NE. ' ') THEN
        OBJECT   = ITITLE(12:20)
      ELSE
        OBJECT   = ITITLE(13:20)
      ENDIF

*     -----------------------------------------------

*     Definition of axes of 4-D dataset...

      CRVAL1  = RFOFF                     ! Offset frequency
      CDELT1  = DFLOAT (JFINC(1))         ! Freq step (Hz)
      CRPIX1  = DFLOAT (NPTS(1)+1) *0.5   ! Reference pixel

      CRVAL2  = RA                        ! RA at ref pixel (deg).
      CDELT2  = DRA/3600.                 ! RA map pixel size (deg)
      CRPIX2  = 0.00D0                    ! Offset to reference pixel

      CRVAL3  = DEC                       ! Dec at ref pixel (deg)
      CDELT3  = DDEC/3600.                ! Dec map pixel size (deg)
      CRPIX3  = 0.00D0                    ! Offset to reference pixel

*     -----------------------------------------------

      CALL FIT_WDBLE ('DATAMIN ', DATAMIN, ' ', STATUS)
      CALL FIT_WDBLE ('DATAMAX ', DATAMAX, ' ', STATUS)
      CALL FIT_WSTR  ('BUNIT   ', 'K',     ' ', STATUS)

      CALL FIT_WSTR  ('CTYPE1  ', 'FREQ',  ' ', STATUS)
      CALL FIT_WDBLE ('CRVAL1  ', CRVAL1,
     &                'Offset frequency', STATUS)
      CALL FIT_WDBLE ('CDELT1  ', CDELT1,
     &                'Frequency resolution',STATUS)
      CALL FIT_WDBLE ('CRPIX1  ', CRPIX1,  ' ', STATUS)

      CALL FIT_WSTR  ('CTYPE2  ','RA---SIN',' ', STATUS)
      CALL FIT_WDBLE ('CRVAL2  ', CRVAL2,  ' ', STATUS)
      CALL FIT_WDBLE ('CDELT2  ', CDELT2,  ' ', STATUS)
      CALL FIT_WDBLE ('CRPIX2  ', CRPIX2,  ' ', STATUS)

      CALL FIT_WSTR  ('CTYPE3  ','DEC--SIN',' ', STATUS)
      CALL FIT_WDBLE ('CRVAL3  ', CRVAL3,  ' ', STATUS)
      CALL FIT_WDBLE ('CDELT3  ', CDELT3,  ' ', STATUS)
      CALL FIT_WDBLE ('CRPIX3  ', CRPIX3,  ' ', STATUS)

      CALL FIT_WSTR  ('CTYPE4  ', 'STOKES',' ', STATUS)
      CALL FIT_WDBLE ('CRVAL4  ', 1.00D0,  ' ', STATUS)
      CALL FIT_WDBLE ('CDELT4  ', 1.00D0,  ' ', STATUS)
      CALL FIT_WDBLE ('CRPIX4  ', 1.00D0,  ' ', STATUS)

      CALL FIT_WSTR  ('TELESCOP', OBSTIT,  ' ', STATUS)
      CALL FIT_WSTR  ('OBJECT  ', OBJECT,  ' ', STATUS)

      CALL FIT_WDBLE ('GLON    ', L2,
     &                'Galactic longitude (deg)', STATUS)
      CALL FIT_WDBLE ('GLAT    ', B2,
     &                'Galactic latitude (deg)',  STATUS)
      CALL FIT_WDBLE ('EPOCH   ', 1950.D0, ' ',   STATUS)

*     If values were blanked
      IF (NBLANK .NE. 0) THEN
        BLANK    = (RBLANK-BZERO)/BSCALE+0.5
        CALL FIT_WINT  ('BLANK   ', BLANK,
     &                'Bad channel (blanking) value ', STATUS)
        CALL FIT_WSTR  ('LINE    ', LINE,
     &                'Line name ',             STATUS)
      ENDIF

      IF (NSVEL) THEN
        CALL FIT_WSTR  ('VEL-FRAM ', VFRAME,
     &                  'Velocity frame ',        STATUS)
        CALL FIT_WSTR  ('VEL-LAW  ', VDEF,
     &                  'Velocity law ',          STATUS)
      END IF

      CALL FIT_WDBLE ( VELCODE,   1.D3*DBLE(VLSR_0),
     &                'Velocity of reference channel (m/s)', STATUS)
      CALL FIT_WDBLE ('DELTAV  ', DELTAV,
     &                'Nominal velocity resolution (m/s)',   STATUS)
      CALL FIT_WINT  ('VELREF  ', VELREF,
     &                '>256 RADIO, 1 LSR 2 HEL 3 OBS', STATUS)
      CALL FIT_WDBLE ('ALTRVAL ',1.D3*DBLE(VLSR_0),
     &                'ALTERNATE FREQ/VEL REF VALUE', STATUS)
      CALL FIT_WDBLE ('ALTRPIX ', DFLOAT(NPTS(1)+1)*0.5,
     &                'ALTERNATE FREQ/VEL REF PIXEL', STATUS)

      CALL FIT_WDBLE ('RESTFREQ', DFLOAT(JFREST(1))*1.D3,
     &                'Rest frequency (Hz)',                 STATUS)
      CALL FIT_WDBLE ('LOFREQ',   1.D9*LOFREQ(1),
     &                'Local oscillator frequency (Hz) ',    STATUS)
      CALL FIT_WDBLE ('IFFREQ',   1.D9*IFFREQ(1),
     &                'Intermediate frequency (Hz) ',        STATUS)
      CALL FIT_WDBLE ('IMAGFREQ', IMAGFREQ,
     &                'Image sideband frequency (Hz) ',      STATUS)
      CALL FIT_WDBLE ('TSYS    ', DBLE(TSYS(1)),
     &                'System temperature (K)',              STATUS)
      CALL FIT_WDBLE ('OBSTIME ', DFLOAT(INTT)/1000.,
     &                'Integration time (sec)',              STATUS)
      CALL FIT_WDBLE ('SCAN-NUM', DFLOAT(LSCAN),
     &                'Scan number ',                        STATUS)
      CALL FIT_WDBLE ('TAU-ATM ', TAUATM,
     &                'Zenith opacity (Nepers) ',            STATUS)
      CALL FIT_WDBLE ('BEAMEFF ', BEAMEFF,
     &                'Beam efficiency ',                    STATUS)
      CALL FIT_WDBLE ('FORWEFF ', FORWEFF,
     &                'Forward efficiency ',                 STATUS)
      CALL FIT_WDBLE ('GAINIMAG', GAINIMAG,
     &                'Image sideband gain ratio ',          STATUS)
      CALL FIT_WDBLE ('MJD-OBS', SPECXJD_TO_MJD(JULIAN_DATE),
     &     'Modified Julian Date of observation', STATUS)

      CALL FIT_WSTR  ('ORIGIN  ', ORIGIN,
     &                'Originating reduction system ',       STATUS)
      CALL FIT_WSTR  ('DATE    ', WRITE_DATE,
     &                'Date FITS file written ',             STATUS)
      CALL FIT_WSTR  ('DATE-OBS', OBS_DATE,
     &                'Date of observation ',                STATUS)

      CALL FIT_WDBLE ('ELEVATIO', EL8,
     &                'Telescope elevation (deg) ',          STATUS)
      CALL FIT_WDBLE ('AZIMUTH ', AZ8,
     &                'Telescope azimuth (deg) ',            STATUS)

      CALL FIT_WSTR  ('UT      ', UTSTR,
     &                'Universal time at start of obs''n ',  STATUS)
      CALL FIT_WSTR  ('LST     ', LSTSTR,
     &                'Sidereal time at start of obs''n ',   STATUS)

*     Signal the end of the header and write data array:

      CALL FIT_WEND  (STATUS)
      IF (STATUS.NE.0) GO TO 99
      CALL FIT_WRAYF (DATA, NPTS(1), BITPIX, BSCALE, BZERO, STATUS)
      IF (STATUS.NE.0) GO TO 99

*     Close the image

      CALL FIT_CLOSE (STATUS)
      IF (STATUS.NE.0) GO TO 99

      RETURN

*     Error handling

   99 CONTINUE

      PRINT *, 'Error in writing spectrum to FITS file: Status =',
     :     STATUS
      CALL FIT_ERROR (STATUS, ERROR)
      PRINT *,  ERROR

      RETURN
      END

*-----------------------------------------------------------------------
