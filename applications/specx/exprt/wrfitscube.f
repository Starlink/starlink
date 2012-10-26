*-----------------------------------------------------------------------
*   History:  02/09/95  Created from SPECX_WRFITSMAP.FOR  (RP)
*             18/09/00  Define and use TRUBYT to pass to ASTRO_TIMES (AJC)
*                       Unused ISTAT, IMAG_CHAN, DECRAD, RARAD, ISB,
*                         IFREVM, DMS_TO_RAD
*              3/05/01  Initialise STATUS (AJC)
*             22/08/05  Init using DATA (TIMJ)
*-----------------------------------------------------------------------

      SUBROUTINE SPECX_WRFITSCUBE (IFAIL)

*  Routine to write current map cube  to a tape or disk-FITS file.
*  We assume that the FITS job is fired up elsewhere by means of the
*  FIT_INIT command, and similarly closed down elsewhere by means of the
*  FIT_END command.

      IMPLICIT  NONE

*     formal parameters:

      INTEGER   IFAIL       !  Error status, returned

*     include files

      INCLUDE  'FLAGCOMM'       !  Environment (e.g. location of telescope etc)
      INCLUDE  'DOPPLER'        !  LO freq and doppler factor
      INCLUDE  'MAPS'           !  Info about maps (interpolation, rotation etc)
      INCLUDE  'MAPHD'          !  Header of currently open map file
      INCLUDE  'CUBE'           !  Addresses & dimensions of data cubes
      INCLUDE  'SPECX_FITS'     !  FITS file system parameters

      INCLUDE  'STACKCOMM'      !  Info on Stack which is used as temp
      INCLUDE  'STAKPAR'        !  storage space for swapped cube
      INCLUDE  'CNF_PAR'        ! For CNF_PVAL function

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
      DOUBLE PRECISION    CRVAL1, CDELT1, CRPIX1, CROTA1
      DOUBLE PRECISION    CRVAL2, CDELT2, CRPIX2, CROTA2
      DOUBLE PRECISION    CRVAL3, CDELT3, CRPIX3, CROTA3
      DOUBLE PRECISION    AZ8, EL8
      DOUBLE PRECISION    RA8, DEC8
      DOUBLE PRECISION    L2,  B2
      DOUBLE PRECISION    IMAGFREQ
      DOUBLE PRECISION    DELTAV
      DOUBLE PRECISION    TAUATM
      DOUBLE PRECISION    FORWEFF
      DOUBLE PRECISION    BEAMEFF
      DOUBLE PRECISION    GAINIMAG
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

*     Map handling

      INTEGER            IPTR

*     Local and intermediate variables

      INTEGER            I, J, K, NVAR
      CHARACTER          ERROR*64

      LOGICAL            AUTO_CENTRE
      REAL               DXC,    DYC
      REAL               XMID,   YMID

      LOGICAL            NSVEL
      INTEGER            NBLANK
      REAL               VLSR_0
      REAL               RBLANK
      REAL               FMID
      CHARACTER          WRDATE*9
      CHARACTER          WRTIME*8
      CHARACTER          VELCODE*8
      CHARACTER          VFRAME*4         ! 'TELL', 'HELI', 'GEO' or 'LSR'
      CHARACTER          VDEF*3           ! 'OPT', 'RAD', or 'REL'

      DOUBLE PRECISION   DTEMP
      DOUBLE PRECISION   C
      DOUBLE PRECISION   HOUR_ANGLE
      DOUBLE PRECISION   UTD, UTHRS, JULIAN_DATE, SIDEREAL_TIME
      DOUBLE PRECISION   DPI
      DOUBLE PRECISION   WR_JULIAN_DATE

*     TRUE value to pass as IUTFLG to ASTRO_TIMES
      BYTE                TRUBYT
      DATA TRUBYT/1/

      DATA C /299792.0D3/
      DATA DPI  /3.141592654/

*  Ok, go...

      IFAIL = 0
      STATUS = 0

*     Check there is at least one spectrum in the cube

      IF (NSPEC .le. 0) THEN
        IFAIL = 60
        RETURN
      END IF

*     Check FITS output file open

      IF (.NOT. FITS_OPEN) THEN
        IFAIL = 111
        RETURN
      END IF

*     Give error message if RA size exceeds max size stack DATA array
*     which is used as temp space for swapping order of the cube.

      IF (MSTEP .GT. 1024) THEN
         PRINT *, 'X-size map exceeds max. dim. Contact prog. staff'
         STATUS = 1
         GO TO 99
      ENDIF

*     For brevity, use IPTR to point to cube

      IPTR  = CURRENT_CUBE_ADDRESS

*     Substitute BAD values with RBLANK (returned). Scale RBLANK to
*     BLANK after call to FIT_SCALC.

      CALL BAD2BLANK (3, %VAL(CNF_PVAL(IPTR)), 
     :                NPTS1*MSTEP*NSTEP, RBLANK,
     &                NBLANK, IFAIL)

*     Find scaling for data using FIT_SCALC

      CHECK = .FALSE.
      CALL FIT_SCALC  (%VAL(CNF_PVAL(IPTR)), NPTS1*MSTEP*NSTEP, CHECK,
     &                  DMIN, DMAX, SCALES, ZEROS, ERRORS)

*     Write a standard header using FIT_HSTAN

      BITPIX   = 32
      BSCALE   = SCALES(3)
      BZERO    = ZEROS(3)
      NAXIS    = 3                   ! Ignore Stokes for now

      NAXES(1) = MSTEP
      NAXES(2) = NSTEP
      NAXES(3) = NPTS1
      NAXES(4) = 1

      CALL FIT_HSTAN (BITPIX, NAXIS, NAXES, BSCALE, BZERO, STATUS)
      IF (STATUS.NE.0) GO TO 99

*     Now evaluate and write the rest of the header parameters

*     Max and min of data array...

      DATAMIN = DBLE (DMIN)
      DATAMAX = DBLE (DMAX)

*     Astronomical times...

      CALL ASTRO_TIMES (ITIME, IDATE, ALONG, TIMCOR, IUTFLG,
     &                  UTD, SIDEREAL_TIME, JULIAN_DATE)
      UTHRS   = (UTD - DFLOAT(INT(UTD))) * 24.

*    (Should we use the scan "map centre" as map centre?
*     Not if a map_centre has been set explicitly)

      AUTO_CENTRE = .TRUE.
      IF (RAM.ne.0.D0 .or. DECM.ne.0.D0) AUTO_CENTRE = .FALSE.

      IF (AUTO_CENTRE) THEN
        RA8  = RA
        DEC8 = DEC
        WRITE (6,*) 'Map centre set to that of current spectrum'
      ELSE
        RA8  = RAM
        DEC8 = DECM
        WRITE (6,*) 'Map centre as specified when map opened'
      END IF

*     ***** Map axes *****

*     Increment (in arcseconds) of X & Y associated with each
*     pixel (L->R, T->B).
*

      DXC  = -CELL_XSIZE         ! RA increment negative for astron data
      DYC  = +CELL_YSIZE         ! NOTE: Dec is inverted upon writeout

*     Which pixel in original map represents the map zero?

      FMID = FLOAT(NPTS1+1)/2.
      XMID = FLOAT(MSTEP+1)/2.
      YMID = FLOAT(NSTEP+1)/2.

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

      HOUR_ANGLE = SIDEREAL_TIME*DPI/12. - RA8

      CALL HADEC_TO_AZEL (HOUR_ANGLE, DEC8*DPI/180.D0, ALAT*DPI/180.,
     &                    AZ8,        EL8)
      CALL RADEC_TO_L2B2   (RA8*DPI/180.D0, DEC8*DPI/180.D0, L2, B2)
      AZ8 = AZ8 * 180./DPI
      EL8 = EL8 * 180./DPI

*     Gain and frequency parameters...
*     (n.b.: second call to SETXDOPP in CALC_IMAGE also sets DOPPFAC)

      CALL CALC_IMAGE (1, LOFREQ, IFFREQ, JFCEN, JFREST, JFINC,
     &                 IQCEN, VLSR, VTE, VES, VSL, LSRFLG,
     &                 RA8, DEC8, ITIME, IDATE, IUTFLG,
     &                 IMAGFREQ, VLSR_0, IFAIL)

      DELTAV   = - DFLOAT (JFINC(1)) * C / (1.D3*JFREST(1)*DOPPFAC)

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
        PRINT *, '*** specx_wrfitscube ***'
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
        PRINT *, '*** specx_wrfitscube ***'
        PRINT *, '    non-standard velocity frame/law combination!'
        PRINT *, '    -- encoded as FITS keyword VLSR; be warned.'
        VELCODE = 'VLSR'
        NSVEL   = .TRUE.
      END IF

      PRINT *, ' -- specx_wrfitscube --'
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

      ORIGIN   = 'UNIX Specx V6.8   '
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

      CRVAL1  = RA8                          ! RA at ref pixel (deg).
      CDELT1  = DXC/3600.                    ! RA map pixel size (deg)
      CRPIX1  = XMID                         ! Offset to reference pixel
      CROTA1  = 0.00D0                       ! Rotation angle ?

      CRVAL2  = DEC8                         ! Dec at ref pixel (deg)
      CDELT2  = DYC/3600.                    ! Dec map pixel size (deg)
      CRPIX2  = YMID                         ! Offset to reference pixel
      CROTA2  = -1.0D0*POS_ANGLE             ! Rotation angle ?

      CRVAL3  = 1.D3 * DFLOAT (JFCEN(1))     ! Frequency (Hertz)
      CDELT3  = DFLOAT (JFINC(1))            ! Freq step (Hertz)
      CRPIX3  = FMID                         ! Offset to reference pixel
      CROTA3  = 0.00D0

*     -----------------------------------------------

      CALL FIT_WDBLE ('DATAMIN ', DATAMIN, ' ', STATUS)
      CALL FIT_WDBLE ('DATAMAX ', DATAMAX, ' ', STATUS)
      CALL FIT_WSTR  ('BUNIT   ', 'K',     ' ', STATUS)

      CALL FIT_WSTR  ('CTYPE1  ', 'RA---SIN', ' ', STATUS)
      CALL FIT_WDBLE ('CRVAL1  ', CRVAL1,
     &                'Centre frequency', STATUS)
      CALL FIT_WDBLE ('CDELT1  ', CDELT1,
     &                'Frequency resolution',STATUS)
      CALL FIT_WDBLE ('CRPIX1  ', CRPIX1,  ' ', STATUS)
      CALL FIT_WDBLE ('CROTA1  ', CROTA1,  ' ', STATUS)

      CALL FIT_WSTR  ('CTYPE2  ', 'DEC--SIN', ' ', STATUS)
      CALL FIT_WDBLE ('CRVAL2  ', CRVAL2,     ' ', STATUS)
      CALL FIT_WDBLE ('CDELT2  ', CDELT2,     ' ', STATUS)
      CALL FIT_WDBLE ('CRPIX2  ', CRPIX2,     ' ', STATUS)
      CALL FIT_WDBLE ('CROTA2  ', CROTA2,     ' ', STATUS)

      CALL FIT_WSTR  ('CTYPE3  ', 'FREQ',  ' ', STATUS)
      CALL FIT_WDBLE ('CRVAL3  ', CRVAL3,     ' ', STATUS)
      CALL FIT_WDBLE ('CDELT3  ', CDELT3,     ' ', STATUS)
      CALL FIT_WDBLE ('CRPIX3  ', CRPIX3,     ' ', STATUS)
      CALL FIT_WDBLE ('CROTA3  ', CROTA3,     ' ', STATUS)

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
      CALL FIT_WDBLE ('EPOCH  ',  1950.D0, ' ',   STATUS)

*     If values were blanked
      IF (NBLANK .NE. 0) THEN
        BLANK    = (RBLANK-BZERO)/BSCALE+0.5
        CALL FIT_WINT  ('BLANK   ', BLANK,
     &                  'Bad channel (blanking) value ', STATUS)
      ENDIF

      CALL FIT_WSTR  ('LINE    ', LINE,
     &                'Line name ',             STATUS)

      IF (NSVEL) THEN
        CALL FIT_WSTR  ('VEL-FRAM ', VFRAME,
     &                  'Velocity frame ',        STATUS)
        CALL FIT_WSTR  ('VEL-LAW  ', VDEF,
     &                  'Velocity law ',          STATUS)
      END IF

      CALL FIT_WDBLE ('VLSR    ', 1.D3*DBLE(VLSR_0),
     &                'Velocity of reference channel (m/s)', STATUS)
      CALL FIT_WDBLE ('VEL     ', 1.D3*DBLE(VLSR_0),
     &                'Velocity of reference channel (m/s)', STATUS)
      CALL FIT_WDBLE ('DELTAV  ', DELTAV,
     &                'Nominal velocity resolution (m/s)',   STATUS)
      CALL FIT_WDBLE ( VELCODE,   1.D3*DBLE(VLSR_0),
     &                'Velocity of reference channel (m/s)', STATUS)
      CALL FIT_WINT  ('VELREF  ', VELREF,
     &                '>256 RADIO, 1 LSR 2 HEL 3 OBS', STATUS)
      CALL FIT_WDBLE ('ALTRVAL ',1.D3*DBLE(VLSR_0),
     &                'ALTERNATE FREQ/VEL REF VALUE', STATUS)
      CALL FIT_WDBLE ('ALTRPIX ', DFLOAT(NPTS(1)+1)*0.5,
     &                'ALTERNATE FREQ/VEL REF PIXEL', STATUS)

      CALL FIT_WDBLE ('RESTFREQ', DFLOAT(JFREST(1))*1.D3,
     &                'Rest frequency (Hz)',                 STATUS)
      CALL FIT_WDBLE ('IMAGFREQ', IMAGFREQ,
     &                'Image sideband frequency (Hz) ',      STATUS)
      CALL FIT_WDBLE ('LOFREQ',   1.D9*LOFREQ(1),
     &                'Local oscillator frequency (Hz) ',    STATUS)
      CALL FIT_WDBLE ('IFFREQ',   1.D9*IFFREQ(1),
     &                'Intermediate frequency (Hz) ',        STATUS)
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
     &     'Modified Julian Date of first observation', STATUS)

      CALL FIT_WSTR  ('ORIGIN  ', ORIGIN,
     &                'Originating reduction system ',       STATUS)
      CALL FIT_WSTR  ('DATE-MAP', WRITE_DATE,
     &                'Date FITS file written ',             STATUS)
      CALL FIT_WSTR  ('DATE', WRITE_DATE,
     &                'file creation date(YYYY-MM-DDThh:mm:ss UTC)',
     &                                                       STATUS)
      CALL FIT_WSTR  ('DATE-OBS', OBS_DATE,
     &                'Date of first observation ',          STATUS)

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

*     The cube is Freq,RA,-Dec whereas we want RA,Dec, Freq. Hence
*     use stack position to swap data.
*     Push existing data onto the stack to make room for the new scan

      IF (.NOT.XCLEAR) CALL PUSH
      IF (JTOP.EQ.0) JTOP = 1

*     Zero the existing data (just to be neat)

      CALL INITHD
      CALL INIT_ARRAY (LSTK-128, DATA, 0.0)

*     Loop over all channels
      DO K = 1, NPTS1

*        Write buffer upside down in DEC to conform to FITS standard
         DO J = NSTEP, 1, -1

*           Loop over RA (copy in subroutine to be able to use ptr)
*           Subroutine is below

            DO I = 1, MSTEP
               NVAR = (I-1) * NPTS1 + (J-1) * NPTS1 * MSTEP + K
               CALL CP_PNT(DATA(I),%VAL(CNF_PVAL(IPTR)+4*(NVAR-1)))
            END DO

*           Write row to FITS image
            CALL FIT_WRAYF (DATA, MSTEP, BITPIX, BSCALE, BZERO, STATUS)
            IF (STATUS.NE.0) GO TO 98
         END DO

      END DO

*     put old stack X back

   98 IF (.NOT.XCLEAR) CALL POP

*     close the image

      CALL FIT_CLOSE (STATUS)
      IF (STATUS.NE.0) GO TO 99

      RETURN

*     Error handling

   99 CONTINUE

      PRINT *, 'Error writing CUBE to FITS file: status = ', STATUS
      CALL FIT_ERROR (STATUS, ERROR)
      PRINT *, ERROR

      RETURN
      END

*-----------------------------------------------------------------------

      SUBROUTINE CP_PNT(VAR1,VAR2)

      REAL VAR1, VAR2

      VAR1 = VAR2

      RETURN
      END

*-----------------------------------------------------------------------
