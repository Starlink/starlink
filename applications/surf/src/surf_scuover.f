      SUBROUTINE REDS_SCUOVER (STATUS)
*+
*  Name:
*     SCUOVER

*  Purpose:
*     Routine to overlay the bolometer names onto a rebinned image

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL REDS_SCUOVER ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine is used to overlay the array layout onto a rebinned
*     SCUBA image. The displayed image is read from the graphics database
*     unless a command line value is given. In order to calculate the bolometer
*     positions it is also necessary to read in the extinction corrected
*     data file that was used to regrid the data (in fact any extinction
*     corrected file can be used and strange results may be the result for the
*     unwary). The position of the bolometers at the start of the first 
*     integration and zero jiggle offset is plotted.

*  Usage:
*     scuover [NDF] [DEVICE] EXT [COL] [NAME]

*  ADAM parameters:
*     COL = LITERAL (Read)
*        Colour of overlay (by name or number). The previous value is used
*        by default.
*     DEVICE = DEVICE (Read)
*        The graphics device on which the bolometers are to be drawn.
*        The global default (set with Kappa GDSET) will be used unless
*        specified otherwise.
*     EXT = NDF (Read)
*        The name of the extinction corrected data from which the bolometer
*        positions should be taken.
*     NDF = NDF (Read)
*        The name of the regridded data set (taken from the AGI graphics
*        database).
*     NAME = _LOGICAL (Read)
*        Label with bolometer name if true, else bolometer number. The default
*        is true.

*  Examples:
*     scuover
*        The bolometer names will be overlaid using the default colour.
*     scuover col=red name=false
*        This command will overlay bolometer numbers over the image in red.

*  Notes: 
*     - An image must have already been displayed before using SCUOVER.
*     - The array position is that of the first integration with zero
*     jiggle offset. Images containing many integrations will not match
*     correctly to the drawn array position.
*     - This routine does not take into account the use of SHIFT_DX or
*     SHIFT_DY in REBIN. (the relevant information is not stored in the
*     rebinned image).
*     - Pointing shifts are taken into account.


*  Implementation status:

*  Authors :
*     JFL: J.Lightfoot (ROE)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History :
*     1996 December 17 (TIMJ)
*        Original version
*     {note_history_here}

*  Bugs:
*     {note_any_bugs_here}
 
*-

* Type Definitions :
      IMPLICIT NONE

* Global constants :
      INCLUDE 'DAT_PAR'                ! DAT__ constants
      INCLUDE 'NDF_PAR'                ! for NDF__xxxx constants
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx constants
      INCLUDE 'PAR_ERR'                ! for PAR__ constants
      INCLUDE 'PAR_PAR'                ! for PAR__ constants
      INCLUDE 'REDS_SYS'               ! REDS definitions
      INCLUDE 'SAE_PAR'                ! SSE global definitions
      INCLUDE 'GKS_PAR'                ! GKS constants (e.g. GSET)

*  Status :
      INTEGER STATUS
*  External references :
      INTEGER CHR_LEN                  ! CHR used-string-length function

*  Local Constants :
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      INTEGER MPEN                     ! SGS pen number used to draw graphics
      PARAMETER ( MPEN = 3 )
      REAL AR                          ! A `normal' text aspect ratio
      PARAMETER ( AR = 0.66667 )

*  Local variables :
      REAL AR0                         ! Text aspect ratio on entry
      CHARACTER*(1)    B1              ! Name of bolometer (1 char)
      CHARACTER*(2)    B2              ! Name of bolometer (2 char)
      CHARACTER*(3)    BOL             ! Name of bolometer
      LOGICAL          BOLNAME         ! Show true bolometer name?
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! A/D numbers of bolometers measured in
                                       ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                       ! in input file
      INTEGER          BOL_DEC_END
                                       ! pointer to end of BOL_DEC_PTR scratch
                                       ! space
      INTEGER          BOL_DEC_PTR
                                       ! pointer to scratch space holding
                                       ! apparent Dec / y offset positions of
                                       ! measured points in input file (radians)
      REAL             BOL_DIST        ! Separation of bols
      REAL             BOL_DIST_WORLD  ! Separation of bols in WORLD coords
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU4 Nasmyth coord of bolometers
      INTEGER          BOL_RA_END
                                       ! pointer to end of BOL_RA_PTR scratch
                                       ! space
      INTEGER          BOL_RA_PTR
                                       ! pointer to scratch space holding
                                       ! apparent RA / x offset positions of
                                       ! measured points in input file (radians)
      REAL             CENTRE_DU3      ! dU3 Nasmyth coordinate of point on
                                       ! focal plane that defines telescope axis
      REAL             CENTRE_DU4      ! dU4 Nasmyth coordinate of point on
                                       ! focal plane that defines telescope axis
      INTEGER          CHR_STATUS      ! status from CHR routines
      INTEGER          CI              ! Colour index required for graphics
      INTEGER          COLI            ! Original colour index of current pen
      INTEGER          DATA_OFFSET     ! offset within data array
      REAL             DEC_START       ! Dec offset of scan start (arcsec)
      REAL             DEC_VEL         ! Dec velocity of scan (arcsec/sec)
      LOGICAL          DEVCAN          ! The device parameter is to be cancelled?
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      INTEGER          DIMX (MAX_DIM)  ! expected array dimensions
      DOUBLE PRECISION DTEMP           ! scratch double
      INTEGER          EXPOSURE        ! exposure index in DO loop
      INTEGER          EXP_END         ! end index of data for an exposure
      DOUBLE PRECISION EXP_LST         ! sidereal time at which exposure
                                       ! started (radians)
      INTEGER          EXP_START       ! start index of data for an exposure
      REAL             EXP_TIME        ! exposure time per measurement in an
                                       ! input file (seconds)
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      INTEGER          EXT_NDF         ! NDF id of extinction file
      CHARACTER*40     FILENAME        ! default Name of extinction file
                                       ! names of input files read
      CHARACTER*80     FITS (SCUBA__MAX_FITS) 
                                       ! array of FITS keywords
      LOGICAL          FLATFIELD       ! .TRUE. if the FLATFIELD application
                                       ! has been run on the input file
      LOGICAL          GOTLOC          ! A locator to the NDF has been
                                       ! obtained?
      LOGICAL          GOTNAM          ! A reference name of the NDF has been
                                       ! obtained?
      REAL             HGT             ! Height for key text
      REAL             HT0             ! Text height on entry
      INTEGER          I               ! DO loop index
      INTEGER          ID              ! day of an input observation
      INTEGER          IEND            ! index of end of sub-string
      INTEGER          IERR            ! Position of error from VEC_
      INTEGER          IHOUR           ! hour in which observation started
      INTEGER          IM              ! month in which observation started
      INTEGER          IMIN            ! minute at which observation started
      INTEGER          INTEGRATION     ! integration index in DO loop
      CHARACTER*15     IN_CENTRE_COORDS! coord system of telescope centre in
                                       ! an input file
      DOUBLE PRECISION IN_DEC_CEN      ! apparent Dec of input file map centre
                                       ! (radians)
      INTEGER          IN_DEC_STRT_ARY ! array identifier to .SCUCD.DEC_STRT
      INTEGER          IN_DEC_STRT_PTR ! pointer to .SCUCD.DEC_STRT
      INTEGER          IN_DEC_VEL_ARY  ! array identifier to .SCUCD.DEC_VEL
      INTEGER          IN_DEC_VEL_PTR  ! array pointer to .SCUCD.DEC_VEL
      INTEGER          IN_DEM_PNTR_ARY ! array identifier to .SCUBA.DEM_PNTR
      INTEGER          IN_DEM_PNTR_PTR ! pointer to .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                       ! locator to FITS extension in input
                                       ! file
      DOUBLE PRECISION IN_LAT_RAD      ! latitude of telescope centre in input
                                       ! file (radians)
      DOUBLE PRECISION IN_LAT2_RAD     ! latitude of telescope centre at MJD2
                                       ! (radians)
      DOUBLE PRECISION IN_LONG_RAD     ! longitude of telescope centre in
                                       ! input file (radians)
      DOUBLE PRECISION IN_LONG2_RAD    ! longitude of telescope centre at MJD2
                                       ! (radians)
      INTEGER          IN_LST_STRT_ARY ! array identifier to .SCUBA.LST_STRT
      INTEGER          IN_LST_STRT_PTR ! pointer to .SCUBA.LST_STRT
      DOUBLE PRECISION IN_MJD1         ! modified Julian day at which object
                                       ! was at IN_LAT,IN_LONG for PLANET centre
                                       ! coordinate system
      DOUBLE PRECISION IN_MJD2         ! modified Julian day at which object
                                       ! was at IN_LAT2,IN_LONG2 for PLANET
                                       ! centre coordinate system
      DOUBLE PRECISION IN_RA_CEN       ! apparent RA of input file map centre
                                       ! (radians)
      INTEGER          IN_RA_STRT_ARY  ! array identifier to .SCUCD.RA_STRT
      INTEGER          IN_RA_STRT_PTR  ! pointer to .SCUCD.RA_STRT
      INTEGER          IN_RA_VEL_ARY   ! array identifier to .SCUCD.RA_VEL
      INTEGER          IN_RA_VEL_PTR   ! pointer to .SCUCD.RA_VEL
      CHARACTER*(DAT__SZLOC) IN_REDSX_LOC
                                       ! locator to REDS extension in input
                                       ! file
      DOUBLE PRECISION IN_ROTATION     ! angle between apparent N and N of
                                       ! input coord system (radians)
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                       ! locator to SCUBA extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                       ! locator to SCUCD extension in input
                                       ! file
      DOUBLE PRECISION IN_UT1          ! UT1 at start of an input observation,
                                       ! expressed as modified Julian day
      INTEGER          ISTART          ! index of start of sub-string
      INTEGER          ITEMP           ! scratch integer
      INTEGER          IWKID           ! GKS workstation identifier
      INTEGER          IY              ! year in which input observation started
      INTEGER          JIGGLE          ! jiggle index
      INTEGER          JIGGLE_COUNT    ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT   ! number of times jiggle pattern is
                                       ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                       ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                       ! y jiggle offsets (arcsec)
      DOUBLE PRECISION LAT_OBS         ! latitude of observatory (radians)
      INTEGER          LNTYPE          ! Line type to be used
      INTEGER          LNTYPI          ! Initial line type for current SGS pen
      CHARACTER * ( DAT__SZLOC ) LOCI  ! Locator for input data structure
      DOUBLE PRECISION LST             ! sidereal time at which measurement
                                       ! made (radians)
      REAL             LWIDTH          ! The width of the current SGS pen
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      INTEGER          MEASUREMENT     ! measurement index in DO loop
      DOUBLE PRECISION MJD_STANDARD    ! date for which apparent RA,Decs of all
                                       ! measured positions are calculated
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NDF             ! NDF id of input image
      INTEGER          NERR            ! Number of errors from VEC_
      INTEGER          NF              ! Font number
      INTEGER          NPR             ! Text precision
      INTEGER          NREC            ! number of history records in input file
      INTEGER          N_BOL           ! number of bolometers measured in input
                                       ! files
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
      INTEGER          N_FITS          ! number of items in FITS array
      INTEGER          N_INTEGRATIONS  ! number of integrations per measurement
                                       ! in input file
      INTEGER          N_MEASUREMENTS  ! number of measurements in input file
      INTEGER          N_POINT         ! dimension of pointing correction 
                                       ! array in input file
      INTEGER          N_POS           ! number of positions measured in input
                                       ! files
      INTEGER          N_SWITCHES      ! number of switches per exposure in
                                       ! input file
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      CHARACTER*15     OFFSET_COORDS   ! coord system of OFFSET_X and OFFSET_Y
      REAL             OFFSET_X        ! x offset of measurement
      REAL             OFFSET_Y        ! y offset of measurement
      CHARACTER*40     OUTCRDS         ! dummy coord system of output map
      CHARACTER*3      OUT_COORDS      ! coordinate system of output map
      DOUBLE PRECISION OUT_DEC_CEN     ! apparent Dec of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_LAT         ! longitude of output map centre 
                                       ! (radians)
      DOUBLE PRECISION OUT_LONG        ! longitude of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_RA_CEN      ! apparent RA of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_ROTATION    ! angle between apparent N and N of
                                       ! output coord system (radians)
      INTEGER PEN                      ! Current SGS pen
      INTEGER PICID                    ! Input picture identifier
      INTEGER PICIDI                   ! Data image picture identifier 
      REAL             POINT_DAZ (SCUBA__MAX_POINT)
                                       ! azimuth pointing corrections (radians)
      REAL             POINT_DEL (SCUBA__MAX_POINT)
                                       ! elevation pointing corrections
                                       ! (radians)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                       ! LST of pointing corrections (radians)
      REAL             RA_START        ! RA offset of scan start (arcsec)
      REAL             RA_VEL          ! RA velocity of scan (arcsec/sec)
      LOGICAL          REBIN           ! .TRUE. if REBIN application has 
                                       ! been run on input file
      LOGICAL          REDUCE_SWITCH   ! .TRUE. if REDUCE_SWITCH application
                                       ! has been run on input file
      CHARACTER * ( 256 ) REFNAM       ! Reference name
      REAL             RDTEMP          ! scratch real
      REAL             RTEMP           ! scratch real
      INTEGER          RUN_NUMBER      ! run number of input file
      CHARACTER*15     SAMPLE_COORDS   ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE     ! sample mode of input file
      REAL             SAMPLE_PA       ! position angle of sample x axis
                                       ! relative to x axis of SAMPLE_COORDS
                                       ! system
      DOUBLE PRECISION SEC             ! second at which observation started
      LOGICAL SETPLR                   ! Polyline representation to be reset?
      REAL             SHIFT_DX
                                       ! x shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      REAL             SHIFT_DY
                                       ! y shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      REAL SP0                         ! Space between characters on entry
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER * ( 2 ) TXJ0           ! Text justification on entry
      CHARACTER*15     UTDATE          ! date of input observation
      CHARACTER*15     UTSTART         ! UT of start of input observation
      REAL XTEMP(2)                    ! X pos of first two bols
      REAL XU0                         ! X comp. of text up-vector on entry
      REAL YTEMP(2)                    ! Y pos of first two bols
      REAL YU0                         ! Y comp. of text up-vector on entry
      INTEGER ZONEO                    ! SGS zone of the displayed image
      INTEGER ZONEOV                   ! SGS zone of the input picture

*-

      IF (STATUS .NE. SAI__OK) RETURN

*     Start up the error system
      CALL ERR_BEGIN(STATUS)

      DEVCAN = .FALSE.
      GOTNAM = .FALSE.
      GOTLOC = .FALSE.

*     Obtain an SGS zone for the last DATA picture.
*     =============================================
      
*     Associate graphics device and start database activity.  Update access
*     is used so that line can be drawn without destroying the existing
*     plot.
      CALL AGS_ASSOC( 'DEVICE', 'UPDATE', ' ', PICID, ZONEOV, STATUS )
      
*     Find the last DATA picture.
      CALL KPG1_AGFND( 'DATA', PICIDI, STATUS )
      
*     Obtain the SGS zone identifier for the current DATA picture.
      CALL AGS_NZONE( ZONEO, STATUS )
      
*     Report the name, comment, and label, if one exists, for the current
*     picture.
      CALL KPG1_AGATC( STATUS )

*     Obtain a reference to the NDF.
*     ==============================
      CALL KPG1_AGREF( PICIDI, 'READ', GOTNAM, REFNAM, STATUS )
      
*     See whether the reference is a name or locator.  The latter should
*     be phased out, but there may be some old databases and software in
*     circulation.
      CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
      IF ( GOTLOC ) LOCI = REFNAM
      
*     End immediately if there was an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*     Obtain the NDF.
*     ===============
      
*     Begin an NDF context.
      CALL NDF_BEGIN
      
*     Obtain the NDF.  If the name is given on the command line it will be
*     used.  If not, the database data reference is used, if there is one.
*     Otherwise, the user is prompted.
      CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, NDF, STATUS )


*     Read in FITS header
*     ===================

      CALL NDF_XLOC (NDF, 'FITS', 'READ', IN_FITSX_LOC,
     :     STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: input '//
     :           'file contains too many FITS items', STATUS)
         END IF
      END IF
      
      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, 
     :     N_FITS, STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

*     Read in SCUPROJ and FILE_1

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SCUPROJ', OUT_COORDS, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'FILE_1', FILENAME, STATUS)
      CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MJD-OBS', MJD_STANDARD, STATUS)

      IF (OUT_COORDS.NE.'AZ' .AND. OUT_COORDS.NE.'NA') THEN
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LAT', OUT_LAT, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LONG', OUT_LONG, STATUS)
      ELSE
         OUT_LAT = 0.0D0
         OUT_LONG = 0.0D0
      END IF

*  Tidy up
      CALL NDF_ANNUL(NDF, STATUS)

*  Abort if STATUS is bad
      IF (STATUS .NE. SAI__OK) THEN
         CALL NDF_END(STATUS)
         DEVCAN = .TRUE.
         GO TO 980
      END IF


*     Open EXTINCTION file
*     ====================

      CALL PAR_DEF0C('EXT', FILENAME, STATUS)

      CALL NDF_ASSOC('EXT', 'READ', EXT_NDF, STATUS)

*     Check HISTORY to make sure it is a SCUBA extinction corrected
*     demodulated data file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (EXT_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF
         
         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.
         FLATFIELD = .FALSE.
         REBIN = .FALSE.
         
         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (EXT_NDF, 'APPLICATION', 
     :              I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               ELSE IF (STEMP .EQ. 'FLATFIELD') THEN
                  FLATFIELD = .TRUE.
               ELSE IF (STEMP .EQ. 'REBIN') THEN
                  REBIN = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ','REDS_BESSEL_REBIN: '//
     :              'the input data has not been corrected for '//
     :              'EXTINCTION. Please try again.', STATUS)
            END IF
         END IF
      END IF


*     Get the Bolometer NA coordinates 
*     ================================

      CALL NDF_DIM (EXT_NDF, MAX_DIM, DIM, NDIM, STATUS)
      N_BOL = DIM (1)
      N_POS = DIM (2)


*  Get locators

      CALL NDF_XLOC (EXT_NDF, 'SCUBA', 'READ', 
     :     IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (EXT_NDF, 'SCUCD', 'READ', 
     :     IN_SCUCDX_LOC, STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_XLOC (EXT_NDF, 'REDS', 'READ', IN_REDSX_LOC,
     :        STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            IN_REDSX_LOC = DAT__NOLOC
         END IF
      END IF


      NDIM = 2
      DIMX (1) = SCUBA__NUM_CHAN
      DIMX (2) = SCUBA__NUM_ADC
      CALL CMP_GETNR (IN_SCUBAX_LOC, 'BOL_DU3', NDIM, DIMX,
     :     BOL_DU3, DIM, STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :        (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :        (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'REDS_SCUOVER: '//
     :           '.SCUBA.BOL_DU3 array has bad dimensions - '//
     :           '(^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF
      
      NDIM = 2
      DIMX (1) = SCUBA__NUM_CHAN
      DIMX (2) = SCUBA__NUM_ADC
      CALL CMP_GETNR (IN_SCUBAX_LOC, 'BOL_DU4', NDIM, DIMX,
     :     BOL_DU4, DIM, STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :        (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :        (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'REDS_SCUOVER: '//
     :           '.SCUBA.BOL_DU4 array has bad dimensions - '//
     :           '(^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF
      
      CALL CMP_GET1I (IN_SCUBAX_LOC, 'BOL_CHAN', 
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :     BOL_CHAN, ITEMP, STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_SCUOVER: dimension '//
     :           'of .SCUBA.BOL_CHAN does not match main data '//
     :           'array', STATUS)
         END IF
      END IF
      
      CALL CMP_GET1I(IN_SCUBAX_LOC, 'BOL_ADC', 
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_ADC, ITEMP,
     :     STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOL) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_SCUOVER: dimension '//
     :           'of .SCUBA.BOL_ADC does not match main data '//
     :           'array', STATUS)
         END IF
      END IF

*     Read in FITS header from EXT
*     ============================

      CALL NDF_XLOC (EXT_NDF, 'FITS', 'READ', IN_FITSX_LOC,
     :     STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: input '//
     :           'file contains too many FITS items', STATUS)
         END IF
      END IF
      
      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, 
     :     N_FITS, STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'RUN', RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'OBJECT', OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MODE', OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'SAM_MODE', SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)
      
*     Get centre offset
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CNTR_DU3', CENTRE_DU3, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CNTR_DU4', CENTRE_DU4, STATUS)
      CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

*  get some other FITS items that will be needed
 
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'EXP_TIME', EXP_TIME, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_CRDS', SAMPLE_COORDS, STATUS)
      CALL CHR_UCASE (SAMPLE_COORDS)

*     coords of telescope centre

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'CENT_CRD', IN_CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (IN_CENTRE_COORDS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'LAT', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT_RAD, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'LONG', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG_RAD, STATUS)

      IF (IN_CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LAT2', STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT2_RAD, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LONG2', STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG2_RAD,
     :        STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MJD1', IN_MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'MJD2', IN_MJD2, STATUS)
      END IF

      IF ((IN_CENTRE_COORDS .NE. 'AZ') .AND.
     :     (IN_CENTRE_COORDS .NE. 'GA')) THEN
         IN_LONG_RAD = IN_LONG_RAD * 15.0D0
         IN_LONG2_RAD = IN_LONG2_RAD * 15.0D0
      END IF

*     offset from telescope centre

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'MAP_X', MAP_X, STATUS)
      MAP_X = MAP_X / REAL (R2AS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MAP_Y', MAP_Y, STATUS)
      MAP_Y = MAP_Y / REAL (R2AS)

*     the UT of the observation expressed as modified Julian day

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'UTDATE', UTDATE, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'UTSTART', UTSTART, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CHR_STATUS = SAI__OK

         ISTART = 1
         IEND = INDEX (UTDATE,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTDATE (ISTART:IEND-1), IY, CHR_STATUS)
         UTDATE (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = INDEX (UTDATE,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTDATE (ISTART:IEND-1), IM, CHR_STATUS)
         UTDATE (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = MAX (ISTART,CHR_LEN(UTDATE))
         CALL CHR_CTOI (UTDATE (ISTART:IEND), ID, CHR_STATUS)

         ISTART = 1
         IEND = INDEX (UTSTART,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTSTART (ISTART:IEND-1), IHOUR, 
     :        CHR_STATUS)
         UTSTART (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = INDEX (UTSTART,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTSTART (ISTART:IEND-1), IMIN,
     :        CHR_STATUS)
         UTSTART (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = MAX (ISTART,CHR_LEN(UTSTART))
         CALL CHR_CTOD (UTSTART (ISTART:IEND), SEC, CHR_STATUS)

         IF (CHR_STATUS .EQ. SAI__OK) THEN
            CALL SLA_CLDJ (IY, IM, ID, IN_UT1, STATUS)
            IN_UT1 = IN_UT1 + 
     :           ((SEC/60.0D0 + DBLE(IMIN)) / 60.0D0 +
     :           DBLE(IHOUR)) / 24.0D0
            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETI ('SLA', STATUS)
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: error '//
     :              'returned by SLA_CLDJ - status = ^SLA', STATUS)
            END IF
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC ('UTDATE', UTDATE)
            CALL MSG_SETC ('UTSTART', UTSTART)
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: error '//
     :           'converting UTDATE=^UTDATE and UTSTART=^UTSTART '//
     :           'to UT1', STATUS)
         END IF
      END IF


*     search for pointing correction structure in the REDS extension, if there
*     is one read in the corrections

      IF (STATUS .EQ. SAI__OK) THEN

         CALL CMP_GET1D(IN_REDSX_LOC,'POINT_LST',SCUBA__MAX_POINT,
     :        POINT_LST, N_POINT, STATUS)
         CALL CMP_GET1R(IN_REDSX_LOC,'POINT_DAZ',SCUBA__MAX_POINT,
     :        POINT_DAZ, N_POINT, STATUS)
         CALL CMP_GET1R(IN_REDSX_LOC,'POINT_DEL',SCUBA__MAX_POINT,
     :        POINT_DEL, N_POINT, STATUS)
         
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            N_POINT = 0
         END IF
      END IF

*     Find the start LST
*     map the DEM_PNTR and LST arrays and check their dimensions
      
      CALL ARY_FIND (IN_SCUBAX_LOC, 'DEM_PNTR', IN_DEM_PNTR_ARY,
     :     STATUS)
      CALL ARY_DIM (IN_DEM_PNTR_ARY, MAX_DIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IN_DEM_PNTR_ARY, '_INTEGER', 'READ',
     :     IN_DEM_PNTR_PTR, ITEMP, STATUS)
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 3) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :           '.SCUBA.DEM_PNTR array has bad number of '//
     :           'dimensions', STATUS)
         ELSE
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1',DIM(1))
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUBA.DEM_PNTR array contains bad number '//
     :              'of exposures - ^DIM1', STATUS)
            END IF
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM2',DIM(2))
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUBA.DEM_PNTR array contains bad number '//
     :              'of integrations - ^DIM2', STATUS)
            END IF
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM3',DIM(3))
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUBA.DEM_PNTR array contains bad number '//
     :              'of measurements - ^DIM3', STATUS)
            END IF
         END IF
      END IF
      
      N_EXPOSURES = DIM (1)
      N_INTEGRATIONS = DIM (2)
      N_MEASUREMENTS = DIM (3)

      CALL ARY_FIND (IN_SCUCDX_LOC, 'LST_STRT', 
     :     IN_LST_STRT_ARY, STATUS)
      CALL ARY_DIM (IN_LST_STRT_ARY, MAX_DIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IN_LST_STRT_ARY, '_DOUBLE', 'READ', 
     :     IN_LST_STRT_PTR, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 4) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :           '.SCUCD.LST_STRT array has bad number of '//
     :           'dimensions - ^NDIM', STATUS)
         ELSE
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUCD.LST_STRT array contains bad '//
     :              'number of switch(es) - ^DIM1', STATUS)
            END IF
            IF (DIM(2) .NE. N_EXPOSURES) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NEXP', N_EXPOSURES)
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              'there is a mismatch between the number of '//
     :              'exposures in .SCUBA.DEM_PNTR (^NEXP) and '//
     :              'in .SCUCD.LST_STRT (^DIM2)', STATUS)
            END IF
            IF (DIM(3) .NE. N_INTEGRATIONS) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NINT', N_INTEGRATIONS)
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in .SCUBA.DEM_PNTR (^NINT) '//
     :              'and in .SCUCD.LST_STRT (^DIM3)', STATUS)
            END IF
            IF (DIM(4) .NE. N_MEASUREMENTS) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
               CALL MSG_SETI ('DIM4', DIM(4))
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              'there is a mismatch between the number of '//
     :              'measurements in .SCUBA.DEM_PNTR (^NMEAS) '//
     :              'and in .SCUCD.LST_STRT (^DIM4)', STATUS)
            END IF
         END IF
      END IF

      N_SWITCHES = DIM (1)


*     calculate the apparent RA and Dec of the map centre at IN_UT1

      CALL SCULIB_CALC_APPARENT (IN_LONG_RAD, IN_LAT_RAD,
     :     IN_LONG2_RAD, IN_LAT2_RAD, DBLE(MAP_X), DBLE(MAP_Y), 
     :     IN_CENTRE_COORDS, %VAL(IN_LST_STRT_PTR), IN_UT1,
     :     IN_MJD1, IN_MJD2, IN_RA_CEN, IN_DEC_CEN, IN_ROTATION,
     :     STATUS)

*     now read in data specific to the sample mode of the observation

      IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'JIGL_CNT', JIGGLE_COUNT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'J_REPEAT', JIGGLE_REPEAT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'J_PER_S', JIGGLE_P_SWITCH, STATUS)

*     the jiggle pattern itself

         CALL CMP_GET1R (IN_SCUCDX_LOC, 'JIGL_X',
     :        SCUBA__MAX_JIGGLE, JIGGLE_X, ITEMP, STATUS)

         IF (ITEMP .NE. JIGGLE_COUNT) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              'mismatch between JIGGLE_COUNT and number '//
     :              'of X jiggle offsets read', STATUS)
            END IF
         END IF

         CALL CMP_GET1R (IN_SCUCDX_LOC, 'JIGL_Y',
     :        SCUBA__MAX_JIGGLE, JIGGLE_Y, ITEMP, STATUS)

         IF (ITEMP .NE. JIGGLE_COUNT) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              'mismatch between JIGGLE_COUNT and number '//
     :              'of Y jiggle offsets read', STATUS)
            END IF
         END IF

*     the rotation of the jiggle coordinate system

         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'SAM_PA', SAMPLE_PA, STATUS)

*     likewise for raster maps

      ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN
*     CALL NDF_XIARY (INDF, 'SCUCD', 'RA_STRT', 'READ',
*     :           IN_RA_STRT_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'RA_STRT', IN_RA_STRT_ARY,
     :        STATUS)
         CALL ARY_DIM (IN_RA_STRT_ARY, MAX_DIM, DIM, NDIM, STATUS)
         CALL ARY_MAP (IN_RA_STRT_ARY, '_REAL', 'READ',
     :        IN_RA_STRT_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUCD.RA_STRT array has bad number of '//
     :              'dimensions - ^NDIM', STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_SWITCHES)
                  CALL MSG_SETI ('D', DIM(1))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_STRT (^D)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_EXPOSURES)
                  CALL MSG_SETI ('D', DIM(2))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_STRT (^D)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_INTEGRATIONS)
                  CALL MSG_SETI ('D', DIM(3))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_STRT (^D)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_MEASUREMENTS)
                  CALL MSG_SETI ('D', DIM(4))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_STRT (^D)', STATUS)
               END IF
            END IF
         END IF

*     CALL NDF_XIARY (INDF, 'SCUCD', 'RA_VEL', 'READ',
*     :           IN_RA_VEL_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'RA_VEL', IN_RA_VEL_ARY,
     :        STATUS)
         CALL ARY_DIM (IN_RA_VEL_ARY, MAX_DIM, DIM, NDIM, STATUS)
         CALL ARY_MAP (IN_RA_VEL_ARY, '_REAL', 'READ',
     :        IN_RA_VEL_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUCD.RA_VEL array has bad number of '//
     :              'dimensions - ^NDIM', STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_SWITCHES)
                  CALL MSG_SETI ('D', DIM(1))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_VEL (^D)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_EXPOSURES)
                  CALL MSG_SETI ('D', DIM(2))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_VEL (^D)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_INTEGRATIONS)
                  CALL MSG_SETI ('D', DIM(3))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_VEL (^D)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_MEASUREMENTS)
                  CALL MSG_SETI ('D', DIM(4))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.RA_VEL (^D)', STATUS)
               END IF
            END IF
         END IF

*     CALL NDF_XIARY (INDF, 'SCUCD', 'DEC_STRT', 'READ',
*     :           IN_DEC_STRT_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'DEC_STRT', 
     :        IN_DEC_STRT_ARY, STATUS)
         CALL ARY_DIM (IN_DEC_STRT_ARY, MAX_DIM, DIM, NDIM, 
     :        STATUS)
         CALL ARY_MAP (IN_DEC_STRT_ARY, '_REAL', 'READ',
     :        IN_DEC_STRT_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUCD.DEC_STRT array has bad number of '//
     :              'dimensions - ^NDIM', STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_SWITCHES)
                  CALL MSG_SETI ('D', DIM(1))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_STRT (^D)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_EXPOSURES)
                  CALL MSG_SETI ('D', DIM(2))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_STRT (^D)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_INTEGRATIONS)
                  CALL MSG_SETI ('D', DIM(3))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_STRT (^D)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_MEASUREMENTS)
                  CALL MSG_SETI ('D', DIM(4))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_STRT (^D)', STATUS)
               END IF
            END IF
         END IF

*     CALL NDF_XIARY (INDF, 'SCUCD', 'DEC_VEL', 'READ',
*     :           IN_DEC_VEL_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'DEC_VEL', 
     :        IN_DEC_VEL_ARY, STATUS)
         CALL ARY_DIM (IN_DEC_VEL_ARY, MAX_DIM, DIM, NDIM, 
     :        STATUS)
         CALL ARY_MAP (IN_DEC_VEL_ARY, '_REAL', 'READ',
     :        IN_DEC_VEL_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :              '.SCUCD.DEC_VEL array has bad number of '//
     :              'dimensions - ^NDIM', STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_SWITCHES)
                  CALL MSG_SETI ('D', DIM(1))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of switches in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_VEL (^D)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_EXPOSURES)
                  CALL MSG_SETI ('D', DIM(2))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of exposures in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_VEL (^D)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_INTEGRATIONS)
                  CALL MSG_SETI ('D', DIM(3))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of integrations in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_VEL (^D)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('N', N_MEASUREMENTS)
                  CALL MSG_SETI ('D', DIM(4))
                  CALL ERR_REP (' ', 'REDS_BESSEL_REBIN: '//
     :                 'there is a mismatch between the number '//
     :                 'of measurements in .SCUCD.DEM_PNTR (^N) '//
     :                 'and in .SCUCD.DEC_VEL (^D)', STATUS)
               END IF
            END IF
         END IF
      END IF



*     calculate position of each bolometer at each measurement
      N_POS = 1
      CALL SCULIB_MALLOC (N_BOL * VAL__NBD,
     :     BOL_RA_PTR, BOL_RA_END, STATUS)
      CALL SCULIB_MALLOC (N_BOL * VAL__NBD,
     :     BOL_DEC_PTR, BOL_DEC_END, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

*     now go through the various exposures of the observation calculating the
*     observed positions

         MEASUREMENT = 1
         INTEGRATION = 1
         EXPOSURE    = 1

*     calculate mean LST for the switch sequence making up the exposure

         EXP_LST = 0.0D0
         DO I = 1, N_SWITCHES
            DATA_OFFSET = (((MEASUREMENT-1) *
     :           N_INTEGRATIONS + INTEGRATION - 1) *
     :           N_EXPOSURES + EXPOSURE - 1) *
     :           N_SWITCHES + I - 1
            CALL VEC_DTOD(.FALSE., 1,
     :           %val(IN_LST_STRT_PTR + DATA_OFFSET *
     :           VAL__NBD), DTEMP, IERR, NERR, STATUS)
            EXP_LST = EXP_LST + DTEMP
         END DO
         EXP_LST = EXP_LST / DBLE (N_SWITCHES)

*     get the scan parameters for a raster map

         IF (SAMPLE_MODE .EQ. 'RASTER') THEN
            CALL VEC_RTOR(.FALSE., 1,
     :           %val(IN_RA_STRT_PTR + DATA_OFFSET *
     :           VAL__NBR), RA_START, IERR, NERR, STATUS)
            CALL VEC_RTOR(.FALSE., 1,
     :           %val(IN_RA_VEL_PTR + DATA_OFFSET *
     :           VAL__NBR), RA_VEL, IERR, NERR, STATUS)
            CALL VEC_RTOR(.FALSE., 1,
     :           %val(IN_DEC_STRT_PTR + DATA_OFFSET *
     :           VAL__NBR), DEC_START, IERR, NERR,STATUS)
            CALL VEC_RTOR(.FALSE., 1,
     :           %val(IN_DEC_VEL_PTR + DATA_OFFSET *
     :           VAL__NBR), DEC_VEL, IERR, NERR, STATUS)
         END IF
         
*     find where the exposure starts and finishes in the data array

         CALL SCULIB_FIND_SWITCH (
     :        %val(IN_DEM_PNTR_PTR), 1, N_EXPOSURES,
     :        N_INTEGRATIONS, N_MEASUREMENTS,N_POS,
     :        1, EXPOSURE, INTEGRATION, MEASUREMENT,
     :        EXP_START, EXP_END, STATUS)
         
*     cycle through the measurements in the exposure

         I = EXP_START

*     calculate the LST at which the measurement was made (hardly worth the
*     bother because it's averaged over the switches anyway)

         LST = EXP_LST + DBLE(I - EXP_START) *
     :        DBLE(EXP_TIME) * 1.0027379D0 * 
     :        2.0D0 * PI / (3600.0D0 * 24.0D0)
         
*     work out the pointing offset at which the measurement was made,
*     remembering that for the `raster' mode the RA offset increases towards
*     decreasing RA

         IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
            IF (JIGGLE_REPEAT .EQ. 1) THEN
               JIGGLE = (EXPOSURE-1) *
     :              JIGGLE_P_SWITCH +
     :              I - EXP_START + 1
            ELSE
               JIGGLE = MOD (I - EXP_START,
     :              JIGGLE_COUNT) + 1
            END IF
            
            OFFSET_X = 0.0
            OFFSET_Y = 0.0
            OFFSET_COORDS = SAMPLE_COORDS
         ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN
            OFFSET_X = - RA_START - RA_VEL *
     :           (REAL(I - EXP_START) + 0.5) *
     :           EXP_TIME
            OFFSET_Y = DEC_START + DEC_VEL *
     :           (REAL(I - EXP_START) + 0.5) *
     :           EXP_TIME
            OFFSET_COORDS = 'RD'
         END IF
         
*     now call a routine to work out the apparent RA,Dec of the measured
*     bolometers at this position

         DATA_OFFSET = (I - 1) * N_BOL

         IF (OUT_COORDS.EQ.'NA'.OR.
     :        OUT_COORDS.EQ.'AZ') THEN
            OUTCRDS = OUT_COORDS
         ELSE
            OUTCRDS = 'RA'
         END IF
         
         CALL SCULIB_CALC_BOL_COORDS (OUTCRDS, 
     :        IN_RA_CEN, IN_DEC_CEN, LST, LAT_OBS,
     :        OFFSET_COORDS, OFFSET_X, OFFSET_Y,
     :        IN_ROTATION, N_POINT,
     :        SCUBA__MAX_POINT,
     :        POINT_LST, POINT_DAZ, POINT_DEL, 
     :        SCUBA__NUM_CHAN, SCUBA__NUM_ADC, 
     :        N_BOL, BOL_CHAN,
     :        BOL_ADC, BOL_DU3, BOL_DU4,
     :        CENTRE_DU3, CENTRE_DU4,
     :        %val(BOL_RA_PTR + DATA_OFFSET *
     :        VAL__NBD),
     :        %val(BOL_DEC_PTR + DATA_OFFSET*
     :        VAL__NBD),
     :        STATUS)

*     convert the coordinates to apparent RA,Dec on MJD_STANDARD
         IF (OUTCRDS .EQ. 'RA') THEN

            CALL SCULIB_STANDARD_APPARENT (N_BOL,
     :           %val(BOL_RA_PTR + DATA_OFFSET * VAL__NBD),
     :           %val(BOL_DEC_PTR + DATA_OFFSET * VAL__NBD),
     :           IN_UT1, MJD_STANDARD, STATUS)
         END IF
      END IF


*     annul locators and array identifiers and close the file

      CALL ARY_ANNUL (IN_DEM_PNTR_ARY, STATUS)
      CALL ARY_ANNUL (IN_LST_STRT_ARY, STATUS)

 1    CONTINUE                  ! Jump here if error before mapping ARY

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
      IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL (EXT_NDF, STATUS)

*     END NDF CONTEXT
      CALL NDF_END(STATUS)

*     OK, all the data required should have been read in by now, check that
*     there is some input data

*     Now just need to plot the bolometer positions

*     Nasmyth rebin doesn't need a coordinate frame

      IF (OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ') THEN

*     calculate the apparent RA,Dec of the selected output centre

         CALL SCULIB_CALC_APPARENT (OUT_LONG, OUT_LAT, 0.0D0, 0.0D0,
     :        0.0D0, 0.0D0, OUT_COORDS, 0.0, MJD_STANDARD, 0.0D0, 0.0D0,
     :        OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, STATUS)


*     convert the RA,Decs of the observed points to tangent plane offsets
*     from the chosen output centre

         IF (STATUS .EQ. SAI__OK) THEN
               CALL SCULIB_APPARENT_2_TP (N_BOL, 
     :              %val(BOL_RA_PTR), %val(BOL_DEC_PTR), 
     :              OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, 
     :              DBLE(SHIFT_DX), DBLE(SHIFT_DY), STATUS)
         END IF

      END IF

*  Get information on line style and colour.
*  =========================================
 
*  Inquire the workstation identifier for GKS inquiries.
      CALL SGS_ICURW( IWKID )
      SETPLR = .FALSE.
 
*  Obtain the colour index for the desired colour of the lines.
*  Do not restrict the colours to the palette.
      CALL KPG1_IVCI( 'DEVICE', 'COL', .FALSE., CI, STATUS )
 
*   Can now start plotting these on the graph     

*  Inquire the current colour index of this pen (it will be restored
*  after all plotting is complete).
      CALL GQPLR( IWKID, MPEN, GSET, IERR, LNTYPI, LWIDTH, COLI )
 
*     Need to find half distance between bolometers
      CALL VEC_DTOR(.FALSE., 2, %val(BOL_RA_PTR),
     :     XTEMP, IERR, NERR, STATUS)
      CALL VEC_DTOR(.FALSE., 2, %val(BOL_DEC_PTR),
     :     YTEMP, IERR, NERR, STATUS)
      
      BOL_DIST = SQRT((XTEMP(2)-XTEMP(1))**2 + (YTEMP(2)-YTEMP(1))**2)
      BOL_DIST = BOL_DIST * REAL(R2AS) * 0.5

*     Now calculate (half) this distance in World coordinates
*       (I am assuming dx=dy for simplicity)

      CALL AGI_TDTOW(PICIDI, 1, 0., 0., XTEMP(1), YTEMP(1), STATUS)
      CALL AGI_TDTOW(PICIDI, 1, 0., BOL_DIST,XTEMP(2), YTEMP(2), STATUS)

      BOL_DIST_WORLD = ABS(YTEMP(2) - YTEMP(1))

*   Text attributes

      LNTYPE = 1
      LWIDTH = 1

*  Get the current text attributes.
      CALL SGS_ITXA( NF, NPR, HT0, AR0, XU0, YU0, SP0, TXJ0 )

      CALL SGS_SARTX( AR )
      CALL SGS_SUPTX( 0.0, 1.0 )
      CALL SGS_STXJ( 'CC' )
      CALL SGS_SSPTX( 0.0 )

*     Convert bolometer separation to world coordinates for textht
      HGT = 0.55 * BOL_DIST_WORLD
      CALL SGS_SHTX (HGT)

*  Store the new colour index and line style for this pen.
      CALL GSPLR( IWKID, MPEN, LNTYPE, LWIDTH, CI )
      SETPLR = .TRUE.
 
*  See if a GKS error has occurred.
      CALL GKS_GSTAT( STATUS )
 
*  Inquire the current SGS pen, and then select the pen used to draw
*  markers.
      CALL SGS_IPEN( PEN )
      CALL SGS_SPEN( MPEN )

*     Which type of label do we want
      CALL PAR_GET0L('NAME', BOLNAME, STATUS)
 
*     Loop through all bolometers
      DO I = 1, N_BOL

         CALL VEC_DTOR(.FALSE., 1, %val(BOL_RA_PTR+(I-1)*VAL__NBD), 
     :        RTEMP, IERR,
     :        NERR, STATUS)
         CALL VEC_DTOR(.FALSE., 1, %val(BOL_DEC_PTR+(I-1)*VAL__NBD),
     :        RDTEMP, IERR,
     :        NERR, STATUS)

         RTEMP = RTEMP * REAL(R2AS)
         RDTEMP = RDTEMP * REAL(R2AS)

*     Transform to world coordinates
         CALL AGI_TDTOW(PICIDI, 1, RTEMP, RDTEMP, RTEMP,RDTEMP,STATUS)

*     Convert to proper bol name if necessary
         IF (BOLNAME) THEN
            CALL SCULIB_BOLNAME(BOL_ADC(I), BOL_CHAN(I), BOL, STATUS)
         ELSE 
            CALL CHR_ITOC(I, BOL, ITEMP)
         END IF

*       Now plot on map (Need to call 3 times depending on length of string)
         ITEMP = CHR_LEN(BOL)
         IF (ITEMP .EQ. 1) THEN
            B1 = BOL
            CALL SGS_TX(RTEMP, RDTEMP, B1)
         ELSE IF (ITEMP .EQ. 2) THEN
            B2 = BOL
            CALL SGS_TX(RTEMP, RDTEMP, B2)
         ELSE
            CALL SGS_TX(RTEMP, RDTEMP, BOL)
         END IF

         CALL SGS_CIRCL(RTEMP, RDTEMP, BOL_DIST_WORLD * 0.85)
      END DO

*  If necessary, re-instate the original colour index for the marker
*  pen, and reinstate the original pen.
      CALL SGS_SPEN( PEN )
      IF ( SETPLR ) CALL GSPLR( IWKID, MPEN, LNTYPI, LWIDTH, COLI )

 980  CONTINUE

*  Need to tidy up the graphics database before exiting.
      CALL AGS_DEASS( 'DEVICE', DEVCAN, STATUS )

      CALL ERR_END(STATUS)
 
      END
