*+  REDS_EXTINCTION - remove the effect of atmospheric extinction from a 
*                     SCUBA observation
      SUBROUTINE REDS_EXTINCTION (STATUS)
*    Description :
*     This application extracts from a demodulated-data file data for a 
*     specified SCUBA sub-instrument and corrects it for the effect of 
*     atmospheric extinction. The airmass at which each bolometer measurement
*     was made is calculated, then multiplied by the zenith sky extinction at
*     the time of the measurement to give the extinction optical depth along
*     the line of sight. The data point in question is then multiplied by the
*     exponential of the optical depth to give the value that would have been
*     measured in the absence of the atmosphere.
*       The zenith optical depth is assumed to vary linearly with time between
*     the values input in parameters FIRST_TAU and LAST_TAU. If the measurement
*     was taken at a time outside the range covered by FIRST_TAU and LAST_TAU
*     then the value closest in time will be used.
*       The parameters used are:-
*
*     IN                   The name of the input file containing demodulated
*                          SCUBA data.
*
*     SUB_INSTRUMENT       The name of the sub-instrument whose data are to
*                          be selected from the input file and extinction
*                          corrected. Permitted values are SHORT, LONG,
*                          P1100, P1300 and P2000.
*
*     FIRST_TAU            The zenith sky opacity before the observation.
*
*     FIRST_LST            The local sidereal time at which FIRST_TAU was
*                          the zenith sky opacity, in hh mm ss.ss format.
*
*     SECOND_TAU           The zenith sky opacity after the observation.
*
*     SECOND_LST           The local sidereal time at which SECOND_TAU was
*                          the zenith sky opacity, in hh mm ss.ss format.
*
*     OUT                  The name of the output file to contain the
*                          extinction corrected data for the specified
*                          sub-instrument.
*
*     If status is good on entry the routine will open the IN file, read
*     some FITS items describing the observation and report them to the
*     user. The file `history' is read and a check made that the REDUCE_SWITCH
*     application has been run on the file and that EXTINCTION has not.
*       Other FITS items are read, describing the sampling system used, sample
*     period, observatory latitude, Nasmyth coords of the telescope axis, the
*     coordinates of the telescope `centre', date and time of the observation.
*     SCULIB_CALC_APPARENT is called to work out the apparent RA and Dec of the
*     telescope centre.
*       Next, the components of the main data array are mapped. All the
*     component exposures making up the observation are butted end to end 
*     in this array, so the `pointer' array is also mapped, which contains
*     the start and finish indices of each exposure in the main data array.
*     The array holding the local sidereal times of the start of each exposure
*     is also mapped.
*       If the sample mode was `jiggle' the jiggle pattern used will be read
*     from the file. If the sample mode was `raster' then arrays containing
*     the start offsets and velocities of each scan will be mapped.
*       FITS items holding the sidereal times of the start and finish of the
*     observation are read and reported, as are the names of the SCUBA
*     sub-instruments used. The name of the sub-instrumnet whose data are
*     required is read from parameter SUB_INSTRUMENT, and a check made that
*     the file deos contain data for the one selected. Arrays holding the 
*     Nasmyth coords of the bolometers and their types are read in.
*       Next, the zenith sky opacities at times before and after the 
*     observation are read in from parameters FIRST_TAU, FIRST_LST and
*     SECOND_TAU, SECOND_LST. SLA_DAFIN is called to convert the LST strings
*     to radians.
*       The OUT file is opened and the dimensions of the data array reset
*     to reflect that only data for those bolometers belonging to the 
*     selected SUB_INSTRUMENT will be written out. Data for these bolometers
*     is extracted from the input file and written to the output file by
*     SCULIB_GET_SUB_BOLS. The subsidiary arrays .SCUBA.BOL_CHAN and 
*     .SCUBA.BOL_ADC are reset to reflect the new set of bolometers in the 
*     data array, as are other affected FITS items in the output file.
*       Now the application cycles through the exposures, integrations and
*     measurements in the observation. A mean LST is calculated for each
*     exposure from the start LSTs of the component switches, SCULIB_FIND_
*     SWITCH is called to get the location of the exposure data in the 
*     data array. 
*       Now, cycling through the measurements in the exposure, the 
*     application estimates the LST at which each measurement was made and
*     works out the zenith sky opacity for this time. For `jiggle' observations
*     the jiggle offset of the measurement is calculated, for `raster' the
*     offset in apparent RA,Dec from the map centre. SCULIB_CALC_BOL_COORDS
*     is then called to calculate the apparent RA and Dec of each bolometer
*     for the measurement. SCULIB_CORRECT_EXTINCTION is called to calculate
*     the airmass of each bolometer and correct its data for the effect of
*     sky opacity.
*       Lastly, the IN and OUT files are closed.
*    Invocation :
*     CALL REDS_EXTINCTION (STATUS)
*    Parameters :
*     STATUS           = INTEGER (Given and returned)
*           global status
*    Method :
*    Deficiencies :
*    Bugs :
*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*    History :
*     $Id$
*     1-AUG-1995: original version.
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                 ! for VAL__xxxx
      INCLUDE 'REDS_SYS'                ! REDS constants
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN                   ! CHR used string length function
*    Global variables :
*    Local Constants :
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
*    Local variables :
      BYTE             BADBIT           ! Bad bit mask
      INTEGER          BEAM             ! beam number in DO loop
      DOUBLE PRECISION BOL_DEC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! apparent Dec of the bolometers at
                                        ! the time of a measurement
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU4 Nasmyth coord of bolometers
      DOUBLE PRECISION BOL_RA (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! apparent RA of the bolometers at
                                        ! the time of a measurement
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! bolometer types
      CHARACTER*15     CENTRE_COORDS    ! coord system of telescope centre
      REAL             CENTRE_DU3       ! dU3 Nasmyth coord of point on focal
                                        ! plane that defines telescope axis
      REAL             CENTRE_DU4       ! dU4 Nasmyth coord of point on focal
                                        ! plane that defines telescope axis
      INTEGER          CHR_STATUS       ! status from CHR routines
      INTEGER          DATA_OFFSET      ! offset of a datum in an array
      DOUBLE PRECISION DEC_CENTRE       ! apparent declination of map centre
                                        ! (radians)
      REAL             DEC_START        ! Dec offset of scan start (arcsec)
      REAL             DEC_VEL          ! Dec velocity of scan (arcsec/sec)
      INTEGER          DIM (MAXDIM)     ! the dimensions of an array
      INTEGER          DIMX (MAXDIM)    ! expected dimensions of an array
      DOUBLE PRECISION DTEMP            ! scratch double
      INTEGER          EXPOSURE         ! exposure index in DO loop
      INTEGER          EXP_END          ! end index of data for an exposure
      DOUBLE PRECISION EXP_LST          ! sidereal time at start of exposure
      INTEGER          EXP_START        ! start index of data for an exposure
      REAL             EXP_TIME         ! exposure time per measurement (secs)
      LOGICAL          EXTINCTION       ! .TRUE. if EXTINCTION has been run
      CHARACTER*20     FIRST_LST        ! sidereal time at which FIRST_TAU
                                        ! measured
      DOUBLE PRECISION FIRST_LST_RAD    ! FIRST_LST in radians
      REAL             FIRST_TAU        ! zenith sky opacity at FIRST_LST
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      INTEGER          I                ! DO loop variable
      INTEGER          ID               ! day of observation
      INTEGER          IEND             ! index of end of sub-string
      INTEGER          IHOUR            ! hour in which observation started
      INTEGER          IM               ! month of observation
      INTEGER          IMIN             ! minute in which observation started
      INTEGER          INDF             ! NDF identifier of input file
      INTEGER          INTEGRATION      ! integration index in DO loop
      INTEGER          IN_BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER          IN_BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
                                        ! measured in input file
      INTEGER          IN_DATA_PTR      ! pointer to data array of input file
      INTEGER          IN_DEC_STRT_ARY  ! array identifier to .SCUCD.DEC_STRT
      INTEGER          IN_DEC_STRT_PTR  ! pointer to .SCUCD.DEC_STRT
      INTEGER          IN_DEC_VEL_ARY   ! array identifier to .SCUCD.DEC_VEL
      INTEGER          IN_DEC_VEL_PTR   ! pointer to .SCUCD.DEC_VEL
      INTEGER          IN_DEM_PNTR_ARY  ! array identifer to .SCUBA.DEM_PNTR
      INTEGER          IN_DEM_PNTR_PTR  ! pointer to input .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                        ! locator to FITS extension in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_LOC     ! locator to item in input file
      INTEGER          IN_LST_STRT_ARY  ! array identifier to .SCUCD.LST_STRT
      INTEGER          IN_LST_STRT_PTR  ! pointer to input .SCUCD.LST_STRT
      INTEGER          IN_PHOT_BB (SCUBA__MAX_BEAM, SCUBA__MAX_SUB)
                                        ! indices in input data array of 
                                        ! bolometers observing the source in
                                        ! PHOTOM mode
      INTEGER          IN_POINTER (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! array connecting output bolometer
                                        ! positions to input data array
      INTEGER          IN_QUALITY_PTR   ! pointer to quality array in input
                                        ! file
      INTEGER          IN_RA_STRT_ARY   ! array identifier to .SCUCD.RA_STRT
      INTEGER          IN_RA_STRT_PTR   ! pointer to .SCUCD.RA_STRT
      INTEGER          IN_RA_VEL_ARY    ! array identifier to .SCUCD.RA_VEL
      INTEGER          IN_RA_VEL_PTR    ! pointer to .SCUCD.RA_VEL
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                        ! locator to SCUBA extension in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                        ! locator to SCUCD extension in input
                                        ! file
      INTEGER          IN_VARIANCE_PTR  ! pointer to variance array in input
                                        ! file
      INTEGER          IPOSN            ! Position in string
      INTEGER          ISTART           ! index of start of sub-string
      INTEGER          ITEMP            ! scratch integer
      INTEGER          IY               ! year of observation
      INTEGER          J                ! Loop counter
      INTEGER          JIGGLE           ! jiggle index
      INTEGER          JIGGLE_COUNT     ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH  ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT    ! number of times jiggle pattern is
                                        ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                        ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                        ! y jiggle offsets (arcsec)
      DOUBLE PRECISION LAT_OBS          ! latitude of observatory (radians)
      DOUBLE PRECISION LAT_RAD          ! latitude of telescope centre (radians)
      DOUBLE PRECISION LAT2_RAD         ! latitude of telescope centre at MJD2
                                        ! (radians)
      INTEGER          LBND (MAXDIM)    ! lower bounds of array
      DOUBLE PRECISION LONG_RAD         ! longitude of telescope centre 
                                        ! (radians)
      DOUBLE PRECISION LONG2_RAD        ! apparent RA of telescope centre at
                                        ! MJD2 (radians)
      DOUBLE PRECISION LST              ! sidereal time of measurement
      REAL             MAP_X            ! x offset of map centre from telescope
                                        ! centre (arcsec)
      REAL             MAP_Y            ! y offset of map centre from telescope
                                        ! centre (arcsec)
      INTEGER          MEASUREMENT      ! measurement index in DO loop
      DOUBLE PRECISION MJD1             ! modified Julian day at which object 
                                        ! was at LAT,LONG for PLANET centre
                                        ! coordinate system
      DOUBLE PRECISION MJD2             ! modified Julian day at which object
                                        ! was at LAT2,LONG2 for PLANET centre
                                        ! coordinate system
      INTEGER          NDIM             ! the number of dimensions in an array
      INTEGER          NREC             ! number of history records in file
      INTEGER          N_BEAM           ! number of beams for which data have
                                        ! been reduced
      INTEGER          N_BOL_IN         ! number of bolometers measured in 
                                        ! input file
      INTEGER          N_BOL_OUT        ! number of bolometers measured in
                                        ! output file
      INTEGER          N_EXPOSURES      ! number of exposures per integration
      INTEGER          N_FITS           ! number of FITS lines read from file
      INTEGER          N_INTEGRATIONS   ! number of integrations per measurement
      INTEGER          N_MEASUREMENTS   ! number of measurements in the file
      INTEGER          N_POINT          ! used size of pointing correction 
                                        ! arrays
      INTEGER          N_POS            ! the total number of positions measured
      INTEGER          N_SUB            ! number of sub-instruments used
      INTEGER          N_SWITCHES       ! number of switches per exposure
      CHARACTER*30     OBJECT           ! name of object observed
      CHARACTER*15     OBSERVING_MODE   ! type of observation
      CHARACTER*15     OFFSET_COORDS    ! coord system of OFFSET_X and OFFSET_Y
      REAL             OFFSET_X         ! x offset of measurement
      REAL             OFFSET_Y         ! y offset of measurement
      INTEGER          OUTNDF           ! NDF identifier of output file
      INTEGER          OUT_A_PTR        ! Pointer to AXIS 
      INTEGER          OUT_BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers in output
                                        ! file
      INTEGER          OUT_BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers in
                                        ! output file
      INTEGER          OUT_DATA_PTR     ! pointer to data array in output file
      CHARACTER*(DAT__SZLOC) OUT_FITSX_LOC
                                        ! locator to FITS extension in output
                                        ! file
      INTEGER          OUT_PHOT_BB (SCUBA__MAX_BEAM,SCUBA__MAX_SUB)
                                        ! indices in output data array of
                                        ! bolometers measuring the source in
                                        ! PHOTOM mode
      CHARACTER*(DAT__SZLOC) OUT_SCUBAX_LOC
                                        ! locator to SCUBA extension in output
                                        ! file
      INTEGER          OUT_QUALITY_PTR  ! pointer to quality array in output 
      INTEGER          OUT_VARIANCE_PTR ! pointer to variance array in output
      INTEGER          POSITION         ! Position in array
      REAL             POINT_DAZ (SCUBA__MAX_POINT)
                                        ! azimuth pointing corrections (arcsec)
      REAL             POINT_DEL (SCUBA__MAX_POINT)
                                        ! elevation pointing corrections
                                        ! (arcsec)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                        ! LST of pointing corrections (radians)
      INTEGER          POSITION         ! Position in array
      DOUBLE PRECISION RA_CENTRE        ! apparent RA of map centre (radians)
      REAL             RA_START         ! RA offset of scan start (arcsec)
      REAL             RA_VEL           ! RA velocity of scan (arcsec/sec)
      LOGICAL          REDUCE_SWITCH    ! .TRUE. if REDUCE_SWITCH has been run
      DOUBLE PRECISION ROTATION         ! angle between apparent north and 
                                        ! north of input coord system (radians,
                                        ! measured clockwise from input north) 
      REAL             RTEMP            ! Scratch real
      INTEGER          RUN_NUMBER       ! run number of observation
      CHARACTER*15     SAMPLE_COORDS    ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE      ! SAMPLE_MODE of observation
      DOUBLE PRECISION SEC              ! second at which observation started
      CHARACTER*20     SECOND_LST       ! sidereal time at which SECOND_TAU
                                        ! measured
      DOUBLE PRECISION SECOND_LST_RAD   ! SECOND_LST in radians
      REAL             SECOND_TAU       ! zenith sky opacity at SECOND_LST
      INTEGER          SLA_STATUS       ! status return from SLA routine
      CHARACTER*80     STEMP            ! scratch string
      CHARACTER*40     SUBLIST          ! List of available sub instruments
      CHARACTER*15     SUB_FILTER (SCUBA__MAX_SUB)
                                        ! filters in front of sub-instruments
      CHARACTER*15     SUB_INSTRUMENT (SCUBA__MAX_SUB)
                                        ! sub-instruments used
      INTEGER          SUB_POINTER      ! index of SUB_REQUIRED in sub-
                                        ! instruments observed
      CHARACTER*15     SUB_REQUIRED     ! sub-instrument required for reduction
      REAL             SUB_WAVE (SCUBA__MAX_SUB)
                                        ! wavelengths of observation
      REAL             TAUZ             ! zenith sky opacity at time of 
                                        ! measurement
      INTEGER          UBND (MAXDIM)    ! upper bounds of array
      CHARACTER*15     UTDATE           ! date of observation
      CHARACTER*15     UTSTART          ! UT of start of observation
      DOUBLE PRECISION UT1              ! UT1 of start of observation expressed
                                        ! as modified Julian day
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  start up the NDF system and read in the demodulated data file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', INDF, STATUS)

* Read in badbit mask
      CALL NDF_BB(INDF, BADBIT, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (INDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (INDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (INDF, 'SCUCD', 'READ', IN_SCUCDX_LOC, STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_EXTINCTION: input file '//
     :        'contains too many FITS items', STATUS)
         END IF
      END IF
      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS, 
     :  STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN', 
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :  OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SAM_MODE',
     :  SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETC ('SAMPLE', SAMPLE_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_OUT (' ', 'REDS: run ^RUN was a ^MODE observation '//
     :  'with ^SAMPLE sampling of object ^OBJECT', STATUS)

*  get the number of history records present in the file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (INDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

*  check that the history of the input file is OK

         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (INDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_EXTINCTION: the '//
     :           'REDUCE_SWITCH application has not been run '//
     :           'on the input file', STATUS)
            END IF

            IF (EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_EXTINCTION: the '//
     :           'EXTINCTION application has already been run '//
     :           'on the input file', STATUS)
            END IF
         END IF
      END IF

*  get some other FITS items needed for this stage of reduction

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'EXP_TIME',
     :  EXP_TIME, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SAM_CRDS',
     :  SAMPLE_COORDS, STATUS)
      CALL CHR_UCASE (SAMPLE_COORDS)

*  the latitude of the observatory

      CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT-OBS',
     :  LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

*  Nasmyth coords of point on focal plane that the telescope is tracking
*  on the sky

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'CNTR_DU3',
     :  CENTRE_DU3, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'CNTR_DU4',
     :  CENTRE_DU4, STATUS)

*  coordinate system and coords of telescope `centre'

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'CENT_CRD',
     :  CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (CENTRE_COORDS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT',
     :  STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, LAT_RAD, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG',
     :  STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, LONG_RAD, STATUS)

      IF (CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LAT2',
     :     STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, LAT2_RAD, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'LONG2',
     :     STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, LONG2_RAD, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD1',
     :     MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 'MJD2',
     :     MJD2, STATUS)
      END IF

      IF ((CENTRE_COORDS .NE. 'AZ')  .AND.
     :    (CENTRE_COORDS .NE. 'GA')) THEN
         LONG_RAD = LONG_RAD * 15.0D0
         LONG2_RAD = LONG2_RAD * 15.0D0
      END IF

*  UT at which observation was made expressed as modified Julian day

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'UTDATE',
     :  UTDATE, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'UTSTART',
     :  UTSTART, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CHR_STATUS = SAI__OK

         ISTART = 1
         IEND = INDEX (UTDATE,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTDATE(ISTART:IEND-1), IY, CHR_STATUS)
         UTDATE (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = INDEX (UTDATE,':')
         CALL CHR_CTOI (UTDATE(ISTART:IEND-1), IM, CHR_STATUS)
         UTDATE (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = MAX (ISTART, CHR_LEN(UTDATE))
         CALL CHR_CTOI (UTDATE(ISTART:IEND), ID, CHR_STATUS)

         ISTART = 1
         IEND = INDEX(UTSTART,':')
         IEND = MAX (ISTART,IEND)
         CALL CHR_CTOI (UTSTART(ISTART:IEND-1), IHOUR, CHR_STATUS)
         UTSTART (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = INDEX(UTSTART,':')
         CALL CHR_CTOI (UTSTART(ISTART:IEND-1), IMIN, CHR_STATUS)
         UTSTART (IEND:IEND) = ' '
         ISTART = IEND + 1
         IEND = MAX (ISTART, CHR_LEN(UTSTART))
         CALL CHR_CTOD (UTSTART(ISTART:IEND), SEC, CHR_STATUS)

         IF (CHR_STATUS .NE. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('UTDATE', UTDATE)
            CALL MSG_SETC ('UTSTART', UTSTART)
            CALL ERR_REP (' ', 'REDS_EXTINCTION: error converting '//
     :        'UTDATE=^UTDATE and UTSTART=^UTSTART to UT1', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SLA_CLDJ (IY, IM, ID, UT1, STATUS)
         UT1 = UT1 + ((SEC/60.0D0 + DBLE(IMIN)) / 60.0D0 +
     :     DBLE(IHOUR)) / 24.0D0

         IF (STATUS .NE. SAI__OK) THEN
            CALL MSG_SETI ('SLA', STATUS)
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_EXTINCTION: bad status '//
     :        'returned from SLA_CLDJ - ^SLA', STATUS)
         END IF
      END IF

*  telescope offset from telescope centre

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_X',
     :  MAP_X, STATUS)
      MAP_X = MAP_X / REAL (R2AS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_Y',
     :  MAP_Y, STATUS)
      MAP_Y = MAP_Y / REAL (R2AS)

*  the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOL_IN, STATUS)

*  calculate the apparent RA and Dec of the object for the time of the
*  observation

      CALL SCULIB_CALC_APPARENT (LONG_RAD, LAT_RAD, LONG2_RAD,
     :  LAT2_RAD, DBLE(MAP_X), DBLE(MAP_Y), CENTRE_COORDS, UT1, 
     :  MJD1, MJD2, RA_CENTRE, DEC_CENTRE, ROTATION, STATUS)

*  map the various components of the data array and check the data dimensions 

      CALL NDF_DIM (INDF, MAXDIM, DIM, NDIM, STATUS)

* Map QUALITY first to stop automatic masking
      CALL NDF_MAP (INDF, 'QUALITY', '_UBYTE', 'READ',
     :  IN_QUALITY_PTR, ITEMP, STATUS)

      CALL NDF_MAP (INDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'VARIANCE', '_REAL', 'READ', IN_VARIANCE_PTR,
     :  ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .NE. N_BOL_IN)         .OR.
     :          (DIM(2) .LT. 1)                .OR.
     :          (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 '//
     :           '^DIM3', STATUS)
            END IF
         ELSE
            IF ((NDIM .NE. 2)          .OR.
     :          (DIM(1) .NE. N_BOL_IN) .OR.
     :          (DIM(2) .LT. 1))       THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :           STATUS)
            END IF
         END IF
      END IF

      N_POS = DIM (2)

*  map the DEM_PNTR array and check its dimensions

*      CALL NDF_XIARY (INDF, 'SCUBA', 'DEM_PNTR', 'READ', 
*    :  IN_DEM_PNTR_ARY, STATUS)
      CALL ARY_FIND (IN_SCUBAX_LOC, 'DEM_PNTR', IN_DEM_PNTR_ARY, STATUS)
      CALL ARY_DIM (IN_DEM_PNTR_ARY, MAXDIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IN_DEM_PNTR_ARY, '_INTEGER', 'READ', 
     :  IN_DEM_PNTR_PTR, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 3) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUBA.DEM_PNTR '//
     :        'array has bad number of dimensions', STATUS)
         ELSE 
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'exposures - ^DIM1', STATUS) 
            END IF
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'integrations - ^DIM2', STATUS)
            END IF
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'measurements - ^DIM3', STATUS)
            END IF
         END IF
      END IF

      N_EXPOSURES = DIM (1)
      N_INTEGRATIONS = DIM (2)
      N_MEASUREMENTS = DIM (3)

*  map the .SCUCD.LST_STRT array and check its dimensions

*      CALL NDF_XIARY (INDF, 'SCUCD', 'LST_STRT', 'READ', 
*    :  IN_LST_STRT_ARY, STATUS)
      CALL ARY_FIND (IN_SCUCDX_LOC, 'LST_STRT', IN_LST_STRT_ARY,
     :  STATUS)
      CALL ARY_DIM (IN_LST_STRT_ARY, MAXDIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IN_LST_STRT_ARY, '_DOUBLE', 'READ', 
     :  IN_LST_STRT_PTR, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 4) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUCD.LST_STRT '//
     :        'array has bad number of dimensions - ^NDIM', STATUS)
         ELSE
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :           'SCUCD.LST_STRT array contains bad number of '//
     :           'switch(es) - ^DIM1', STATUS)
            END IF
            IF (DIM(2) .NE. N_EXPOSURES) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NEXP', N_EXPOSURES)
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :           'there is a mismatch between the number of '//
     :           'exposures in SCUBA.DEM_PNTR (^NEXP) and in '//
     :           'SCUCD.LST_STRT (^DIM2)', STATUS)
            END IF
            IF (DIM(3) .NE. N_INTEGRATIONS) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NINT', N_INTEGRATIONS)
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :           'there is a mismatch between the number of '//
     :           'integrations in SCUBA.DEM_PNTR (^NINT) and in '//
     :           'SCUCD.LST_STRT (^DIM3)', STATUS)
            END IF
            IF (DIM(4) .NE. N_MEASUREMENTS) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
               CALL MSG_SETI ('DIM4', DIM(4))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :           'there is a mismatch between the number of '//
     :           'measurements in SCUBA.DEM_PNTR (^NMEAS) and in '//
     :           'SCUCD.LST_STRT (^DIM4)', STATUS)
            END IF
         END IF
      END IF

      N_SWITCHES = DIM (1)

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      CALL MSG_OUT (' ', 'REDS: file contains data for ^N_E '//
     :  'exposure(s) in ^N_I integration(s) in '//
     :  '^N_M measurement(s)', STATUS)

*  If the sampling was done by jiggling the secondary then read in the
*  relevant jiggle information

      IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'JIGL_CNT', JIGGLE_COUNT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'J_REPEAT', JIGGLE_REPEAT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'J_PER_S', JIGGLE_P_SWITCH, STATUS)

*  and get the jiggle pattern itself

         CALL DAT_FIND (IN_SCUCDX_LOC, 'JIGL_X', IN_LOC, STATUS)
         CALL DAT_GET1R (IN_LOC, SCUBA__MAX_JIGGLE, JIGGLE_X, ITEMP, 
     :     STATUS)
         IF (ITEMP .NE. JIGGLE_COUNT) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_EXTINCTION: mismatch '//
     :           'between JIGGLE_COUNT and number of X jiggle '//
     :           'offsets read', STATUS)
            END IF
         END IF
         CALL DAT_ANNUL (IN_LOC, STATUS)

         CALL DAT_FIND (IN_SCUCDX_LOC, 'JIGL_Y', IN_LOC, STATUS)
         CALL DAT_GET1R (IN_LOC, SCUBA__MAX_JIGGLE, JIGGLE_Y, ITEMP,
     :     STATUS)
         IF (ITEMP .NE. JIGGLE_COUNT) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_EXTINCTION: mismatch '//
     :           'between JIGGLE_COUNT and number of Y jiggle '// 
     :           'offsets read', STATUS)
            END IF
         END IF
         CALL DAT_ANNUL (IN_LOC, STATUS)

*  likewise for RASTER

      ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN
*         CALL NDF_XIARY (INDF, 'SCUBA', 'RA_STRT', 'READ',
*    :     IN_RA_STRT_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'RA_STRT', IN_RA_STRT_ARY,
     :     STATUS)
         CALL ARY_DIM (IN_RA_STRT_ARY, MAXDIM, DIM, NDIM, STATUS)
         CALL ARY_MAP (IN_RA_STRT_ARY, '_REAL', 'READ',
     :     IN_RA_STRT_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUCD.RA_STRT '//
     :           'array has bad number of dimensions - ^NDIM', 
     :           STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NS', N_SWITCHES)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'switches in SCUCD.DEM_PNTR (^NS and in '//
     :              'SCUCD.RA_STRT (^DIM2)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NEXP', N_EXPOSURES)
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'exposures in SCUBA.DEM_PNTR (^NEXP) and in '//
     :              'SCUCD.RA_STRT (^DIM2)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NINT', N_INTEGRATIONS)
                  CALL MSG_SETI ('DIM3', DIM(3))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in SCUBA.DEM_PNTR (^NINT) and in '//
     :              'SCUCD.RA_STRT (^DIM3)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
                  CALL MSG_SETI ('DIM4', DIM(4))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in .SCUBA.DEM_PNTR (^NMEAS) and '//
     :              'in SCUCD.RA_STRT (^DIM4)', STATUS)
               END IF
            END IF
         END IF

*        CALL NDF_XIARY (INDF, 'SCUBA', 'RA_VEL', 'READ',
*    :     IN_RA_VEL_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'RA_VEL', IN_RA_VEL_ARY,
     :     STATUS)

         CALL ARY_DIM (IN_RA_VEL_ARY, MAXDIM, DIM, NDIM, STATUS)
         CALL ARY_MAP (IN_RA_VEL_ARY, '_REAL', 'READ', IN_RA_VEL_PTR,
     :     ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUCD.RA_VEL '//
     :           'array has bad number of dimensions - ^NDIM', 
     :           STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NS', N_SWITCHES)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'switches in SCUCD.DEM_PNTR (^NS and in '//
     :              'SCUCD.RA_VEL (^DIM2)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NEXP', N_EXPOSURES)
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'exposures in SCUBA.DEM_PNTR (^NEXP) and in '//
     :              'SCUCD.RA_VEL (^DIM2)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NINT', N_INTEGRATIONS)
                  CALL MSG_SETI ('DIM3', DIM(3))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in SCUBA.DEM_PNTR (^NINT) and in '//
     :              'SCUCD.RA_VEL (^DIM3)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
                  CALL MSG_SETI ('DIM4', DIM(4))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in .SCUBA.DEM_PNTR (^NMEAS) and '//
     :              'in SCUCD.RA_VEL (^DIM4)', STATUS)
               END IF
            END IF
         END IF

*        CALL NDF_XIARY (INDF, 'SCUBA', 'DEC_STRT', 'READ',
*    :     IN_DEC_STRT_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'DEC_STRT', IN_DEC_STRT_ARY,
     :     STATUS)

         CALL ARY_DIM (IN_DEC_STRT_ARY, MAXDIM, DIM, NDIM, STATUS)
         CALL ARY_MAP (IN_DEC_STRT_ARY, '_REAL', 'READ', 
     :     IN_DEC_STRT_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUCD.DEC_STRT '//
     :           'array has bad number of dimensions - ^NDIM', 
     :           STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NS', N_SWITCHES)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'switches in SCUCD.DEM_PNTR (^NS and in '//
     :              'SCUCD.DEC_STRT (^DIM2)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NEXP', N_EXPOSURES)
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'exposures in SCUBA.DEM_PNTR (^NEXP) and in '//
     :              'SCUCD.DEC_STRT (^DIM2)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NINT', N_INTEGRATIONS)
                  CALL MSG_SETI ('DIM3', DIM(3))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in SCUBA.DEM_PNTR (^NINT) and in '//
     :              'SCUCD.DEC_STRT (^DIM3)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
                  CALL MSG_SETI ('DIM4', DIM(4))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in .SCUBA.DEM_PNTR (^NMEAS) and '//
     :              'in SCUCD.DEC_STRT (^DIM4)', STATUS)
               END IF
            END IF
         END IF

*        CALL NDF_XIARY (INDF, 'SCUBA', 'DEC_VEL', 'READ',
*    :     IN_DEC_VEL_ARY, STATUS)
         CALL ARY_FIND (IN_SCUCDX_LOC, 'DEC_VEL', IN_DEC_VEL_ARY,
     :     STATUS)

         CALL ARY_DIM (IN_DEC_VEL_ARY, MAXDIM, DIM, NDIM, STATUS)
         CALL ARY_MAP (IN_DEC_VEL_ARY, '_REAL', 'READ', 
     :     IN_DEC_VEL_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (NDIM .NE. 4) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUCD.DEC_VEL '//
     :           'array has bad number of dimensions - ^NDIM', 
     :           STATUS)
            ELSE
               IF (DIM(1) .NE. N_SWITCHES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NS', N_SWITCHES)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'switches in SCUCD.DEM_PNTR (^NS and in '//
     :              'SCUCD.DEC_VEL (^DIM2)', STATUS)
               END IF
               IF (DIM(2) .NE. N_EXPOSURES) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NEXP', N_EXPOSURES)
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'exposures in SCUBA.DEM_PNTR (^NEXP) and in '//
     :              'SCUCD.DEC_VEL (^DIM2)', STATUS)
               END IF
               IF (DIM(3) .NE. N_INTEGRATIONS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NINT', N_INTEGRATIONS)
                  CALL MSG_SETI ('DIM3', DIM(3))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in SCUBA.DEM_PNTR (^NINT) and in '//
     :              'SCUCD.DEC_VEL (^DIM3)', STATUS)
               END IF
               IF (DIM(4) .NE. N_MEASUREMENTS) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
                  CALL MSG_SETI ('DIM4', DIM(4))
                  CALL ERR_REP (' ', 'REDS_EXTINCTION: '//
     :              'there is a mismatch between the number of '//
     :              'integrations in .SCUBA.DEM_PNTR (^NMEAS) and '//
     :              'in SCUCD.DEC_VEL (^DIM4)', STATUS)
               END IF
            END IF
         END IF
      END IF

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STSTART',
     :  STEMP, STATUS)
      DO I = 1, 2
         ITEMP = INDEX (STEMP,':')
         IF (ITEMP .NE. 0) THEN
            STEMP (ITEMP:ITEMP) = ' '
         END IF
      END DO
      CALL MSG_SETC ('START_LST', STEMP)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STEND',
     :  STEMP, STATUS)
      DO I = 1, 2
         ITEMP = INDEX (STEMP,':')
         IF (ITEMP .NE. 0) THEN
            STEMP (ITEMP:ITEMP) = ' '
         END IF
      END DO
      CALL MSG_SETC ('END_LST', STEMP)
      CALL MSG_OUT (' ', 'REDS: observation started at sidereal '//
     :  'time ^START_LST and ended at ^END_LST', STATUS)

*  find and report the sub instruments used and filters for this observation

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_SUBS',
     :  N_SUB, STATUS)

      CALL MSG_OUT (' ', 'REDS: file contains data for the '//
     :  'following sub-instrument(s)', STATUS)

      IF (N_SUB .GT. 0) THEN
         STEMP = 'SUB_'
         DO I = 1, N_SUB
            ITEMP = 4
            CALL CHR_PUTI (I, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        STEMP, SUB_INSTRUMENT(I), STATUS)
            CALL CHR_UCASE (SUB_INSTRUMENT(I))
         END DO
         STEMP = 'FILT_'
         DO I = 1, N_SUB
            ITEMP = 5
            CALL CHR_PUTI (I, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        STEMP, SUB_FILTER(I), STATUS)
            CALL CHR_UCASE (SUB_FILTER(I))
         END DO
         STEMP = 'WAVE_'
         DO I = 1, N_SUB
            ITEMP = 5
            CALL CHR_PUTI (I, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        STEMP, SUB_WAVE(I), STATUS)
         END DO

         DO I = 1, N_SUB
            CALL MSG_SETC ('SUB', SUB_INSTRUMENT(I))
            CALL MSG_SETC ('FILT', SUB_FILTER(I))
            CALL MSG_OUT (' ', ' - ^SUB with filter ^FILT', STATUS)
         END DO
      END IF 

*  for a PHOTOM observation read the PHOT_BB array and check its dimensions

      IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
         DIMX (1) = SCUBA__MAX_BEAM
         DIMX (2) = SCUBA__MAX_SUB
         CALL CMP_GETNI (IN_SCUBAX_LOC, 'PHOT_BB', 2, DIMX, 
     :     IN_PHOT_BB, DIM , STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            IF ((DIM(1) .NE. SCUBA__MAX_BEAM) .OR.
     :          (DIM(2) .NE. N_SUB))         THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUBA.PHOT_BB '//
     :           'array has bad dimensions (^NDIM) ^DIM1 ^DIM2', STATUS)
            END IF
         END IF
      END IF
            
*  get the sub-instrument of interest and check it's OK

      IF (N_SUB .EQ. 1) THEN
*     If we only have one wavelength we dont need to ask
         SUB_POINTER = 1
         SUB_REQUIRED = SUB_INSTRUMENT(SUB_POINTER)
      ELSE
*     Put all possible answers in a string
         SUBLIST = ''
         IPOSN = 0
         DO I = 1, N_SUB
            CALL CHR_APPND(SUB_INSTRUMENT(I), SUBLIST, IPOSN)
            CALL CHR_APPND(',',SUBLIST,IPOSN)
         END DO
*     Ask for the sub array
         IF (N_SUB .GT. 0) THEN
         CALL PAR_CHOIC('SUB_INSTRUMENT', SUB_INSTRUMENT(1), SUBLIST,
     :        .TRUE., SUB_REQUIRED, STATUS)
         CALL CHR_UCASE (SUB_REQUIRED)
         END IF

         SUB_POINTER = VAL__BADI
         IF (N_SUB .GT. 0) THEN
            DO I = 1, N_SUB
               IF (SUB_REQUIRED .EQ. SUB_INSTRUMENT(I)) THEN
                  SUB_POINTER =I 
               END IF
            END DO
         END IF
      END IF
         
      IF (STATUS .EQ. SAI__OK) THEN
         IF (SUB_POINTER .EQ. VAL__BADI) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SUB', SUB_REQUIRED)
            CALL ERR_REP (' ', 'REDS_EXTINCTION: the file does not '//
     :        'contain data for sub-instrument ^SUB', STATUS)
         END IF
      END IF

*  get the bolometer description arrays

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_TYPE', IN_LOC, STATUS)
      NDIM = 2
      DIMX (1) = SCUBA__NUM_CHAN
      DIMX (2) = SCUBA__NUM_ADC
      CALL DAT_GETNC (IN_LOC, NDIM, DIMX, BOL_TYPE, DIM, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :       (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :       (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUBA.BOL_TYPE '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_DU3', IN_LOC, STATUS)
      NDIM = 2
      DIMX (1) = SCUBA__NUM_CHAN
      DIMX (2) = SCUBA__NUM_ADC
      CALL DAT_GETNR (IN_LOC, NDIM, DIMX, BOL_DU3, DIM, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :       (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :       (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUBA.BOL_DU3 '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_DU4', IN_LOC, STATUS)
      NDIM = 2
      DIMX (1) = SCUBA__NUM_CHAN
      DIMX (2) = SCUBA__NUM_ADC
      CALL DAT_GETNR (IN_LOC, NDIM, DIMX, BOL_DU4, DIM, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)                 .OR.
     :       (DIM(1) .NE. SCUBA__NUM_CHAN) .OR.
     :       (DIM(2) .NE. SCUBA__NUM_ADC)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL ERR_REP (' ', 'REDS_EXTINCTION: .SCUBA.BOL_DU4 '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_ADC', IN_LOC, STATUS)
      CALL DAT_GET1I (IN_LOC, SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :  IN_BOL_ADC, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (ITEMP .NE. N_BOL_IN) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_EXTINCTION: dimension of '//
     :        '.SCUBA.BOL_ADC does not match main data array',
     :        STATUS)
         END IF
      END IF

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_CHAN', IN_LOC, STATUS)
      CALL DAT_GET1I (IN_LOC, SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :  IN_BOL_CHAN, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (ITEMP .NE. N_BOL_IN) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_EXTINCTION: dimension of '//
     :        '.SCUBA.BOL_CHAN does not match main data array',
     :        STATUS)
         END IF
      END IF

*  find how many bolometers the input data has in the required sub-instrument
*  and their ADC/channel numbers and positions in the array

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CALC_SUB_BOLS (N_BOL_IN, IN_BOL_ADC, IN_BOL_CHAN,
     :     SCUBA__NUM_CHAN, SCUBA__NUM_ADC, BOL_TYPE, SUB_REQUIRED,
     :     N_BOL_OUT, OUT_BOL_ADC, OUT_BOL_CHAN, IN_POINTER, STATUS)
      END IF

      IF (N_BOL_OUT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_EXTINCTION: none of the '//
     :        'measured bolometers belongs to the requested '//
     :        'sub-instrument', STATUS)
         END IF
      END IF

*  get the sky opacities at times bracketing the observation

      CALL PAR_GET0R ('FIRST_TAU', FIRST_TAU, STATUS)
      CALL PAR_GET0C ('FIRST_LST', FIRST_LST, STATUS)
      CALL PAR_GET0R ('SECOND_TAU', SECOND_TAU, STATUS)
      CALL PAR_GET0C ('SECOND_LST', SECOND_LST, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         ITEMP = 1
         CALL SLA_DAFIN (FIRST_LST, ITEMP, FIRST_LST_RAD, SLA_STATUS)
         FIRST_LST_RAD = FIRST_LST_RAD * 15.0D0
         IF (SLA_STATUS .NE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('LST', FIRST_LST)
            CALL ERR_REP (' ', 'REDS_EXTINCTION: error decoding '//
     :        'LST - ^LST', STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         ITEMP = 1
         CALL SLA_DAFIN (SECOND_LST, ITEMP, SECOND_LST_RAD,
     :     SLA_STATUS)
         SECOND_LST_RAD = SECOND_LST_RAD * 15.0D0
         IF (SLA_STATUS .NE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('LST', SECOND_LST)
            CALL ERR_REP (' ', 'REDS_EXTINCTION: error decoding '//
     :        'LST - ^LST', STATUS)
         END IF
      END IF

*  now open the output NDF, propagating it from the input file

      CALL NDF_PROP (INDF, ' ', 'OUT', OUTNDF, STATUS)

*  reset the data array bounds and map the various components

      N_BEAM = 1
      NDIM = 2
      LBND (1) = 1
      LBND (2) = 1
      UBND (1) = N_BOL_OUT
      UBND (2) = N_POS
      IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
         N_BEAM = SCUBA__MAX_BEAM
         NDIM = 3
         LBND (3) = 1
         UBND (3) = SCUBA__MAX_BEAM
      END IF
      CALL NDF_SBND (NDIM, LBND, UBND, OUTNDF, STATUS)

      CALL NDF_MAP (OUTNDF, 'QUALITY', '_UBYTE', 'WRITE',
     :  OUT_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'DATA', '_REAL', 'WRITE', 
     :  OUT_DATA_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'VARIANCE', '_REAL', 'WRITE',
     :  OUT_VARIANCE_PTR, ITEMP, STATUS)

* Bad bit mask
      CALL NDF_SBB(BADBIT, OUTNDF, STATUS)

*  extract data for the required bolometers into the output data arrays

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_SUB_BOLS (N_BOL_IN, N_POS, N_BEAM,
     :     %val(IN_DATA_PTR), %val(IN_VARIANCE_PTR), 
     :     %val(IN_QUALITY_PTR), N_BOL_OUT,
     :     IN_POINTER, %val(OUT_DATA_PTR), %val(OUT_VARIANCE_PTR),
     :     %val(OUT_QUALITY_PTR), STATUS)
      END IF

*  set the bolometer description arrays to their new values

      CALL NDF_XLOC (OUTNDF, 'SCUBA', 'UPDATE', OUT_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (OUTNDF, 'FITS', 'UPDATE', OUT_FITSX_LOC, STATUS)

      CALL CMP_MOD (OUT_SCUBAX_LOC, 'BOL_ADC', '_INTEGER', 1,
     :  N_BOL_OUT, STATUS)
      CALL CMP_PUT1I (OUT_SCUBAX_LOC, 'BOL_ADC', N_BOL_OUT, 
     :  OUT_BOL_ADC, STATUS)

      CALL CMP_MOD (OUT_SCUBAX_LOC, 'BOL_CHAN', '_INTEGER', 1,
     :  N_BOL_OUT, STATUS)
      CALL CMP_PUT1I (OUT_SCUBAX_LOC, 'BOL_CHAN', N_BOL_OUT,
     :  OUT_BOL_CHAN, STATUS)

*  modify the FITS keywords to reflect the fact that the data only come
*  from 1 sub-instrument in the output file

      CALL SCULIB_REWRITE_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :  'N_SUBS', 1, STATUS)
      CALL SCULIB_REWRITE_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :  'SUB_1', SUB_INSTRUMENT(SUB_POINTER), STATUS)
      CALL SCULIB_REWRITE_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :  'FILT_1', SUB_FILTER(SUB_POINTER), STATUS)
      CALL SCULIB_REWRITE_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :  'WAVE_1', SUB_WAVE(SUB_POINTER), STATUS)
      CALL SCULIB_REWRITE_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :  'N_BOLS', N_BOL_OUT, STATUS)
      CALL DAT_PUT1C (OUT_FITSX_LOC, N_FITS, FITS, STATUS)

*  for a PHOTOM observation recalculate the PHOT_BB array for the bolometers
*  in the selected sub-instrument

      IF ((OBSERVING_MODE.EQ.'PHOTOM') .AND. (STATUS.EQ.SAI__OK)) THEN
	 DO BEAM = 1, SCUBA__MAX_BEAM
            ITEMP = IN_PHOT_BB(BEAM,SUB_POINTER)
            IF (ITEMP .EQ. 0) THEN
               OUT_PHOT_BB(BEAM,1) = 0
            ELSE
               DO I = 1, N_BOL_OUT
                  IF ((IN_BOL_CHAN(ITEMP) .EQ. OUT_BOL_CHAN(I)) .AND.
     :                (IN_BOL_ADC(ITEMP) .EQ. OUT_BOL_ADC(I)))  THEN
                     OUT_PHOT_BB(BEAM,1) = I
                  END IF
               END DO
            END IF
         END DO

         DIM (1) = SCUBA__MAX_BEAM
         DIM (2) = 1
         CALL CMP_MOD (OUT_SCUBAX_LOC, 'PHOT_BB', '_INTEGER',
     :     2, DIM, STATUS)
	 CALL CMP_PUTVI (OUT_SCUBAX_LOC, 'PHOT_BB', SCUBA__MAX_BEAM,
     :     OUT_PHOT_BB, STATUS)
      END IF

*  now go through the various exposures in the observation

      IF (STATUS .EQ. SAI__OK) THEN

         DO MEASUREMENT = 1, N_MEASUREMENTS
            DO INTEGRATION = 1, N_INTEGRATIONS
               DO EXPOSURE = 1, N_EXPOSURES

*  calculate a mean LST for the switch sequence in the exposure

                  EXP_LST = 0.0D0
                  DO I = 1, N_SWITCHES
                     DATA_OFFSET = (((MEASUREMENT-1) * N_INTEGRATIONS +
     :                 INTEGRATION - 1) * N_EXPOSURES + 
     :                 EXPOSURE - 1) * N_SWITCHES + I - 1
                     CALL SCULIB_COPYD (1, %val(IN_LST_STRT_PTR +
     :                 DATA_OFFSET * VAL__NBD), DTEMP)
                     EXP_LST = EXP_LST + DTEMP
                  END DO
                  EXP_LST = EXP_LST / DBLE (N_SWITCHES)

*  get the scan parameters for a raster map

                  IF (SAMPLE_MODE .EQ. 'RASTER') THEN
                     CALL SCULIB_COPYR (1, %val(IN_RA_STRT_PTR +
     :                 DATA_OFFSET * VAL__NBR), RA_START)
                     CALL SCULIB_COPYR (1, %val(IN_RA_VEL_PTR +
     :                 DATA_OFFSET * VAL__NBR), RA_VEL)
                     CALL SCULIB_COPYR (1, %val(IN_DEC_STRT_PTR +
     :                 DATA_OFFSET * VAL__NBR), DEC_START)
                     CALL SCULIB_COPYR (1, %val(IN_DEC_VEL_PTR +
     :                 DATA_OFFSET * VAL__NBR), DEC_VEL)
                  END IF

*  find where the exposure starts and finishes in the data array

                  CALL SCULIB_FIND_SWITCH (%val(IN_DEM_PNTR_PTR),
     :              1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :              N_POS, 1, EXPOSURE, INTEGRATION, MEASUREMENT,
     :              EXP_START, EXP_END, STATUS)

*  cycle through the measurements in the exposure

                  DO I = EXP_START, EXP_END

*  calculate the LST at which the measurement was made (hardly worth
*  the bother because it's only an average over the switches anyway)

                     LST = EXP_LST + DBLE(I - EXP_START) * 
     :                 DBLE(EXP_TIME) * 1.0027379D0 * 2.0D0 * 
     :                 PI / (3600.0D0 * 24.0D0)

*  work out the zenith sky opacity at this LST

                     IF (LST .LE. FIRST_LST_RAD) THEN
                        TAUZ = FIRST_TAU
                     ELSE IF (LST .GE. SECOND_LST_RAD) THEN
                        TAUZ = SECOND_TAU
                     ELSE
                        TAUZ = FIRST_TAU + (SECOND_TAU - FIRST_TAU) *
     :                    (LST - FIRST_LST_RAD) /
     :                    (SECOND_LST_RAD - FIRST_LST_RAD)
                     END IF

*  work out the offset at which the measurement was made

                     IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
                        IF (JIGGLE_REPEAT .EQ. 1) THEN
                           JIGGLE = (EXPOSURE - 1) *
     :                       JIGGLE_P_SWITCH +
     :                       I - EXP_START + 1
                        ELSE
                           JIGGLE = MOD (I - EXP_START, 
     :                       JIGGLE_COUNT) + 1
                        END IF
                        OFFSET_X = JIGGLE_X (JIGGLE)
                        OFFSET_Y = JIGGLE_Y (JIGGLE)
                        OFFSET_COORDS = SAMPLE_COORDS
                     ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN
                        OFFSET_X = RA_START + RA_VEL * 
     :                    (REAL (I - EXP_START) + 0.5) * EXP_TIME
                        OFFSET_Y = DEC_START + DEC_VEL *
     :                    (REAL (I - EXP_START) + 0.5) * EXP_TIME
                        OFFSET_COORDS = 'RD'
                     END IF

*  now call a routine to work out the apparent RA,Dec of the measured
*  bolometers at this position (no pointing corrections applied)

                     N_POINT = 0
  
                     CALL SCULIB_CALC_BOL_COORDS (RA_CENTRE, 
     :                 DEC_CENTRE, LST, LAT_OBS, OFFSET_COORDS,
     :                 OFFSET_X, OFFSET_Y, ROTATION, N_POINT,
     :                 SCUBA__MAX_POINT, POINT_LST, POINT_DAZ, 
     :                 POINT_DEL, SCUBA__NUM_CHAN, SCUBA__NUM_ADC,
     :                 N_BOL_OUT, OUT_BOL_CHAN, OUT_BOL_ADC,
     :                 BOL_DU3, BOL_DU4, CENTRE_DU3, CENTRE_DU4,
     :                 BOL_RA, BOL_DEC, STATUS)

*  correct the bolometer data for the sky opacity

		     DO BEAM = 1, N_BEAM
                        DATA_OFFSET = (BEAM-1) * N_POS * N_BOL_OUT +
     :                    (I-1) * N_BOL_OUT
                        CALL SCULIB_CORRECT_EXTINCTION (
     :                    SCUBA__NUM_ADC * SCUBA__NUM_CHAN, N_BOL_OUT,
     :                    %val(OUT_DATA_PTR + DATA_OFFSET * VAL__NBR), 
     :                    %val(OUT_VARIANCE_PTR+DATA_OFFSET*VAL__NBR),
     :                    BOL_RA, BOL_DEC, LST, LAT_OBS, TAUZ,
     :                    STATUS)
                     END DO
                  END DO
               END DO
            END DO
         END DO

* Put in some axis information

         CALL NDF_AMAP(OUTNDF, 'CENTRE', 1, '_INTEGER', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_NFILLI (N_BOL_OUT, %val(OUT_A_PTR))
         END IF
         CALL NDF_ACPUT ('Bolometer', OUTNDF, 'LABEL', 1, STATUS)
         CALL NDF_AUNMP (OUTNDF, 'CENTRE', 1, STATUS)

         CALL NDF_AMAP (OUTNDF, 'CENTRE', 2, '_REAL', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN

            POSITION = 0

            DO I = 1, N_MEASUREMENTS * N_INTEGRATIONS
               DO J = 1, JIGGLE_COUNT
                  RTEMP = REAL(I)+ (REAL(J-1)/REAL(JIGGLE_COUNT))
                  CALL SCULIB_CFILLR(1,RTEMP,
     :                 %VAL(OUT_A_PTR+(POSITION*VAL__NBR)))
                  POSITION = POSITION + 1
               END DO
            END DO
            
         END IF
         CALL NDF_ACPUT ('Integration', OUTNDF, 'LABEL', 2, STATUS)
         
         CALL NDF_AUNMP (OUTNDF, 'CENTRE', 2, STATUS)

* and a title

         CALL NDF_CPUT(OBJECT, OUTNDF, 'Title', STATUS)
         CALL NDF_CPUT('Volts', OUTNDF, 'UNITS', STATUS)
         CALL NDF_CPUT('Extinction corrected',OUTNDF, 'LAB', STATUS)
 
*  unmap the main data array

         CALL NDF_UNMAP (OUTNDF, '*', STATUS)
      END IF

*  tidy up

      CALL ARY_ANNUL (IN_DEM_PNTR_ARY, STATUS)
      CALL ARY_ANNUL (IN_LST_STRT_ARY, STATUS)

      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)

      CALL DAT_ANNUL (OUT_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (OUT_FITSX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
