      SUBROUTINE REDS_REMSKY (STATUS)
*+
*  Name:
*     REMSKY

*  Purpose:
*     Remove sky noise and constant offsets from SCUBA jiggle data

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL REDS_REMSKY( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description :
*     This task removes sky noise and constant offsets from SCUBA jiggle
*     data. It does this by requesting 'sky' bolometers, calculating some
*     average value for each jiggle and then subtracts this off the
*     jiggle. Each jiggle is analysed in turn. The average value can be
*     calculated in two ways:
*        1) Median - the median value for all the sky bolometers is taken
*                    from each bolomoter signal.
*        2) Mean   - the mean of the sky bolometers is used as the average.
*                    This mean value is iterative - ie The mean and standard
*                    deviation are calculated, any points greater than the
*                    given distance from the mean are removed and the mean
*                    and standard deviation are calculated.  This process
*                    is repeated until no bolometers are dropped from the
*                    mean.
*     A simple despiking system is also included. Each bolometer is analysed 
*     independently, a mean and standard deviation are calculated, any points
*     greater than NSIGMA sigma from the mean are treated as spikes and
*     removed. Note that this algorithm is only useful for very weak sources.
*     Bright sources will be removed (since a source  bolometer jiggles on
*     and off bright sources).

*  Usage:
*     REMSKY IN

*  ADAM Parameters:
*     BOLOMETERS = _CHAR (Read)
*        List of sky bolometers (either by number in the data file, or
*        by id (eg H7,G3)
*     CLIP = _LOGICAL (Read)
*        Answering yes to this will initiate the simple despiking routine.
*     IN = NDF (Read)
*        This is the name of the input demodulated data file
*     ITER_SIGMA = _REAL (Read)
*        When using MEAN to calculate the average, this is the sigma clipping
*        level used. Supplying -1 will turn off clipping.
*     MODE = _CHAR (Read)
*        Method to be used for calculating the average (MEDIAN or MEAN)
*     NSIGMA = _DOUBLE (Read)
*        Number of sigma to despike at.
*     OUT = NDF (Write)
*        Output data file

*  Implementation status:
*     The despiking routine sets QUALITY bit 5 to bad. It does not affect
*     the data.
 
*  Authors:
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*     {enter_new_authors_here}
 
*  History :
*     3 Nov 1996: TIMJ
*        Original version
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions :
      IMPLICIT NONE                     ! No implicit typing

*  Global constants :
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                 ! for VAL__xxxx
      INCLUDE 'REDS_SYS'                ! REDS constants

*  Status :
      INTEGER STATUS

*  External references :
      INTEGER CHR_LEN                   ! CHR used string length function
      BYTE    SCULIB_BITON              ! Turn on skybit

*  Local Constants :
      INTEGER          MAX__BOL                  ! max number of bolometers
      PARAMETER (MAX__BOL = 100)                 ! that can be specified
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
*  Local variables :
      INTEGER          B                ! Loop counter
      INTEGER          BB               ! Loop counter
      BYTE             BADBIT           ! Bad bit mask
      INTEGER          BEAM             ! beam number in DO loop
      INTEGER          BOL              ! Loop counter
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
      INTEGER          BOL_PTR          ! Pointer to single bolometer
      INTEGER          BOL_PTR_END      ! Pointer to end single bolometer
      INTEGER          BOL_QPTR         ! Pointer to single bolometer quality
      INTEGER          BOL_QPTR_END     ! Pointer to end single bol quality

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
      LOGICAL          DOCLIP           ! Clip over bolometers
      REAL             ITERCLIP         ! Number of bols to drop from mean
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
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
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
      DOUBLE PRECISION MJD1             ! modified Julian day at which object 
                                        ! was at LAT,LONG for PLANET centre
                                        ! coordinate system
      DOUBLE PRECISION MJD2             ! modified Julian day at which object
                                        ! was at LAT2,LONG2 for PLANET centre
                                        ! coordinate system
      CHARACTER * (10) MODE             ! Method of sky removal
      INTEGER          NDIM             ! the number of dimensions in an array
      INTEGER          NREC             ! number of history records in file
      DOUBLE PRECISION NSIGMA           ! clipping level
      INTEGER          N_BEAMS          ! number of beams for which data have
                                        ! been reduced
      INTEGER          N_BOLS           ! number of bolometers measured in
                                        ! output file
      INTEGER          N_EXPOSURES      ! number of exposures per integration
      INTEGER          N_FITS           ! number of FITS lines read from file
      INTEGER          N_GOODBOLS       ! Number of good bols in list
      INTEGER          N_INTEGRATIONS   ! number of integrations per measurement
      INTEGER          N_MEASUREMENTS   ! number of measurements in the file
      INTEGER          N_POINT          ! used size of pointing correction 
                                        ! arrays
      INTEGER          N_POS            ! the total number of positions measured
      INTEGER          N_SKYBOLS        ! Number of skybols
      INTEGER          N_SWITCHES       ! number of switches per exposure
      CHARACTER*30     OBJECT           ! name of object observed
      CHARACTER*15     OBSERVING_MODE   ! type of observation
      INTEGER          OFFSET           ! byte offset of data set
      CHARACTER*15     OFFSET_COORDS    ! coord system of OFFSET_X and OFFSET_Y
      REAL             OFFSET_X         ! x offset of measurement
      REAL             OFFSET_Y         ! y offset of measurement
      INTEGER          OUTNDF           ! NDF identifier of output file
      INTEGER          OUT_A_PTR        ! Pointer to AXIS 
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
      INTEGER          SECNDF           ! NDF id of section
      INTEGER          SKY_ADC          ! ADC of sky bol
      CHARACTER*3      SKYBOLC(MAX__BOL)       ! indices or names of 
                                               ! bolometers whose data are to
                                               ! be treated as sky
      INTEGER          SKYBOLS(MAX__BOL)! Indices of sky bolometers
      INTEGER          SKY_CHAN         ! Chan of SKY bol
      INTEGER          SNDF             ! identifier of section
      CHARACTER*80     STEMP            ! scratch string
      INTEGER          UBND(MAXDIM)     ! Upper bounds of section
*.

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
               CALL ERR_REP (' ', 'REDS_REMSKY: the '//
     :           'REDUCE_SWITCH application has not been run '//
     :           'on the input file', STATUS)
            END IF

            IF (.NOT.EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_REMSKY: the '//
     :           'EXTINCTION application has not been run '//
     :           'on the input file', STATUS)
            END IF
         END IF
      END IF

*  get the sub-instrument and wavelength of the data, check for consistency
 
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_1', STEMP, STATUS)
      
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'WAVE_1', RTEMP, STATUS)

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


*  the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)

*  map the various components of the data array and check the data dimensions 

      CALL NDF_DIM (INDF, MAXDIM, DIM, NDIM, STATUS)

      CALL NDF_MAP (INDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .NE. N_BOLS)         .OR.
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
     :          (DIM(1) .NE. N_BOLS) .OR.
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

      CALL NDF_UNMAP(INDF, '*', STATUS)



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
     :  BOL_ADC, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (ITEMP .NE. N_BOLS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_EXTINCTION: dimension of '//
     :        '.SCUBA.BOL_ADC does not match main data array',
     :        STATUS)
         END IF
      END IF

      CALL DAT_FIND (IN_SCUBAX_LOC, 'BOL_CHAN', IN_LOC, STATUS)
      CALL DAT_GET1I (IN_LOC, SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :  BOL_CHAN, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)

      IF (ITEMP .NE. N_BOLS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_EXTINCTION: dimension of '//
     :        '.SCUBA.BOL_CHAN does not match main data array',
     :        STATUS)
         END IF
      END IF

****** END CHECKING ******

*  now open the output NDF, propagating it from the input file

      CALL NDF_PROP (INDF, 'Data,Var,Qual,Axis', 'OUT', OUTNDF, STATUS)

*  get the bad bit mask

      CALL NDF_BB(OUTNDF, BADBIT, STATUS)

*  Get the list of SKY bolometers if not a photometry pixel

      IF (N_BOLS .GT. 1) THEN

         CALL PAR_GET1C ('BOLOMETERS', MAX__BOL, SKYBOLC,
     :        N_SKYBOLS, STATUS)
         CALL PAR_CANCL ('BOLOMETERS', STATUS)

         N_GOODBOLS = 0
         DO B = 1, N_SKYBOLS
            CALL CHR_CTOI (SKYBOLC(B), BOL, STATUS)
            IF (STATUS .EQ. SAI__OK) THEN
               N_GOODBOLS = N_GOODBOLS + 1
               SKYBOLS(N_GOODBOLS) = BOL
            ELSE
               CALL ERR_ANNUL (STATUS)
               CALL SCULIB_BOLDECODE (SKYBOLC(B),SKY_ADC, SKY_CHAN,
     :              STATUS)
 
*  search for the bolometer in the index
 
               IF (STATUS .EQ. SAI__OK) THEN
 
                  DO BB = 1, N_BOLS
                     IF ((SKY_ADC .EQ. BOL_ADC(BB)) .AND.
     :                    (SKY_CHAN .EQ. BOL_CHAN(BB))) THEN
                        N_GOODBOLS = N_GOODBOLS + 1
                        SKYBOLS(N_GOODBOLS) = BB
                     END IF
                  END DO
                  
               ELSE
                  CALL ERR_ANNUL (STATUS)
                  CALL MSG_SETC('BOL', SKYBOLC(B))
                  CALL MSG_OUT(' ','Bolometer ^BOL not found', STATUS)
               END IF
            END IF
         END DO


* Which mode of sky removal
         CALL PAR_CHOIC('MODE', 'MEAN','MEAN,MEDIAN', .TRUE., MODE,
     :        STATUS)

         IF (MODE .EQ. 'MEAN') THEN
            CALL PAR_GET0R('ITER_SIGMA',ITERCLIP, STATUS)
         ELSE
            ITERCLIP = -1.0
         END IF

      END IF

*  Remove spikes from each bolometer?
      CALL PAR_GET0L('CLIP', DOCLIP, STATUS)
*  How many sigma?
      IF (DOCLIP) CALL PAR_GET0D('NSIGMA', NSIGMA, STATUS)

*  Go through the data and remove sky

* Loop through beams if this is a photometry

      IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
         N_BEAMS = SCUBA__MAX_BEAM
         NDIM = 3
      ELSE
         N_BEAMS = 1
         NDIM = 2
      END IF

*  Define the base section
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = N_BOLS
      UBND(2) = N_POS

      DO BEAM = 1, N_BEAMS

*    Get the beam as an NDF section
         UBND(3) = BEAM
         LBND(3) = BEAM

         CALL NDF_SECT(OUTNDF, NDIM, LBND, UBND, SECNDF, STATUS)

*     map the various components

         CALL NDF_MAP (SECNDF, 'QUALITY', '_UBYTE', 'UPDATE',
     :        OUT_QUALITY_PTR, ITEMP, STATUS)
         CALL NDF_MAP (SECNDF, 'DATA', '_REAL', 'UPDATE', 
     :        OUT_DATA_PTR, ITEMP, STATUS)
         CALL NDF_MAP (SECNDF, 'VARIANCE', '_REAL', 'UPDATE',
     :        OUT_VARIANCE_PTR, ITEMP, STATUS)

         IF (N_BOLS .GT. 1) THEN
            CALL SCULIB_REM_SKY(MODE, N_BOLS, N_POS, 
     :           %val(OUT_DATA_PTR),
     :           %val(OUT_VARIANCE_PTR), 
     :           %val(OUT_QUALITY_PTR),
     :           ITERCLIP, N_GOODBOLS, SKYBOLS, BADBIT, STATUS)
         END IF

         IF (DOCLIP) THEN

*  Grab some scratch data
            CALL SCULIB_MALLOC(N_POS * VAL__NBR, BOL_PTR,
     :           BOL_PTR_END, STATUS)
            CALL SCULIB_MALLOC(N_POS * VAL__NBUB, BOL_QPTR,
     :           BOL_QPTR_END, STATUS)

            DO I = 1, N_BOLS

               CALL SCULIB_EXTRACT_BOL(I, N_BOLS, N_POS, 
     :              %val(OUT_DATA_PTR), 
     :              %val(OUT_QUALITY_PTR),
     :              %val(BOL_PTR), %val(BOL_QPTR), STATUS)

*  Despike
               CALL SCULIB_CLIP_BOL(N_POS, %val(BOL_PTR),
     :              %val(BOL_QPTR), NSIGMA, BADBIT, STATUS)

               CALL SCULIB_INSERT_BOL(I, N_BOLS, N_POS, %val(BOL_PTR), 
     :              %val(BOL_QPTR), %val(OUT_DATA_PTR),
     :              %val(OUT_QUALITY_PTR), STATUS)

            END DO

*     Tidy up
            BADBIT = SCULIB_BITON(BADBIT, 4)
            CALL SCULIB_FREE('BOLDATA', BOL_PTR, BOL_PTR_END, STATUS)
            CALL SCULIB_FREE('BOLQDATA', BOL_QPTR, BOL_QPTR_END, STATUS)
         
         END IF

*  unmap the main data array

         CALL NDF_UNMAP (SECNDF, '*', STATUS)
         CALL NDF_ANNUL(SECNDF, STATUS)
      END DO


*  set the bad bit mask

      PRINT *, 'BADBIT is now ' ,BADBIT
      CALL NDF_SBB(BADBIT, OUTNDF, STATUS)



*  tidy up

      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
