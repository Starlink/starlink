      SUBROUTINE SURF_EXTINCTION (STATUS)
*+
*  Name:
*     EXTINCTION

*  Purpose:
*     Remove the effect of atmospheric extinction from a SCUBA observation

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL SURF_EXTINCTION( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This application extracts from a demodulated-data file data for a 
*     specified SCUBA sub-instrument and corrects it for the effect of 
*     atmospheric extinction. The airmass at which each bolometer measurement
*     was made is calculated, then multiplied by the zenith sky extinction at
*     the time of the measurement to give the extinction optical depth along
*     the line of sight. The data point in question is then multiplied by the
*     exponential of the optical depth to give the value that would have been
*     measured in the absence of the atmosphere.
*
*       The zenith optical depth is assumed to vary linearly with time between
*     the values input in parameters FIRST_TAU and LAST_TAU. If the measurement
*     was taken at a time outside the range covered by FIRST_TAU and LAST_TAU
*     then the value closest in time will be used with no extrapolation.

*  Usage:
*     extinction in sub_instrument first_tau first_lst
*                second_tau second_lst out

*  ADAM Parameters:
*     FIRST_LST = CHAR (Read)
*        The local sidereal time at which FIRST_TAU was
*        the zenith sky opacity, in hh mm ss.ss format.
*     FIRST_TAU = REAL (Read)
*        The zenith sky opacity before the observation.
*     IN = NDF (Read)
*        The name of the input file containing demodulated SCUBA data.
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM.
*     OUT = NDF (Write)
*        The name of the output file to contain the
*        extinction corrected data for the specified
*        sub-instrument.
*     SECOND_LST = CHAR (Read)
*        The local sidereal time at which SECOND_TAU was
*        the zenith sky opacity, in hh mm ss.ss format.
*        If this value is less than FIRST_LST it is assumed you
*        are referring to the following day.
*     SECOND_TAU = REAL (Read)
*        The zenith sky opacity after the observation.
*     SUB_INSTRUMENT = CHAR (Read)
*        The name of the sub-instrument whose data are to
*        be selected from the input file and extinction
*        corrected. Permitted values are SHORT, LONG,
*        P1100, P1350 and P2000. This parameter is only used if
*        more than one sub-instrument is present in the file.

*  Examples:
*     extinction flat long 0.24 '01 00 00' 0.3 '02 00 00' corr
*        Process the LONG sub-instrument from flat.sdf using the 
*        knowledge that the 850 tau (assuming LONG refers to the 850
*        micron filter) was 0.24 at 1h LST and 0.3 at 2h LST. The 
*        output is written to corr.sdf
*     extinction test short 0.6 0 0.6 0 test2
*        Process the SHORT sub-instrument from test.sdf assuming
*        a constant tau of 0.6 (since FIRST_LST = SECOND_LST) and write
*        the result to test2.sdf

*  Related Applications:
*     SURF: SCUQUICK, REBIN, SCUPHOT

*  Algorithm:
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

*  Authors:
*     JFL: John Lightfoot (jfl@roe.ac.uk)
*     TIMJ: Tim Jenness (timj@jach.hawaii.edu)
*     {enter_new_authors_here}


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     $Log$
*     Revision 1.36  2005/03/19 01:41:02  timj
*     Propogate focal station from app level to calc_bol_coords
*
*     Revision 1.35  2005/03/18 06:26:32  timj
*     + Initialise some variables
*
*     1-AUG-1995: original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing allowed

*  Global constants:
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                 ! for VAL__xxxx
      INCLUDE 'SURF_PAR'                ! SURF constants
      INCLUDE 'MSG_PAR'                 ! MSG constants
      INCLUDE 'CNF_PAR'                 ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                   ! CHR used string length function
      EXTERNAL CHR_LEN

*  Local Constants:
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      CHARACTER * 10   TSKNAME          ! Name of task
      PARAMETER (TSKNAME = 'EXTINCTION') 

*  Local variables:
      LOGICAL          ABORTED          ! .TRUE. if the observation was
                                        ! aborted
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
      DOUBLE PRECISION DEC_CENTRE       ! apparent declination of map centre
                                        ! (radians)
      INTEGER          DIM (MAXDIM)     ! the dimensions of an array
      INTEGER          DIMX (MAXDIM)    ! expected dimensions of an array
      LOGICAL          EXTINCTION       ! .TRUE. if EXTINCTION has been run
      REAL             FAST_AXIS(SCUBA__MAX_SUB) ! Fast axis angles for POL
      CHARACTER*20     FIRST_LST        ! sidereal time at which FIRST_TAU
                                        ! measured
      DOUBLE PRECISION FIRST_LST_RAD    ! FIRST_LST in radians
      REAL             FIRST_TAU        ! zenith sky opacity at FIRST_LST
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      CHARACTER*132    FNAME            ! Input filename
      CHARACTER*10     FOCAL_STATION    ! Where is the instrument located?
      INTEGER          I                ! DO loop variable
      INTEGER          INDF             ! NDF identifier of input file
      CHARACTER*20     INSTRUMENT       ! Name of instrument
      INTEGER          IN_BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER          IN_BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
                                        ! measured in input file
      INTEGER          IN_DATA_PTR      ! pointer to data array of input file
      INTEGER          IN_DEC1_PTR      ! pointer to .SCUCD.DEC1
      INTEGER          IN_DEC2_PTR      ! pointer to .SCUCD.DEC2
      INTEGER          IN_DEM_PNTR_PTR  ! pointer to input .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                        ! locator to FITS extension in input
                                        ! file
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
      INTEGER          IN_RA1_PTR       ! pointer to .SCUCD.RA1
      INTEGER          IN_RA2_PTR       ! pointer to .SCUCD.RA2
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                        ! locator to SCUBA extension in input
                                        ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                        ! locator to SCUCD extension in input
                                        ! file
      INTEGER          IN_VARIANCE_PTR  ! pointer to variance array in input
                                        ! file
      INTEGER          ITEMP            ! scratch integer
      INTEGER          JIGGLE_COUNT     ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH  ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT    ! number of times jiggle pattern is
                                        ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                        ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                        ! y jiggle offsets (arcsec)
      INTEGER          LAST_EXP         ! the number of the exposure being
                                        ! measured when the abort occurred
      INTEGER          LAST_INT         ! the number of the integration
                                        ! being measured when the abort 
                                        ! occurred
      INTEGER          LAST_MEAS        ! the number of the measurement
                                        ! being measured when the abort 
                                        ! occurred
      DOUBLE PRECISION LAT_OBS          ! Latitude of observatory (radians)
      DOUBLE PRECISION LAT_RAD          ! latitude of telescope centre (radians)
      DOUBLE PRECISION LAT2_RAD         ! latitude of telescope centre at MJD2
                                        ! (radians)
      INTEGER          LBND (MAXDIM)    ! lower bounds of array
      DOUBLE PRECISION LONG_RAD         ! longitude of telescope centre 
                                        ! (radians)
      DOUBLE PRECISION LONG2_RAD        ! apparent RA of telescope centre at
                                        ! MJD2 (radians)
      CHARACTER*(15)   LOCAL_COORDS     ! Coordinate system of MAP_X and MAP_Y
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
      INTEGER          N_POS            ! the total number of positions measured
      INTEGER          N_SUB            ! number of sub-instruments used
      INTEGER          N_SWITCHES       ! number of switches per exposure
      CHARACTER*30     OBJECT           ! name of object observed
      CHARACTER*15     OBSERVING_MODE   ! type of observation
      INTEGER          OUTNDF           ! NDF identifier of output file
      CHARACTER*132    OUTFILE          ! Output filename
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
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC ! Locator to REDS extension
      INTEGER          OUT_VARIANCE_PTR ! pointer to variance array in output
      REAL             POINT_DAZ (SCUBA__MAX_POINT)
                                        ! azimuth pointing corrections (arcsec)
      REAL             POINT_DEL (SCUBA__MAX_POINT)
                                        ! elevation pointing corrections
                                        ! (arcsec)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                        ! LST of pointing corrections (radians)
      DOUBLE PRECISION RA_CENTRE        ! apparent RA of map centre (radians)
      LOGICAL          REDUCE_SWITCH    ! .TRUE. if REDUCE_SWITCH has been run
      DOUBLE PRECISION ROTATION         ! angle between apparent north and 
                                        ! north of input coord system (radians,
                                        ! measured clockwise from input north) 
      REAL             RTEMP            ! Scratch real
      INTEGER          RUN_NUMBER       ! run number of observation
      CHARACTER*15     SAMPLE_COORDS    ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE      ! SAMPLE_MODE of observation
      INTEGER          SECNDF           ! Section NDF identifier
      CHARACTER*20     SECOND_LST       ! sidereal time at which SECOND_TAU
                                        ! measured
      DOUBLE PRECISION SECOND_LST_RAD   ! SECOND_LST in radians
      REAL             SECOND_TAU       ! zenith sky opacity at SECOND_LST
      INTEGER          SLA_STATUS       ! status return from SLA routine
      CHARACTER*80     STATE            ! the state of SCUCD when the 
                                        ! datafile was closed
      CHARACTER*80     STEMP            ! scratch string
      INTEGER          SUB_POINTER      ! index of SUB_REQUIRED in sub-
                                        ! instruments observed
      CHARACTER*15     SUB_REQUIRED     ! sub-instrument required for reduction
                                        ! wavelengths of observation
      CHARACTER * (10) SUFFIX_STRINGS(SCUBA__N_SUFFIX) ! Suffix for OUT
      REAL             TAUZ             ! Tau read from FITS header
      CHARACTER*20     TELESCOPE        ! Name of telescope
      LOGICAL          THERE            ! Is an extension there?
      INTEGER          UBND (MAXDIM)    ! upper bounds of array
      DOUBLE PRECISION UT1              ! UT1 of start of observation expressed
                                        ! as modified Julian day
      REAL             WAVE             ! Wavelength of sub inst

*  Local Data:
      DATA SUFFIX_STRINGS /'!ext','x','ext'/

*.

      IF (STATUS .NE. SAI__OK) RETURN




*  Variable initialisation
      IN_RA1_PTR = 0
      IN_RA2_PTR = 0
      IN_DEC1_PTR = 0
      IN_DEC2_PTR = 0

*  start up the NDF system and read in the demodulated data file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', INDF, STATUS)

*     Get the name of the filename associated with 'IN'

      CALL SCULIB_GET_FILENAME('IN', FNAME, STATUS)

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
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file '//
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
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: run ^RUN was a ^MODE observation '//
     :     'with ^SAMPLE sampling of object ^OBJECT', STATUS)

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
               IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP(:10) .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :           'REDUCE_SWITCH application has not been run '//
     :           'on the input file', STATUS)
            END IF

            IF (EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :           'EXTINCTION application has already been run '//
     :           'on the input file', STATUS)
            END IF
         END IF
      END IF

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

*     initialise so that we do not get a warning with valgrind
*     when we multiply LONG2_RAD by 15.0
      LONG2_RAD = 0.0D0
      LAT2_RAD = 0.0D0
      MJD1 = 0.0D0
      MJD2 = 0.0D0

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
      ELSE
         LAT2_RAD = 0.0D0
         LONG2_RAD = 0.0D0
         MJD2 = 0.0D0
      END IF

      IF ((CENTRE_COORDS .NE. 'AZ')  .AND.
     :    (CENTRE_COORDS .NE. 'GA')) THEN
         LONG_RAD = LONG_RAD * 15.0D0
         LONG2_RAD = LONG2_RAD * 15.0D0
      END IF

*     Read the latitude of the observatory
      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

*  telescope offset from telescope centre

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_X',
     :  MAP_X, STATUS)
      MAP_X = MAP_X / REAL (R2AS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAP_Y',
     :  MAP_Y, STATUS)
      MAP_Y = MAP_Y / REAL (R2AS)

*     and the coordinate frame of these offsets
*     not sure whether old files have this parameter so test for status
*     If it is not available then assume it is CENTRE_COORDS

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LOCL_CRD', LOCAL_COORDS, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            LOCAL_COORDS = CENTRE_COORDS
         END IF
      END IF

*  the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOL_IN, STATUS)


*     Check the dimensions of the input data

      CALL NDF_DIM (INDF, MAXDIM, DIM, NDIM, STATUS)


      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .NE. N_BOL_IN)         .OR.
     :          (DIM(2) .LT. 1)                .OR.
     :          (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
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
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :           STATUS)
            END IF
         END IF
      END IF

      N_POS = DIM (2)

*     Map the input data
*     Map QUALITY first to stop automatic masking

      CALL NDF_MAP (INDF, 'QUALITY', '_UBYTE', 'READ',
     :  IN_QUALITY_PTR, ITEMP, STATUS)

      CALL NDF_MAP (INDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)
      CALL NDF_MAP (INDF, 'VARIANCE', '_REAL', 'READ', IN_VARIANCE_PTR,
     :  ITEMP, STATUS)



*  map the DEM_PNTR array and check its dimensions

      CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS, 
     :     N_MEASUREMENTS, STATUS)

*  map the .SCUCD.LST_STRT array and check its dimensions

      CALL SCULIB_GET_LST_STRT(IN_SCUCDX_LOC, IN_LST_STRT_PTR,
     :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*  UT at which observation was made expressed as modified Julian day

      CALL SCULIB_GET_MJD(N_FITS, FITS, %VAL(CNF_PVAL(IN_LST_STRT_PTR)), 
     :                    UT1,
     :     RTEMP, RTEMP, STATUS)

*  see if the observation completed normally or was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF

* Print out information on observation

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      IF (.NOT. ABORTED) THEN
         CALL MSG_SETC ('PKG', PACKAGE)
         CALL MSG_OUTIF (MSG__NORM, ' ', 
     :        '^PKG: file contains data for ^N_E '//
     :        'exposure(s) in ^N_I integration(s) in '//
     :        '^N_M measurement(s)', STATUS)
      ELSE

*  get the exposure, integration, measurement numbers at which the abort
*  occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'EXP_NO', LAST_EXP, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'INT_NO', LAST_INT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MEAS_NO', LAST_MEAS, STATUS)

         CALL MSG_SETC ('PKG', PACKAGE)
         CALL MSG_OUTIF (MSG__NORM, ' ', 
     :        '^PKG: the observation should have '//
     :        'had ^N_E exposure(s) in ^N_I integration(s) in ^N_M '//
     :        'measurement(s)', STATUS)
         CALL MSG_SETI ('N_E', LAST_EXP)
         CALL MSG_SETI ('N_I', LAST_INT)
         CALL MSG_SETI ('N_M', LAST_MEAS)
         CALL MSG_OUTIF (MSG__NORM, ' ', 
     :        ' - However, the observation was '//
     :        'ABORTED during exposure ^N_E of integration ^N_I '//
     :        'of measurement ^N_M', STATUS)
      END IF


*  calculate the apparent RA and Dec of the object for the time of the
*  observation

      CALL SCULIB_CALC_APPARENT (LAT_OBS, LONG_RAD, LAT_RAD, LONG2_RAD,
     :     LAT2_RAD, 0.0D0, 0.0D0, CENTRE_COORDS, 
     :     %VAL(CNF_PVAL(IN_LST_STRT_PTR)), UT1,
     :     MJD1, MJD2, RA_CENTRE, DEC_CENTRE, ROTATION, STATUS)


*  If the sampling was done by jiggling the secondary then read in the
*  relevant jiggle information

      IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN

         CALL SCULIB_GET_JIGGLE(IN_SCUCDX_LOC, SCUBA__MAX_JIGGLE,
     :        N_FITS, FITS, JIGGLE_COUNT, JIGGLE_REPEAT, 
     :        JIGGLE_P_SWITCH, RTEMP, SAMPLE_COORDS, JIGGLE_X,
     :        JIGGLE_Y, STATUS)

*  likewise for RASTER

      ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN

         CALL SCULIB_GET_RASTER(IN_SCUCDX_LOC, N_SWITCHES,
     :        N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :        IN_RA1_PTR, IN_RA2_PTR, IN_DEC1_PTR, IN_DEC2_PTR,
     :        STATUS)

      END IF

*     Get Sidereal time start and end.
*     This should be merged with SKYDIP and CHANGE_POINTING

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STSTART',
     :  STEMP, STATUS)
      DO I = 1, 2
         ITEMP = INDEX (STEMP,':')
         IF (ITEMP .NE. 0) THEN
            STEMP (ITEMP:ITEMP) = ' '
         END IF
      END DO
      ITEMP = INDEX (STEMP, '.') ! Remove the decimal places
      STEMP = STEMP(:ITEMP-1)

      CALL MSG_SETC ('START_LST', STEMP)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STEND',
     :  STEMP, STATUS)
      DO I = 1, 2
         ITEMP = INDEX (STEMP,':')
         IF (ITEMP .NE. 0) THEN
            STEMP (ITEMP:ITEMP) = ' '
         END IF
      END DO
      ITEMP = INDEX (STEMP, '.') ! Remove the decimal places
      STEMP = STEMP(:ITEMP-1)

      CALL MSG_SETC ('END_LST', STEMP)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: observation started at sidereal '//
     :     'time ^START_LST and ended at ^END_LST', STATUS)

*  find and report the sub instruments used and filters for this observation

      CALL SCULIB_GET_SUB_INST(PACKAGE, N_FITS, FITS, 'SUB_INSTRUMENT',
     :     N_SUB, SUB_POINTER, WAVE, SUB_REQUIRED, STEMP, STATUS)

*  find the focal station. First need the TELESCOP and INSTRUME as well
*  as subinstrument

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'TELESCOP',
     :     TELESCOPE, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'INSTRUME',
     :     INSTRUMENT, STATUS)

      CALL SURFLIB_GET_FOCAL_STATION( TELESCOPE, INSTRUMENT,
     :     SUB_REQUIRED, FOCAL_STATION, STATUS )

*  for a PHOTOM observation read the PHOT_BB array and check its dimensions

      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN
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
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: .SCUBA.PHOT_BB '//
     :           'array has bad dimensions (^NDIM) ^DIM1 ^DIM2', STATUS)
            END IF
         END IF
      END IF


*  get the bolometer description arrays

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOL_IN, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, IN_BOL_ADC, IN_BOL_CHAN, STATUS)


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
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: none of the '//
     :        'measured bolometers belongs to the requested '//
     :        'sub-instrument', STATUS)
         END IF
      END IF

*     Read the tau value from the FITS header
      STEMP = 'TAUZ_'
      ITEMP = 5
      CALL CHR_PUTI(SUB_POINTER ,STEMP, ITEMP)

*     Make sure the entry is present in the FITS
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_R(SCUBA__MAX_FITS, N_FITS, FITS,
     :        STEMP, TAUZ, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            TAUZ = 0.0
            CALL ERR_ANNUL(STATUS)
         END IF
      END IF

*     get the sky opacities at times bracketing the observation

*     Use a default of 0 for FIRST_LST but use
*     the tau as stored in the FITS header as the default FIRST_TAU

      CALL PAR_DEF0R('FIRST_TAU', TAUZ, STATUS)

*     Get the values
      CALL PAR_GET0R ('FIRST_TAU', FIRST_TAU, STATUS)
      CALL PAR_GET0C ('FIRST_LST', FIRST_LST, STATUS)

*     Use a dynamic default of FIRST_* for SECOND_*
*     Set up defaults
      CALL PAR_DEF0R('SECOND_TAU', FIRST_TAU, STATUS)
      CALL PAR_DEF0C('SECOND_LST', FIRST_LST, STATUS)

      CALL PAR_GET0R ('SECOND_TAU', SECOND_TAU, STATUS)
      CALL PAR_GET0C ('SECOND_LST', SECOND_LST, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         ITEMP = 1
         CALL SLA_DAFIN (FIRST_LST, ITEMP, FIRST_LST_RAD, SLA_STATUS)
         FIRST_LST_RAD = FIRST_LST_RAD * 15.0D0
         IF (SLA_STATUS .NE. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('LST', FIRST_LST)
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: error decoding '//
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
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: error decoding '//
     :        'LST - ^LST', STATUS)
         END IF
      END IF

*     Check for the case where FIRST_LST is greater than SECOND_LST
*     In this case we assume this means that the second lst is referring
*     to the following day (rather than precedding which makes no sense)
*     We therefore add 2*PI
      IF (FIRST_LST_RAD .GT. SECOND_LST_RAD) THEN
         SECOND_LST_RAD = SECOND_LST_RAD + (2.0D0 * PI)
         CALL MSG_SETC('TASK',TSKNAME)
         CALL MSG_OUTIF(MSG__NORM,' ',
     :        '^TASK: Assuming second LST refers to following day',
     :        STATUS)
      END IF

*  CREATE an output file

*  Calculate the data array bounds and map the various components

      N_BEAM = 1
      NDIM = 2
      LBND (1) = 1
      LBND (2) = 1
      UBND (1) = N_BOL_OUT
      UBND (2) = N_POS
      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN
         N_BEAM = SCUBA__MAX_BEAM
         NDIM = 3
         LBND (3) = 1
         UBND (3) = SCUBA__MAX_BEAM
      END IF

*     Now create a section of the required size

      CALL NDF_SECT(INDF, NDIM, LBND, UBND, SECNDF, STATUS)

*     For extinction we have the special case that we would like
*     to include a reference to the sub-instrument in the output
*     name. In order to prevent the sub-instrument from being chopped
*     if we are using the 'LONG' scuba_suffix option, the sub-inst
*     code has to be prepended before the suffix.
*     This means that we have to prepend to all suffices since we can't
*     know which one the user has chosen (without reading the environment
*     variable value in two places which is a pain).

*     Loop through all suffices

      DO I = 1, SCUBA__N_SUFFIX

*     For convenience I shall add the first 3 letters of the sub_instrument
*     This is the smallest unique name. Other options are wavelength (since
*     this is really unique) or just a single number to id the sub.
*     The 3 character option is probably okay since it is only possible 
*     to use one wavelength for each sub-instrument so this format will
*     be unique for any given observation.
*     Only problem is that the SHORT form of the default name probably 
*     should not include a long description of the sub-instrument.
*     Probably should use a single id for this case. Need to think
*     about this.

         STEMP = '_' // SUB_REQUIRED(1:3) // '_'

*     Lower case it (since this is a unix filename)
         CALL CHR_LCASE(STEMP)

*     Check for the special character of a '!' since this will indicate
*     a chop
*     If we have one then we have to keep it at the start of the string

         IF (SUFFIX_STRINGS(I)(1:1) .EQ. '!') THEN
            CALL CHR_PREFX(STEMP(1:CHR_LEN(STEMP)), 
     :           SUFFIX_STRINGS(I)(2:), ITEMP)
         ELSE
            CALL CHR_PREFX(STEMP(1:CHR_LEN(STEMP)), 
     :           SUFFIX_STRINGS(I), ITEMP)
         END IF




      END DO


*     Generate a default name for the output file
      CALL SCULIB_CONSTRUCT_OUT(FNAME, SUFFIX_ENV, SCUBA__N_SUFFIX,
     :     SUFFIX_OPTIONS, SUFFIX_STRINGS, OUTFILE, STATUS)

*     set the default
      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)


*     And propogate the section to the output (including the axes)

      CALL NDF_PROP (SECNDF, 'Axis,Units', 'OUT', OUTNDF, STATUS)

*     Annul the section

      CALL NDF_ANNUL(SECNDF, STATUS)

*     Map the output arrays

      CALL NDF_MAP (OUTNDF, 'QUALITY', '_UBYTE', 'WRITE',
     :  OUT_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'DATA', '_REAL', 'WRITE', 
     :  OUT_DATA_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUTNDF, 'VARIANCE', '_REAL', 'WRITE',
     :  OUT_VARIANCE_PTR, ITEMP, STATUS)

*  set the bad bit mask

      CALL NDF_SBB(BADBIT, OUTNDF, STATUS)

*  extract data for the required bolometers into the output data arrays

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_SUB_BOLS (N_BOL_IN, N_POS, N_BEAM,
     :     %VAL(CNF_PVAL(IN_DATA_PTR)), %VAL(CNF_PVAL(IN_VARIANCE_PTR)),
     :     %VAL(CNF_PVAL(IN_QUALITY_PTR)), N_BOL_OUT,
     :     IN_POINTER, %VAL(CNF_PVAL(OUT_DATA_PTR)), 
     :     %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :     %VAL(CNF_PVAL(OUT_QUALITY_PTR)), STATUS)
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
     :  'N_BOLS', N_BOL_OUT, STATUS)
      CALL DAT_PUT1C (OUT_FITSX_LOC, N_FITS, FITS, STATUS)

*     For a polarimetry observation we need to change the
*     .REDS.FAST_AXIS array to include a single value to reflect
*     the single sub-instrument. This is only required if the
*     .REDS.FAST_AXIS array exists (since REMIP can be run before
*     or after EXTINCTION)

*     First need to get the REDS extension locator
      CALL NDF_XSTAT(OUTNDF, 'REDS', THERE, STATUS)

      IF (THERE) THEN
         CALL NDF_XLOC(OUTNDF, 'REDS', 'UPDATE', OUT_REDSX_LOC, 
     :        STATUS)

*     Is the fast_Axis array there
         CALL DAT_THERE(OUT_REDSX_LOC,'FAST_AXIS', THERE, STATUS)

         IF (THERE) THEN
*     Read the array
         CALL CMP_GET1R(OUT_REDSX_LOC, 'FAST_AXIS', SCUBA__MAX_SUB, 
     :        FAST_AXIS, ITEMP, STATUS)

*     Delete the component
         CALL DAT_ERASE(OUT_REDSX_LOC, 'FAST_AXIS', STATUS)

*     Annul the locator
         CALL DAT_ANNUL(OUT_REDSX_LOC, STATUS)

*     Write the single value
         CALL NDF_XPT0R(FAST_AXIS(SUB_POINTER), OUTNDF, 'REDS',
     :        'FAST_AXIS', STATUS)
         END IF

      END IF


*  for a PHOTOM observation recalculate the PHOT_BB array for the bolometers
*  in the selected sub-instrument

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE.EQ.'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            DO BEAM = 1, SCUBA__MAX_BEAM
               ITEMP = IN_PHOT_BB(BEAM,SUB_POINTER)
               IF (ITEMP .EQ. 0) THEN
                  OUT_PHOT_BB(BEAM,1) = 0
               ELSE
                  DO I = 1, N_BOL_OUT
                     IF ((IN_BOL_CHAN(ITEMP) .EQ. OUT_BOL_CHAN(I)) .AND.
     :                    (IN_BOL_ADC(ITEMP) .EQ. OUT_BOL_ADC(I)))  THEN
                        OUT_PHOT_BB(BEAM,1) = I
                     END IF
                  END DO
               END IF
            END DO
            
            DIM (1) = SCUBA__MAX_BEAM
            DIM (2) = 1
            CALL CMP_MOD (OUT_SCUBAX_LOC, 'PHOT_BB', '_INTEGER',
     :           2, DIM, STATUS)
            CALL CMP_PUTVI (OUT_SCUBAX_LOC, 'PHOT_BB', SCUBA__MAX_BEAM,
     :           OUT_PHOT_BB, STATUS)
         END IF
      END IF

*  now go through the various exposures in the observation
*     (No pointing corrections applied)

      IF (STATUS .EQ. SAI__OK) THEN

         CALL SURFLIB_PROCESS_BOLS(TSKNAME, N_BEAM, N_BOL_OUT,
     :        N_POS, 1, N_SWITCHES, N_EXPOSURES, 
     :        N_INTEGRATIONS, N_MEASUREMENTS, 
     :        1, N_EXPOSURES, 1, N_INTEGRATIONS, 1, N_MEASUREMENTS,
     :        1, N_FITS, FITS,
     :        %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)), 
     :        %VAL(CNF_PVAL(IN_LST_STRT_PTR)),
     :        ROTATION, SAMPLE_MODE,
     :        SAMPLE_COORDS, 'RA', JIGGLE_REPEAT,
     :        JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y, JIGGLE_P_SWITCH,
     :        FOCAL_STATION, RA_CENTRE, DEC_CENTRE,
     :        %VAL(CNF_PVAL(IN_RA1_PTR)), %VAL(CNF_PVAL(IN_RA2_PTR)),
     :        %VAL(CNF_PVAL(IN_DEC1_PTR)), %VAL(CNF_PVAL(IN_DEC2_PTR)), 
     :        UT1, UT1,
     :        MJD1, LONG_RAD, LAT_RAD, MJD2, LONG2_RAD, LAT2_RAD,
     :        LOCAL_COORDS, DBLE(MAP_X), DBLE(MAP_Y),
     :        0, POINT_LST, POINT_DAZ, POINT_DEL,
     :        SCUBA__NUM_CHAN, SCUBA__NUM_ADC,OUT_BOL_ADC,OUT_BOL_CHAN,
     :        BOL_DU3, BOL_DU4, .FALSE., FIRST_LST_RAD, SECOND_LST_RAD,
     :        FIRST_TAU, SECOND_TAU, BOL_RA, BOL_DEC,
     :        %VAL(CNF_PVAL(OUT_DATA_PTR)), 
     :        %VAL(CNF_PVAL(OUT_VARIANCE_PTR)), .FALSE., 0,
     :        0,0,
     :        STATUS)


* and a title

         CALL NDF_CPUT('Extinction corrected',OUTNDF, 'LAB', STATUS)
 
*  unmap the main data array

         CALL NDF_UNMAP (OUTNDF, '*', STATUS)
      END IF

*  tidy up
      CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL CMP_UNMAP (IN_SCUCDX_LOC, 'LST_STRT', STATUS)

      IF (SAMPLE_MODE .EQ. 'RASTER') THEN
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA1', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA2', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC1', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC2', STATUS)
      END IF

      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)

      CALL DAT_ANNUL (OUT_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (OUT_FITSX_LOC, STATUS)

      CALL NDF_ANNUL (INDF, STATUS)
      CALL NDF_ANNUL (OUTNDF, STATUS)

      CALL NDF_END (STATUS)

      END
