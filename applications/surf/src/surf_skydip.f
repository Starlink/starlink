      SUBROUTINE REDS_SKYDIP (STATUS)
*+
*  Name:
*     SKYDIP

*  Purpose:
*     calculate sky properties from SCUBA skydip data
 
*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL REDS_SKYDIP( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description :

*     This application takes raw SKYDIP data and calculates tau, eta_tel
*     and B by fitting. Sky temperatures are taken at different airmasses.

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the raw skydip data file
*     B_FIT = _REAL (Read)
*        The B parameter (filter transmission). This efficiency factor
*        must be between 0 and 1. A negative value allows this parameter
*        to be free.
*     ETA_TEL = _REAL (Read)
*        The telescope efficiency. Must be between 0 and 1.0. 
*        A negative value allows this parameter to be free.
*     MODEL_OUT = NDF (Write)
*        The name of the output file that contains the fitted sky
*        temperatures.
*     OUT = NDF (Write)
*        The name of the output file that contains the measured 
*        sky temperatures.
*     SUB_INSTRUMENT = _CHAR (Read)
*        The name of the sub-instrument whose data are to be
*        selected from the input file and fitted. Permitted 
*        values are SHORT, LONG, P1100, P1300 and P2000
*     T_COLD = _REAL (Read)
*        Temperature of the cold load. The default value is the one
*        taken from the input file.

*  Algorithm:
*     If status is good on entry, the routine reads in some general information
*     about the run and checks the HISTORY record to make sure this is a SKYDIP
*     observation. 
*     The sub-instrument information is then read and checked against the 
*     requested sub-instrument. 
*     Bolometer information is then read and matched to the requested sub-
*     instrument.
*     If everything is okay, the DATA is mapped and the dimensions checked.
*     This is followed by a check of DEM_PNTR.
*     The MAX and MIN elevation are then read from the FITS data.
*     The AIRMASS steps are then calculated and fitting paramters requested.
*     Now loop over each measurement, find the data (with SCULIB_FIND_SWITCH),
*     Select a part of the input data (with SKYDIP_GET_SLICE) and then convert
*     to a sky temperature with SCULIB_SKYDIP_TEMPERATURES.
*
*     The data is then fitted with SCULIB_FIT_SKYDIP. Both the measured and 
*     model data are then written to NDFs for plotting in KAPPA-LINPLOT. (The
*     model data is calculated with SCULIB_J_THEORETICAL.

*  Implementation status:
*     Data, Variance and Quality arrays are copied. 
*     Bad pixels are recognised.
*     Uses NAG routine for fit.

*  Authors :
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History :
*     $Id$
*     $Log$
*     Revision 1.7  1996/12/12 21:05:28  timj
*     Fix Starlink header.
*     Replace COPYR with VEC
*     Use PAR_CHOIC for SUB_INSTRUMENT.
*
c Revision 1.6  1996/08/28  03:07:57  timj
c Added BADBIT
c
c Revision 1.5  1996/08/27  03:42:44  timj
c Fix UBYTES for QUALITY
c
c Revision 1.4  1996/08/26  19:36:40  timj
c Quality array was being mapped as REAL instead of UBYTE.
c
c Revision 1.3  1996/08/26  19:31:53  timj
c Remove LTEMP variable (left over from BAD_PIXEL experiment)
c
c Revision 1.2  1996/08/26  19:27:43  timj
c Use SCULIB_COPYX to copy data to mapped arrays.
c Fix bug when writing out T_COLD
c
c Revision 1.1  1996/08/16  15:26:31  timj
c Initial revision
c
*     {enter_further_changes_here}
 
*  Bugs:
*     Aborted datasets have an incorrect Y-axis scale.
*     {note_any_bugs_here}

*-


*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'                 ! SSE global definitions
      INCLUDE 'DAT_PAR'                 ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                 ! for VAL__xxxx
      INCLUDE 'REDS_SYS'                ! REDS constants
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER CHR_LEN
*    Global variables :
*    Local Constants :
      REAL    ARCSEC                    ! 1 arcsec in radians
      PARAMETER (ARCSEC = 4.8481368E-6)
      BYTE    BADBIT                    ! Bad bit mask
      PARAMETER (BADBIT = 1)
      REAL    DEG2RAD                   ! Convert degrees to radians
      PARAMETER (DEG2RAD = 0.017453292)
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      INTEGER N_MODEL                   ! Number of points in output model
      PARAMETER (N_MODEL = 100)
      INTEGER N_TEMP                    ! Number of temp measurements(SKY,COLD,AMB)
      PARAMETER (N_TEMP = 3)
*    Local variables :
      REAL    AIR_MODEL(N_MODEL)        ! Airmass values for MODEL
      REAL    AIRMASS(SCUBA__MAX_INT)   ! Array of AIRMASS data
      REAL    AIRMASS_MAX               ! Max airmass
      REAL    AIRMASS_MIN               ! Min airmass
      REAL    AIRMASS_VAR(SCUBA__MAX_INT)! Array of AIRMASS variance
      REAL    AIRSTEP                   ! AIRMASS increment for DO loop
      REAL    B                         ! requested B
      REAL    B_FIT                     ! B parameter
      CHARACTER*20 BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! bolometer types
      INTEGER DIM (MAXDIM)              ! the dimensions of an array
      INTEGER DIMX (MAXDIM)             ! expected dimensions of an array
      REAL    EL                        ! Elevation in radians used for loop
      REAL    ETA_TEL                   ! Telescope efficiency
      REAL    ETA_TEL_FIT               ! Fitted eta_tel
      INTEGER EXP_END                   ! end index of data for an exposure
      INTEGER EXP_START                 ! start index of data for an exposure
      CHARACTER*15 FILT                 ! Selected filter
      LOGICAL FITFAIL                   ! Status of the Model fit
      CHARACTER*80 FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      CHARACTER*(DAT__SZLOC) FITS_LOC   ! HDS locator to FITS structure
      INTEGER I                         ! DO loop index
      INTEGER IARY1                     ! ARY array identifier
      INTEGER IERR                      ! For VEC_
      CHARACTER*15 INST                 ! Selected instrument
      INTEGER IPOSN                     ! posn in string
      INTEGER ITEMP                     ! scratch integer
      INTEGER IN_BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER IN_BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
                                        ! measured in input file
      INTEGER IN_DEM_PNTR_PTR           ! pointer to input .SCUBA.DEM_PNTR array
      INTEGER IN_DATA_PTR               ! pointer to data array of input file
      CHARACTER*(DAT__SZLOC) IN_LOC     ! locator to item in input file
      INTEGER IN_NDF                    ! NDF identifier of input file
      INTEGER IN_POINTER (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! array connecting output bolometer
                                        ! positions to input data array
      REAL    JSKY (SCUBA__MAX_INT)     ! Average SKY data for used SUB-INS
      REAL    JSKY_VAR (SCUBA__MAX_INT) ! Variance of JSKY
      INTEGER JSKY_QUAL (SCUBA__MAX_INT)! Quality of JSKY
      REAL    J_SKY (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! brightness temperature of sky
      REAL    J_SKY_VARIANCE (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! variance of j_sky
      BYTE    J_SKY_QUALITY (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! quality of j_sky
      REAL    J_SKY_AV (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! average brightness temperature of sky
      REAL    J_SKY_AV_VARIANCE (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! variance of j_sky average
      BYTE J_SKY_AV_QUALITY (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! quality of j_sky average
      REAL    J_THEORETICAL (N_MODEL)   ! Array of model sky data
      INTEGER LBND (MAXDIM)             ! lower bounds of array
      CHARACTER*(DAT__SZLOC) LOC1       ! Dummy locator
      REAL    MAX_EL                    ! Max elevation of skydip
      INTEGER MEASUREMENT               ! measurement index in DO loop
      REAL    MIN_EL                    ! Min elevation of skydip
      INTEGER MOD_NDF                   ! NDF identifier of output file
      INTEGER NDIM                      ! the number of dimensions in an array
      INTEGER NERR                      ! For VEC_
      INTEGER NREC                      ! number of history records in file
      INTEGER N_BOLS                    ! number of bolometers measured
      INTEGER N_BOL_OUT                 ! number of bolometers measured in
                                        ! output file
      INTEGER N_EXPOSURES               ! number of exposures per integration
      INTEGER N_FITS                    ! number of FITS lines read from file
      INTEGER N_INTEGRATIONS            ! number of integrations per measurement
      INTEGER N_MEASUREMENTS            ! number of measurements in the file
      INTEGER N_POS                     ! the total number of positions measured
      INTEGER N_SAMP_IN                 ! Actual number of samples taken
      INTEGER N_SUB                     ! number of sub-instruments used
      INTEGER N_SWITCHES                ! number of switches per exposure
      CHARACTER*15 OBSERVING_MODE       ! type of observation
      INTEGER OUT_AXIS_PTR              ! pointer to axis of observed output file
      INTEGER OUT_AXIS_PTR_M            ! pointer to axis of model output file
      INTEGER OUT_BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers in output
                                        ! file
      INTEGER OUT_BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers in
                                        ! output file
      INTEGER OUT_DATA_PTR              ! pointer to data array of output file
      INTEGER OUT_DATA_PTR_M            ! pointer to data array of output file(MODEL)
      INTEGER OUT_NDF                   ! NDF identifier of output file
      INTEGER OUT_QUAL_PTR               ! pointer to quality array of output file
      INTEGER OUT_VAR_PTR               ! pointer to variance array of output file
      INTEGER RUN_NUMBER                ! run number of observation
      LOGICAL SKYDIP                    ! .TRUE. if not RAW data
      INTEGER SLICE_PTR                 ! Pointer to start of slice
      INTEGER SLICE_PTR_END             ! Pointer to end of slice
      CHARACTER*80 STEMP                ! scratch string
      CHARACTER*40     SUBLIST          ! List of available sub instruments
      CHARACTER*15 SUB_FILTER (SCUBA__MAX_SUB)
                                        ! filters in front of sub-instruments
      CHARACTER*15 SUB_INSTRUMENT (SCUBA__MAX_SUB)
                                        ! sub-instruments used
      INTEGER SUB_POINTER               ! index of SUB_REQUIRED in sub-
                                        ! instruments observed
      CHARACTER*15 SUB_REQUIRED     ! sub-instrument required for reduction
      REAL    SUB_WAVE (SCUBA__MAX_SUB) ! wavelengths of observation
      INTEGER SWITCHES                  ! number of switches implied by SWITCH_MODE
      CHARACTER*15 SWITCH_MODE          ! switch mode used
      REAL    TAUZ_FIT                  ! Fitted TAU
      REAL    T_ASK                     ! Given T_COLD
      REAL    T_AMB                     ! Temperature of ambient load
      REAL    T_COLD (SCUBA__MAX_SUB)   ! Temperature of cold load
      REAL    T_HOT                     ! Hot load
      REAL    T_TEL                     ! Temperature of telescope
      INTEGER UBND (MAXDIM)             ! upper bounds of array
      REAL    WAVE                      ! Selectred wavelength
      CHARACTER*(DAT__SZLOC) XLOC       ! Locator to EXTENSIONS

*.

      IF (STATUS .NE. SAI__OK) RETURN

* Start up the NDF system and read in some data

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'READ', IN_NDF, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', FITS_LOC, STATUS)
      CALL DAT_SIZE (FITS_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_SKYDIP: input file '//
     :        'contains too many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (FITS_LOC, SCUBA__MAX_FITS, FITS, N_FITS, STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN', 
     :  RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :  OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_OUT (' ', 'REDS: run ^RUN was a ^MODE observation ',
     :     STATUS)


*  check that the history of the input file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         SKYDIP = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'SKYDIP') THEN
                  SKYDIP = .TRUE.
               END IF
            END DO
         ENDIF

         IF (SKYDIP) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_SKYDIP: SKYDIP '//
     :           'has already been run on the input data', STATUS)
            END IF
         END IF
      END IF


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

*  get the sub-instrument of interest and check it's OK
 
      IF (N_SUB .EQ. 1) THEN
 
*  If we only have one wavelength we dont need to ask
 
         SUB_POINTER = 1
         SUB_REQUIRED = SUB_INSTRUMENT(SUB_POINTER)
      ELSE
         SUB_POINTER = VAL__BADI
 
*  Put all possible answers in a string
 
         IF (N_SUB .GT. 0) THEN
            SUBLIST = ''
            IPOSN = 0
            CALL CHR_APPND(SUB_INSTRUMENT(1), SUBLIST, IPOSN)
            DO I = 2, N_SUB
               CALL CHR_APPND(',',SUBLIST,IPOSN)
               CALL CHR_APPND(SUB_INSTRUMENT(I), SUBLIST, IPOSN)
            END DO
 
*  Ask for the sub array
 
            CALL PAR_CHOIC('SUB_INSTRUMENT', SUB_INSTRUMENT(1), SUBLIST,
     :           .TRUE., SUB_REQUIRED, STATUS)
            CALL CHR_UCASE (SUB_REQUIRED)
 
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

* Add _DC suffix to data

      ITEMP = CHR_LEN(SUB_REQUIRED)
      CALL CHR_APPND('_DC',SUB_REQUIRED,ITEMP)

*  get the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)


* Find the SCUBA extension

      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', XLOC, STATUS)

* Get the bolometer description arrays

      CALL DAT_FIND (XLOC, 'BOL_TYPE', IN_LOC, STATUS)
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
            CALL ERR_REP (' ', 'REDS_SKYDIP: .SCUBA.BOL_TYPE '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2', STATUS)
         END IF
      END IF



* Find number of ADC

      CALL DAT_FIND (XLOC, 'BOL_ADC', IN_LOC, STATUS)
      CALL DAT_GET1I (IN_LOC, SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :  IN_BOL_ADC, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)
 
      IF (ITEMP .NE. N_BOLS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_SKYDIP: dimension of '//
     :        '.SCUBA.BOL_ADC does not match main data array',
     :        STATUS)
         END IF
      END IF



* Get the bolometer channel numbers

      CALL DAT_FIND (XLOC, 'BOL_CHAN', IN_LOC, STATUS)
      CALL DAT_GET1I (IN_LOC, SCUBA__NUM_CHAN * SCUBA__NUM_ADC,
     :  IN_BOL_CHAN, ITEMP, STATUS)
      CALL DAT_ANNUL (IN_LOC, STATUS)
 
      IF (ITEMP .NE. N_BOLS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_SKYDIP: dimension of '//
     :        '.SCUBA.BOL_CHAN does not match main data array',
     :        STATUS)
         END IF
      END IF
 
*  find how many bolometers the input data has in the required sub-instrument
*  and their ADC/channel numbers and positions in the array
 
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CALC_SUB_BOLS (N_BOLS, IN_BOL_ADC, IN_BOL_CHAN,
     :     SCUBA__NUM_CHAN, SCUBA__NUM_ADC, BOL_TYPE, SUB_REQUIRED,
     :     N_BOL_OUT, OUT_BOL_ADC, OUT_BOL_CHAN, IN_POINTER, STATUS)
      END IF

      IF (N_BOL_OUT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_SKYDIP: none of the '//
     :        'measured bolometers belongs to the requested '//
     :        'sub-instrument', STATUS)
         END IF
      END IF




*  map the data array and check its dimensions 

      CALL NDF_DIM (IN_NDF, MAXDIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 3)        .OR.
     :       (DIM(1) .NE. N_TEMP)      .OR.
     :       (DIM(2) .NE. N_BOLS) .OR.
     :       (DIM(3) .LT. 1))     THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL MSG_SETI ('DIM3', DIM(3))
            CALL ERR_REP (' ', 'REDS_SKYDIP: main data '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 ^DIM3',
     :        STATUS)
         END IF
      END IF

      N_POS = DIM (3)


*  map the DEM_PNTR array and check its dimensions

      CALL ARY_FIND (XLOC, 'DEM_PNTR', IARY1, STATUS)
      CALL ARY_DIM (IARY1, MAXDIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IARY1, '_INTEGER', 'READ', IN_DEM_PNTR_PTR, ITEMP,
     :  STATUS)

      SWITCH_MODE = 'SKYDIP'  ! Skydip has no switch mode
      SWITCHES = 1            ! Skydip does not switch

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 4) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_SKYDIP: .SCUBA.DEM_PNTR '//
     :        'array has bad number of dimensions', STATUS)
         ELSE 
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL ERR_REP (' ', 'REDS_SKYDIP: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'switches - ^DIM1', STATUS)
            END IF
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_SKYDIP: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'exposures - ^DIM2', STATUS) 
            END IF
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'REDS_SKYDIP: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'integrations - ^DIM3', STATUS)
            END IF
            IF (DIM(4) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM4', DIM(4))
               CALL ERR_REP (' ', 'REDS_SKYDIP: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'measurements - ^DIM4', STATUS)
            END IF
         END IF
      END IF

*  check that the number of switches matches the SWITCH_MODE

      IF (STATUS .EQ. SAI__OK) THEN
         IF (DIM(1) .NE. SWITCHES) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SWITCH_MODE', SWITCH_MODE)
            CALL MSG_SETI ('N_S', DIM(1))
            CALL ERR_REP (' ', 'SKYDIP: number '//
     :        'of switches in .SCUBA.DEM_PNTR (^N_S) does not '//
     :        'match SWITCH_MODE (^SWITCH_MODE)', STATUS)
         END IF
      END IF

      N_SWITCHES = DIM (1)
      N_EXPOSURES = DIM (2)
      N_INTEGRATIONS = DIM (3)
      N_MEASUREMENTS = DIM (4)

      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      CALL MSG_OUT (' ', 'REDS: file contains data for '//
     :  '^N_I integration(s) in ^N_M measurement(s)', STATUS)

* Read in the elevation data from the FITS header

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MAX_EL',
     :  MAX_EL, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'MIN_EL',
     :  MIN_EL, STATUS)

* Calculate AIRMASS array (High EL to low EL)

*     Min airmass
      EL = (REAL(PI)/2.0) - MAX_EL * DEG2RAD
      CALL SCULIB_AIRMASS (EL, AIRMASS_MIN, STATUS)

*     Max airmass
      EL = (REAL(PI)/2.0) - MIN_EL * DEG2RAD
      CALL SCULIB_AIRMASS (EL, AIRMASS_MAX, STATUS)

      AIRSTEP = (AIRMASS_MAX - AIRMASS_MIN) / REAL(N_MEASUREMENTS - 1)
      DO I = 1, N_MEASUREMENTS
         AIRMASS(I) = AIRMASS_MIN + AIRSTEP * REAL(I-1) 
*  Calculate the variance Delta A = (TAN / SIN) Delta EL
         EL = ASIN(1.0/AIRMASS(I))
         AIRMASS_VAR(I) = ARCSEC * TAN(EL) / SIN(EL) ! 1 arcsec error in EL
      END DO

* Read temperatures from FITS information

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_AMB',
     :  T_AMB, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_HOT',
     :  T_HOT, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_TEL',
     :  T_TEL, STATUS)


      DO I = 1, N_SUB         
         STEMP = 'T_COLD_'
         ITEMP = 7
         CALL CHR_PUTI(I, STEMP, ITEMP)
         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        STEMP, T_COLD(I), STATUS)
      END DO

* Get all the fitting parameters


      CALL PAR_DEF0R('T_COLD', T_COLD(SUB_POINTER), STATUS)
      CALL PAR_GET0R ('T_COLD', T_ASK, STATUS )

      IF (T_ASK.NE. T_COLD(SUB_POINTER)) THEN
         CALL MSG_SETR ('T_COLD', T_COLD)
         CALL MSG_OUT(' ', 'Redefining T_COLD from ^T_COLD', STATUS)
         T_COLD(SUB_POINTER) = T_ASK
      ENDIF

      CALL PAR_GET0R ('ETA_TEL', ETA_TEL, STATUS )
      CALL PAR_GET0R ('B_FIT', B, STATUS )

* Allocate some memory for slice and SKYDIP TEMPERATURES

      CALL SCULIB_MALLOC (N_TEMP * N_BOLS * N_INTEGRATIONS * VAL__NBR, 
     :     SLICE_PTR, SLICE_PTR_END)

* Now we can read in the data and average it all together

      DO MEASUREMENT = 1, N_MEASUREMENTS

*  find where the exposure starts and finishes in the data array
 
         CALL SCULIB_FIND_SWITCH (%val(IN_DEM_PNTR_PTR),
     :        1, 1, N_INTEGRATIONS, N_MEASUREMENTS,
     :        N_POS, 1, 1, 1, MEASUREMENT,
     :        EXP_START, EXP_END, STATUS)


*  cycle through the measurements in the exposure

         N_SAMP_IN = EXP_END - EXP_START + 1

         CALL SKYDIP_GET_SLICE(%VAL(IN_DATA_PTR), N_TEMP, N_BOLS,
     :        EXP_START, EXP_END, N_SAMP_IN, N_POS, %VAL(SLICE_PTR),
     :        STATUS)


         CALL SCULIB_SKYDIP_TEMPERATURES(T_COLD, T_HOT, 
     :        N_SUB, SUB_INSTRUMENT, SUB_WAVE, SCUBA__NUM_CHAN,
     :        SCUBA__NUM_ADC, BOL_TYPE, N_BOLS, IN_BOL_CHAN, IN_BOL_ADC,
     :        N_SAMP_IN, %VAL(SLICE_PTR),
     :        N_INTEGRATIONS, J_SKY, J_SKY_VARIANCE, 
     :        J_SKY_QUALITY, J_SKY_AV,
     :        J_SKY_AV_VARIANCE, J_SKY_AV_QUALITY, STATUS)

* Extract the info for the bolometer we want

            JSKY(MEASUREMENT) = J_SKY_AV(IN_POINTER(1))
            JSKY_VAR(MEASUREMENT) = J_SKY_AV_VARIANCE(IN_POINTER(1))
            JSKY_QUAL(MEASUREMENT) = J_SKY_AV_QUALITY(IN_POINTER(1))


      END DO

*      PRINT *, '    N  AIRMASS  JSKY   VARIANCE'
*      DO MEASUREMENT = 1, N_MEASUREMENTS
* Print out input data
*         PRINT *,MEASUREMENT,AIRMASS(MEASUREMENT),'+/-', 
*     :        AIRMASS_VAR(MEASUREMENT),
*     :        JSKY(MEASUREMENT), '+/-', JSKY_VAR(MEASUREMENT)
*      END DO

* Send to fit skydip

      WAVE = SUB_WAVE(SUB_POINTER)
      INST = SUB_INSTRUMENT(SUB_POINTER)
      FILT = SUB_FILTER(SUB_POINTER)

      CALL SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, JSKY, JSKY_VAR,
     :     WAVE, INST, FILT, T_TEL, T_AMB, ETA_TEL, B, ETA_TEL_FIT,
     :     B_FIT, TAUZ_FIT, STATUS)

*      CALL FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, JSKY, JSKY_VAR,
*     :     WAVE, INST, FILT, T_TEL, T_AMB, ETA_TEL, B, ETA_TEL_FIT,
*     :     B_FIT, TAUZ_FIT, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         FITFAIL = .FALSE.
      ELSE
         STATUS = SAI__OK
         FITFAIL = .TRUE.
      ENDIF

* DATA has now been fitted. Write DATA and MODEL

*  now open the output NDF, propagating it from the input file
 
      CALL NDF_PROP (IN_NDF, 'NOEXTENSION(SCUCD,SCUBA) ', 'OUT',
     :     OUT_NDF, STATUS)

* Create a REDS extension to store the FIT parameters
      CALL NDF_XNEW (OUT_NDF, 'REDS', 'REDS_EXTENSION', 0, 0, 
     :     LOC1, STATUS)
      CALL DAT_ANNUL (LOC1, STATUS)

* Store fit parameters

      CALL NDF_XPT0C(INST, OUT_NDF, 'REDS', 'SUB_INSTRUMENT', STATUS)
      CALL NDF_XPT0C(FILT, OUT_NDF, 'REDS', 'FILTER', STATUS)
      CALL NDF_XPT0R(WAVE, OUT_NDF, 'REDS', 'WAVELENGTH', STATUS)
      CALL NDF_XPT0R(T_COLD(SUB_POINTER), OUT_NDF, 'REDS', 'T_COLD', 
     :     STATUS)

*  create a history component in the output file

      CALL NDF_HCRE (OUT_NDF, STATUS)

* Title of DATASET
      CALL NDF_CPUT ('Skydip',OUT_NDF,'TITLE',STATUS)

* Data label
      CALL NDF_CPUT ('Jsky (measured)', OUT_NDF, 'LABEL',STATUS)

* Data unit
      CALL NDF_CPUT ('K', OUT_NDF, 'UNITS', STATUS)

* Set-up the DATA array

      NDIM = 1
      LBND (1) = 1
      UBND (1) = N_MEASUREMENTS

* Set new bounds of data array
      CALL NDF_SBND (NDIM, LBND, UBND, OUT_NDF, STATUS)

* Turn off automatic quality masking (just to make sure)
      CALL NDF_SQMF(.FALSE., OUT_NDF, STATUS)

*  Map the data
      CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE',
     : OUT_DATA_PTR, ITEMP, STATUS)

      CALL VEC_RTOR(.FALSE., UBND(1),JSKY, %VAL(OUT_DATA_PTR),
     :     IERR, NERR, STATUS)

* Write VARIANCE

      CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 'WRITE',
     : OUT_VAR_PTR, ITEMP, STATUS)

      CALL VEC_RTOR(.FALSE., UBND(1),JSKY_VAR, %VAL(OUT_VAR_PTR),
     :     IERR, NERR, STATUS)


* Write QUALITY

      CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     : OUT_QUAL_PTR, ITEMP, STATUS)
      CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)

      CALL VEC_BTOB(.FALSE.,UBND(1),JSKY_QUAL, %VAL(OUT_QUAL_PTR),
     :     IERR, NERR, STATUS)

* Create the AXES

      CALL NDF_ACRE(OUT_NDF, STATUS)

* Label everything

      CALL NDF_ACPUT('AIRMASS',OUT_NDF,'LABEL',1,STATUS)

* Write AXES data points

      CALL NDF_AMAP (OUT_NDF, 'Centre', 1, '_REAL', 'WRITE',
     :     OUT_AXIS_PTR, UBND(1), STATUS)

      CALL VEC_RTOR(.FALSE., UBND(1), AIRMASS, %VAL(OUT_AXIS_PTR),
     :     IERR, NERR, STATUS)

* Write the AXIS variance (probably pretty inaccurate)
      CALL NDF_AMAP (OUT_NDF, 'Error', 1, '_REAL', 'WRITE',
     :     OUT_AXIS_PTR, UBND(1), STATUS)

      CALL VEC_RTOR(.FALSE., UBND(1), AIRMASS_VAR, %VAL(OUT_AXIS_PTR),
     :     IERR, NERR, STATUS)

* If FIT worked...

      IF (.NOT.FITFAIL) THEN

**** Now calculate the model and write out to another NDF *****

* Calculate model values
      AIRSTEP = (AIRMASS(N_MEASUREMENTS) - AIRMASS(1)) /
     :     (N_MODEL - 1)

      DO I = 1, N_MODEL
         AIR_MODEL(I) = AIRMASS(1) + AIRSTEP * (I - 1)
         CALL SCULIB_J_THEORETICAL (TAUZ_FIT, AIR_MODEL(I), T_TEL,
     :        T_AMB, WAVE, ETA_TEL_FIT, B_FIT, J_THEORETICAL(I),
     :        STATUS)
      END DO

*  now open the output NDF, propagating it from the input file
 
      CALL NDF_PROP (IN_NDF, 'NOEXTENSION(SCUCD,SCUBA) ', 'MODEL_OUT',
     :     MOD_NDF, STATUS)

* Create a REDS extension to store the FIT parameters
      CALL NDF_XNEW (MOD_NDF, 'REDS', 'REDS_EXTENSION', 0, 0, 
     :     LOC1, STATUS)
      CALL DAT_ANNUL (LOC1, STATUS)

* Store fit parameters
      CALL NDF_XPT0C(INST, MOD_NDF, 'REDS', 'SUB_INSTRUMENT', STATUS)
      CALL NDF_XPT0C(FILT, MOD_NDF, 'REDS', 'FILTER', STATUS)
      CALL NDF_XPT0R(WAVE, MOD_NDF, 'REDS', 'WAVELENGTH', STATUS)
      CALL NDF_XPT0R(T_COLD(SUB_POINTER), MOD_NDF, 'REDS', 'T_COLD',
     :     STATUS)
      CALL NDF_XPT0R(ETA_TEL_FIT, MOD_NDF, 'REDS', 'ETA_TEL', STATUS)
      CALL NDF_XPT0R(B_FIT, MOD_NDF, 'REDS', 'B_FIT', STATUS)
      CALL NDF_XPT0R(TAUZ_FIT, MOD_NDF, 'REDS', 'TAU_Z', STATUS)

*  create a history component in the output file

      CALL NDF_HCRE (MOD_NDF, STATUS)

* Title of DATASET
      CALL NDF_CPUT ('Skydip',MOD_NDF,'TITLE',STATUS)
* Data label
      CALL NDF_CPUT ('Jsky (Theoretical)', MOD_NDF, 'LABEL',STATUS)
* Data unit
      CALL NDF_CPUT ('K', MOD_NDF, 'UNITS', STATUS)


* Set-up the DATA array

      NDIM = 1
      LBND (1) = 1
      UBND (1) = N_MODEL

* Set new bounds of data array
      CALL NDF_SBND (NDIM, LBND, UBND, MOD_NDF, STATUS)

*  Map the data
      CALL NDF_MAP (MOD_NDF, 'DATA', '_REAL', 'WRITE',
     : OUT_DATA_PTR_M, ITEMP, STATUS)

      CALL VEC_RTOR(.FALSE.,UBND(1),J_THEORETICAL, %VAL(OUT_DATA_PTR_M),
     :     IERR, NERR, STATUS)

* Set the BAD pixel flag
      CALL NDF_SBAD(.FALSE., MOD_NDF, 'Data', STATUS)

* Create the AXES
      CALL NDF_ACRE(MOD_NDF, STATUS)
* Label everything
      CALL NDF_ACPUT('AIRMASS',MOD_NDF,'LABEL',1,STATUS)
* Write AXES data points
      CALL NDF_AMAP (MOD_NDF, 'Centre', 1, '_REAL', 'WRITE',
     :     OUT_AXIS_PTR_M, UBND(1), STATUS)

      CALL VEC_RTOR(.FALSE., UBND(1), AIR_MODEL, %VAL(OUT_AXIS_PTR_M),
     :     IERR, NERR, STATUS)

      ENDIF

* Finish up

      CALL SCULIB_FREE ('SLICE' , SLICE_PTR, SLICE_PTR_END, STATUS )

      CALL NDF_ANNUL (IN_NDF, STATUS)
      CALL NDF_ANNUL (OUT_NDF, STATUS)

      IF (.NOT.FITFAIL)  CALL NDF_ANNUL (MOD_NDF, STATUS)

      CALL NDF_END (STATUS)

      END


* Subroutine to get part of the input data array

      SUBROUTINE SKYDIP_GET_SLICE(RAW_DATA, N_TEMP, N_BOLS, INT_START, 
     :     INT_END, N_INTEGRATIONS, N_POS, SLICE, STATUS)
*    Description :
*     This routine extracts a slice of data from the mapped DATA_ARRAY. 
*     It is used to retrieve an entire measurement for a set of integrations.
*    Invocation :
*     CALL SKYDIP_GET_SLICE(RAW_DATA, N_TEMP, N_BOLS, INT_START, 
*    :     INT_END, N_INTEGRATIONS, N_POS, SLICE, STATUS)
*    Parameters :
*     RAW_DATA               = REAL (Given)
*               mapped skydip data array
*     N_TEMP                 = INTEGER (Given)
*               number of temperature components
*     N_BOLS                 = INTEGER (Given)
*               number of bolometers
*     INT_START              = INTEGER (Given)
*               start of slice
*     INT_END                = INTEGER (Given)
*               end of slice
*     N_INTEGRATIONS         = INTEGER (Given)
*               size of slice
*     N_POS                  = INTEGER (Given)
*               size of DATA_ARRAY (N_INTEGRATION * N_MEASUREMENTS)
*     SLICE                  = REAL (Array) (Returned)
*               (N_TEMP, N_BOLS, N_INTEGRATIONS) slice of DATA_ARRAY
*     STATUS                 = INTEGER (Given & retrurned)
*    Authors :
*     T. Jenness (timj@jach.hawaii.edu)
*    Type Definitions :
      IMPLICIT NONE
*    Gloval Constants :
      INCLUDE 'SAE_PAR'
*    Import :
      INTEGER INT_END                   ! end index of data for an integration
      INTEGER INT_START                 ! start index of data for an integration
      INTEGER N_BOLS                    ! number of bolometers measured
      INTEGER N_INTEGRATIONS            ! Number of integrations
      INTEGER N_POS                     ! Total number of data points taken
      INTEGER N_TEMP                    ! Number of temperatures
      REAL RAW_DATA(N_TEMP, N_BOLS, N_POS)
*                                       ! Raw data
*    Import-Export :
*    Export :
      REAL SLICE(N_TEMP, N_BOLS, N_INTEGRATIONS) ! Slice
*    Status :
      INTEGER STATUS
*    Local Variables :
      INTEGER BOL                       ! Loop counter for bolometer
      INTEGER COUNT                     ! Dummy counter
      INTEGER INTEGRATION               ! Loop counter for integrations
      INTEGER LOAD                      ! Loop counter for sky temp
*-

      IF (STATUS .NE. SAI__OK) RETURN

      DO LOAD = 1, N_TEMP
         DO BOL = 1, N_BOLS
            DO INTEGRATION = INT_START, INT_END
               COUNT = INTEGRATION - INT_START + 1
               SLICE(LOAD, BOL,COUNT) = RAW_DATA(LOAD, BOL, INTEGRATION)
            END DO
         END DO
      END DO

      END


