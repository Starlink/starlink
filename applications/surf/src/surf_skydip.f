      SUBROUTINE SURF_SKYDIP (STATUS)
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
*     CALL SURF_SKYDIP( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This application takes raw SKYDIP data and calculates tau, eta_tel
*     and B by fitting. Sky brightness temperatures are calculated for 
*     different airmasses and then fitted with a model of the sky.

*  Usage:
*     skydip in sub_instrument t_cold eta_tel b_fit out model_out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the raw skydip data file
*     B_FIT = REAL (Read)
*        The B parameter (filter transmission). This efficiency factor
*        must be between 0 and 1. A negative value allows this parameter
*        to be free.
*     ETA_TEL = REAL (Read)
*        The telescope efficiency. Must be between 0 and 1.0. 
*        A negative value allows this parameter to be free.
*     MODEL_OUT = CHAR (Write)
*        The name of the output file that contains the fitted sky
*        temperatures.
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM.
*     OUT = CHAR (Write)
*        The name of the output file that contains the measured 
*        sky temperatures.
*     SUB_INSTRUMENT = CHAR (Read)
*        The name of the sub-instrument whose data are to be
*        selected from the input file and fitted. Permitted 
*        values are SHORT, LONG, P1100, P1350 and P2000
*     T_COLD = REAL (Read)
*        Temperature of the cold load. The default value is
*        taken from the input file.

*  Examples:
*     skydip jun10_dem_0002 short \
*        Process the short sub-instrument using the default value
*        for T_COLD and allowing ETA_TEL and B to be free parameters.
*        No output files are written.
*     skydip 19970610_dem_0003 long eta_tel=0.9 out=sky model_out=model
*        Process the long wave sub-instrument with ETA_TEL fixed at 0.9
*        and B free. Write the sky temperature to sky.sdf and the fitted
*        model to model.sdf.

*  Notes:
*     If the input file is not found in the current directory, the directory
*     specified by the DATADIR environment variable is searched. This means
*     that the raw data does not have to be in the working directory.
*     

*  Related Applications:
*     SURF: EXTINCTION, SDIP

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
*     to a sky temperature with CULIB_SKYDIP_TEMPERATURES.
*
*     The data is then fitted with SCULIB_FIT_SKYDIP. Both the measured and 
*     model data are then written to NDFs for plotting in KAPPA-LINPLOT. (The
*     model data is calculated with SCULIB_J_THEORETICAL.

*  Authors :
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History :
*     $Id$
*     $Log$
*     Revision 1.17  1997/06/27 23:23:22  timj
*     Slight changes to header.
*     Make sure I am not setting status when status is already bad.
*
*     Revision 1.16  1997/06/20 21:46:10  timj
*     Add printout of the LST of the observation since this is useful for
*     EXTINCTION.
*
*     Revision 1.15  1997/06/13 00:19:45  timj
*     Use MSG_OUTIF
*     Change name to SURF
*     Doc updates.
*
*     Revision 1.14  1997/06/05 23:14:28  timj
*     Initialise pointer.
*
*     Revision 1.13  1997/05/27 22:19:55  timj
*     Use %LOC instead of LOC - for Alpha compiler
*
*     Revision 1.12  1997/05/22 03:27:03  timj
*     Fix problem with setting status to bad early on.
*     Now allows remote access (DATADIR) of input files.
*
*     Revision 1.11  1997/05/22 01:16:07  timj
*     Check that this is SKYDIP data.
*
*     Revision 1.10  1997/04/15 18:30:30  timj
*     Remove debugging print statement.
*
*     Revision 1.9  1997/04/03 20:13:31  timj
*     Add SCULIB_GET_SUB_INST and SCULIB_GET_DEM_PNTR.
*     Write both output files inside a loop to reduce amount of repeating code.
*     Add PKG and TASK names for MSG_OUT.
*
*     Revision 1.8  1997/03/07 02:33:30  timj
*     Add support for PAR__NULL.
*     Add support for START_EL and END_EL.
*     Minor tweak in call name for non-NAG.
*
c Revision 1.7  1996/12/12  21:05:28  timj
c Fix Starlink header.
c Replace COPYR with VEC
c Use PAR_CHOIC for SUB_INSTRUMENT.
c
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
      INCLUDE 'MSG_PAR'                 ! MSG__ constants
      INCLUDE 'PAR_ERR'                 ! for PAR__ constants
      INCLUDE 'PRM_PAR'                 ! for VAL__ constants
      INCLUDE 'SURF_PAR'                ! SURF  constants
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
      CHARACTER * 8 TSKNAME             ! Name of task
      PARAMETER (TSKNAME = 'SKYDIP')

*    Local variables :
      REAL    AIR_MODEL(N_MODEL)        ! Airmass values for MODEL
      REAL    AIRMASS(SCUBA__MAX_INT)   ! Array of AIRMASS data
      REAL    AIRMASS_START             ! Start airmass
      REAL    AIRMASS_END               ! End airmass
      REAL    AIRMASS_VAR(SCUBA__MAX_INT)! Array of AIRMASS variance
      REAL    AIRSTEP                   ! AIRMASS increment for DO loop
      INTEGER AREF                      ! Pointer to axis data
      REAL    B                         ! requested B
      LOGICAL BADPIX                    ! are there bad pixels
      REAL    B_FIT                     ! B parameter
      CHARACTER*20 BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! bolometer types
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU4 Nasmyth coord of bolometers
      INTEGER DIM (MAXDIM)              ! the dimensions of an array
      INTEGER DREF                      ! Pointer to output data
      REAL    EL                        ! Elevation in radians used for loop
      REAL    END_EL                    ! End elevation of skydip
      REAL    ETA_TEL                   ! Telescope efficiency
      REAL    ETA_TEL_FIT               ! Fitted eta_tel
      INTEGER EXP_END                   ! end index of data for an exposure
      INTEGER EXP_START                 ! start index of data for an exposure
      INTEGER FILE                      ! File counter in DO loop
      CHARACTER*15 FILT                 ! Selected filter
      LOGICAL FITFAIL                   ! Status of the Model fit
      CHARACTER*80 FITS (SCUBA__MAX_FITS)
                                        ! array of FITS keyword lines
      CHARACTER*(DAT__SZLOC) FITS_LOC   ! HDS locator to FITS structure
      INTEGER I                         ! DO loop index
      INTEGER IERR                      ! For VEC_
      INTEGER ITEMP                     ! scratch integer
      INTEGER IN_BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER IN_BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
                                        ! measured in input file
      INTEGER IN_DEM_PNTR_PTR           ! pointer to input .SCUBA.DEM_PNTR array
      INTEGER IN_DATA_PTR               ! pointer to data array of input file
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
      CHARACTER * 15 LABEL              ! File label
      INTEGER LBND (MAXDIM)             ! lower bounds of array
      CHARACTER*(DAT__SZLOC) LOC1       ! Dummy locator
      INTEGER MEASUREMENT               ! measurement index in DO loop
      INTEGER NDIM                      ! the number of dimensions in an array
      INTEGER NERR                      ! For VEC_
      INTEGER NFILES                    ! Number of output files
      INTEGER NPTS                      ! Number of output data points
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
      INTEGER OUT_BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers in output
                                        ! file
      INTEGER OUT_BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers in
                                        ! output file
      INTEGER OUT_DATA_PTR              ! pointer to data array of output file
      CHARACTER *80 OUT_NAME            ! Name of output NDF
      INTEGER OUT_NDF                   ! NDF identifier of output file
      INTEGER OUT_QUAL_PTR               ! pointer to quality array of output file
      INTEGER OUT_VAR_PTR               ! pointer to variance array of output file
      CHARACTER * 15 PARAM              ! Name of output file parameter
      INTEGER PLACE                     ! A placeholder
      INTEGER RUN_NUMBER                ! run number of observation
      LOGICAL SKYDIP                    ! .TRUE. if not RAW data
      INTEGER SLICE_PTR                 ! Pointer to start of slice
      INTEGER SLICE_PTR_END             ! Pointer to end of slice
      CHARACTER*80 STEMP                ! scratch string
      REAL    START_EL                  ! Start elevation of skydip
      CHARACTER * 15 SUB_INST           ! Name of selected SUB instrument
      INTEGER SUB_POINTER               ! index of SUB_REQUIRED in sub-
                                        ! instruments observed
      REAL    SWAVE(SCUBA__MAX_SUB)     ! Dummy SUB_WAVE array
      CHARACTER*15 SSUB(SCUBA__MAX_SUB) ! Dummy SUB_INST array
      REAL    STCOLD(SCUBA__MAX_SUB)    ! Dummy T_COLD array
      INTEGER SWITCHES                  ! number of switches implied by SWITCH_MODE
      CHARACTER*15 SWITCH_MODE          ! switch mode used
      REAL    TAUZ_FIT                  ! Fitted TAU
      REAL    T_ASK                     ! Given T_COLD
      REAL    T_AMB                     ! Temperature of ambient load
      REAL    T_COLD                    ! Temperature of cold load
      REAL    T_HOT                     ! Hot load
      REAL    T_TEL                     ! Temperature of telescope
      INTEGER UBND (MAXDIM)             ! upper bounds of array
      REAL    WAVE                      ! Selectred wavelength
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC ! Locator to EXTENSIONS

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)

* Start up the NDF system and read in some data

      CALL NDF_BEGIN

      CALL SCULIB_SEARCH_DATADIR('IN', IN_NDF, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', FITS_LOC, STATUS)
      CALL DAT_SIZE (FITS_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file '//
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
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: run ^RUN was a ^MODE observation ',
     :     STATUS)

*     Check that this is a SKYDIP observation

      IF (OBSERVING_MODE .NE. 'SKYDIP') THEN
         CALL MSG_SETC('TASK', TSKNAME)

         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP(' ', '^TASK: This is not a SKYDIP observation',
     :           STATUS)
         END IF
      END IF


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
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: SKYDIP '//
     :           'has already been run on the input data', STATUS)
            END IF
         END IF
      END IF

*     Get Sidereal time start and end.
*     (Should probably merge this with the identical code 
*     found in EXTINCTION)
 
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
     :     N_SUB, SUB_POINTER, WAVE, SUB_INST, FILT, STATUS)

* Add _DC suffix to data

      IF (STATUS .EQ. SAI__OK) THEN
         ITEMP = CHR_LEN(SUB_INST)
         CALL CHR_APPND('_DC', SUB_INST, ITEMP)
      END IF

*  get the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)


* Find the SCUBA extension

      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

* Get the bolometer description arrays (NB BOL_DU3 and DU4 are not used)

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOLS, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, IN_BOL_ADC, IN_BOL_CHAN, STATUS)
 
*  find how many bolometers the input data has in the required sub-instrument
*  and their ADC/channel numbers and positions in the array
 
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_CALC_SUB_BOLS (N_BOLS, IN_BOL_ADC, IN_BOL_CHAN,
     :     SCUBA__NUM_CHAN, SCUBA__NUM_ADC, BOL_TYPE, SUB_INST,
     :     N_BOL_OUT, OUT_BOL_ADC, OUT_BOL_CHAN, IN_POINTER, STATUS)
      END IF

      IF (N_BOL_OUT .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', '^TASK: none of the '//
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
            CALL MSG_SETC ('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: main data '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 ^DIM3',
     :        STATUS)
         END IF
      END IF

      N_POS = DIM (3)

*  map the DEM_PNTR array and check its dimensions
 
      CALL SCULIB_GET_DEM_PNTR(4, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS, 
     :     N_MEASUREMENTS, STATUS)

      IF (N_MEASUREMENTS .LE. 1) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ','^TASK: Not enough measurements for fit',
     :           STATUS)
         END IF
      END IF


*  check that the number of switches matches the SWITCH_MODE

      SWITCH_MODE = 'SKYDIP'  ! Skydip has no switch mode
      SWITCHES = 1            ! Skydip does not switch

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_SWITCHES .NE. SWITCHES) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SWITCH_MODE', SWITCH_MODE)
            CALL MSG_SETI ('N_S', N_SWITCHES)
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: number '//
     :        'of switches in .SCUBA.DEM_PNTR (^N_S) does not '//
     :        'match SWITCH_MODE (^SWITCH_MODE)', STATUS)
         END IF
      END IF

      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)
      CALL MSG_SETC ('PKG', PACKAGE)
      
      CALL MSG_OUTIF (MSG__NORM, ' ', '^PKG: file contains data for '//
     :  '^N_I integration(s) in ^N_M measurement(s)', STATUS)

* Read in the elevation data from the FITS header
* Read from START and END EL first

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'START_EL',START_EL, STATUS)
         CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'END_EL', END_EL, STATUS)

* Doesnt have START_EL so look for MAX_EL
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :           'MAX_EL',START_EL, STATUS)
            CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :           'MIN_EL', END_EL, STATUS)
         ENDIF
      ENDIF

* Calculate AIRMASS array (Assume constant space between airmasses)

      IF (STATUS .EQ. SAI__OK) THEN
*     Start airmass
         EL = (REAL(PI)/2.0) - START_EL * DEG2RAD
         CALL SCULIB_AIRMASS (EL, AIRMASS_START, STATUS)

*     End airmass
         EL = (REAL(PI)/2.0) - END_EL * DEG2RAD
         CALL SCULIB_AIRMASS (EL, AIRMASS_END, STATUS)

         AIRSTEP = (AIRMASS_END - AIRMASS_START) / 
     :        REAL(N_MEASUREMENTS - 1)
         DO I = 1, N_MEASUREMENTS
            AIRMASS(I) = AIRMASS_START + AIRSTEP * REAL(I-1)
 
*  Calculate the error Delta A = (TAN / SIN) Delta EL
*     TAN = 1/SQRT((AIR-1)(AIR+1))
            AIRMASS_VAR(I) = 2.0 * ARCSEC * AIRMASS(I) / ! 2 arcsec error
     :           SQRT((AIRMASS(I)-1) * (AIRMASS(I)+1))
         END DO
      END IF

* Read temperatures from FITS information

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_AMB',
     :  T_AMB, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_HOT',
     :  T_HOT, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_TEL',
     :  T_TEL, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         STEMP = 'T_COLD_'
         ITEMP = 7
         CALL CHR_PUTI(SUB_POINTER, STEMP, ITEMP)
      END IF

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, STEMP,
     :  T_COLD, STATUS)

* Get all the fitting parameters

      CALL PAR_DEF0R('T_COLD', T_COLD, STATUS)
      CALL PAR_GET0R ('T_COLD', T_ASK, STATUS )

      IF (T_ASK.NE. T_COLD) THEN
         CALL MSG_SETR ('T_COLD', T_COLD)
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM,' ', 
     :        '^PKG: Redefining T_COLD from ^T_COLD', 
     :        STATUS)
         T_COLD = T_ASK
      ENDIF

      CALL PAR_GET0R ('ETA_TEL', ETA_TEL, STATUS )
      CALL PAR_GET0R ('B_FIT', B, STATUS )

* Allocate some memory for slice and SKYDIP TEMPERATURES
      SLICE_PTR = 0
      SLICE_PTR_END = 0

      CALL SCULIB_MALLOC (N_TEMP * N_BOLS * N_INTEGRATIONS * VAL__NBR, 
     :     SLICE_PTR, SLICE_PTR_END, STATUS)

* This is a kludge for sculib_skydip_temperatures
      DO I = 1, N_SUB
         IF (I .EQ. SUB_POINTER) THEN
            SWAVE(I) = WAVE
            SSUB(I)  = SUB_INST
            STCOLD(I) = T_COLD
         ELSE
            SWAVE(I) = 1.0
            SSUB(I) = 'NONE'
            STCOLD(I) = 1.0
         END IF
      END DO


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

         CALL SCULIB_SKYDIP_TEMPERATURES(STCOLD, T_HOT, 
     :        N_SUB, SSUB, SWAVE, SCUBA__NUM_CHAN,
     :        SCUBA__NUM_ADC, BOL_TYPE, N_BOLS, 
     :        IN_BOL_CHAN, IN_BOL_ADC, N_SAMP_IN, %VAL(SLICE_PTR),
     :        N_INTEGRATIONS, J_SKY, J_SKY_VARIANCE, 
     :        J_SKY_QUALITY, J_SKY_AV,
     :        J_SKY_AV_VARIANCE, J_SKY_AV_QUALITY, STATUS)

* Extract the info for the bolometer we want

         JSKY(MEASUREMENT) = J_SKY_AV(IN_POINTER(1))
         JSKY_VAR(MEASUREMENT) = J_SKY_AV_VARIANCE(IN_POINTER(1))
         JSKY_QUAL(MEASUREMENT) = J_SKY_AV_QUALITY(IN_POINTER(1))


      END DO

*     Have now finished with input data

      CALL SCULIB_FREE ('SLICE' , SLICE_PTR, SLICE_PTR_END, STATUS)
      CALL CMP_UNMAP(IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL DAT_ANNUL(IN_SCUBAX_LOC, STATUS)
      CALL NDF_ANNUL (IN_NDF, STATUS)

* Send to fit skydip

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_FIT_SKYDIP (N_MEASUREMENTS, AIRMASS, JSKY,JSKY_VAR,
     :        WAVE, SUB_INST, FILT, T_TEL, T_AMB, ETA_TEL,B,ETA_TEL_FIT,
     :        B_FIT, TAUZ_FIT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            FITFAIL = .FALSE.
            NFILES = 2
         ELSE
            STATUS = SAI__OK
            FITFAIL = .TRUE.
            NFILES =1
         ENDIF
      END IF

*     Now create output files
*     Only create model if we fitted okay
      DO FILE = 1, NFILES

         IF (STATUS .EQ. SAI__OK) THEN

*     Setup bits that are DIFFERENT between model and data

         IF (FILE .EQ. 1) THEN
*     Data parameters
            PARAM = 'OUT'
            NPTS = N_MEASUREMENTS
            LABEL = 'Jsky'
            BADPIX = .TRUE.
            DREF = %LOC(JSKY)
            AREF = %LOC(AIRMASS)

         ELSE
*     Model parameters and create
            PARAM = 'MODEL_OUT'
            NPTS = N_MODEL
            LABEL = 'Jsky (Model)'
            BADPIX = .FALSE.
            DREF = %LOC(J_THEORETICAL)   ! Reference to data
            AREF = %LOC(AIR_MODEL)       ! Reference to axis

            AIRSTEP = (AIRMASS(N_MEASUREMENTS) - AIRMASS(1)) /
     :           (N_MODEL - 1)

            DO I = 1, N_MODEL
               AIR_MODEL(I) = AIRMASS(1) + AIRSTEP * (I - 1)
               CALL SCULIB_J_THEORETICAL (TAUZ_FIT, AIR_MODEL(I), T_TEL,
     :              T_AMB, WAVE, ETA_TEL_FIT, B_FIT, J_THEORETICAL(I),
     :              STATUS)
            END DO

         END IF

*     Find out the name of the output file
         CALL PAR_GET0C(PARAM, OUT_NAME, STATUS)
 
*     Only write out if required
         IF (STATUS .EQ. PAR__NULL) THEN
            CALL ERR_ANNUL(STATUS)
         ELSE

*  now open the output NDF
            NDIM = 1
            LBND (1) = 1
            UBND (1) = NPTS
         
            CALL NDF_PLACE(DAT__ROOT, OUT_NAME, PLACE, STATUS)
            CALL NDF_NEW('_REAL',NDIM, LBND, UBND, PLACE, OUT_NDF, 
     :           STATUS)

* probably should store the FITS header

            CALL NDF_XNEW(OUT_NDF, 'FITS','_CHAR*80', 1, N_FITS, LOC1,
     :           STATUS)
            CALL DAT_PUT1C(LOC1, N_FITS, FITS, STATUS)
            CALL DAT_ANNUL(LOC1, STATUS)

* Create a REDS extension to store the FIT parameters
            CALL NDF_XNEW (OUT_NDF, 'REDS', 'SURF_EXTENSION', 0, 0, 
     :           LOC1, STATUS)
            CALL DAT_ANNUL (LOC1, STATUS)

* Store fit parameters

            CALL NDF_XPT0C(SUB_INST, OUT_NDF, 'REDS', 'SUB_INSTRUMENT', 
     :           STATUS)
            CALL NDF_XPT0C(FILT, OUT_NDF, 'REDS', 'FILTER', STATUS)
            CALL NDF_XPT0R(WAVE, OUT_NDF, 'REDS', 'WAVELENGTH', STATUS)
            CALL NDF_XPT0R(T_COLD, OUT_NDF,'REDS','T_COLD', 
     :           STATUS)

            IF (FILE .EQ. 2) THEN
               CALL NDF_XPT0R(ETA_TEL_FIT, OUT_NDF, 'REDS', 'ETA_TEL',
     :              STATUS)
               CALL NDF_XPT0R(B_FIT, OUT_NDF, 'REDS', 'B_FIT', STATUS)
               CALL NDF_XPT0R(TAUZ_FIT, OUT_NDF, 'REDS', 'TAU_Z',STATUS)
            END IF

*  create a history component in the output file
            CALL NDF_HCRE (OUT_NDF, STATUS)

*     Title, label and units
            CALL NDF_CPUT ('Skydip', OUT_NDF,'TITLE',STATUS)
            CALL NDF_CPUT (LABEL, OUT_NDF,'LABEL',STATUS)
            CALL NDF_CPUT ('K', OUT_NDF, 'UNITS', STATUS)

* Create and label the AXES
            CALL NDF_ACRE(OUT_NDF, STATUS)
            CALL NDF_ACPUT('AIRMASS',OUT_NDF,'LABEL',1,STATUS)

*     Map the data
            CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE',
     :           OUT_DATA_PTR, ITEMP, STATUS)

            CALL VEC_RTOR(.FALSE.,UBND(1), %VAL(DREF),
     :           %VAL(OUT_DATA_PTR), IERR, NERR, STATUS)

*     Only data has VARIANCE and QUALITY
            IF (FILE .EQ. 1) THEN
               CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 'WRITE',
     :              OUT_VAR_PTR, ITEMP, STATUS)
         
               CALL VEC_RTOR(.FALSE., UBND(1), JSKY_VAR, 
     :              %VAL(OUT_VAR_PTR), IERR, NERR, STATUS)

               CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     :              OUT_QUAL_PTR, ITEMP, STATUS)
               CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)
         
               CALL VEC_UBTOUB(.FALSE., UBND(1), JSKY_QUAL,
     :              %VAL(OUT_QUAL_PTR), IERR, NERR, STATUS)
            END IF

*     Map the Axes
            CALL NDF_AMAP (OUT_NDF, 'Centre', 1, '_REAL', 'WRITE',
     :           OUT_AXIS_PTR, UBND(1), STATUS)

            CALL VEC_RTOR(.FALSE., UBND(1), %VAL(AREF),
     :           %VAL(OUT_AXIS_PTR), IERR, NERR, STATUS)

* if data then write the AXIS variance (probably pretty inaccurate)
            IF (FILE .EQ. 1) THEN
               
               CALL NDF_AMAP (OUT_NDF, 'Error', 1, '_REAL', 'WRITE',
     :              OUT_AXIS_PTR, UBND(1), STATUS)

               CALL VEC_RTOR(.FALSE., UBND(1), AIRMASS_VAR,
     :              %VAL(OUT_AXIS_PTR), IERR, NERR, STATUS)
            END IF
         
*     Set the bad pixel flag
            CALL NDF_SBAD(BADPIX, OUT_NDF, 'Data', STATUS)

*     Tidy up
            CALL NDF_ANNUL(OUT_NDF, STATUS)

         ENDIF

         END IF

      END DO

* Shut down the NDF system

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


