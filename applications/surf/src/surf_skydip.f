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
*     This application accepts raw data or data processed by REDUCE_SWITCH.

*  Usage:
*     skydip in sub_instrument t_cold eta_tel b_fit out model_out

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the raw skydip data file or of the file processed
*        by REDUCE_SWITCH.
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
*        taken from the input file. This parameter is ignored if the
*        REDUCE_SWITCH'ed data is supplied.

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
*     that the raw data does not have to be in the working directory. In 
*     addition 'IN' accepts a number. This number is converted to a demodulated
*     data filename by prepending it with information specified in 
*     the SCUBA_PREFIX environment variable. This filename expansion only works
*     for demodulated data (ie data containing '_dem_'). The '_dem_' is 
*     assumed and should not be present in $SCUBA_PREFIX.
*     

*  Related Applications:
*     SURF: EXTINCTION, SDIP, REDUCE_SWITCH

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

*     If we have been given the raw data we calculate the sky temperatures
*     for each measurement with SCULIB_CALC_SKYDIP_TEMPS. If we are processing
*     the output from reduce_switch the skydip temperatures have already
*     been calculated and we just have to average the data from each 
*     integration into a measurement.
*
*     The data is then fitted with SCULIB_FIT_SKYDIP. Both the measured and 
*     model data are then written to NDFs for plotting in KAPPA-LINPLOT. (The
*     model data is calculated with SCULIB_J_THEORETICAL.

*  Authors :
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  History :
*     $Id$
*     $Log$
*     Revision 1.20  1997/07/19 02:41:47  timj
*     Add header information to describe SCUBA_PREFIX.
*
*     Revision 1.19  1997/07/19 00:26:49  timj
*     Change SEARCH_DATADIR so that it passes PACKAGE.
*
*     Revision 1.18  1997/07/19 00:23:22  timj
*     Now can read REDUCE_SWITCH output.
*     Uses SCULIB_CALC_SKYDIP_TEMPS
*
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
      CHARACTER * 8 TSKNAME             ! Name of task
      PARAMETER (TSKNAME = 'SKYDIP')

*    Local variables :
      REAL    AIR_MODEL(N_MODEL)        ! Airmass values for MODEL
      REAL    AIRMASS(SCUBA__MAX_MEAS)  ! Array of AIRMASS data
      REAL    AIRMASST(SCUBA__MAX_MEAS)  ! First Array of AIRMASS data
      REAL    AIRMASS_START             ! Start airmass
      REAL    AIRMASS_END               ! End airmass
      REAL    AIRMASS_TVAR(SCUBA__MAX_MEAS)! First Array of AIRMASS variance
      REAL    AIRMASS_VAR(SCUBA__MAX_MEAS)! Array of AIRMASS variance
      REAL    AIRSTEP                   ! AIRMASS increment for DO loop
      INTEGER AREF                      ! Pointer to axis data
      REAL         AV_DATA(SCUBA__MAX_SUB, SCUBA__MAX_MEAS) 
                                        ! Average skydip
      BYTE         AV_QUAL(SCUBA__MAX_SUB, SCUBA__MAX_MEAS) 
                                        ! Quality of AV_QUAL
      REAL         AV_VAR(SCUBA__MAX_SUB, SCUBA__MAX_MEAS)  
                                        ! Var for average skydip
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
      INTEGER DUM_DATA_PTR_END          ! Scratch space end
      INTEGER DUM_DATA_PTR              ! Scratch space 
      INTEGER DUM_VAR_PTR_END           ! Scratch space 2end
      INTEGER DUM_VAR_PTR               ! Scratch space 2
      INTEGER DUM_QUAL_PTR_END          ! Scratch space 3 end
      INTEGER DUM_QUAL_PTR              ! Scratch space 3
      INTEGER DUMMY_PTR_END             ! Scratch space 4 end
      INTEGER DUMMY_PTR                 ! Scratch space 4
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
      INTEGER BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
                                        ! measured in input file
      BYTE    IN_BADBIT                 ! input Bad bit mask
      INTEGER IN_DEM_PNTR_PTR           ! pointer to input .SCUBA.DEM_PNTR array
      INTEGER IN_DATA_PTR               ! pointer to data array of input file
      INTEGER IN_NDF                    ! NDF identifier of input file
      INTEGER IN_QUAL_PTR            ! pointer to qual array of input file
      REAL    JSKY (SCUBA__MAX_MEAS)     ! Average SKY data for used SUB-INS
      REAL    JSKY_VAR (SCUBA__MAX_MEAS) ! Variance of JSKY
      REAL    J_THEORETICAL (N_MODEL)   ! Array of model sky data
      CHARACTER * 15 LABEL              ! File label
      INTEGER LBND (MAXDIM)             ! lower bounds of array
      CHARACTER*(DAT__SZLOC) LOC1       ! Dummy locator
      DOUBLE PRECISION MEAN             ! Mean of measurement
      DOUBLE PRECISION MEDIAN           ! Median of measurement
      INTEGER NDIM                      ! the number of dimensions in an array
      INTEGER NERR                      ! For VEC_
      INTEGER NFILES                    ! Number of output files
      INTEGER NGOOD                     ! Number of good points from stats
      INTEGER NKEPT                     ! Number of good measurements
      INTEGER NPTS                      ! Number of output data points
      INTEGER NREC                      ! number of history records in file
      INTEGER N_BOLS                    ! number of bolometers measured
      INTEGER N_EXPOSURES               ! number of exposures per integration
      INTEGER N_FITS                    ! number of FITS lines read from file
      INTEGER N_INTEGRATIONS            ! number of integrations per measurement
      INTEGER N_MEASUREMENTS            ! number of measurements in the file
      INTEGER N_POS                     ! the total number of positions measured
      INTEGER N_SUB                     ! number of sub-instruments used
      INTEGER N_SWITCHES                ! number of switches per exposure
      CHARACTER*15 OBSERVING_MODE       ! type of observation
      INTEGER OUT_AXIS_PTR              ! pointer to axis of observed output file
      INTEGER OUT_DATA_PTR              ! pointer to data array of output file
      CHARACTER *80 OUT_NAME            ! Name of output NDF
      INTEGER OUT_NDF                   ! NDF identifier of output file
      INTEGER OUT_VAR_PTR               ! pointer to variance array of output file
      CHARACTER * 15 PARAM              ! Name of output file parameter
      INTEGER PLACE                     ! A placeholder
      REAL    QSORT(SCUBA__MAX_INT)     ! Scratch space for SCULIB_STATR
      LOGICAL RESW                      ! Was the data reduce_switched
      INTEGER RUN_NUMBER                ! run number of observation
      LOGICAL SKYDIP                    ! .TRUE. if not RAW data
      INTEGER SLICE_PTR                 ! Pointer to start of slice
      INTEGER SLICE_PTR_END             ! Pointer to end of slice
      CHARACTER*80 STEMP                ! scratch string
      REAL    START_EL                  ! Start elevation of skydip
      CHARACTER * 15 SUB_INST           ! Name of selected SUB instrument
      INTEGER SUB_POINTER               ! index of SUB_REQUIRED in sub-
                                        ! instruments observed
      DOUBLE PRECISION SUM              ! Sum from SCULIB_STATR
      DOUBLE PRECISION SUMSQ            ! Sum of squares from SCULIB_STATR
      DOUBLE PRECISION STDEV            ! Standard deviation of measurement
      CHARACTER*15 SWITCH_MODE          ! switch mode used
      REAL    TAUZ_FIT                  ! Fitted TAU
      CHARACTER * 15 TITLE              ! File title
      REAL    T_AMB                     ! Temperature of ambient load
      REAL    T_COLD                    ! Temperature of cold load
      REAL    T_TEL                     ! Temperature of telescope
      INTEGER UBND (MAXDIM)             ! upper bounds of array
      REAL    WAVE                      ! Selectred wavelength
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC ! Locator to EXTENSIONS

*    External functions:
      INCLUDE 'NDF_FUNC'


*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)

* Start up the NDF system and read in some data

      CALL NDF_BEGIN

      CALL SCULIB_SEARCH_DATADIR(PACKAGE, 'IN', IN_NDF, STATUS)

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
         RESW = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'SKYDIP') THEN
                  SKYDIP = .TRUE.
               ELSE IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  RESW = .TRUE.
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
*  and ask for the sub-instrument of interest.
 
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
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)
 
*     Check the dimensions of the input data array

      CALL NDF_DIM (IN_NDF, MAXDIM, DIM, NDIM, STATUS)

*     We have to distinguish the SKYDIP raw data from the output
*     of reduce_switch

      IF (STATUS .EQ. SAI__OK) THEN

         IF (RESW) THEN

            IF ((NDIM .NE. 2)        .OR.
     :           (DIM(1) .NE. N_BOLS)      .OR.
     :           (DIM(2) .LT. 1)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC ('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :              'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2',
     :              STATUS)
            END IF
            
            N_POS = DIM(2)

         ELSE
            IF ((NDIM .NE. 3)        .OR.
     :           (DIM(1) .NE. SCUBA__N_TEMPS)      .OR.
     :           (DIM(2) .NE. N_BOLS) .OR.
     :           (DIM(3) .LT. 1))     THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC ('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :              'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 '//
     :              '^DIM3', STATUS)
            END IF

            N_POS = DIM (3)
         

         END IF

      END IF



*     I should use automatic quality masking but the SCULIB_STATR
*     routine uses a quality mask so it is easier to just map it
*     rather than trying to change an existing routine

      IF (RESW) THEN

         CALL NDF_MAP (IN_NDF, 'QUALITY', '_UBYTE', 'READ', IN_QUAL_PTR,
     :        ITEMP, STATUS)

*     and the BADBIT mask...

         CALL NDF_BB(IN_NDF, IN_BADBIT, STATUS)

      END IF

*     Finally map the data array

      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)

*     We do not use the variance array since the variance is derived
*     from the error of the mean (for REDUCE_SWITCH'ed data)
*     and at present the output from SKYDIP REDUCE_SWITCH contains
*     no errors for the skydip temperatures.



*  map the DEM_PNTR array and check its dimensions
*     Note that the processed data no longer has switches.
 
      IF (RESW) THEN
         NDIM = 3
      ELSE
         NDIM = 4
      END IF

      CALL SCULIB_GET_DEM_PNTR(NDIM, IN_SCUBAX_LOC,
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
*  SKYDIP does not switch and the REDUCE_SWITCHed data doesn't either
*  This is not really necessary!

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_SWITCHES .GT. 1) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SWITCH_MODE', SWITCH_MODE)
            CALL MSG_SETI ('N_S', N_SWITCHES)
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: number '//
     :        'of switches in .SCUBA.DEM_PNTR (^N_S) does not '//
     :        'match SWITCH_MODE (SKYDIP)', STATUS)
         END IF
      END IF

*     Report general information about the SKYDIP

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
            AIRMASST(I) = AIRMASS_START + AIRSTEP * REAL(I-1)
 
*  Calculate the error Delta A = (TAN / SIN) Delta EL
*     TAN = 1/SQRT((AIR-1)(AIR+1))
            AIRMASS_TVAR(I) = 2.0 * ARCSEC * AIRMASST(I) / ! 2 arcsec error
     :           SQRT((AIRMASST(I)-1) * (AIRMASST(I)+1))
         END DO
      END IF


*     Now I need to convert he input data array into an array
*     of data - one average temperature (var + qual) for each
*     measurement

*     If the data has been through REDUCE_SWITCH I simply have to 
*     average the data from each integration

      IF (RESW) THEN

*     First we need to extract the one bolometer out of the input data
*     we'll make use of the dummy variables used to calculate the
*     skydips. So we need some memory

         DUM_DATA_PTR = 0
         DUM_DATA_PTR_END = 0
         DUM_QUAL_PTR = 0
         DUM_QUAL_PTR_END = 0

         CALL SCULIB_MALLOC(N_POS * VAL__NBR, DUM_DATA_PTR, 
     :        DUM_DATA_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_POS * VAL__NBUB, DUM_QUAL_PTR, 
     :        DUM_QUAL_PTR_END, STATUS)


         CALL SCULIB_EXTRACT_BOL(SUB_POINTER, N_BOLS, N_POS, 
     :        %VAL(IN_DATA_PTR), %VAL(IN_QUAL_PTR), 
     :        %VAL(DUM_DATA_PTR), %VAL(DUM_QUAL_PTR), STATUS)

*     Setup a counter before the loop so that we can remove
*     bad data

         NKEPT = 0

*     We need to loop over each measurement and 
*     find the start and end of each measurement

         DO I = 1, N_MEASUREMENTS

*  find where the exposure starts and finishes in the data array
 
            CALL SCULIB_FIND_SWITCH (%val(IN_DEM_PNTR_PTR),
     :           1, 1, N_INTEGRATIONS, N_MEASUREMENTS,
     :           N_POS, 1, 1, 1, I,
     :           EXP_START, EXP_END, STATUS)
            
*     I already know that this data has been processed such
*     that there is one data point per integration so I am
*     only interested in EXP_START.

*     Now calculate the mean of the data

            CALL SCULIB_STATR(N_INTEGRATIONS, -1.0, 
     :           %VAL(DUM_DATA_PTR + (EXP_START - 1) * VAL__NBR),
     :           %VAL(DUM_QUAL_PTR + (EXP_START - 1) * VAL__NBUB),
     :           IN_BADBIT, NGOOD, MEAN, MEDIAN, SUM, SUMSQ,
     :           STDEV, QSORT, STATUS)
            
*     Copy data to the measurement array
*     Note that STATR returns bad for MEAN if neceesary.
*     Note that SCULIB_FIT_SKYDIP does not have quality and does not
*     know about VAL__BADR so we have to remove the points ourself
*     Check for bad value and division by zero
*     Dont forget to copy the airmass as well

            IF (MEAN .NE. VAL__BADD .AND. NGOOD .GT. 0) THEN

               NKEPT = NKEPT + 1

               JSKY(NKEPT) = REAL(MEAN)
               JSKY_VAR(NKEPT) = REAL(STDEV * STDEV) / NGOOD
               AIRMASS(NKEPT) = AIRMASST(I)
               AIRMASS_VAR(NKEPT) = AIRMASS_TVAR(I)

            END IF

         END DO

*     Tidy up

         CALL SCULIB_FREE ('SCRATCHd' , DUM_DATA_PTR, DUM_DATA_PTR_END, 
     :        STATUS)
         CALL SCULIB_FREE ('SCRATCHq' , DUM_QUAL_PTR, 
     :        DUM_QUAL_PTR_END, STATUS)



      ELSE

*     If this data has not been through reduce_switch then we
*     need to convert the raw data to temperatures. This is done
*     in the same way as for REDUCE_SWITCH except that we are only
*     interested in one sub instrument. Note that we process all the
*     data but the overhead is not very large and computers are
*     very fast :-)
*     Trying to select a sub-instrument will just make the subroutine
*     more complicated than it needs to be.


*     Allocate some memory for slice and SKYDIP TEMPERATURES
         SLICE_PTR = 0
         SLICE_PTR_END = 0

         CALL SCULIB_MALLOC (SCUBA__N_TEMPS * N_BOLS * N_INTEGRATIONS * 
     :        VAL__NBR, SLICE_PTR, SLICE_PTR_END, STATUS)

*     Allocate some dummy memory for all those temperatures that
*     have not been averaged yet. These are going to be the temperatures
*     before averaging and the modified DEM_PNTR array that are necessary
*     for REDUCE_SWITCH (which shares SCULIB_CALC_SKYDIP_TEMPS).

         DUM_DATA_PTR = 0
         DUM_DATA_PTR_END = 0
         DUM_VAR_PTR = 0
         DUM_VAR_PTR_END = 0
         DUM_QUAL_PTR = 0
         DUM_QUAL_PTR_END = 0
         DUMMY_PTR = 0
         DUMMY_PTR_END = 0

         CALL SCULIB_MALLOC(N_BOLS * N_POS * VAL__NBR, DUM_DATA_PTR, 
     :        DUM_DATA_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_BOLS * N_POS * VAL__NBR, DUM_VAR_PTR, 
     :        DUM_VAR_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_BOLS * N_POS * VAL__NBUB, DUM_QUAL_PTR, 
     :        DUM_QUAL_PTR_END, STATUS)
         CALL SCULIB_MALLOC(N_INTEGRATIONS * N_MEASUREMENTS * VAL__NBR, 
     :        DUMMY_PTR, DUMMY_PTR_END, STATUS)
         

*     Now calculate the skydip temperatures
 
         CALL SCULIB_CALC_SKYDIP_TEMPS(SCUBA__N_TEMPS, SUB_POINTER, 
     :        N_FITS, FITS,
     :        'T_COLD', N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :        N_BOLS, SCUBA__MAX_SUB, SCUBA__NUM_CHAN, SCUBA__NUM_ADC, 
     :        BOL_CHAN, BOL_TYPE, BOL_ADC, %VAL(IN_DEM_PNTR_PTR), 
     :        %VAL(IN_DATA_PTR), %VAL(DUM_DATA_PTR), 
     :        %VAL(DUM_VAR_PTR), %VAL(DUM_QUAL_PTR),
     :        %VAL(DUMMY_PTR), AV_DATA, AV_VAR, AV_QUAL,
     :        %VAL(SLICE_PTR), STATUS)
         
*     Now extract the necesary data from the average data
*     Keep track of how many bad points we have removed
*     Dont forget to copy over the AIRMASS data as well

         NKEPT = 0  
         IN_BADBIT = VAL__BADUB   ! Any value is bad

         DO I = 1, N_MEASUREMENTS

            IF ((AV_DATA(SUB_POINTER, I) .NE. VAL__BADR).AND.
     :        (NDF_QMASK(AV_QUAL(SUB_POINTER, I), IN_BADBIT))) THEN

               NKEPT = NKEPT + 1

               JSKY(NKEPT) = AV_DATA(SUB_POINTER, I)
               JSKY_VAR(NKEPT) = AV_VAR(SUB_POINTER, I)
               AIRMASS(NKEPT) = AIRMASST(I)
               AIRMASS_VAR(NKEPT) = AIRMASS_TVAR(I)

            END IF

         END DO

*     Have now finished with input data
 
         CALL SCULIB_FREE ('SLICE' , SLICE_PTR, SLICE_PTR_END, STATUS)
         CALL SCULIB_FREE ('SCRATCHd' , DUM_DATA_PTR, DUM_DATA_PTR_END, 
     :        STATUS)
         CALL SCULIB_FREE ('SCRATCHv' , DUM_VAR_PTR,DUM_VAR_PTR_END, 
     :        STATUS)
         CALL SCULIB_FREE ('SCRATCHq' , DUM_QUAL_PTR, 
     :        DUM_QUAL_PTR_END, STATUS)
         CALL SCULIB_FREE ('SCRATCHdp' , DUMMY_PTR, DUMMY_PTR_END, 
     :        STATUS)

      END IF


*     Have now finished with input data

      CALL CMP_UNMAP(IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL DAT_ANNUL(IN_SCUBAX_LOC, STATUS)
      CALL NDF_ANNUL (IN_NDF, STATUS)

*     Get the temperatures from the FITS extension

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_AMB',
     :  T_AMB, STATUS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 'T_TEL',
     :  T_TEL, STATUS)

*     Get the fit parameters

      CALL PAR_GET0R ('ETA_TEL', ETA_TEL, STATUS )
      CALL PAR_GET0R ('B_FIT', B, STATUS )

*     The number of measurements is now actually the number of points
*     that were kept after removing bad data


* Send to fit skydip

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_FIT_SKYDIP (NKEPT, AIRMASS, JSKY,JSKY_VAR,
     :        WAVE, SUB_INST, FILT, T_TEL, T_AMB, ETA_TEL,B,ETA_TEL_FIT,
     :        B_FIT, TAUZ_FIT, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            FITFAIL = .FALSE.
            NFILES = 2
         ELSE
            CALL ERR_FLUSH(STATUS)
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
            NPTS = NKEPT   ! Not necessarily N_MEASUREMENTS
            LABEL = 'Jsky'
            TITLE = 'Skydip'
            BADPIX = .FALSE.  ! I remove all bad data points
            DREF = %LOC(JSKY)
            AREF = %LOC(AIRMASS)

         ELSE
*     Model parameters and create
            PARAM = 'MODEL_OUT'
            NPTS = N_MODEL
            LABEL = 'Jsky'
            TITLE = 'Skydip (model)'
            BADPIX = .FALSE.
            DREF = %LOC(J_THEORETICAL)   ! Reference to data
            AREF = %LOC(AIR_MODEL)       ! Reference to axis

            AIRSTEP = (AIRMASST(N_MEASUREMENTS) - AIRMASST(1)) /
     :           (N_MODEL - 1)

            DO I = 1, N_MODEL
               AIR_MODEL(I) = AIRMASST(1) + AIRSTEP * (I - 1)
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

* Store description of selected sub instrument

            CALL NDF_XPT0C(SUB_INST, OUT_NDF, 'REDS', 'SUB_INSTRUMENT', 
     :           STATUS)
            CALL NDF_XPT0C(FILT, OUT_NDF, 'REDS', 'FILTER', STATUS)
            CALL NDF_XPT0R(WAVE, OUT_NDF, 'REDS', 'WAVELENGTH', STATUS)

*     Retrieve the Cold temperature from the FITS header

            STEMP = 'T_COLD_'
            ITEMP = 7
            CALL CHR_PUTI(SUB_POINTER, STEMP, ITEMP)
            CALL SCULIB_GET_FITS_R (N_FITS, N_FITS, FITS, STEMP,
     :           T_COLD, STATUS)

            CALL NDF_XPT0R(T_COLD, OUT_NDF,'REDS','T_COLD', 
     :           STATUS)

*     Store the fit results

            IF (FILE .EQ. 2) THEN
               CALL NDF_XPT0R(ETA_TEL_FIT, OUT_NDF, 'REDS', 'ETA_TEL',
     :              STATUS)
               CALL NDF_XPT0R(B_FIT, OUT_NDF, 'REDS', 'B_FIT', STATUS)
               CALL NDF_XPT0R(TAUZ_FIT, OUT_NDF, 'REDS', 'TAU_Z',STATUS)
            END IF

*  create a history component in the output file
            CALL NDF_HCRE (OUT_NDF, STATUS)

*     Title, label and units
            CALL NDF_CPUT (TITLE, OUT_NDF,'TITLE',STATUS)
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

*     Only data has VARIANCE
            IF (FILE .EQ. 1) THEN
               CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 'WRITE',
     :              OUT_VAR_PTR, ITEMP, STATUS)
         
               CALL VEC_RTOR(.FALSE., UBND(1), JSKY_VAR, 
     :              %VAL(OUT_VAR_PTR), IERR, NERR, STATUS)

*               CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
*     :              OUT_QUAL_PTR, ITEMP, STATUS)
*               CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)
         
*               CALL VEC_UBTOUB(.FALSE., UBND(1), JSKY_QUAL,
*     :              %VAL(OUT_QUAL_PTR), IERR, NERR, STATUS)
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
