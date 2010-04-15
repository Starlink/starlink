      SUBROUTINE SURF_REDUCE_SWITCH (STATUS)
*+
*  Name:
*     REDUCE_SWITCH

*  Purpose:
*     reduce the switch sequence for a SCUBA observation

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_REDUCE_SWITCH (STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This application takes a SCUBA demodulated data file and splits the
*     data array up into its various `planes'; data, variance and quality.
*     In addition, the application reduces the component switches of an
*     exposure to give the exposure result. Optionally, the routine will
*     divide the internal calibrator signal into the data before doing either
*     of these things. It is also possible to select a single switch
*     from the input data.

*  Usage:
*     reduce_switch IN OUT


*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the demodulated data file.
*     MSG_FILTER = CHAR (Read)
*         Message filter level. Default is NORM.
*     OUT = NDF (Read)
*        The name of the file to contain the output data. The default
*        value is constructed from the run number.
*     SPIKE_LEVEL = INTEGER (Read)
*        Number of spikes tolerated before marking data point bad.
*        The default is that the sample should be marked bad if the
*        transputers detected more than 5 spikes during a 1 second
*        sample.
*     SWITCH = INTEGER (Read)
*        Parameter to indicate which switch to extract. A value of 0 means
*        that all switches should be reduced. Default is 0.
*     TARRAY = LOGICAL (Read)
*        Controls whether the T_COLD parameters are read as an array
*        of values (true) or read as a sequence of scalars (false) .
*        This parameter is useful if the command is to be run in batch mode.
*        Default is false.
*     T_COLD = REAL (Read)
*        Cold load temperatures for each of the sub-instruments present
*        in the data file. If TARRAY is true, the values are read as an
*        array. If it is false, values are read in one at a time, with the
*        parameter annulled between reads.
*     T_HOT = REAL (Read)
*        Hot load temperature. The default value is read from the T_HOT
*        header item. For data taken prior to 19980204 the default value
*        is read from the T_AMB header item.
*     USE_CALIBRATOR = LOGICAL (Read)
*        Yes, if you want the data for each bolometer measurement
*        divided by the corresponding internal calibrator signal.
*        The default is not to use the calibrator.

*  Examples:
*     reduce_switch
*        All parameters will be requested.
*     reduce_switch test nosw
*        This will reduce the switch from input file test.sdf without dividing
*        by the calibrator signal and tolerating up to 5 spikes in a 1 second
*        sample. The output data will be written to nosw.sdf.
*     reduce_switch test nosw SWITCH=2
*        This will select switch 2 from test.sdf and write it to nosw.sdf

*  Notes:
*     If the input file is not found in the current directory, the directory
*     specified by the DATADIR environment variable is searched. This means
*     that the raw data does not have to be in the working directory.
*     In addition 'IN' accepts a number. This number is converted to a demodulated
*     data filename by prepending it with information specified in
*     the SCUBA_PREFIX environment variable. This filename expansion only works
*     for demodulated data (ie data containing '_dem_'). The '_dem_' is
*     assumed and should not be present in $SCUBA_PREFIX.

*  Authors:
*     JFL: J.Lightfoot (jfl@roe.ac.uk)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*  Algorithm:
*     If status is good on entry, the routine reads the USE_CALIBRATOR
*     parameter and opens the IN file.
*        Some FITS items describing the observation are read from the
*     input file and reported to the user. The history records are read
*     and checked to see if REDUCE_SWITCH has already been run on this file,
*     if it has an error will be reported.
*        Next, a check is made that demodulated data really was kept during
*     the observation, demodulated files are used as `markers' to keep the
*     run numbers up to date even if the observer chose not to keep
*     the demodulated data, and this file could be one of those.
*        Then, the chop, switch-mode and internal calibrator status are
*     read and checked for validity.
*        All being well, the data array and the `pointer' array that points
*     to the start and finish of the data slice belonging to each switch are
*     mapped in and checked for consistency with the other parameters
*     describing the observation. Scratch memory is obtained to hold the
*     separate `planes' of the data; data, variance, calibrator,
*     quality; and the input data array is copied into them by a call to
*     SCULIB_COPY_DEMOD_SWITCH. If required, the calibrator signal is
*     divided into the data by a call to SCULIB_DIV_CALIBRATOR.
*        Now the OUT file is opened and data arrays created in it to hold
*     the data,variance and quality of the demodulated data. The `pointer'
*     array in the OUT file has its dimensions modified to reflect the fact
*     that the output arrays will contain data for individual exposures,
*     integrations and measurements but not switches.
*        Next, the routine cycles through the exposures, integrations and
*     measurements in the data, calling SCULIB_FIND_SWITCH to find the
*     start and end indices of each switch in the input array, then calling
*     SCULIB_REDUCE_SWITCH to reduce the switches, then copying the reduced
*     data into the first vacant section of the output arrays, storing the
*     start and finish indices of each exposure in the output `pointer'
*     array.
*        For most observing modes SCULIB_REDUCE_SWITCH reduces the data
*     assuming the bolometers to have been observing the source in the middle
*     beam (see the header of SCULIB_REDUCE_SWITCH for an explanation of
*     what this means). For PHOTOM mode, however, it is possible that some
*     of the bolometers may have been observing the source in the left or right
*     beam so, in this case, the data are reduced assuming all 3 cases and
*     the results are placed in seperate planes of the output data array.
*        Finally, the IN and OUT files are closed and all virtual memory
*     freed.


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Id$
*     24-JUL-1995: original version.
*     10-JUN-1996: modified to handle PHOTOM mode (JFL).
*      9-JUL-1996: modified to handle v200 data with 5 data per demodulated
*                  point (JFL).
*     $Log$
*     Revision 1.33  2005/03/18 06:28:21  timj
*     Initialise some variables
*
*     Revision 1.32  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.31  2000/05/11 19:58:41  timj
*     Add TARRAY, T_HOT and T_COLD to header documentation
*
*     Revision 1.30  1999/08/19 03:32:55  timj
*     use SCUBA__N_TEMPS
*
*     Revision 1.29  1999/08/03 20:01:38  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.28  1999/05/15 01:48:40  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*     Only check first few characters of history app name
*     now that we are writing version number to this string.
*     POLPHOT is synonym for PHOTOM.
*
*     Revision 1.27  1998/07/21 02:11:58  timj
*     Annull SECNDF
*
*     Revision 1.26  1997/10/08 18:44:02  timj
*     Fix 'no data' message when using 2 switches (^PKG instead of PKG)
*
*     Revision 1.25  1997/09/03 23:57:01  timj
*     Supply a default for 'OUT'
*
*     Revision 1.24  1997/07/19 02:43:16  timj
*     Add header information to describe $SCUBA_PREFIX.
*
*     Revision 1.23  1997/07/19 00:21:21  timj
*     Add PACKAGE variable to SEARCH_DATADIR
*     (plus looks like some skydip stuff but I thought that was already done in
*     v1.21)
*
*     Revision 1.22  1997/07/03 20:20:11  timj
*     Forgot to remove a print statement.
*
*     Revision 1.21  1997/07/03 20:18:10  timj
*     Output file size is now much smaller (use NDF_SECT before propogation
*     and copy output data to file after reducing the siwtches).
*
*     Also add axes at this point.
*
*     Revision 1.20  1997/06/20 21:23:49  timj
*     Update header to reflect the fact that SPIKE_LEVEL now has a default
*     of 5 and is no longer asked by default. Same for USE_CALIBRATOR.
*
*     Revision 1.19  1997/06/13 00:01:14  timj
*     Change name to SURF
*     Minor doc changes
*
*     Revision 1.18  1997/06/05 22:28:11  timj
*     Initialise pointer variables.
*
*     Revision 1.17  1997/05/22 03:33:22  timj
*     Use SCULIB_SEARCH_DATADIR routine.
*
*     Revision 1.16  1997/04/30 02:24:02  timj
*     Add range checking for 'SWITCH' parameter.
*     Add MSG_OUTIF.
*
*     Revision 1.15  1997/04/24 02:22:35  timj
*     Add ability to select an individual switch via the SWITCH parameter.
*
*     Revision 1.14  1997/04/01 22:46:01  timj
*     Use SCULIB_GET_DEM_PNTR
*     Use PKG and TASK for package name.
*
*     Revision 1.13  1997/03/21 20:35:43  jfl
*     modified to handle aborted observations
*
c Revision 1.12  1997/03/05  23:59:03  timj
c can't remember
c
c Revision 1.11  1996/11/02  01:41:38  timj
c Fix bug in History header
c
c Revision 1.10  1996/11/02  01:21:45  timj
c Add ADAM to header
c
c Revision 1.9  1996/10/31  18:07:24  timj
c Add support for SCUBAWAVE and RAMPWAVE chop functions.
c
c Revision 1.8  1996/10/30  03:01:39  timj
c Use VEC_ITOI instead of SCULIB_COPYI.
c Rewrite header to new Starlink standard.
c
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'                ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx
      INCLUDE 'NDF_PAR'                ! for NDF__NOID
      INCLUDE 'MSG_PAR'                ! MSG__ constants
      INCLUDE 'SURF_PAR'               ! REDS constants
      INCLUDE 'CNF_PAR'                ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External references:

*  Global variables:

*  Local Constants:
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      BYTE BADBIT                      ! Bad bits mask
      PARAMETER (BADBIT = 7)
      CHARACTER * 15 TSKNAME           ! Name of task
      PARAMETER (TSKNAME = 'REDUCE_SWITCH')

*  Local variables:
      LOGICAL      ABORTED             ! .TRUE. if observation was aborted
      REAL         AV_DATA(SCUBA__MAX_SUB, SCUBA__MAX_MEAS) ! Average skydip
      BYTE         AV_QUAL(SCUBA__MAX_SUB,SCUBA__MAX_MEAS) ! Quality of AV_QUAL
      REAL         AV_VAR(SCUBA__MAX_SUB,SCUBA__MAX_MEAS)! Var for av skydip
      INTEGER      BEAM                ! beam index in DO loop
      INTEGER      BEAM_OFFSET         ! offset in output data array due to
                                       ! beam index
      REAL         BEAM_WEIGHT (SCUBA__MAX_BEAM)
                                       ! weight assigned to the reduced data
                                       ! for each beam
      INTEGER      BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! A/D numbers of bolometers measured in
                                        ! input file
      INTEGER      BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                        ! channel numbers of bolometers
      CHARACTER*20 BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! bolometer types
      REAL         BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU3 Nasmyth coord of bolometers
      REAL         BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                        ! dU4 Nasmyth coord of bolometers
      LOGICAL      CALIBRATOR          ! .TRUE. if internal calibrator was ON
      CHARACTER*15 CHOP_FUNCTION       ! type of chop used
      CHARACTER*10 CTEMP               ! Scratch string
      CHARACTER*20 DATA_KEPT           ! types of data stored in file
      INTEGER      DEMOD_CALIBRATOR_END
                                       ! pointer to end of scratch calibrator
      INTEGER      DEMOD_CALIBRATOR_PTR
                                       ! pointer to start of scratch calibrator
      INTEGER      DEMOD_CAL_VARIANCE_END
                                       ! pointer to end of scratch cal variance
      INTEGER      DEMOD_CAL_VARIANCE_PTR
                                       ! pointer to start of scratch cal
                                       ! variance
      INTEGER      DEMOD_DATA_END      ! pointer to end of scratch data
      INTEGER      DEMOD_DATA_PTR      ! pointer to start of scratch data
      INTEGER      DEMOD_QUALITY_END   ! pointer to end of scratch quality
      INTEGER      DEMOD_QUALITY_PTR   ! pointer to start of scratch quality
      INTEGER      DEMOD_VARIANCE_END  ! pointer to end of scratch variance
      INTEGER      DEMOD_VARIANCE_PTR  ! pointer to start of scratch variance
      INTEGER      DIM (MAXDIM)        ! the dimensions of an array
      INTEGER      END_BEAM            ! last reduced beam
      INTEGER      EXPECTED_DIM1       ! expected first dimension of data
                                       ! array
      INTEGER      EXPOSURE            ! exposure index in DO loop
      INTEGER      EXP_POINTER         ! the offset in the output data array
                                       ! of a reduced exposure result
      CHARACTER*80 FITS (SCUBA__MAX_FITS)
                                       ! array of FITS keyword lines
      CHARACTER*(DAT__SZLOC) FITS_LOC  ! HDS locator to FITS structure
      INTEGER      I                   ! DO loop index
      INTEGER      IERR                ! Location of error returned from VEC_ITOI
      INTEGER      IN_NDF              ! NDF identifier of input file
      INTEGER      INTEGRATION         ! integration index in DO loop
      INTEGER      IN_DATA_PTR         ! pointer to data array of input file
      INTEGER      IN_DEM_PNTR_PTR     ! pointer to input .SCUBA.DEM_PNTR array
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC ! Locator to .SCUBA extension
      INTEGER      ITEMP               ! scratch integer
      INTEGER      J                   ! Counter
      INTEGER      JIGGLE_COUNT        ! Size of jiggle
      INTEGER      LAST_EXP            ! the last exposure number in
                                       ! an aborted observation
      INTEGER      LAST_INT            ! the last integration number in
                                       ! an aborted observation
      INTEGER      LAST_MEAS           ! the last measurement number in
                                       ! an aborted observation
      INTEGER      LBND (MAXDIM)       ! lower bounds of array
      INTEGER      MEASUREMENT         ! measurement index in DO loop
      LOGICAL      MISSING_SWITCH      ! .TRUE. if any switches are
                                       ! missing in an exposure
      INTEGER      NDIM                ! the number of dimensions in an array
      INTEGER      NERR                ! Number of errors returned from VEC_ITOI
      INTEGER      NINTS               ! Integration number for AXES
      INTEGER      NJIGGLE             ! Jiggle number for AXES
      INTEGER      NREC                ! number of history records in file
      INTEGER      N_BOLS              ! number of bolometers measured
      INTEGER      N_EXPOSURES         ! number of exposures per integration
      INTEGER      N_FITS              ! number of FITS lines read from file
      INTEGER      N_INTEGRATIONS      ! number of integrations per measurement
      INTEGER      N_MEASUREMENTS      ! number of measurements in the file
      INTEGER      N_POS               ! the total number of positions measured
      INTEGER      N_SWITCHES          ! number of switches per exposure
      INTEGER      N_SWITCH_POS        ! the number of positions in a switch
      CHARACTER*30 OBJECT              ! name of object observed
      CHARACTER*15 OBSERVING_MODE      ! type of observation
      CHARACTER*132 OUTFILE            ! Name of default output name
      INTEGER      OUT_A_PTR           ! Pointer to AXIS
      INTEGER      OUT_NDF             ! NDF identifier of output file
      INTEGER      OUT_DAT_PTR         ! Pntr to data in output file
      INTEGER      OUT_DATA_END        ! pointer to end data array
      INTEGER      OUT_DATA_PTR        ! pointer to output data array
      INTEGER      OUT_DEM_PNTR_PTR    ! pointer to output .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC
                                       ! HDS locator of REDS extension in
                                       ! output file
      CHARACTER*(DAT__SZLOC) OUT_FITSX_LOC ! Locator .FITS output
      INTEGER      OUT_QUL_PTR         ! Mapped output file quality
      INTEGER      OUT_QUALITY_END     ! end of qual array.
      INTEGER      OUT_QUALITY_PTR     ! pointer to quality array in output
      CHARACTER*(DAT__SZLOC) OUT_SCUBAX_LOC ! Locator .SCUBA output
      INTEGER      OUT_VAR_PTR         ! Pointer to output variance file
      INTEGER      OUT_VARIANCE_END    ! pointer to end of variance array
      INTEGER      OUT_VARIANCE_PTR    ! pointer to variance array
      LOGICAL      REDUCE_SWITCH       ! .TRUE. if REDUCE_SWITCH has already
                                       ! been run on the file
      REAL         RTEMP               ! Temporary real
      INTEGER      RUN_NUMBER          ! run number of observation
      CHARACTER*15 SAMPLE_MODE         ! sample mode of observation
      INTEGER      SECNDF              ! Section NDF identifier
      INTEGER      SLICE_PTR_END       ! End of skydip work space
      INTEGER      SLICE_PTR           ! Skydip workspace
      INTEGER      SPIKE_LEVEL         ! level to despike
      INTEGER      START_BEAM          ! first reduced beam
      CHARACTER*80 STATE               ! string describing the 'state'
                                       ! of SCUCD when the observation
                                       ! finished
      CHARACTER*20 STEMP               ! scratch string
      INTEGER      SWITCH              ! Which switch to use (0,1,2,3) for
                                       ! Reduce, 1st, 2nd, 3rd
      INTEGER      SWITCHES            ! number of switches implied by
                                       ! SWITCH_MODE
      CHARACTER*15 SWITCH_MODE         ! switch mode used
      INTEGER      SWITCH1_END         ! index of last point in switch 1
      INTEGER      SWITCH1_START       ! index of first point in switch 1
      INTEGER      SWITCH2_END         ! index of last point in switch 2
      INTEGER      SWITCH2_START       ! index of first point in switch 2
      INTEGER      SWITCH3_END         ! index of last point in switch 3
      INTEGER      SWITCH3_START       ! index of first point in switch 3
      LOGICAL      THERE               ! Is extension present
      INTEGER      UBND (MAXDIM)       ! upper bounds of array
      LOGICAL      USE_CALIBRATOR      ! .TRUE. if internal calibrator signal
                                       ! is be divided into the data
*  Internal References:
*  Local data:

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise variables
      DEMOD_DATA_PTR = 0
      DEMOD_DATA_END = 0
      DEMOD_VARIANCE_PTR = 0
      DEMOD_VARIANCE_END = 0
      DEMOD_CALIBRATOR_PTR = 0
      DEMOD_CALIBRATOR_END = 0
      DEMOD_CAL_VARIANCE_PTR = 0
      DEMOD_CAL_VARIANCE_END = 0
      DEMOD_QUALITY_PTR = 0
      DEMOD_QUALITY_END = 0
      SWITCHES = 0
      DO I = 1, SCUBA__MAX_BEAM
         BEAM_WEIGHT(I) = 0
      END DO




*  start up the NDF system and read in the demodulated data file

      CALL NDF_BEGIN

      CALL SCULIB_SEARCH_DATADIR(PACKAGE, 'IN', IN_NDF, STATUS)

*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', FITS_LOC, STATUS)
      CALL DAT_SIZE (FITS_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file '//
     :        'contains too many FITS items', STATUS)
         END IF
      END IF
      CALL DAT_GET1C (FITS_LOC, SCUBA__MAX_FITS, FITS, N_FITS, STATUS)

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
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC('PKG', PACKAGE)
      IF (OBSERVING_MODE .NE. 'SKYDIP')THEN
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: run ^RUN was a ^MODE observation '//
     :        'of object ^OBJECT', STATUS)
      ELSE
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: run ^RUN was a ^MODE observation ', STATUS)
      END IF

*  check that the history of the input file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         REDUCE_SWITCH = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               END IF
            END DO
         ENDIF

         IF (REDUCE_SWITCH) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: REDUCE_SWITCH '//
     :           'has already been run on the input data', STATUS)
            END IF
         END IF
      END IF

*  check that demodulated data was stored in the observation

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'DATA_KPT',
     :  DATA_KEPT, STATUS)
      CALL CHR_UCASE (DATA_KEPT)
      IF (INDEX(DATA_KEPT,'DEMOD') .EQ. 0) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: DATA_KEPT was '//
     :        'not set to DEMOD for the observation, the file does '//
     :        'not contain all the demodulated data', STATUS)
         END IF
      END IF

*  see if the observation completed normally or was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF

*  now find the chop function, switch mode and whether the internal
*  calibrator was turned on

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'CHOP_FUN',
     :  CHOP_FUNCTION, STATUS)
      CALL CHR_UCASE (CHOP_FUNCTION)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'SWTCH_MD',
     :  SWITCH_MODE, STATUS)
      CALL CHR_UCASE (SWITCH_MODE)
      CALL SCULIB_GET_FITS_L (SCUBA__MAX_FITS, N_FITS, FITS, 'CALIBRTR',
     :  CALIBRATOR, STATUS)

*  get the number of bolometers measured

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'N_BOLS',
     :  N_BOLS, STATUS)

*  does the user want to divide the data by the internal calibrator
*  Not relevant for SKYDIP

      IF (OBSERVING_MODE .NE. 'SKYDIP') THEN

         CALL PAR_GET0L ('USE_CALIBRATOR', USE_CALIBRATOR, STATUS)

      ELSE

         USE_CALIBRATOR = .FALSE.

      END IF

*  check input file is OK (irrelevant for SKYDIP)

      IF (STATUS .EQ. SAI__OK .AND. OBSERVING_MODE .NE. 'SKYDIP') THEN
         IF ((CHOP_FUNCTION .NE. 'SQUARE') .AND.
     :        (CHOP_FUNCTION .NE. 'SCUBAWAVE') .AND.
     :        (CHOP_FUNCTION .NE. 'RAMPWAVE') .AND.
     :       (CHOP_FUNCTION .NE. 'TRIPOS')) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('CHOP', CHOP_FUNCTION)
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: invalid chop '//
     :        'function - ^CHOP', STATUS)
         END IF

         IF (USE_CALIBRATOR .AND. .NOT. CALIBRATOR) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: USE_CALIBRATOR '//
     :        'is .TRUE. but the calibrator was not turned on '//
     :        'during this observation', STATUS)
         END IF

         IF (SWITCH_MODE .EQ. '3PSW') THEN
            SWITCHES = 3
         ELSE IF (SWITCH_MODE .EQ. 'BMSW') THEN
            SWITCHES = 2
         ELSE IF (SWITCH_MODE .EQ. 'NOSW') THEN
            SWITCHES = 1
         ELSE
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SWITCH', SWITCH_MODE)
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: the switch mode '//
     :        '- ^SWITCH is invalid', STATUS)
         END IF
      END IF

*  Switch off automatic quality checking

      CALL NDF_SQMF(.FALSE., IN_NDF, STATUS)

*  map the data array and check its dimensions

*     Check the dimensions of the input data

      CALL NDF_DIM (IN_NDF, MAXDIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN

         IF (OBSERVING_MODE .EQ. 'SKYDIP') THEN
*     This is the number of skydip temperatures
            EXPECTED_DIM1 =   SCUBA__N_TEMPS
         ELSE IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
            EXPECTED_DIM1 = 5
         ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN
            EXPECTED_DIM1 = 4
         END IF
         IF ((NDIM .NE. 3)               .OR.
     :       (DIM(1) .NE. EXPECTED_DIM1) .OR.
     :       (DIM(2) .NE. N_BOLS)        .OR.
     :       (DIM(3) .LT. 1))            THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL MSG_SETI ('DIM3', DIM(3))
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: main data '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 ^DIM3',
     :        STATUS)
         END IF
      END IF

      N_POS = DIM (3)

*     Map the input data

      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :     ITEMP, STATUS)


*  map the DEM_PNTR array and check its dimensions

      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

      CALL SCULIB_GET_DEM_PNTR(4, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*  check that the number of switches matches the SWITCH_MODE
*  irrelevant for SKYDIP

      IF (STATUS .EQ. SAI__OK .AND. OBSERVING_MODE .NE. 'SKYDIP') THEN
         IF (N_SWITCHES .NE. SWITCHES) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('SWITCH_MODE', SWITCH_MODE)
            CALL MSG_SETI ('N_S', N_SWITCHES)
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: number '//
     :        'of switches in .SCUBA.DEM_PNTR (^N_S) does not '//
     :        'match SWITCH_MODE (^SWITCH_MODE)', STATUS)
         END IF
      END IF

      CALL MSG_SETI ('N_S', N_SWITCHES)
      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      IF (.NOT. ABORTED) THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: file contains data for ^N_S '//
     :        'switch(es) in ^N_E exposure(s) in ^N_I integration(s) '//
     :        'in ^N_M measurement(s)', STATUS)
      ELSE

*  get the exposure, integration, measurement numbers at which the
*  abort occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'EXP_NO', LAST_EXP, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'INT_NO', LAST_INT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MEAS_NO', LAST_MEAS, STATUS)

         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        '^PKG: the observation should have '//
     :        'had ^N_S switch(es) in ^N_E exposure(s) in ^N_I '//
     :        'integration(s) in ^N_M measurement(s)', STATUS)
         CALL MSG_SETI ('N_E', LAST_EXP)
         CALL MSG_SETI ('N_I', LAST_INT)
         CALL MSG_SETI ('N_M', LAST_MEAS)
         CALL MSG_OUTIF (MSG__NORM, ' ',
     :        ' - However, the observation was '//
     :        'ABORTED during exposure ^N_E of integration ^N_I '//
     :        'of measurement ^N_M', STATUS)

      END IF

*     Do some non-SKYDIP stuff

      IF (OBSERVING_MODE .NE. 'SKYDIP') THEN

*  Ask for the level at switch spikes should be removed

         CALL PAR_GET0I('SPIKE_LEVEL', SPIKE_LEVEL, STATUS)
         SPIKE_LEVEL = SPIKE_LEVEL * 256

*  Ask which switch we are going to keep
*     0:  Reduce as normal
*     1:  Select 1st switch
*     2:  Select 2nd switch
*     3:  Select 3rd switch

         SWITCH = 0

         IF (N_SWITCHES .GT. 1) THEN
            CALL PAR_DEF0I('SWITCH', SWITCH, STATUS)
            CALL PAR_MINI('SWITCH', SWITCH, STATUS)
            CALL PAR_MAXI('SWITCH', N_SWITCHES, STATUS)
            CALL PAR_GET0I('SWITCH', SWITCH, STATUS)
         END IF

*  get some scratch memory and copy the different sections of the data array
*  into it

         CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR, DEMOD_DATA_PTR,
     :        DEMOD_DATA_END, STATUS)
         CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR,
     :        DEMOD_VARIANCE_PTR, DEMOD_VARIANCE_END, STATUS)
         CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR,
     :        DEMOD_CALIBRATOR_PTR, DEMOD_CALIBRATOR_END, STATUS)
         CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR,
     :        DEMOD_CAL_VARIANCE_PTR, DEMOD_CAL_VARIANCE_END, STATUS)
         CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBUB,
     :        DEMOD_QUALITY_PTR, DEMOD_QUALITY_END, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_COPY_DEMOD_SWITCH (N_BOLS, EXPECTED_DIM1, N_POS,
     :           SPIKE_LEVEL, %VAL(CNF_PVAL(IN_DATA_PTR)),
     :           %VAL(CNF_PVAL(DEMOD_DATA_PTR)),
     :           %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(DEMOD_CALIBRATOR_PTR)),
     :           %VAL(CNF_PVAL(DEMOD_CAL_VARIANCE_PTR)),
     :           %VAL(CNF_PVAL(DEMOD_QUALITY_PTR)),
     :           STATUS)
         END IF

      END IF

*     Propogating the input to the output results in an output NDF that
*     is far bigger than needed (since the space is not given back). To
*     get round this we first create a section of the required size and
*     propogate that


*     First need to calculate new bounds of the array

*     First need to set the bounds of the array to small

      NDIM = 2
      LBND (1) = 1
      LBND (2) = 1
      UBND (1) = N_BOLS
      UBND (2) = 1     ! N_POS
      LBND(3)  = 1
      UBND(3)  = 1
      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN
         NDIM = 3
         LBND (3) = 1
         UBND (3) = SCUBA__MAX_BEAM
      END IF

*     Now create a section of the input data so that we can
*     copy only the necessary size

      CALL NDF_SECT(IN_NDF, NDIM, LBND, UBND, SECNDF, STATUS)

*     Generate a default output filename
*     Takes the form of 'o'//RUN_NUMBER

      OUTFILE = 'o'
      CALL CHR_ITOC( RUN_NUMBER, CTEMP, ITEMP)
      ITEMP = 1
      CALL CHR_APPND( CTEMP, OUTFILE, ITEMP)

      CALL PAR_DEF0C('OUT', OUTFILE, STATUS)


*     Propogate the section to the output

      CALL NDF_PROP (SECNDF, ' ', 'OUT', OUT_NDF, STATUS)

*     Annull the section
      CALL NDF_ANNUL(SECNDF, STATUS)

*     create a history component in the output file

      CALL NDF_HCRE (OUT_NDF, STATUS)

*     Since the number of positions in the output file can reduce by a
*     factor of 2 or 3 (depending on the number of switches) it is
*     more efficient (for space) to use a malloced array first and
*     then copy the data to the output file. Note that this may not
*     be the most efficient use of memory or speed.

      OUT_DATA_PTR = 0
      OUT_DATA_END = 0
      OUT_VARIANCE_PTR = 0
      OUT_VARIANCE_END = 0
      OUT_QUALITY_PTR = 0
      OUT_QUALITY_END = 0

      CALL SCULIB_MALLOC(N_POS * N_BOLS * UBND(3) * VAL__NBR,
     :     OUT_DATA_PTR, OUT_DATA_END, STATUS)
      CALL SCULIB_MALLOC(N_POS * N_BOLS * UBND(3) * VAL__NBR,
     :     OUT_VARIANCE_PTR, OUT_VARIANCE_END, STATUS)
      CALL SCULIB_MALLOC(N_POS * N_BOLS * UBND(3) * VAL__NBUB,
     :     OUT_QUALITY_PTR, OUT_QUALITY_END, STATUS)

*  find the .SCUBA.DEM_PNTR array, change its bounds and map it

      NDIM = 3
      UBND (1) = N_EXPOSURES
      UBND (2) = N_INTEGRATIONS
      UBND (3) = N_MEASUREMENTS
      CALL NDF_XLOC (OUT_NDF, 'SCUBA', 'UPDATE', OUT_SCUBAX_LOC, STATUS)
      CALL CMP_MOD(OUT_SCUBAX_LOC, 'DEM_PNTR', '_INTEGER', NDIM, UBND,
     :     STATUS)
      CALL CMP_MAPV(OUT_SCUBAX_LOC, 'DEM_PNTR', '_INTEGER', 'WRITE',
     :     OUT_DEM_PNTR_PTR, ITEMP, STATUS)

*     Run SKYDIP reduction if needed
*     else do a normal reduce_switch

      IF (OBSERVING_MODE .EQ. 'SKYDIP') THEN

*     We need the bolometer descriptions for the skydip

      CALL SCULIB_GET_BOL_DESC(OUT_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOLS, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)


*     Need to get some scratch data to work with
         SLICE_PTR = 0
         SLICE_PTR_END = 0

         CALL SCULIB_MALLOC (SCUBA__N_TEMPS * N_BOLS * N_INTEGRATIONS *
     :        VAL__NBR, SLICE_PTR, SLICE_PTR_END, STATUS)

*     Now calculate the skydip temperatures

         CALL SCULIB_CALC_SKYDIP_TEMPS(SCUBA__N_TEMPS, 0, N_FITS, FITS,
     :        'T_COLD', N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :        N_BOLS, SCUBA__MAX_SUB, SCUBA__NUM_CHAN, SCUBA__NUM_ADC,
     :        BOL_CHAN, BOL_TYPE, BOL_ADC,
     :        %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :        %VAL(CNF_PVAL(IN_DATA_PTR)), %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :        %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :        %VAL(CNF_PVAL(OUT_QUALITY_PTR)),
     :        %VAL(CNF_PVAL(OUT_DEM_PNTR_PTR)),
     :        AV_DATA, AV_VAR, AV_QUAL,
     :        %VAL(CNF_PVAL(SLICE_PTR)), STATUS)

*     Have now finished with input data

         CALL SCULIB_FREE ('SLICE' , SLICE_PTR, SLICE_PTR_END, STATUS)

*     We still have the same number of points so just make sure the
*     copy to the output file works okay
         EXP_POINTER = N_POS + 1

*     Now write the possibly modified FITS header back to the output

         CALL NDF_XLOC (OUT_NDF, 'FITS', 'UPDATE', OUT_FITSX_LOC,
     :        STATUS)

         CALL DAT_PUT1C (OUT_FITSX_LOC, N_FITS, FITS, STATUS)

      ELSE
*  now got through the various exposures in the observation, reducing the
*  component switches into the output array

      IF (STATUS .EQ. SAI__OK) THEN
         EXP_POINTER = 1

         DO MEASUREMENT = 1, N_MEASUREMENTS
            DO INTEGRATION = 1, N_INTEGRATIONS
               DO EXPOSURE = 1, N_EXPOSURES

                  MISSING_SWITCH = .FALSE.

*  find the offset of the switches in the data array

                  CALL SCULIB_FIND_SWITCH (
     :   %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :              N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :              N_MEASUREMENTS, N_POS, 1, EXPOSURE, INTEGRATION,
     :              MEASUREMENT, SWITCH1_START, SWITCH1_END, STATUS)
                  IF ((SWITCH1_START .EQ. VAL__BADI) .OR.
     :                (SWITCH1_START .EQ. 0))        THEN
                     CALL MSG_SETI ('E', EXPOSURE)
                     CALL MSG_SETI ('I', INTEGRATION)
                     CALL MSG_SETI ('M', MEASUREMENT)
                     CALL MSG_SETC('PKG', PACKAGE)
                     CALL MSG_OUTIF (MSG__NORM,' ',
     :                    '^PKG: no data for '//
     :                    'switch 1 in exp ^E, int ^I, meas ^M',
     :                    STATUS)
                     MISSING_SWITCH = .TRUE.
                  END IF

                  IF (N_SWITCHES .GE. 2) THEN
                     CALL SCULIB_FIND_SWITCH (
     :   %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :                 N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :                 N_MEASUREMENTS, N_POS, 2, EXPOSURE,
     :                 INTEGRATION, MEASUREMENT, SWITCH2_START,
     :                 SWITCH2_END, STATUS)
                     IF ((SWITCH2_START .EQ. VAL__BADI) .OR.
     :                   (SWITCH2_START .EQ. 0))        THEN
                        CALL MSG_SETI ('E', EXPOSURE)
                        CALL MSG_SETI ('I', INTEGRATION)
                        CALL MSG_SETI ('M', MEASUREMENT)
                        CALL MSG_SETC('PKG', PACKAGE)
                        CALL MSG_OUTIF (MSG__NORM, ' ',
     :                       '^PKG: no data for '//
     :                       'switch 2 in exp ^E, int ^I, meas ^M',
     :                       STATUS)
                        MISSING_SWITCH = .TRUE.
                     END IF
                  ELSE
                     SWITCH2_START = SWITCH1_START
                  END IF

                  IF (N_SWITCHES .GE. 3) THEN
                     CALL SCULIB_FIND_SWITCH (
     :   %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :                 N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :                 N_MEASUREMENTS, N_POS, 3, EXPOSURE,
     :                 INTEGRATION, MEASUREMENT, SWITCH3_START,
     :                 SWITCH3_END, STATUS)
                     IF ((SWITCH3_START .EQ. VAL__BADI) .OR.
     :                   (SWITCH3_START .EQ. 0))        THEN
                        CALL MSG_SETI ('E', EXPOSURE)
                        CALL MSG_SETI ('I', INTEGRATION)
                        CALL MSG_SETI ('M', MEASUREMENT)
                        CALL MSG_SETC('PKG', PACKAGE)
                        CALL MSG_OUTIF (MSG__NORM,' ',
     :                       '^PKG: no data for '//
     :                       'switch 3 in exp ^E, int ^I, meas ^M',
     :                       STATUS)
                        MISSING_SWITCH = .TRUE.
                     END IF
                  ELSE
                     SWITCH3_START = SWITCH2_START
                  END IF


                  IF (.NOT. MISSING_SWITCH) THEN

*  we, have data for all the switches in the exposure, find the number of
*  data in the switch

                     N_SWITCH_POS = SWITCH1_END - SWITCH1_START + 1

*  if required, divide the mean calibrator signal for each switch into the data

                     IF (USE_CALIBRATOR) THEN
                        CALL SCULIB_DIV_CALIBRATOR_2 (N_BOLS,
     :                    N_SWITCH_POS,
     :                    %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                    (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                    %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                    (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                    %VAL(CNF_PVAL(DEMOD_CALIBRATOR_PTR) +
     :                    (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                    %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                    (SWITCH1_START - 1) * N_BOLS * VAL__NBUB),
     :                       STATUS)

                        IF (N_SWITCHES .GE. 2) THEN
                           CALL SCULIB_DIV_CALIBRATOR_2 (N_BOLS,
     :                       N_SWITCH_POS,
     :                       %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                       (SWITCH2_START - 1) * N_BOLS * VAL__NBR),
     :                       %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                       (SWITCH2_START - 1) * N_BOLS * VAL__NBR),
     :                       %VAL(CNF_PVAL(DEMOD_CALIBRATOR_PTR) +
     :                       (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                       %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                       (SWITCH2_START - 1) * N_BOLS * VAL__NBUB),
     :                          STATUS)
                        END IF

                        IF (N_SWITCHES .GE. 3) THEN
                           CALL SCULIB_DIV_CALIBRATOR_2 (N_BOLS,
     :                       N_SWITCH_POS,
     :                       %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                       (SWITCH3_START - 1) * N_BOLS * VAL__NBR),
     :                       %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                       (SWITCH3_START - 1) * N_BOLS * VAL__NBR),
     :                       %VAL(CNF_PVAL(DEMOD_CALIBRATOR_PTR) +
     :                       (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                       %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                       (SWITCH3_START - 1) * N_BOLS * VAL__NBUB),
     :                          STATUS)
                        END IF
                     END IF

*  set up the number of `beams' to be reduced

                     IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :                    OBSERVING_MODE .EQ. 'POLPHOT') THEN
                        START_BEAM = 1
                        END_BEAM = SCUBA__MAX_BEAM
                     ELSE
                        START_BEAM = 2
                        END_BEAM = 2
                     END IF

*  reduce the switch

		     DO BEAM = START_BEAM, END_BEAM

*  calculate the data offset in the `beam' plane

                        BEAM_OFFSET = (BEAM - START_BEAM) * N_POS *
     :                    N_BOLS

                        IF (SWITCH .EQ. 0) THEN

                           CALL SCULIB_REDUCE_SWITCH (CHOP_FUNCTION,
     :                          N_SWITCHES, N_SWITCH_POS * N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                          (SWITCH1_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                          (SWITCH1_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                          (SWITCH1_START - 1) * N_BOLS*VAL__NBUB),
     :                          %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                          (SWITCH2_START - 1) * N_BOLS*VAL__NBR),
     :                          %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                          (SWITCH2_START - 1) * N_BOLS*VAL__NBR),
     :                          %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                          (SWITCH2_START - 1) * N_BOLS*VAL__NBUB),
     :                          %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                          (SWITCH3_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                          (SWITCH3_START - 1) * N_BOLS *VAL__NBR),
     :                          %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                          (SWITCH3_START - 1) * N_BOLS*VAL__NBUB),
     :                          BEAM,%VAL(CNF_PVAL(OUT_DATA_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_VARIANCE_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_QUALITY_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBUB),
     :                          BEAM_WEIGHT(BEAM), STATUS)

                        ELSE IF (SWITCH .EQ. 1) THEN
*     Should do this with %LOC
                           CALL VEC_RTOR(.FALSE., N_SWITCH_POS * N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                          (SWITCH1_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_DATA_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          IERR, NERR, STATUS)

                           CALL VEC_RTOR(.FALSE., N_SWITCH_POS * N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                          (SWITCH1_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_VARIANCE_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          IERR, NERR, STATUS)

                           CALL VEC_UBTOUB(.FALSE., N_SWITCH_POS*N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                          (SWITCH1_START - 1) * N_BOLS*VAL__NBUB),
     :                          %VAL(CNF_PVAL(OUT_QUALITY_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBUB),
     :                          IERR, NERR, STATUS)

                        ELSE IF (SWITCH .EQ. 2) THEN
*     Should do this with %LOC
                           CALL VEC_RTOR(.FALSE., N_SWITCH_POS * N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                          (SWITCH2_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_DATA_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          IERR, NERR, STATUS)

                           CALL VEC_RTOR(.FALSE., N_SWITCH_POS * N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                          (SWITCH2_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_VARIANCE_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          IERR, NERR, STATUS)

                           CALL VEC_UBTOUB(.FALSE., N_SWITCH_POS*N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                          (SWITCH2_START - 1) * N_BOLS*VAL__NBUB),
     :                          %VAL(CNF_PVAL(OUT_QUALITY_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBUB),
     :                          IERR, NERR, STATUS)

                        ELSE IF (SWITCH .EQ. 3) THEN
*     Should do this with %LOC
                           CALL VEC_RTOR(.FALSE., N_SWITCH_POS * N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_DATA_PTR) +
     :                          (SWITCH3_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_DATA_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          IERR, NERR, STATUS)

                           CALL VEC_RTOR(.FALSE., N_SWITCH_POS * N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_VARIANCE_PTR) +
     :                          (SWITCH3_START - 1) * N_BOLS* VAL__NBR),
     :                          %VAL(CNF_PVAL(OUT_VARIANCE_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                          IERR, NERR, STATUS)

                           CALL VEC_UBTOUB(.FALSE., N_SWITCH_POS*N_BOLS,
     :                          %VAL(CNF_PVAL(DEMOD_QUALITY_PTR) +
     :                          (SWITCH3_START - 1) * N_BOLS*VAL__NBUB),
     :                          %VAL(CNF_PVAL(OUT_QUALITY_PTR)
     :                          + (BEAM_OFFSET +
     :                          (EXP_POINTER-1) * N_BOLS) * VAL__NBUB),
     :                          IERR, NERR, STATUS)
                        END IF
                     END DO

*  increment the output offset

                     ITEMP = ((MEASUREMENT-1) * N_INTEGRATIONS +
     :                 INTEGRATION-1) * N_EXPOSURES + EXPOSURE-1
                     CALL VEC_ITOI(.FALSE., 1, EXP_POINTER,
     :                    %VAL(CNF_PVAL(OUT_DEM_PNTR_PTR) +
     :                    ITEMP * VAL__NBI),
     :                    IERR, NERR, STATUS)
                     EXP_POINTER = EXP_POINTER + N_SWITCH_POS
                  ELSE

*  we are missing data for at least one switch, we can't reduce the
*  exposure so put a 0 in the DEM_PNTR array to indicate this

                     ITEMP = ((MEASUREMENT-1) * N_INTEGRATIONS +
     :                 INTEGRATION-1) * N_EXPOSURES + EXPOSURE-1
                     CALL VEC_ITOI (.FALSE., 1, 0,
     :   %VAL(CNF_PVAL(OUT_DEM_PNTR_PTR) + ITEMP * VAL__NBI),
     :                 IERR, NERR, STATUS)
                  END IF
               END DO
            END DO
         END DO

*     This is the end of the If SKYDIP ELSE section
      END IF

      END IF

*  set the dimensions of the main data array to their final values

      IF (STATUS .EQ. SAI__OK) THEN

*     make sure we have some data
         IF (EXP_POINTER .EQ. 1) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP( ' ','^TASK: There seems to be no valid '//
     :           'data in this file - ABORTING',
     :           STATUS)
         END IF

         NDIM = 2
         LBND (1) = 1
         LBND (2) = 1
         UBND (1) = N_BOLS
         UBND (2) = EXP_POINTER - 1
         UBND (3) = 1
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            NDIM = 3
            LBND (3) = 1
            UBND (3) = SCUBA__MAX_BEAM
         END IF
         CALL NDF_SBND (NDIM, LBND, UBND, OUT_NDF, STATUS)
      END IF

*     Map the output file

      CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     :     OUT_QUL_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE',
     :     OUT_DAT_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 'WRITE',
     :     OUT_VAR_PTR, ITEMP, STATUS)

*     Copy the reduced data to the output array now that we know the
*     correct size.
*     This is made more complicated because the adjustable bounds are
*     not from the last dimension if beams are involved.
*     Its easier to use a small subroutine which I will add to the end
*     of this code for the moment.

      CALL SURF_LOCAL_COPY(N_BOLS, N_POS, UBND(3),
     :                     %VAL(CNF_PVAL(OUT_DATA_PTR)),
     :     %VAL(CNF_PVAL(OUT_VARIANCE_PTR)),
     :     %VAL(CNF_PVAL(OUT_QUALITY_PTR)), EXP_POINTER-1,
     :     %VAL(CNF_PVAL(OUT_DAT_PTR)), %VAL(CNF_PVAL(OUT_VAR_PTR)),
     :     %VAL(CNF_PVAL(OUT_QUL_PTR)),
     :     STATUS)

*  write the BEAM_WEIGHT array to the REDS extension
*  if this is not a SKYDIP


      IF (OBSERVING_MODE .NE. 'SKYDIP') THEN

*     Look for the REDS extension
         CALL NDF_XSTAT(OUT_NDF, 'REDS', THERE, STATUS)

         IF (THERE) THEN
            CALL NDF_XLOC (OUT_NDF, 'REDS', 'UPDATE', OUT_REDSX_LOC,
     :           STATUS)
         ELSE
            CALL NDF_XNEW (OUT_NDF, 'REDS', 'SURF_EXTENSION',
     :           0, 0, OUT_REDSX_LOC, STATUS)
         END IF
         CALL CMP_MOD (OUT_REDSX_LOC, 'BEAM_WT', '_REAL', 1,
     :        SCUBA__MAX_BEAM, STATUS)
         CALL CMP_PUT1R (OUT_REDSX_LOC, 'BEAM_WT', SCUBA__MAX_BEAM,
     :        BEAM_WEIGHT, STATUS)
         CALL DAT_ANNUL (OUT_REDSX_LOC, STATUS)
      END IF

* Write the axis info
*     For PHOTOM observations there are 3 axes
*
*     MAPS:    Axis 1: bolometers   2: Data
*     PHOTOM:  Axis 1: Bolometers   2: Data   3: Beam

*     Deal with BOLOMETER axis

      CALL NDF_AMAP(OUT_NDF, 'CENTRE', 1, '_INTEGER', 'WRITE',
     :     OUT_A_PTR, ITEMP, STATUS)
      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_NFILLI (N_BOLS, %VAL(CNF_PVAL(OUT_A_PTR)))
      END IF
      CALL NDF_ACPUT ('Bolometer', OUT_NDF, 'LABEL', 1, STATUS)
      CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 1, STATUS)

*     Deal with BEAM if necessary

      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN

         CALL NDF_AMAP(OUT_NDF, 'CENTRE', 3, '_INTEGER', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_NFILLI (3, %VAL(CNF_PVAL(OUT_A_PTR)))
         END IF
         CALL NDF_ACPUT ('Beam', OUT_NDF, 'LABEL', 3, STATUS)
         CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 3, STATUS)

      END IF

*     Deal with the integrations - SCAN/MAP doesnt use jiggles so I
*     have to think about that to work out how big a an integration is
*     - probably use DEM_PNTR.
*     SKYDIP will use measurements.

      IF (OBSERVING_MODE .EQ. 'SKYDIP') THEN

         CALL NDF_AMAP (OUT_NDF, 'CENTRE', 2, '_REAL', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)

*     I know that these are multiple measurements and I know how
*     many integrations there are per measurement. So this is fairly
*     easy. I also know how many numbers I am dealing with (from ITEMP)

         N_POS = ITEMP

         IF (STATUS .EQ. SAI__OK) THEN
*     Fill with numbers 1->N_POS
            CALL SCULIB_NFILLR(N_POS, %VAL(CNF_PVAL(OUT_A_PTR)))
*     Start the counting at 0
            CALL SCULIB_ADDCAR(N_POS, %VAL(CNF_PVAL(OUT_A_PTR)),
     :           -1.0, %VAL(CNF_PVAL(OUT_A_PTR)))
*     Mulitply these numbers by 1/N_INTEGRATIONS
            CALL SCULIB_MULCAR(N_POS, %VAL(CNF_PVAL(OUT_A_PTR)),
     :           1.0/REAL(N_INTEGRATIONS), %VAL(CNF_PVAL(OUT_A_PTR)))
*     Offset these number by 1 unit so that the first entry is 1
            CALL SCULIB_ADDCAR(N_POS, %VAL(CNF_PVAL(OUT_A_PTR)),
     :           1.0, %VAL(CNF_PVAL(OUT_A_PTR)))
         END IF

         CALL NDF_ACPUT ('Measurements', OUT_NDF, 'LABEL', 2, STATUS)
         CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 2, STATUS)



      ELSE IF (SAMPLE_MODE .NE. 'RASTER') THEN

         CALL NDF_AMAP (OUT_NDF, 'CENTRE', 2, '_REAL', 'WRITE',
     :        OUT_A_PTR, ITEMP, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            ITEMP = 0

*     Read the JIGGLE_COUNT from the FITS header

            CALL SCULIB_GET_FITS_I (N_FITS, N_FITS, FITS,
     :           'JIGL_CNT', JIGGLE_COUNT, STATUS)

*     For aborted datasets need to be careful so must use N_POS
*     and not just N_INTEGRATIONS. Note that N_POS is in fact
*     EXP_POINTER - 1 after removing the switches

            N_POS = EXP_POINTER - 1

            NINTS = N_POS / JIGGLE_COUNT
            IF (NINTS * JIGGLE_COUNT .LT. N_POS) THEN
               NINTS = NINTS + 1
            END IF

            DO I = 1, NINTS
               NJIGGLE = MOD (N_POS - (I-1) * JIGGLE_COUNT,
     :              JIGGLE_COUNT)
               IF (NJIGGLE .EQ. 0) THEN
                  NJIGGLE = JIGGLE_COUNT
               END IF

               DO J = 1, NJIGGLE
                  RTEMP = REAL(I)+ (REAL(J-1)/REAL(JIGGLE_COUNT))
                  CALL SCULIB_CFILLR(1,RTEMP,
     :                 %VAL(CNF_PVAL(OUT_A_PTR)+(ITEMP * VAL__NBR)))
                  ITEMP = ITEMP + 1
               END DO
            END DO

         END IF

         CALL NDF_ACPUT ('Integration', OUT_NDF, 'LABEL', 2, STATUS)
         CALL NDF_AUNMP (OUT_NDF, 'CENTRE', 2, STATUS)
      END IF

*     and a title

      IF (OBSERVING_MODE .NE. 'SKYDIP') THEN

         CALL NDF_CPUT('Raw data', OUT_NDF, 'LAB', STATUS)
         CALL NDF_CPUT(OBJECT, OUT_NDF, 'Title', STATUS)
         CALL NDF_CPUT('Volts', OUT_NDF, 'UNITS', STATUS)

      ELSE

         CALL NDF_CPUT('Sky temperature', OUT_NDF, 'LAB', STATUS)
         CALL NDF_CPUT('SKYDIP', OUT_NDF, 'Title', STATUS)
         CALL NDF_CPUT('Kelvin', OUT_NDF, 'UNITS', STATUS)

      END IF


*  tidy up

      CALL SCULIB_FREE('RESW_DATA', OUT_DATA_PTR, OUT_DATA_END, STATUS)
      CALL SCULIB_FREE('RESW_VAR', OUT_VARIANCE_PTR, OUT_VARIANCE_END,
     :     STATUS)
      CALL SCULIB_FREE('RESW_QUAL', OUT_QUALITY_PTR, OUT_QUALITY_END,
     :     STATUS)

      IF (OBSERVING_MODE .NE. 'SKYDIP') THEN

         CALL SCULIB_FREE ('DEMOD_DATA', DEMOD_DATA_PTR, DEMOD_DATA_END,
     :        STATUS)
         CALL SCULIB_FREE ('DEMOD_VARIANCE', DEMOD_VARIANCE_PTR,
     :        DEMOD_VARIANCE_END, STATUS)
         CALL SCULIB_FREE ('DEMOD_CALIBRATOR', DEMOD_CALIBRATOR_PTR,
     :        DEMOD_CALIBRATOR_END, STATUS)
         CALL SCULIB_FREE ('DEMOD_CAL_VARIANCE', DEMOD_CAL_VARIANCE_PTR,
     :        DEMOD_CAL_VARIANCE_END, STATUS)
         CALL SCULIB_FREE ('DEMOD_QUALITY', DEMOD_QUALITY_PTR,
     :        DEMOD_QUALITY_END, STATUS)

      END IF

* Set the bit mask
      CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)

* Unmap and annul
      CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL CMP_UNMAP (OUT_SCUBAX_LOC, 'DEM_PNTR', STATUS)

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (OUT_SCUBAX_LOC, STATUS)

      CALL NDF_UNMAP(IN_NDF, '*', STATUS)
      CALL NDF_UNMAP(OUT_NDF, '*', STATUS)

      CALL NDF_ANNUL (IN_NDF, STATUS)
      CALL NDF_ANNUL (OUT_NDF, STATUS)

      CALL NDF_END (STATUS)

      END

*****************************************************************************

      SUBROUTINE SURF_LOCAL_COPY(N_BOL, N_POS, N_BEAM, IN_DATA, IN_VAR,
     :     IN_QUAL, OUT_POS, OUT_DATA, OUT_VAR, OUT_QUAL, STATUS)
*+
*  Name:
*     SURF_LOCAL_COPY

*  Purpose:
*     Compress the reduce switched data to the output size

*  Description:
*     Copy the input to the output reducing the size of the output
*     array if necessary. This is needed since the middle dimension changes
*     in size when multiple beams are being used (ie PHOTOM).

*-

      IMPLICIT NONE

      INCLUDE 'SAE_PAR'

*  Global status:
      INTEGER STATUS

*  Arguments Given:
      INTEGER N_BOL
      INTEGER N_POS
      INTEGER N_BEAM
      INTEGER OUT_POS

      REAL    IN_DATA(N_BOL, N_POS, N_BEAM)
      REAL    IN_VAR (N_BOL, N_POS, N_BEAM)
      BYTE    IN_QUAL(N_BOL, N_POS, N_BEAM)

*  Arguments returned:
      REAL    OUT_DATA(N_BOL, OUT_POS, N_BEAM)
      REAL    OUT_VAR (N_BOL, OUT_POS, N_BEAM)
      BYTE    OUT_QUAL(N_BOL, OUT_POS, N_BEAM)

*  Local Variables:
      INTEGER I  ! Loop counter
      INTEGER J  ! Loop counter
      INTEGER K  ! Loop counter

*.

      IF (STATUS .NE. SAI__OK) RETURN

      DO K = 1, N_BEAM
         DO J = 1, OUT_POS
            DO I = 1, N_BOL

               OUT_DATA(I, J, K) = IN_DATA(I, J, K)
               OUT_VAR (I, J, K) = IN_VAR (I, J, K)
               OUT_QUAL(I, J, K) = IN_QUAL(I, J, K)

            END DO
         END DO
      END DO

      END
