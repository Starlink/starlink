      SUBROUTINE REDS_REDUCE_SWITCH (STATUS)
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
*     CALL REDS_REDUCE_SWITCH (STATUS)

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This application takes a SCUBA demodulated data file and splits the 
*     data array up into its various `planes'; data, variance and quality.
*     In addition, the application reduces the component switches of an
*     exposure to give the exposure result. Optionally, the routine will
*     divide the internal calibrator signal into the data before doing either
*     of these things. 
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

*  ADAM Parameters:
*     IN = NDF (Read)
*        The name of the demodulated data file.
*     OUT = NDF (Read)
*        The name of the file to contain the output data.
*     SPIKE_LEVEL = _INTEGER (Read)
*        Number of spikes tolerated before marking data point bad.
*     USE_CALIBRATOR = _LOGICAL (Read)
*        Yes, if you want the data for each bolometer measurement
*        divided by the corresponding internal calibrator signal.

*  Examples:
*     reduce_switch IN=test USE_CALIBRATOR=no SPIKE_LEVEL=0 OUT=nosw
*        This will reduce the switch from input file test.sdf without dividing
*        by the calibrator signal and no toleration of any spikes detected by
*        the transputers. The output data will be written to nosw.sdf.

*  Notes:

*  Implementation status:
*      - Deals with QUALITY arrays

*  Authors :
*     JFL: J.Lightfoot (jfl@roe.ac.uk)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)

*    Authors :
*     J.Lightfoot (jfl@roe.ac.uk)
*    History :
*     $Id$
*     24-JUL-1995: original version.
*     10-JUN-1996: modified to handle PHOTOM mode (JFL).
*      9-JUL-1996: modified to handle v200 data with 5 data per demodulated
*                  point (JFL).
*     $Log$
*     Revision 1.10  1996/11/02 01:21:45  timj
*     Add ADAM to header
*
c Revision 1.9  1996/10/31  18:07:24  timj
c Add support for SCUBAWAVE and RAMPWAVE chop functions.
c
c Revision 1.8  1996/10/30  03:01:39  timj
c Use VEC_ITOI instead of SCULIB_COPYI.
c Rewrite header to new Starlink standard.
c
*    endhistory

*  Bugs:
*     {note_any_bugs_here}
 
*-

*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'                ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx
      INCLUDE 'NDF_PAR'                ! for NDF__NOID
      INCLUDE 'REDS_SYS'               ! REDS constants
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      INTEGER GETCWD
      INTEGER CHDIR
      EXTERNAL GETCWD, CHDIR
*    Global variables :
*    Local Constants :
      INTEGER MAXDIM
      PARAMETER (MAXDIM = 4)
      BYTE BADBIT                      ! Bad bits mask
      PARAMETER (BADBIT = 7)
*    Local variables :
      INTEGER      BEAM                ! beam index in DO loop
      INTEGER      BEAM_OFFSET         ! offset in output data array due to
                                       ! beam index
      REAL         BEAM_WEIGHT (SCUBA__MAX_BEAM)
                                       ! weight assigned to the reduced data
                                       ! for each beam
      LOGICAL      CALIBRATOR          ! .TRUE. if internal calibrator was ON
      CHARACTER*15 CHOP_FUNCTION       ! type of chop used
      CHARACTER*64 CWD                 ! Current directory
      CHARACTER*64 DATA_DIR            ! Data directory
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
      INTEGER I                        ! DO loop index
      INTEGER      IARY1               ! ARY array identifier
      INTEGER      IARY2               ! ARY array identifier
      INTEGER      IERR                ! Location of error returned from VEC_ITOI
      INTEGER      IN_NDF              ! NDF identifier of input file
      INTEGER      INTEGRATION         ! integration index in DO loop
      INTEGER      IN_DATA_PTR         ! pointer to data array of input file
      INTEGER      IN_DEM_PNTR_PTR     ! pointer to input .SCUBA.DEM_PNTR array
      INTEGER      ISTAT               ! temporary status
      INTEGER      ITEMP               ! scratch integer
      INTEGER      LBND (MAXDIM)       ! lower bounds of array
      CHARACTER*(DAT__SZLOC) LOC1      !
      CHARACTER*(DAT__SZLOC) LOC2      !
      INTEGER      MEASUREMENT         ! measurement index in DO loop
      INTEGER      NDIM                ! the number of dimensions in an array
      INTEGER      NERR                ! Number of errors returned from VEC_ITOI
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
      INTEGER      OUT_NDF             ! NDF identifier of output file
      INTEGER      OUT_DATA_PTR        ! pointer to data array in output file
      INTEGER      OUT_DEM_PNTR_PTR    ! pointer to output .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) OUT_REDSX_LOC
                                       ! HDS locator of REDS extension in 
                                       ! output file
      INTEGER      OUT_QUALITY_PTR     ! pointer to quality array in output 
      INTEGER      OUT_VARIANCE_PTR    ! pointer to variance array in output
      LOGICAL      REDUCE_SWITCH       ! .TRUE. if REDUCE_SWITCH has already
                                       ! been run on the file
      INTEGER      RUN_NUMBER          ! run number of observation
      CHARACTER*15 SAMPLE_MODE         ! sample mode of observation
      INTEGER      SPIKE_LEVEL         ! level to despike
      INTEGER      START_BEAM          ! first reduced beam
      CHARACTER*20 STEMP               ! scratch string
      INTEGER      SWITCHES            ! number of switches implied by
                                       ! SWITCH_MODE
      CHARACTER*15 SWITCH_MODE         ! switch mode used
      INTEGER      SWITCH1_END         ! index of last point in switch 1
      INTEGER      SWITCH1_START       ! index of first point in switch 1
      INTEGER      SWITCH2_END         ! index of last point in switch 2
      INTEGER      SWITCH2_START       ! index of first point in switch 2
      INTEGER      SWITCH3_END         ! index of last point in switch 3
      INTEGER      SWITCH3_START       ! index of first point in switch 3
      LOGICAL      TRYING              ! Logical to find NDF files
      INTEGER      UBND (MAXDIM)       ! upper bounds of array
      LOGICAL      USE_CALIBRATOR      ! .TRUE. if internal calibrator signal
                                       ! is be divided into the data
*    Internal References :
*    Local data :
*-

      IF (STATUS .NE. SAI__OK) RETURN

*  start up the NDF system and read in the demodulated data file

      CALL NDF_BEGIN

      TRYING = .TRUE.
      DO WHILE (TRYING)

         CALL NDF_EXIST('IN', 'READ', IN_NDF, STATUS)

*  The file could be in DATADIR

         IF (IN_NDF.EQ.NDF__NOID) THEN
            CALL GETENV('DATADIR', DATA_DIR)

            ISTAT = GETCWD(CWD)
            IF (ISTAT .EQ. 0) THEN
               ISTAT = CHDIR(DATA_DIR)
               IF (ISTAT .EQ. 0) THEN
                  CALL NDF_EXIST('IN', 'READ', IN_NDF, STATUS)
                  ISTAT = CHDIR(CWD)
                  IF (IN_NDF .NE. NDF__NOID) TRYING = .FALSE.
               END IF
            END IF
         ELSE
            TRYING = .FALSE.
         END IF

        IF (TRYING) THEN
            CALL ERR_ANNUL(STATUS)
            CALL MSG_OUT(' ','REDS_REDUCE_SWITCH: Failed to'//
     :           ' find requested file in CWD and DATADIR',
     :           STATUS)
            CALL PAR_CANCL('IN', STATUS)
         END IF
         
      END DO

*  does the user want to divide the data by the internal calibrator

      CALL PAR_GET0L ('USE_CALIBRATOR', USE_CALIBRATOR, STATUS)


*  get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', FITS_LOC, STATUS)
      CALL DAT_SIZE (FITS_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: input file '//
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
      CALL MSG_OUT (' ', 'REDS: run ^RUN was a ^MODE observation '//
     :  'of object ^OBJECT', STATUS)

      IF (OBSERVING_MODE .EQ. 'SKYDIP') THEN
         STATUS = SAI__ERROR
         CALL ERR_REP (' ','REDS: REDUCE_SWITCH can not be run on '//
     :        '^MODE observations', STATUS)
      ENDIF


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
               IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               END IF
            END DO
         ENDIF

         IF (REDUCE_SWITCH) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: REDUCE_SWITCH '//
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
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: DATA_KEPT was '//
     :        'not set to DEMOD for the observation, the file does '//
     :        'not contain all the demodulated data', STATUS)
         END IF
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

*  check input file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         IF ((CHOP_FUNCTION .NE. 'SQUARE') .AND.
     :        (CHOP_FUNCTION .NE. 'SCUBAWAVE') .AND.
     :        (CHOP_FUNCTION .NE. 'RAMPWAVE') .AND.
     :       (CHOP_FUNCTION .NE. 'TRIPOS')) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('CHOP', CHOP_FUNCTION)
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: invalid chop '//
     :        'function - ^CHOP', STATUS)
         END IF

         IF (USE_CALIBRATOR .AND. .NOT. CALIBRATOR) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: USE_CALIBRATOR '//
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
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: the switch mode '//
     :        '- ^SWITCH is invalid', STATUS)
         END IF
      END IF

* Switch off automatic quality checking
      CALL NDF_SQMF(.FALSE., IN_NDF, STATUS)

*  map the data array and check its dimensions 

      CALL NDF_DIM (IN_NDF, MAXDIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', IN_DATA_PTR,
     :  ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN
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
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: main data '//
     :        'array has bad dimensions - (^NDIM) ^DIM1 ^DIM2 ^DIM3',
     :        STATUS)
         END IF
      END IF

      N_POS = DIM (3)

*  map the DEM_PNTR array and check its dimensions

      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', LOC1, STATUS)
      CALL ARY_FIND (LOC1, 'DEM_PNTR', IARY1, STATUS)
      CALL ARY_DIM (IARY1, MAXDIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IARY1, '_INTEGER', 'READ', IN_DEM_PNTR_PTR, ITEMP,
     :  STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 4) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: .SCUBA.DEM_PNTR '//
     :        'array has bad number of dimensions', STATUS)
         ELSE 
            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'switches - ^DIM1', STATUS)
            END IF
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'exposures - ^DIM2', STATUS) 
            END IF
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: '//
     :           '.SCUBA.DEM_PNTR array contains bad number of '//
     :           'integrations - ^DIM3', STATUS)
            END IF
            IF (DIM(4) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM4', DIM(4))
               CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: '//
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
            CALL ERR_REP (' ', 'REDS_REDUCE_SWITCH: number '//
     :        'of switches in .SCUBA.DEM_PNTR (^N_S) does not '//
     :        'match SWITCH_MODE (^SWITCH_MODE)', STATUS)
         END IF
      END IF

      N_SWITCHES = DIM (1)
      N_EXPOSURES = DIM (2)
      N_INTEGRATIONS = DIM (3)
      N_MEASUREMENTS = DIM (4)

      CALL MSG_SETI ('N_S', N_SWITCHES)
      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      CALL MSG_OUT (' ', 'REDS: file contains data for ^N_S '//
     :  'switch(es) in ^N_E exposure(s) in ^N_I integration(s) in '//
     :  '^N_M measurement(s)', STATUS)

* Ask for the level at switch spikes should be removed

      CALL PAR_GET0I('SPIKE_LEVEL', SPIKE_LEVEL, STATUS)
      SPIKE_LEVEL = SPIKE_LEVEL * 256

*  get some scratch memory and copy the different sections of the data array
*  into it

      CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR, DEMOD_DATA_PTR,
     :  DEMOD_DATA_END, STATUS)
      CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR, DEMOD_VARIANCE_PTR,
     :  DEMOD_VARIANCE_END, STATUS)
      CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR,
     :  DEMOD_CALIBRATOR_PTR, DEMOD_CALIBRATOR_END, STATUS)
      CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBR, 
     :  DEMOD_CAL_VARIANCE_PTR, DEMOD_CAL_VARIANCE_END, STATUS)
      CALL SCULIB_MALLOC (N_BOLS * N_POS * VAL__NBUB, DEMOD_QUALITY_PTR,
     :  DEMOD_QUALITY_END, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_COPY_DEMOD_SWITCH (N_BOLS, EXPECTED_DIM1, N_POS,
     :     SPIKE_LEVEL, %val(IN_DATA_PTR), %val(DEMOD_DATA_PTR),
     :     %val(DEMOD_VARIANCE_PTR), %val(DEMOD_CALIBRATOR_PTR),
     :     %val(DEMOD_CAL_VARIANCE_PTR), %val(DEMOD_QUALITY_PTR),
     :     STATUS)
      END IF

*  now open the output NDF, propagating it from the input one

      CALL NDF_PROP (IN_NDF, ' ', 'OUT', OUT_NDF, STATUS)

*  create a history component in the output file

      CALL NDF_HCRE (OUT_NDF, STATUS)

*  reset the data array bounds and map the various components

      NDIM = 2
      LBND (1) = 1
      LBND (2) = 1
      UBND (1) = N_BOLS
      UBND (2) = N_POS
      IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
         NDIM = 3
         LBND (3) = 1
         UBND (3) = SCUBA__MAX_BEAM
      END IF
      CALL NDF_SBND (NDIM, LBND, UBND, OUT_NDF, STATUS)

      CALL NDF_MAP (OUT_NDF, 'QUALITY', '_UBYTE', 'WRITE',
     :  OUT_QUALITY_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUT_NDF, 'DATA', '_REAL', 'WRITE', 
     :  OUT_DATA_PTR, ITEMP, STATUS)
      CALL NDF_MAP (OUT_NDF, 'VARIANCE', '_REAL', 'WRITE',
     :  OUT_VARIANCE_PTR, ITEMP, STATUS)

*  find the .SCUBA.DEM_PNTR array, change its bounds and map it

      CALL NDF_XLOC (OUT_NDF, 'SCUBA', 'UPDATE', LOC2, STATUS)
      CALL ARY_FIND (LOC2, 'DEM_PNTR', IARY2, STATUS)
      NDIM = 3
      LBND (1) = 1
      LBND (2) = 1
      LBND (3) = 1
      UBND (1) = N_EXPOSURES
      UBND (2) = N_INTEGRATIONS
      UBND (3) = N_MEASUREMENTS
      CALL ARY_SBND (NDIM, LBND, UBND, IARY2, STATUS)
      CALL ARY_MAP (IARY2, '_INTEGER', 'WRITE', OUT_DEM_PNTR_PTR, ITEMP,
     :  STATUS)

*  now got through the various exposures in the observation, reducing the
*  component switches into the output array

      IF (STATUS .EQ. SAI__OK) THEN
         EXP_POINTER = 1

         DO MEASUREMENT = 1, N_MEASUREMENTS
            DO INTEGRATION = 1, N_INTEGRATIONS
               DO EXPOSURE = 1, N_EXPOSURES

*  find the offset of the switches in the data array

                  CALL SCULIB_FIND_SWITCH (%val(IN_DEM_PNTR_PTR),
     :              N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :              N_MEASUREMENTS, N_POS, 1, EXPOSURE, INTEGRATION,
     :              MEASUREMENT, SWITCH1_START, SWITCH1_END, STATUS)

                  IF (N_SWITCHES .GE. 2) THEN
                     CALL SCULIB_FIND_SWITCH (%val(IN_DEM_PNTR_PTR),
     :                 N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :                 N_MEASUREMENTS, N_POS, 2, EXPOSURE, INTEGRATION,
     :                 MEASUREMENT, SWITCH2_START, SWITCH2_END, 
     :                 STATUS)
                  ELSE
                     SWITCH2_START = SWITCH1_START
                  END IF

                  IF (N_SWITCHES .GE. 3) THEN
                     CALL SCULIB_FIND_SWITCH (%val(IN_DEM_PNTR_PTR),
     :                 N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :                 N_MEASUREMENTS, N_POS, 3, EXPOSURE, INTEGRATION,
     :                 MEASUREMENT, SWITCH3_START, SWITCH3_END,
     :                 STATUS)
                  ELSE
                     SWITCH3_START = SWITCH2_START
                  END IF

*  find the number of data in the switch 

                  N_SWITCH_POS = SWITCH1_END - SWITCH1_START + 1

*  if required, divide the mean calibrator signal for each switch into the data

                  IF (USE_CALIBRATOR) THEN
                     CALL SCULIB_DIV_CALIBRATOR_2 (N_BOLS,
     :                 N_SWITCH_POS,
     :                 %val(DEMOD_DATA_PTR + 
     :                 (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_VARIANCE_PTR + 
     :                 (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_CALIBRATOR_PTR +
     :                 (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_QUALITY_PTR + 
     :                 (SWITCH1_START - 1) * N_BOLS * VAL__NBUB))

                     IF (N_SWITCHES .GE. 2) THEN
                        CALL SCULIB_DIV_CALIBRATOR_2 (N_BOLS,
     :                    N_SWITCH_POS,
     :                    %val(DEMOD_DATA_PTR + 
     :                    (SWITCH2_START - 1) * N_BOLS * VAL__NBR),
     :                    %val(DEMOD_VARIANCE_PTR + 
     :                    (SWITCH2_START - 1) * N_BOLS * VAL__NBR),
     :                    %val(DEMOD_CALIBRATOR_PTR +
     :                    (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                    %val(DEMOD_QUALITY_PTR + 
     :                    (SWITCH2_START - 1) * N_BOLS * VAL__NBUB))
                     END IF

                     IF (N_SWITCHES .GE. 3) THEN
                        CALL SCULIB_DIV_CALIBRATOR_2 (N_BOLS,
     :                    N_SWITCH_POS,
     :                    %val(DEMOD_DATA_PTR + 
     :                    (SWITCH3_START - 1) * N_BOLS * VAL__NBR),
     :                    %val(DEMOD_VARIANCE_PTR + 
     :                    (SWITCH3_START - 1) * N_BOLS * VAL__NBR),
     :                    %val(DEMOD_CALIBRATOR_PTR +
     :                    (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                    %val(DEMOD_QUALITY_PTR + 
     :                    (SWITCH3_START - 1) * N_BOLS * VAL__NBUB))
                     END IF
                  END IF

*  set up the number of `beams' to be reduced

		  IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
                     START_BEAM = 1
                     END_BEAM = SCUBA__MAX_BEAM
                  ELSE
                     START_BEAM = 2
                     END_BEAM = 2
                  END IF

*  reduce the switch

		  DO BEAM = START_BEAM, END_BEAM

*  calculate the data offset in the `beam' plane

                     BEAM_OFFSET = (BEAM - START_BEAM) * N_POS * N_BOLS

                     CALL SCULIB_REDUCE_SWITCH (CHOP_FUNCTION,
     :                 N_SWITCHES, N_SWITCH_POS * N_BOLS,
     :                 %val(DEMOD_DATA_PTR + 
     :                 (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_VARIANCE_PTR + 
     :                 (SWITCH1_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_QUALITY_PTR + 
     :                 (SWITCH1_START - 1) * N_BOLS * VAL__NBUB),
     :                 %val(DEMOD_DATA_PTR +
     :                 (SWITCH2_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_VARIANCE_PTR + 
     :                 (SWITCH2_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_QUALITY_PTR + 
     :                 (SWITCH2_START - 1) * N_BOLS * VAL__NBUB),
     :                 %val(DEMOD_DATA_PTR + 
     :                 (SWITCH3_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_VARIANCE_PTR + 
     :                 (SWITCH3_START - 1) * N_BOLS * VAL__NBR),
     :                 %val(DEMOD_QUALITY_PTR + 
     :                 (SWITCH3_START - 1) * N_BOLS * VAL__NBUB),
     :                 BEAM,
     :                 %val(OUT_DATA_PTR + (BEAM_OFFSET +
     :                 (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                 %val(OUT_VARIANCE_PTR + (BEAM_OFFSET +
     :                 (EXP_POINTER-1) * N_BOLS) * VAL__NBR),
     :                 %val(OUT_QUALITY_PTR + (BEAM_OFFSET +
     :                 (EXP_POINTER-1) * N_BOLS) * VAL__NBUB),
     :                 BEAM_WEIGHT(BEAM),
     :                 STATUS)
                  END DO

*  increment the output offset

                  ITEMP = ((MEASUREMENT-1) * N_INTEGRATIONS +
     :                 INTEGRATION-1) * N_EXPOSURES + EXPOSURE-1
                  CALL VEC_ITOI(.FALSE., 1, EXP_POINTER,
     :                 %val(OUT_DEM_PNTR_PTR + ITEMP * VAL__NBI), IERR,
     :                 NERR, STATUS)
                  EXP_POINTER = EXP_POINTER + N_SWITCH_POS
               END DO
            END DO
         END DO

*  unmap and set the dimensions of the main data array to their final values

         CALL NDF_UNMAP (OUT_NDF, '*', STATUS)
         NDIM = 2
         LBND (1) = 1
         LBND (2) = 1
         UBND (1) = N_BOLS
         UBND (2) = EXP_POINTER - 1
         IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
            NDIM = 3
            LBND (3) = 1
            UBND (3) = SCUBA__MAX_BEAM
         END IF
         CALL NDF_SBND (NDIM, LBND, UBND, OUT_NDF, STATUS)
      END IF

*  write the BEAM_WEIGHT array to the REDS extension

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_XLOC (IN_NDF, 'REDS', 'UPDATE', OUT_REDSX_LOC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            CALL NDF_XNEW (OUT_NDF, 'REDS', 'REDS_EXTENSION',
     :        0, 0, OUT_REDSX_LOC, STATUS)
         END IF
      END IF
      CALL CMP_MOD (OUT_REDSX_LOC, 'BEAM_WT', '_REAL', 1,
     :  SCUBA__MAX_BEAM, STATUS)
      CALL CMP_PUT1R (OUT_REDSX_LOC, 'BEAM_WT', SCUBA__MAX_BEAM,
     :  BEAM_WEIGHT, STATUS)
      CALL DAT_ANNUL (OUT_REDSX_LOC, STATUS)

*  tidy up

      CALL SCULIB_FREE ('DEMOD_DATA', DEMOD_DATA_PTR, DEMOD_DATA_END,
     :  STATUS)
      CALL SCULIB_FREE ('DEMOD_VARIANCE', DEMOD_VARIANCE_PTR,
     :  DEMOD_VARIANCE_END, STATUS)
      CALL SCULIB_FREE ('DEMOD_CALIBRATOR', DEMOD_CALIBRATOR_PTR,
     :  DEMOD_CALIBRATOR_END, STATUS)
      CALL SCULIB_FREE ('DEMOD_CAL_VARIANCE', DEMOD_CAL_VARIANCE_PTR,
     :  DEMOD_CAL_VARIANCE_END, STATUS)
      CALL SCULIB_FREE ('DEMOD_QUALITY', DEMOD_QUALITY_PTR,
     :  DEMOD_QUALITY_END, STATUS)

* Set the bit mask
      CALL NDF_SBB(BADBIT, OUT_NDF, STATUS)

* Unmap and annul
      CALL ARY_UNMAP (IARY1, STATUS)
      CALL NDF_ANNUL (IN_NDF, STATUS)
      CALL ARY_UNMAP (IARY2, STATUS)
      CALL NDF_ANNUL (OUT_NDF, STATUS)

      CALL NDF_END (STATUS)

      END
