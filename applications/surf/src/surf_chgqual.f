      SUBROUTINE REDS_CHGQUAL (STATUS)
*+
*  Name:
*     CHANGE_QUALITY

*  Purpose:
*     Routine to flag bad SCUBA data.

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL REDS_CHGQUAL( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This application is used to flag bad SCUBA data. The quality of the
*     data can be marked as good or bad.
*        The application will prompt for the name of the demodulated data file
*     to be modified and check that the file does actually contain demodulated
*     data; if it doesn't an error message will be output and the file will
*     be closed unchanged. Otherwise, the type of observation and name of the
*     observed object will be reported, together with the local sidereal times
*     of the start and finish of the observation.

*     This task is designed such
*     that specified bolometers in a given integration can be marked bad.
*     You will first be asked for a list of integrations that are to be
*     changed and then a list of bolometers. A `-1' response to either of these
*     questions will abort the action. A `0'(zero) response will imply all 
*     integrations or all bolometers. For example, specifying `1' for 
*     INTEGRATIONS and `0' for BOLOMETERS will select the whole of integration
*     1. Conversely, specifying '0' for INTEGRATIONS and `1' for BOLOMETERS
*     will select bolometer 1 for ALL integrations. Finally, setting
*     INTEGRATIONS to `1,4' and BOLOMETERS to `[c14,a1]' will select the first
*     and fourth integrations of bolometers C14 and A1 (note that either
*     number or bolometer name can be used to specify a bolometer). Once a
*     specific integration/bolometer has been selected the area can be marked
*     either good or bad depending on the response to BAD_QUALITY. A `yes'
*     answer will mark the area bad, a `no' answer will mark the area good
*     (an area will only be good if no other QUALITY bits are set - MODIFY
*     only uses QUALITY bit 3).

*  Usage:
*     change_quality in

*  ADAM Parameters:
*     BAD_QUALITY = _LOGICAL (Read)
*         Set quality to BAD. Answering this question with a 'yes' will
*         mean that the integrations/bolometers specified by the BOLOMETERS,
*         INTEGRATIONS and MEASUREMENTS parameters will be set to BAD. 'no'
*         will set them to good.
*     BOLOMETERS = _CHAR (Read)
*         List of bolometers to change quality of. Zero indicates that all
*         bolometers will be affected.
*     IN = NDF (Read)
*         Name of NDF to change.
*     INTEGRATIONS = _INTEGER (Read)
*         The integrations to change. Zero indicates that all integrations
*         are to be changed.
*     MEASUREMENTS = _INTEGER (Read)
*         The measurements to change.

*  Notes:
*     Samples are marked bad by setting bit 3 of the quality array. The effects
*     of MODIFY can be removed by changing the value of the bad bit mask
*     (with the KAPPA task SETBB) so that bit 3 (decimal value of 8) is no 
*     longer used as a masking bit.

*  Related Application:
*     REBIN, SCUPHOT

*  Authors:
*     JFL:  J.Lightfoot (jfl@roe.ac.uk)
*     TIMJ: T.Jenness   (timj@jach.hawaii.edu)

*  History:
*     $Id$
*     {enter_further_changes_here}
 
*  Bugs:
*     {note_any_bugs_here}

*-

*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'REDS_SYS'                         ! REDS constants
*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      BYTE SCULIB_BITON                          ! function to set a specified
                                                 ! bit in a byte 
*    Global variables :
*    Local Constants :
      INTEGER          MAX__BOL                  ! max number of bolometers
      PARAMETER (MAX__BOL = 20)                  ! that can be specified
      INTEGER          MAX__DIM                  ! max number of dimensions in
      PARAMETER (MAX__DIM = 4)                   ! array
      INTEGER          MAX__INT                  ! max number of integrations 
      PARAMETER (MAX__INT = 20)                  ! that can be specified
      INTEGER          MAX__INTS                 ! max number of integrations 
      PARAMETER (MAX__INTS = 200)                 ! in a file
      INTEGER          MAX__MEAS                 ! max number of measurements
      PARAMETER (MAX__MEAS = 20)                 ! that can be specified
      CHARACTER * 14   TSKNAME                   ! Name of task
      PARAMETER (TSKNAME = 'CHANGE_QUALITY')

*    Local variables :
      INTEGER          B                         ! DO loop index
      BYTE             BADBIT                    ! NDF badbit mask
      INTEGER          BADINT                    ! Loop counter
      INTEGER          BADMEAS                   ! Loop counter
      INTEGER          BAD_ADC                   ! ADC number of bolometer
                                                 ! whose data are to be set bad
                                                 ! be set bad
      INTEGER          BAD_CHAN                  ! channel number of bolometer
                                                 ! whose data are to be set bad
      LOGICAL          BAD_QUALITY               ! .TRUE. if the specified
                                                 ! quality is to be set bad
      INTEGER          BB                        ! DO loop index
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                                 ! ADC numbers of bolometers
                                                 ! used in the observation
      CHARACTER*3      BOL_BAD (MAX__BOL)        ! indices or names of 
                                                 ! bolometers whose data are to
                                                 ! be set bad
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                                 ! channel numbers of
                                                 ! bolometers used in the 
                                                 ! observation
      LOGICAL          CHANGE_BAD                ! .TRUE. if the user wants
                                                 ! to flag bad data
      INTEGER          CHANGE_INT                ! Index of integration to change
      CHARACTER *(80)  CTEMP                     ! scratch character
      INTEGER          DIM (MAX__DIM)            ! dimensions of array
      INTEGER          END_BOL                   ! last bolometer with data
                                                 ! to be set bad
      INTEGER          END_INT                   ! last integer with data to
                                                 ! be set bad
      INTEGER          END_MEAS                  ! last measurement with data
                                                 ! to be set bad
      INTEGER          EXPOSURE                  ! exposure number
      CHARACTER*80     FITS (SCUBA__MAX_FITS)    ! array of FITS keywords
      LOGICAL          FLATFIELD                 ! .TRUE. if the FLATFIELD
                                                 ! application has been run on
                                                 ! the input file
      INTEGER          I                         ! DO loop index
      INTEGER          INTEGRATION               ! integration number
      INTEGER          INT_BAD (MAX__INT)        ! numbers of integrations that
                                                 ! contain data to be set bad
      CHARACTER*(80)   INT_LIST                  ! String of bad ints
      INTEGER          IN_DEM_PNTR_ARY           ! array identifier to
                                                 ! .SCUBA.DEM_PNTR
      INTEGER          IN_DEM_PNTR_PTR           ! pointer to .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC        ! HDS locator of .FITS
                                                 ! extension
      INTEGER          IN_NDF                    ! NDF index of input file
      INTEGER          IN_QUALITY_PTR            ! pointer to QUALITY array
      CHARACTER*(DAT__SZLOC) IN_REDSX_LOC        ! HDS locator of .REDS
                                                 ! extension
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC       ! HDS locator of .SCUBA
                                                 ! extension
      INTEGER          INT_QUALITY(MAX__INTS)    ! List of changed quality ints
      INTEGER          IPOSN                     ! position in string
      INTEGER          ITEMP                     ! scratch integer
      INTEGER          ITEMP2                    ! scratch integer 2
      INTEGER          M                         ! DO loop index
      INTEGER          MEASUREMENT               ! measurement number
      INTEGER          MEAS_BAD (MAX__MEAS)      ! numbers of measurements that
                                                 ! contain data to be set bad
      INTEGER          NDIM                      ! number of dimensions in 
                                                 ! array
      INTEGER          NREC                      ! number of history records
                                                 ! in input file
      INTEGER          N_BEAM                    ! the 'beam' dimension of the
                                                 ! data array
      INTEGER          N_BOLS                    ! number of bolometers
                                                 ! measured in observation
      INTEGER          N_BOL_BAD                 ! the number of bolometers
                                                 ! with data to be set bad
      INTEGER          N_EXPOSURES               ! number of exposures per
                                                 ! integration
      INTEGER          N_FITS                    ! number of items in FITS
                                                 ! array
      INTEGER          N_INTEGRATIONS            ! number of integrations in
                                                 ! measurement
      INTEGER          N_INT_BAD                 ! the number of integrations
                                                 ! with data to be set bad
      INTEGER          N_MEASUREMENTS            ! number of measurements in
                                                 ! observation
      INTEGER          N_MEAS_BAD                ! the number of measurements
                                                 ! with data to be set bad
      INTEGER          N_POS                     ! number of positions measured
                                                 ! in observation
      CHARACTER*40     OBJECT                    ! name of observed object
      CHARACTER*40     OBSERVING_MODE            ! observing mode of file
      LOGICAL          PHOTOM                    ! .TRUE. if the PHOTOM
                                                 ! application has been run
                                                 ! on the input file
      INTEGER          QUALITY_BIT ! the value to which the 3rd
                                ! quality bit is to be set
      LOGICAL          QUALITY_MAPPED ! .TRUE. if the QUALITY array
                                ! has been mapped
      LOGICAL          REBIN    ! .TRUE. if the REBIN
                                ! application has been run on
                                ! the input file
      LOGICAL          REDUCE_SWITCH ! .TRUE. if the REDUCE_SWITCH
                                ! application has been run on
                                ! the input file
      INTEGER          RUN_NUMBER ! run number of input file
      INTEGER          START_BOL ! first bolometer with data
                                ! to be set bad
      INTEGER          START_INT ! first integer with data to
                                ! be set bad
      INTEGER          START_MEAS ! first measurement with data
                                ! to be set bad
      CHARACTER*80     STEMP    ! scratch string
      INTEGER          SWITCH_END ! data index of last point in
                                ! switch
      INTEGER          SWITCH_START ! data index of first point in
                                ! switch
      INTEGER          TOTAL_INTS ! Total number of integrations
*     Internal References :
*     Local data :
*     .

      IF (STATUS .NE. SAI__OK) RETURN

*     initialise some flags and locators

      QUALITY_MAPPED = .FALSE.
      CHANGE_BAD = .TRUE.

      IN_FITSX_LOC = DAT__NOLOC
      IN_SCUBAX_LOC = DAT__NOLOC
      IN_REDSX_LOC = DAT__NOLOC

*     start up the NDF system and open the demodulated data file

      CALL NDF_BEGIN

      CALL NDF_ASSOC ('IN', 'UPDATE', IN_NDF, STATUS)

*     check that the history of the file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         FLATFIELD = .FALSE.
         PHOTOM = .FALSE.
         REBIN = .FALSE.
         REDUCE_SWITCH = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'FLATFIELD') THEN
                  FLATFIELD = .TRUE.
               ELSE IF (STEMP .EQ. 'PHOTOM') THEN
                  PHOTOM = .TRUE.
               ELSE IF (STEMP .EQ. 'REBIN') THEN
                  REBIN = .TRUE.
               ELSE IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file has not '//
     :              'been through the REDUCE_SWITCH application', 
     :              STATUS)
            END IF
            IF (PHOTOM) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file contains '//
     :              'PHOTOM data that has already been reduced', STATUS)
            END IF
            IF (REBIN) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK',TSKNAME)
               CALL ERR_REP (' ', '^TASK: this file contains '//
     :              'data that has already been rebinned', STATUS)
            END IF
         END IF
      END IF

*     get some locators

      CALL NDF_XLOC (IN_NDF, 'FITS', 'UPDATE', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'UPDATE', IN_SCUBAX_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_XLOC (IN_NDF, 'REDS', 'UPDATE', IN_REDSX_LOC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            IN_REDSX_LOC = DAT__NOLOC
         END IF
      END IF
      
*     and read in some parameters describing the observation
      
      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: input file contains too '//
     :           'many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, N_FITS,
     :     STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS, 'RUN',
     :     RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'OBJECT',
     :     OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'MODE',
     :     OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUT (' ', '^PKG: run ^RUN was a ^MODE observation of '//
     :     '^OBJECT', STATUS)



*     read the number and A/D,channel of the bolometers used

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'N_BOLS', N_BOLS, STATUS)

      CALL CMP_GET1I(IN_SCUBAX_LOC, 'BOL_CHAN', 
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_CHAN, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: dimension of '//
     :           '.SCUBA.BOL_CHAN does not match main data array',
     :           STATUS)
         END IF
      END IF

      CALL CMP_GET1I(IN_SCUBAX_LOC, 'BOL_ADC', 
     :     SCUBA__NUM_CHAN * SCUBA__NUM_ADC, BOL_ADC, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (ITEMP .NE. N_BOLS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: dimension of '//
     :           '.SCUBA.BOL_ADC does not match main data array',
     :           STATUS)
         END IF
      END IF

*     map in the DEM_PNTR array and get its dimensions

      CALL ARY_FIND (IN_SCUBAX_LOC, 'DEM_PNTR', IN_DEM_PNTR_ARY,
     :     STATUS)
      CALL ARY_DIM (IN_DEM_PNTR_ARY, MAX__DIM, DIM, NDIM, STATUS)
      CALL ARY_MAP (IN_DEM_PNTR_ARY, '_INTEGER', 'READ',
     :     IN_DEM_PNTR_PTR, ITEMP, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (NDIM .NE. 3) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: .SCUBA.DEM_PNTR '//
     :           'array has bad number of dimensions', STATUS)
         ELSE
            N_EXPOSURES = DIM (1)
            N_INTEGRATIONS = DIM (2)
            N_MEASUREMENTS = DIM (3)

            IF (DIM(1) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM', DIM(1))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: .SCUBA.DEM_PNTR '//
     :              'array has bad number of exposures - ^DIM',
     :              STATUS)
            END IF
            IF (DIM(2) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM', DIM(2))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: .SCUBA.DEM_PNTR '//
     :              'array has bad number of integrations - ^DIM',
     :              STATUS)
            END IF
            IF (DIM(3) .LE. 0) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('DIM', DIM(3))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: .SCUBA.DEM_PNTR '//
     :              'array has bad number of measurements - ^DIM', 
     :              STATUS)
            END IF
         END IF
      END IF

      CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
      CALL MSG_SETI ('NINT', N_INTEGRATIONS)
      CALL MSG_SETI ('NEXP', N_EXPOSURES)
      CALL MSG_SETC('PKG', PACKAGE)
      CALL MSG_OUT (' ', '^PKG: data array has ^NEXP exposure(s) '//
     :     'in ^NINT integration(s) in ^NMEAS measurements', STATUS)

*     Total number of integrations
      TOTAL_INTS = N_MEASUREMENTS * N_INTEGRATIONS

*     Read in current state of integrations (stored in REDS extension)
*     Find INT_QUALITY extension
      DO I = 1, TOTAL_INTS
         INT_QUALITY (I) = 0
      END DO
      
      IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
         CALL CMP_GET1I(IN_REDSX_LOC, 'INT_QUALITY',TOTAL_INTS,
     :        INT_QUALITY, ITEMP, STATUS)

*     Write out list of flagged integrations
         IF (ITEMP .GT. 0) THEN
            INT_LIST = ''
            IPOSN = 0
            DO I = 1, ITEMP
               IF (INT_QUALITY(I) .EQ. 1) THEN
                  IF (IPOSN.GT.0) CALL CHR_APPND(',',INT_LIST, IPOSN)
                  CALL CHR_ITOC(I, CTEMP, ITEMP2)
                  CALL CHR_APPND(CTEMP, INT_LIST, IPOSN)
               END IF
            END DO
            IF (INT_LIST .NE. '') THEN
               CALL MSG_SETC('BADINT', INT_LIST)
               CALL MSG_SETC('PKG', PACKAGE)
               CALL MSG_OUT(' ','^PKG: Flagged integrations: '//
     :              '^BADINT', STATUS)
            END IF
         END IF
         IF (STATUS .NE. SAI__OK)  CALL ERR_ANNUL(STATUS)
      END IF
      
*     map the quality array and check its dimensions

      IF (.NOT. QUALITY_MAPPED) THEN
         CALL NDF_DIM (IN_NDF, MAX__DIM, DIM, NDIM, STATUS)
         CALL NDF_MAP (IN_NDF, 'QUALITY', '_UBYTE', 'UPDATE', 
     :        IN_QUALITY_PTR, ITEMP, STATUS)
         IF (STATUS .EQ. SAI__OK) THEN
            QUALITY_MAPPED = .TRUE.
         END IF

         N_POS = DIM (2)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
               N_BEAM = DIM (3)
               IF ((NDIM .NE. 3)                  .OR.
     :              (DIM(1) .NE. N_BOLS)           .OR.
     :              (DIM(2) .LT. 1)                .OR.
     :              (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL MSG_SETI ('DIM3', DIM(3))
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL ERR_REP (' ', '^TASK: main data '//
     :                 'array has bad dimensions - (^NDIM) ^DIM1 '//
     :                 '^DIM2 ^DIM3', STATUS)
               END IF
            ELSE
               N_BEAM = 1
               IF ((NDIM .NE. 2)        .OR.
     :              (DIM(1) .NE. N_BOLS) .OR.
     :              (DIM(2) .LT. 1))     THEN
                  STATUS = SAI__ERROR
                  CALL MSG_SETI ('NDIM', NDIM)
                  CALL MSG_SETI ('DIM1', DIM(1))
                  CALL MSG_SETI ('DIM2', DIM(2))
                  CALL MSG_SETC('TASK', TSKNAME)
                  CALL ERR_REP (' ', '^TASK: main data '//
     :                 'array has bad dimensions - (^NDIM) ^DIM1 '//
     :                 '^DIM2', STATUS)
               END IF
            END IF
         END IF
      END IF

*     loop, reading the measurement, integration and bolometer IDs of the data 
*     whose quality are to be changed, exit the loop on negative input

      DO WHILE ((STATUS .EQ. SAI__OK) .AND. CHANGE_BAD)

*     read number(s) of measurements whose data quality are to be changed

         IF (N_MEASUREMENTS .EQ. 1) THEN
            N_MEAS_BAD = 1
            MEAS_BAD (1) = 1
         ELSE
            CALL PAR_DEF1I ('MEASUREMENTS', 1, -1, STATUS)
            CALL PAR_GET1I ('MEASUREMENTS', MAX__MEAS, MEAS_BAD,
     :           N_MEAS_BAD, STATUS)
            CALL PAR_CANCL ('MEASUREMENTS', STATUS)
            IF ((N_MEAS_BAD .GE. 1) .AND. (MEAS_BAD(1) .LT. 0)) THEN
               CHANGE_BAD = .FALSE.
            END IF
         END IF

*     read number(s) of integrations whose data quality are to be changed

         IF (CHANGE_BAD) THEN
            IF (N_INTEGRATIONS .EQ. 1) THEN
               N_INT_BAD = 1
               INT_BAD (1) = 1
            ELSE
               CALL PAR_DEF1I ('INTEGRATIONS', 1, -1, STATUS)
               CALL PAR_GET1I ('INTEGRATIONS', MAX__INT, INT_BAD,
     :              N_INT_BAD, STATUS)
               CALL PAR_CANCL ('INTEGRATIONS', STATUS)
               IF ((N_INT_BAD .GE. 1) .AND. (INT_BAD(1) .LT. 0)) THEN
                  CHANGE_BAD = .FALSE.
               END IF
            END IF
         END IF

*     read the bolometer(s) whose data quality are to be changed

         IF (CHANGE_BAD) THEN
            IF (N_BOLS .EQ. 1) THEN
               N_BOL_BAD = 1
               BOL_BAD (1) = '1'
            ELSE
               CALL PAR_DEF1C ('BOLOMETERS', 1, '-1', STATUS)
               CALL PAR_GET1C ('BOLOMETERS', MAX__BOL, BOL_BAD,
     :              N_BOL_BAD, STATUS)
               CALL PAR_CANCL ('BOLOMETERS', STATUS)
               IF ((N_BOL_BAD .GE. 1) .AND. (BOL_BAD(1) .EQ. '-1'))
     :              THEN
                  CHANGE_BAD = .FALSE.
               END IF
            END IF
         END IF

*     do we want to set quality bad or good

         IF (CHANGE_BAD) THEN
            CALL PAR_GET0L ('BAD_QUALITY', BAD_QUALITY, STATUS)
            CALL PAR_CANCL ('BAD_QUALITY', STATUS)
            IF (BAD_QUALITY) THEN
               QUALITY_BIT = 1
            ELSE
               QUALITY_BIT = 0
            END IF
         END IF

*     set the badbit mask to include bit 3 - the one set by this routine, then
*     modify the quality array

         IF (CHANGE_BAD .AND. (STATUS .EQ. SAI__OK)) THEN

*     Find current bad bit mask and then set bit 3
            CALL NDF_BB (IN_NDF, BADBIT, STATUS)
            BADBIT = SCULIB_BITON (BADBIT, 3)
            CALL NDF_SBB (BADBIT, IN_NDF, STATUS)

*     Loop through measurements and integrations
            DO M = 1, N_MEAS_BAD
               START_MEAS = -1

               IF (MEAS_BAD(M) .EQ. 0) THEN
                  START_MEAS = 1
                  END_MEAS = N_MEASUREMENTS
               ELSE IF ((MEAS_BAD(M) .GE. 1)              .AND.
     :                 (MEAS_BAD(M) .LE. N_MEASUREMENTS)) THEN
                  START_MEAS = MEAS_BAD (M)
                  END_MEAS = MEAS_BAD (M)
               END IF 

               DO I = 1, N_INT_BAD
                  START_INT = -1

                  IF (INT_BAD(I) .EQ. 0) THEN
                     START_INT = 1
                     END_INT = N_INTEGRATIONS
                  ELSE IF ((INT_BAD(I) .GE. 1)              .AND. 
     :                    (INT_BAD(I) .LE. N_INTEGRATIONS)) THEN
                     START_INT = INT_BAD (I)
                     END_INT = INT_BAD (I)
                  END IF

                  DO B = 1, N_BOL_BAD
                     IF (BOL_BAD(B) .EQ. '0') THEN
                        DO BADMEAS = START_MEAS, END_MEAS
                           DO BADINT = START_INT, END_INT
                              CHANGE_INT = BADINT +
     :                             ((BADMEAS -1) * N_INTEGRATIONS)
                              INT_QUALITY(CHANGE_INT) = 
     :                             INT(QUALITY_BIT)
                           END DO
                        END DO
                     END IF
                  END DO

                  DO B = 1, N_BOL_BAD
                     START_BOL = -1

                     IF (BOL_BAD(B) .EQ. '0') THEN
                        START_BOL = 1
                        END_BOL = N_BOLS
                     ELSE

*     attempt to interpret the bolometer string first as a data index then as
*     a bolometer name

                        IF (STATUS .EQ. SAI__OK) THEN
                           CALL CHR_CTOI (BOL_BAD(B), END_BOL,
     :                          STATUS)
                           IF (STATUS .EQ. SAI__OK) THEN
                              START_BOL = END_BOL
                           ELSE
                              CALL ERR_ANNUL (STATUS)
                              CALL SCULIB_BOLDECODE (BOL_BAD(B),
     :                             BAD_ADC, BAD_CHAN, STATUS)

*     search for the bolometer in the index

                              IF (STATUS .EQ. SAI__OK) THEN

                                 DO BB = 1, N_BOLS
                                    IF ((BAD_ADC .EQ. BOL_ADC(BB))
     :                                   .AND.
     :                                   (BAD_CHAN .EQ. BOL_CHAN(BB)))
     :                                   THEN
                                       START_BOL = BB
                                       END_BOL = BB
                                    END IF
                                 END DO

                              ELSE
                                 CALL ERR_ANNUL (STATUS)
                              END IF
                           END IF
                        END IF
                     END IF

*     OK, set quality for the selected data

                     IF ((START_MEAS .NE. -1) .AND.
     :                    (START_INT .NE. -1)  .AND.
     :                    (START_BOL .NE. -1)) THEN

                        DO MEASUREMENT = START_MEAS, END_MEAS
                           DO INTEGRATION = START_INT, END_INT 
                              DO EXPOSURE = 1, N_EXPOSURES

*     find the start and end indices of each switch in the data array

                                 CALL SCULIB_FIND_SWITCH (
     :                                %val(IN_DEM_PNTR_PTR),
     :                                1, N_EXPOSURES, 
     :                                N_INTEGRATIONS,
     :                                N_MEASUREMENTS, N_POS, 
     :                                1, EXPOSURE,
     :                                INTEGRATION, MEASUREMENT, 
     :                                SWITCH_START, SWITCH_END, 
     :                                STATUS)

*     and set quality for the selected bolometers in that switch

                                 CALL SCULIB_SET_QUALITY (
     :                                N_BOLS, N_POS, N_BEAM,
     :                                %val (IN_QUALITY_PTR),
     :                                START_BOL, END_BOL,
     :                                SWITCH_START, SWITCH_END,
     :                                1, N_BEAM, 3, QUALITY_BIT, 
     :                                STATUS)
                              END DO
                           END DO
                        END DO

                     END IF

                  END DO
               END DO
            END DO

         END IF

      END DO

*     Write out the INT_QUALITY array
      IF (IN_REDSX_LOC .EQ. DAT__NOLOC) THEN
         CALL NDF_XNEW (IN_NDF, 'REDS', 'REDS_EXTENSION',
     :        0, 0, IN_REDSX_LOC, STATUS)
      END IF

*     Make sure the component exists
      CALL CMP_MOD(IN_REDSX_LOC, 'INT_QUALITY','_INTEGER', 1, 
     :     TOTAL_INTS, STATUS)

      CALL CMP_PUT1I(IN_REDSX_LOC, 'INT_QUALITY', TOTAL_INTS, 
     :     INT_QUALITY, STATUS)



*     close file and tidy up

      IF (IN_FITSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      END IF
      IF (IN_SCUBAX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      END IF

      IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL(IN_NDF, STATUS)

      CALL NDF_END (STATUS)

      END
