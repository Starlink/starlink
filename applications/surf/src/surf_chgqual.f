      SUBROUTINE REDS_CHGQUAL (STATUS)
*+
*  Name:
*     CHANGE_QUALITY

*  Purpose:
*     Routine to set SCUBA data quality bad or good.

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
*     This application is used to set SCUBA data quality bad or good.
*        The application will read from parameter IN the specification
*     of the data whose quality are to be modified. The specification must
*     have the format:-
*
*         ndf{data-spec}
*
*     where <ndf> is the name of the ndf holding the data and the 
*     <data-spec> specifies the subset of data that is to be modified.
*       The <data-spec> will be of the form {component;component;...}, 
*     where each <component> is one of the following:-
*
*         Bindex_spec   - specifying bolometer indices
*         Pindex_spec   -            position indices
*         Eindex_spec   -            exposure indices
*         Iindex_spec   -            integration indices
*         Mindex_spec   -            measurement indices
*
*     and the <index_spec> is a list like, for example, 2,5:7,17 to select
*     indices 2, 5 through 7, and 17. Alternatively, <index_spec> can be *
*     which will select all data in that component coordinate.
*       By default all components in a dataset are selected. Thus the
*     empty data-spec {} will return all components selected. 
*       Example complete specifications are:-
*
*      mars{}                    select all data in the ndf 'mars'
*      map1{B7,12;P57}           select data for bolometers 7 and 12 at
*                                measurement position 57 in ndf 'map1'
*      flat{E1;I3:M2}            select data for all bolometers in
*                                exposure 1 of integration 3 in 
*                                measurement 2 of the observation
*                                in ndf 'flat'
*      phot{B29}                 select all data for bolometer 29 in
*                                ndf 'phot'
*      scan{B29;E1}              select data for bolometer 29 in the
*                                first exposure of each integration in
*                                ndf 'scan'
*
*     The specification is case-insensitive and blanks are ignored.
*       Once the data specification has been decoded the application will
*     read from parameter BAD_QUALITY whether quality should be set good
*     or bad. A `yes' answer will mark the area bad, a `no' answer will
*     mark the area good (an area will only be good if no other QUALITY 
*     bits are set - CHANGE_QUALITY only uses QUALITY bit 3).

*  Usage:
*     change_quality in=mars{b7} bad_quality=yes

*  ADAM Parameters:
*     BAD_QUALITY = _LOGICAL (Read)
*         Set quality to BAD. Answering this question with a 'yes' will
*         mean that the selected data will be set to BAD. 'no'
*         will set them to good.
*     IN = _CHAR (Read)
*         Specification of data set to change.
*     MSG_FILTER = _CHAR (Read)
*         Message filter level. Default is NORM.

*  Notes:
*     Samples are marked bad by setting bit 3 of the quality array. The effects
*     of CHANGE_QUALITY  can be removed by changing the value of the bad 
*     bit mask (with the KAPPA task SETBB) so that bit 3 (decimal value of 8) 
*     is no longer used as a masking bit.

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
      INCLUDE 'DAT_PAR'                   ! for DAT__SZLOC
      INCLUDE 'PRM_PAR'                   ! for VAL__NBx
      INCLUDE 'REDS_SYS'                  ! REDS constants
      INCLUDE 'MSG_PAR'                   ! MSG__ constants

*    Import :
*    Import-Export :
*    Export :
*    Status :
      INTEGER STATUS
*    External references :
      BYTE SCULIB_BITON                   ! function to set a specified
                                          ! bit in a byte 
*    Global variables :
*    Local Constants :
      INTEGER          MAX__DIM           ! max number of dimensions in
      PARAMETER (MAX__DIM = 4)            ! array
      CHARACTER * 14   TSKNAME            ! Name of task
      PARAMETER (TSKNAME = 'CHANGE_QUALITY')
      INTEGER          BITNUM             ! Bit affected by this task
      PARAMETER (BITNUM = 3)

*    Local variables :
      BYTE             BADBIT             ! NDF badbit mask
      INTEGER          BIT_VALUE          ! value to which quality bit is set
      LOGICAL          BAD_QUALITY        ! .TRUE. if quality to be set
                                          ! bad, .FALSE. if to be set good
      INTEGER          BOL_S (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                          ! array containing 1 for bolometers
                                          ! selected in data-spec, 0 otherwise
      CHARACTER*80     DATA_SPEC(SCUBA__MAX_SECT) ! data-spec part of IN
      INTEGER          DIM (MAX__DIM)     ! dimensions of array
      INTEGER          EXP_S (SCUBA__MAX_EXP)   ! array containing 1 for
                                          ! exposures selected in
                                          ! data-spec, 0 otherwise
      CHARACTER*80     FILE               ! ndf name part of IN
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                          ! array of FITS keywords
      LOGICAL          FLATFIELD          ! .TRUE. if the FLATFIELD
                                          ! application has been run on
                                          ! the input file
      INTEGER          GOOD               ! good status
      INTEGER          I                  ! DO loop index
      CHARACTER*80     IN                 ! input filename and data-spec
      INTEGER          INT_S (SCUBA__MAX_INT)   ! array containing 1 for
                                          ! integration selected by
                                          ! data-spec, 0 otherwise
      INTEGER          IN_DEM_PNTR_PTR    ! pointer to .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC ! HDS locator of .FITS
                                          ! extension
      INTEGER          IN_NDF             ! NDF index of input file
      INTEGER          IN_QUALITY_PTR     ! pointer to QUALITY array
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                          ! HDS locator of .SCUBA extension
      INTEGER          ITEMP              ! scratch integer
      INTEGER          MEAS_S (SCUBA__MAX_MEAS) ! array containing 1 for
                                          ! measurement selected by
                                          ! data-spec, 0 otherwise
      INTEGER          NDIM               ! number of dimensions in array
      INTEGER          NREC               ! number of history records in
                                          ! input file
      INTEGER          N_BEAM             ! the 'beam' dimension of the
                                          ! data array
      INTEGER          N_BOLS             ! number of bolometers
                                          ! measured in observation
      INTEGER          N_EXPOSURES        ! number of exposures per
                                          ! integration
      INTEGER          N_FITS             ! number of items in FITS array
      INTEGER          N_INTEGRATIONS     ! number of integrations in
                                          ! measurement
      INTEGER          N_MEASUREMENTS     ! number of measurements in
                                          ! observation
      INTEGER          N_POS              ! number of positions measured
                                          ! in observation
      INTEGER          N_SPEC             ! Number of specifications
      CHARACTER*40     OBJECT             ! name of observed object
      CHARACTER*40     OBSERVING_MODE     ! observing mode of file
      LOGICAL          PHOTOM             ! .TRUE. if the PHOTOM
                                          ! application has been run
                                          ! on the input file
      LOGICAL          POS_SELECTED       ! .TRUE. if selected data were
                                          ! specified by P=....
      INTEGER          POS_S_END          ! end of VM holding POS_S
      INTEGER          POS_S_PTR          ! start of VM holding POS_S
      LOGICAL          REBIN              ! .TRUE. if the REBIN
                                          ! application has been run on
                                          ! the input file
      LOGICAL          REDUCE_SWITCH      ! .TRUE. if the REDUCE_SWITCH
                                          ! application has been run on
                                          ! the input file
      INTEGER          RUN_NUMBER         ! run number of input file
      CHARACTER*80     STEMP              ! scratch string
      LOGICAL          SWITCH_EXPECTED    ! .TRUE. if switch is to be
                                          ! specified in data-spec
      INTEGER          SWITCH_S (SCUBA__MAX_SWITCH)
                                          ! array that has 1 for
                                          ! switches selected by
                                          ! data-spec, 0 otherwise
      LOGICAL          USE_SECT           ! Am I using the section or not?
*     Internal References :
*     Local data :
*     .

      IF (STATUS .NE. SAI__OK) RETURN

*     Set the MSG output level (for use with MSG_OUTIF)
      CALL MSG_IFGET('MSG_FILTER', STATUS)

*     initialise some flags and locators

      IN_FITSX_LOC = DAT__NOLOC
      IN_SCUBAX_LOC = DAT__NOLOC

*     start up the NDF system and open the demodulated data file

      CALL NDF_BEGIN

*     read the filename and data-spec, separate them

      CALL PAR_GET0C ('IN', IN, STATUS)

*     Split up into filename and data spec
      CALL SCULIB_SPLIT_FILE_SPEC(IN, SCUBA__MAX_SECT, FILE, N_SPEC,
     :     DATA_SPEC, STATUS)

*     open the data NDF

      CALL NDF_OPEN (DAT__ROOT, FILE, 'UPDATE', 'OLD', IN_NDF, ITEMP,
     :  STATUS)

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

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)

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
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     '^PKG: run ^RUN was a ^MODE observation of ^OBJECT',
     :     STATUS)

*     read the number of bolometers used

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'N_BOLS', N_BOLS, STATUS)

*     map in the DEM_PNTR array and get its dimensions

      CALL SCULIB_GET_DEM_PNTR (3, IN_SCUBAX_LOC, IN_DEM_PNTR_PTR,
     :  ITEMP, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, STATUS)

*     check there aren't too many measurements, integrations, exposures,
*     switches for this routine

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_MEASUREMENTS .GT. SCUBA__MAX_MEAS) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('N', N_MEASUREMENTS)
            CALL ERR_REP (' ', '^TASK: too many measurements - ^N',
     :        STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_INTEGRATIONS .GT. SCUBA__MAX_INT) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('N', N_INTEGRATIONS)
            CALL ERR_REP (' ', '^TASK: too many integrations - ^N',
     :        STATUS)
         END IF
      END IF

      IF (STATUS .EQ. SAI__OK) THEN
         IF (N_EXPOSURES .GT. SCUBA__MAX_EXP) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('N', N_EXPOSURES)
            CALL ERR_REP (' ', '^TASK: too many exposures - ^N',
     :        STATUS)
         END IF
      END IF

*     map the quality array and check its dimensions

      CALL NDF_DIM (IN_NDF, MAX__DIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (IN_NDF, 'QUALITY', '_UBYTE', 'UPDATE', 
     :  IN_QUALITY_PTR, ITEMP, STATUS)

      N_POS = DIM (2)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM') THEN
            N_BEAM = DIM (3)
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .NE. N_BOLS)           .OR.
     :          (DIM(2) .LT. 1)                .OR.
     :          (DIM(3) .NE. SCUBA__MAX_BEAM)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETI ('DIM3', DIM(3))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 '//
     :           '^DIM2 ^DIM3', STATUS)
            END IF
         ELSE
            N_BEAM = 1
            IF ((NDIM .NE. 2)        .OR.
     :          (DIM(1) .NE. N_BOLS) .OR.
     :          (DIM(2) .LT. 1))     THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: main data '//
     :           'array has bad dimensions - (^NDIM) ^DIM1 '//
     :           '^DIM2', STATUS)
            END IF
         END IF
      END IF

      CALL MSG_SETI ('NBOLS', N_BOLS)
      CALL MSG_SETI ('NPOS', N_POS)
      CALL MSG_SETC('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM,' ', '^PKG: file has data for ^NBOLS '//
     :  'bolometers, measured at ^NPOS positions.', STATUS)
      CALL MSG_SETI ('NMEAS', N_MEASUREMENTS)
      CALL MSG_SETI ('NINT', N_INTEGRATIONS)
      CALL MSG_SETI ('NEXP', N_EXPOSURES)
      CALL MSG_OUTIF (MSG__NORM, ' ', 
     :     ' - there are data for ^NEXP exposure(s) '//
     :     'in ^NINT integration(s) in ^NMEAS measurements.', STATUS)

*     allocate memory for the POS_S array

      POS_S_PTR = 0
      CALL SCULIB_MALLOC (N_POS * VAL__NBI, POS_S_PTR, POS_S_END,
     :  STATUS)

*     Do I want to change the section or the non-sections?

      CALL PAR_GET0L ('USE_SECTION', USE_SECT, STATUS)

*     do we want to set quality bad or good

      CALL PAR_GET0L ('BAD_QUALITY', BAD_QUALITY, STATUS)
      CALL PAR_CANCL ('BAD_QUALITY', STATUS)

      IF (BAD_QUALITY) THEN
         BIT_VALUE = 1
      ELSE
         BIT_VALUE = 0
      END IF

*     set the badbit mask to include bit 3 - the one set by this routine, then
*     modify the quality array

      CALL NDF_BB (IN_NDF, BADBIT, STATUS)
      BADBIT = SCULIB_BITON (BADBIT, BITNUM)
      CALL NDF_SBB (BADBIT, IN_NDF, STATUS)

*     and set quality for the selected bolometers and positions

      IF (STATUS .EQ. SAI__OK) THEN

*     decode each data specification

         SWITCH_EXPECTED = .FALSE.

         DO I = 1, N_SPEC

            CALL SCULIB_DECODE_SPEC (DATA_SPEC(I), 
     :           %val(IN_DEM_PNTR_PTR),
     :           1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :           N_BOLS, SWITCH_EXPECTED, POS_SELECTED, %val(POS_S_PTR),
     :           SWITCH_S, EXP_S, INT_S, MEAS_S, BOL_S, STATUS)
            
            CALL SCULIB_SET_QUAL (USE_SECT,%val(IN_QUALITY_PTR), N_BOLS,
     :           N_POS, N_BEAM, BOL_S, %val(POS_S_PTR), BITNUM, 
     :           BIT_VALUE, STATUS)

         END DO
      END IF

*     close file and tidy up

      IF (IN_FITSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      END IF
      IF (IN_SCUBAX_LOC .NE. DAT__NOLOC) THEN
         CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
         CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL(IN_NDF, STATUS)
      CALL NDF_END (STATUS)

      GOOD = SAI__OK
      CALL SCULIB_FREE ('POS_S', POS_S_PTR, POS_S_END, GOOD)

      END
