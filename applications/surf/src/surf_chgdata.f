      SUBROUTINE REDS_CHGDATA (STATUS)
*+
*  Name:
*     CHANGE_DATA

*  Purpose:
*     Routine to set SCUBA data to any value.

*  Language:
*     Starlink Fortran 77
 
*  Type of Module:
*     ADAM A-task
 
*  Invocation:
*     CALL REDS_CHGDATA( STATUS )
 
*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status
 
*  Description:
*     This application is used to set SCUBA data to any value.
*        The application will read from parameter IN the specification
*     of the data whose values are to be modified. The specification must
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
*     read from parameter VALUE the value of the data that should be used.
*     All data specified by the section will be set to this value.

*  Usage:
*     change_data in=mars{b7} value=??

*  ADAM Parameters:
*     COMP = LITERAL (Read)
*        The name of the NDF array component which should be changed:
*        "Data","Error", "Quality" or "Variance" (where "Error" is the
*        alternative to "Variance" and causes the square root of the
*        variance values to be taken).
*        If "Quality" is specified, then the quality values are treated
*        as numerical values (in the range 0 to 255).
*     IN = _CHAR (Read)
*         Specification of data set to change.
*     MSG_FILTER = _CHAR (Read)
*         Message filter level. Default is NORM.
*     OUT = NDF (Write)
*         Name of the NDF that stores the modified data.
*     USE_SECTION = LOGICAL (Read)
*         This parameter specified whether the specified SCUBA section
*         should be changed (.TRUE.) or the data not specified by the
*         section (.FALSE.). Default is .TRUE.
*     VALUE = LITERAL (Read)
*         Value to which all selected data points should be set. A value
*         of 'bad' will set the data point to VAL__BAD (Starlink bad data
*         value).

*  Notes:
*     - This software sets the actual value in the specified component
*     and so, unlike CHANGE_QUALITY, is not reversible. 
*     For this reason a new output fileis created.
*     - This task does not attempt to create a component if the specified
*     component is missing. A Variance array can be created using the 
*     KAPPA task SETVAR if necessary.

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
      INTEGER CHR_LEN
      INTEGER SCULIB_BTOI                 ! Convert byte to integer*4

*    Global variables :
*    Local Constants :
      INTEGER          MAX__DIM           ! max number of dimensions in
      PARAMETER (MAX__DIM = 4)            ! array
      CHARACTER * 14   TSKNAME            ! Name of task
      PARAMETER (TSKNAME = 'CHANGE_DATA')

*    Local variables :
      INTEGER          BOL_S (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                          ! array containing 1 for bolometers
                                          ! selected in data-spec, 0 otherwise
      BYTE             BVALUE             ! Value of byte
      INTEGER          CURLY              ! index of { in IN
      CHARACTER*10     COMP               ! Selected component
      CHARACTER*80     COMPLIS            ! List of allowed components
      CHARACTER*80     DATA_SPEC          ! data-spec part of IN
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
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                          ! HDS locator of .SCUBA extension
      INTEGER          ITEMP              ! scratch integer
      LOGICAL          ISVAR              ! Is there a VARIANCE component
      LOGICAL          ISQUAL             ! Is there a QUALITY component
      INTEGER          IVALUE             ! Integer form of value
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
      CHARACTER*40     OBJECT             ! name of observed object
      CHARACTER*40     OBSERVING_MODE     ! observing mode of file
      INTEGER          OUT_DATA_PTR       ! Mapped output data
      INTEGER          OUT_NDF            ! Output NDF identifier
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
      CHARACTER*80     SVALUE             ! String version of new value
      CHARACTER*10     TYPE               ! Data type of component
      LOGICAL          USE_SECT           ! Use or not to use
      REAL             VALUE              ! New data value

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

      CURLY = INDEX (IN,'{')
      IF (STATUS .EQ. SAI__OK) THEN
         IF (CURLY .EQ. 0) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: no data section specified',
     :        STATUS)
         ELSE IF (CURLY .EQ. 1) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC ('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: no filename specified',
     :        STATUS)
         ELSE
            FILE = IN (:CURLY-1)
            DATA_SPEC = IN (CURLY:)
         END IF
      END IF

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
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', IN_SCUBAX_LOC, STATUS)
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

*     Get number of output data points
      CALL NDF_DIM (IN_NDF, MAX__DIM, DIM, NDIM, STATUS)
      N_POS = DIM (2)

*     allocate memory for the POS_S array

      POS_S_PTR = 0
      CALL SCULIB_MALLOC (N_POS * VAL__NBI, POS_S_PTR, POS_S_END,
     :  STATUS)

*     decode the data specification

      IF (STATUS .EQ. SAI__OK) THEN
         SWITCH_EXPECTED = .FALSE.

         CALL SCULIB_DECODE_SPEC (DATA_SPEC, %val(IN_DEM_PNTR_PTR),
     :     1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, N_POS,
     :     N_BOLS, SWITCH_EXPECTED, POS_SELECTED, %val(POS_S_PTR),
     :     SWITCH_S, EXP_S, INT_S, MEAS_S, BOL_S, STATUS)
      END IF
      
*     Now propogate the output file from the input and open it.

      CALL NDF_PROP (IN_NDF, 'Data,Variance,Quality,Axis', 'OUT', 
     :     OUT_NDF, STATUS)

*     close input file and tidy up

      IF (IN_FITSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)
      END IF
      IF (IN_SCUBAX_LOC .NE. DAT__NOLOC) THEN
         CALL CMP_UNMAP(IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
         CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL(IN_NDF, STATUS)

*     Find out whether Variance and Quality exist in output file

      CALL NDF_STATE(OUT_NDF, 'Variance', ISVAR, STATUS)
      CALL NDF_STATE(OUT_NDF, 'Quality', ISQUAL, STATUS)

*     Set up the choices
      COMPLIS = 'Data'   ! Always have data
      IF (ISVAR) CALL CHR_APPND(',Variance,Error', COMPLIS, 
     :     CHR_LEN(COMPLIS))
      
      IF (ISQUAL) CALL CHR_APPND(',Quality', COMPLIS, CHR_LEN(COMPLIS))

*     Now ask for the component of choice

      IF ( COMPLIS .EQ. 'Data' ) THEN
         COMP = 'DATA'
         TYPE = '_REAL'
      ELSE
         CALL PAR_CHOIC( 'COMP', 'Data', COMPLIS( :CHR_LEN(COMPLIS)),
     :        .FALSE., COMP, STATUS )
         TYPE = '_REAL'
         IF ( COMP .EQ. 'QUALITY') TYPE = '_UBYTE'
      END IF
      
*     map the data array and check its dimensions

      CALL NDF_DIM (OUT_NDF, MAX__DIM, DIM, NDIM, STATUS)
      CALL NDF_MAP (OUT_NDF, COMP, TYPE, 'UPDATE', 
     :  OUT_DATA_PTR, ITEMP, STATUS)

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


*     Get the new value. Some complication with bytes

      CALL PAR_GET0C ('VALUE', SVALUE, STATUS)
      CALL CHR_UCASE(SVALUE)

      IF (SVALUE .EQ. 'BAD') THEN
         VALUE = VAL__BADR
         IF (COMP .EQ. 'QUALITY') BVALUE = VAL__BADUB
      ELSE
         IF (STATUS .EQ. SAI__OK) THEN

            IF (COMP .EQ.'QUALITY') THEN
*     Convert to INT and then BYTE
               CALL CHR_CTOI(SVALUE, IVALUE, STATUS)

               IF (IVALUE .GT. SCULIB_BTOI(VAL__MAXUB)) THEN
                  BVALUE = VAL__BADUB
               ELSE
                  BVALUE = IVALUE
               END IF

            ELSE
               CALL CHR_CTOR(SVALUE, VALUE, STATUS)
            END IF

            IF (STATUS .NE. SAI__OK) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_SETC('TYPE', TYPE)
               CALL MSG_SETC('VAL', SVALUE)
               CALL ERR_REP(' ','^TASK: Error converting ^VAL to'//
     :              ' ^TYPE', STATUS)
            END IF
         END IF
      END IF

*     Are we setting the section or the inverse section
      CALL PAR_GET0L('USE_SECTION', USE_SECT, STATUS)
      
*     and set data for the selected bolometers and positions

      IF (COMP .EQ. 'QUALITY') THEN

         CALL SCULIB_SET_DATA_UB (USE_SECT, %val(OUT_DATA_PTR), 
     :        N_BOLS, N_POS, N_BEAM, BOL_S, %val(POS_S_PTR), 
     :        BVALUE, STATUS)
      ELSE

         CALL SCULIB_SET_DATA (USE_SECT, %val(OUT_DATA_PTR), 
     :        N_BOLS, N_POS, N_BEAM, BOL_S, %val(POS_S_PTR), 
     :        VALUE, STATUS)
      END IF

*     Close file and tidy up

      CALL NDF_UNMAP(OUT_NDF, '*', STATUS)
      CALL NDF_ANNUL(OUT_NDF, STATUS)

      CALL NDF_END (STATUS)

      GOOD = SAI__OK
      CALL SCULIB_FREE ('POS_S', POS_S_PTR, POS_S_END, GOOD)

      END
