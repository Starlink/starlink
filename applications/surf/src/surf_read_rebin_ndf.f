      SUBROUTINE REDS_READ_REBIN_NDF( IN_NDF, MAX_FILE, NSPEC, 
     :     DATA_SPEC, OUT_COORDS, N_FILE, SECPAR, USE_SECTION,
     :     N_BOL, N_POS, N_INTS,
     :     MJD_STANDARD, OUT_RA_CEN, OUT_DEC_CEN, WAVELENGTH, 
     :     SUB_INSTRUMENT, SOBJECT, SUTDATE, SUTSTART,
     :     BOL_ADC, BOL_CHAN,
     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR, 
     :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
     :     INT_LIST,
     :     STATUS)
*+
*  Name:
*     REDS_READ_REBIN_NDF

*  Language:
*     Starlink Fortran 77
 
*  Invocation:
*      CALL REDS_READ_REBIN_NDF( IN_NDF, MAX_FILE, NSPEC, 
*     :     DATA_SPEC, OUT_COORDS, N_FILE, SECPAR, USE_SECTION,
*     :     N_BOL, N_POS, N_INTS,
*     :     MJD_STANDARD, OUT_RA_CEN, OUT_DEC_CEN, WAVELENGTH, 
*     :     SUB_INSTRUMENT, SOBJECT, SUTDATE, SUTSTART,
*     :     BOL_ADC, BOL_CHAN,
*     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR, 
*     :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
*     :     INT_LIST,
*     :     STATUS)

 
*  Description:
*     This routines reads all necessary information from an NDF (via
*     the given NDF identifier) for rebinning.
 
*  Arguments:
*     IN_NDF = INTEGER (Given)
*        NDF identifier of input NDF
*     MAX_FILE = INTEGER (Given)
*        Max number of files allowed [used for INT_LIST only]
*     NSPEC = INTEGER (Given)
*        Number of SCUBA sections in DATA_SPEC
*     DATA_SPEC( NSPEC ) = CHAR (Given)
*        SCUBA sections
*     OUT_COORDS = CHAR (Given)
*        Output coordinates system. (Passed into REDS_READ_REBIN_NDFS)
*     N_FILE = INTEGER (Given & Returned)
*        Current file number (less than MAX_FILE and greater than 0).
*     SECPAR = LOGICAL (Given)
*        Has the USE_SECTION parameter  been set outside this routine
*     USE_SECTION = LOGICAL (Given)
*        Value of USE_SECTION parameter if SECPAR
*     N_BOL = INTEGER (Returned)
*        Number of bolometers associated with this observation
*     N_POS = INTEGER (Returned)
*        Number of samples in observation
*     N_INTS = INTEGER (Returned)
*        Number of integrations in observation
*     MJD_STANDARD = DOUBLE (Returned)
*        Modified Julian data of observation
*     IN_RA_CEN = DOUBLE (Returned)
*        RA of centre
*     IN_DEC_CEN = DOUBLE (Returned)
*        Dec of centre
*     WAVELENGTH = REAL (Given & Returned)
*        Wavelength of map
*     SUB_INSTRUMENT = CHAR (Given & Returned)
*        Sub instrument of map
*     SOBJECT = CHAR (Returned)
*        Name of object
*     SUTDATE = CHAR (Returned)
*        UT date of the observation
*     SUTSTART = CHAR (Returned)
*        UT time of the observation
*     BOL_ADC = INTEGER (Returned)
*        ADC information for bolometers - only used by BOLREBIN
*     BOL_CHAN = INTEGER (Returned)
*        Channel information for bolometers - only used by BOLREBIN
*     BOL_RA_PTR = INTEGER (Returned)
*        Pointer to RA bolometer positions [Pointer is given, data is
*        returned]
*     BOL_RA_END = INTEGER (Returned)
*        Pointer to end of RA bolometer positions
*     BOL_DEC_PTR = INTEGER (Returned)
*        Pointer to DEC bolometer positions
*     BOL_DEC_END = INTEGER (Returned)
*        Pointer to end of DEC bol positions
*     DATA_PTR = INTEGER (Returned)
*        Pointer to data values
*     DATA_END = INTEGER (Returned)
*        Pointer to end of data values
*     VARIANCE_PTR = INTEGER (Returned)
*        Pointer to variance values
*     VARIANCE_END = INTEGER (Returned)
*        Pointer to end of variance values
*     INT_LIST( MAX_FILE, MAX_INTS+1) = INTEGER (Returned)
*        Position of integrations in each data file
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Notes:

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL: John Lightfoot (RoE)

*  History:
*     1997 May 12 (TIMJ)
*       Initial version removed from reds_wtfn_rebin.f

*-
 
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing
 
*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'REDS_SYS'         ! REDS
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants

*  Local Constants:
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      CHARACTER*25 TSKNAME       ! Name of task
      PARAMETER (TSKNAME = 'REDS_READ_REBIN_NDFS')

*  Arguements Given:
      CHARACTER*(*)     DATA_SPEC(SCUBA__MAX_SECT)
      INTEGER IN_NDF
      INTEGER NSPEC
      CHARACTER*(*)     OUT_COORDS
      LOGICAL SECPAR
      LOGICAL USE_SECTION

*  Arguments Given & Returned:
      CHARACTER*(*) SUB_INSTRUMENT
      REAL WAVELENGTH

*  Arguments Returned:
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER BOL_DEC_END
      INTEGER BOL_DEC_PTR
      INTEGER BOL_RA_END
      INTEGER BOL_RA_PTR
      INTEGER DATA_END
      INTEGER DATA_PTR
      INTEGER INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1)
      INTEGER MAX_FILE
      DOUBLE PRECISION MJD_STANDARD
      INTEGER N_BOL
      INTEGER N_FILE
      INTEGER N_INTS
      INTEGER N_POS
      DOUBLE PRECISION OUT_RA_CEN
      DOUBLE PRECISION OUT_DEC_CEN
      CHARACTER*(*) SOBJECT
      CHARACTER*(*) SUTDATE
      CHARACTER*(*) SUTSTART
      INTEGER VARIANCE_END
      INTEGER VARIANCE_PTR

*  Status
      INTEGER STATUS

*  Local Variables:
      LOGICAL          ABORTED         ! .TRUE. if an observation has been
                                       ! aborted
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU4 Nasmyth coord of bolometers
      INTEGER          BOL_S (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                          ! array containing 1 for bolometers
                                          ! selected in data-spec, 0 otherwise
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! bolometer types
      INTEGER          DATA_OFFSET     ! Offset for pointer arrays
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      INTEGER          DUMMY_ENDVAR_PTR
                                       ! Pointer to end of dummy var
      INTEGER          DUMMY_VARIANCE_PTR
                                       ! Pointer to dummy variance
      INTEGER          EXP_S (SCUBA__MAX_EXP)! array containing 1 for
                                          ! exposures selected in
                                          ! data-spec, 0 otherwise
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      INTEGER          FILE_DATA_PTR   ! pointer to main data array in input
                                       ! file
      INTEGER          FILE_VARIANCE_PTR
                                       ! pointer to variance array in input file
      CHARACTER*80     FITS (SCUBA__MAX_FITS) 
                                       ! array of FITS keywords
      LOGICAL          FLATFIELD       ! .TRUE. if the FLATFIELD application
                                       ! has been run on the input file
      INTEGER          I               ! DO loop index
      INTEGER          IERR            ! Position of error from VEC_
      INTEGER          INTEGRATION     ! integration index in DO loop
      INTEGER          INT_S (SCUBA__MAX_INT)! array containing 1 for
                                       ! integration selected by
                                       ! data-spec, 0 otherwise
      CHARACTER*15     IN_CENTRE_COORDS! coord system of telescope centre in
                                       ! an input file
      DOUBLE PRECISION IN_DEC_CEN      ! apparent Dec of input file map centre
                                       ! (radians)
      INTEGER          IN_DEC1_PTR     ! array pointer to .SCUCD.DEC1
      INTEGER          IN_DEC2_PTR     ! array pointer to .SCUCD.DEC2
      INTEGER          IN_DEM_PNTR_PTR ! pointer to .SCUBA.DEM_PNTR
      CHARACTER*(DAT__SZLOC) IN_FITSX_LOC
                                       ! locator to FITS extension in input
                                       ! file
      DOUBLE PRECISION IN_LAT_RAD      ! latitude of telescope centre in input
                                       ! file (radians)
      DOUBLE PRECISION IN_LAT2_RAD     ! latitude of telescope centre at MJD2
                                       ! (radians)
      DOUBLE PRECISION IN_LONG_RAD     ! longitude of telescope centre in
                                       ! input file (radians)
      DOUBLE PRECISION IN_LONG2_RAD    ! longitude of telescope centre at MJD2
                                       ! (radians)
      INTEGER          IN_LST_STRT_PTR ! pointer to .SCUBA.LST_STRT
      DOUBLE PRECISION IN_MJD1         ! modified Julian day at which object
                                       ! was at IN_LAT,IN_LONG for PLANET centre
                                       ! coordinate system
      DOUBLE PRECISION IN_MJD2         ! modified Julian day at which object
                                       ! was at IN_LAT2,IN_LONG2 for PLANET
                                       ! centre coordinate system
      INTEGER          IN_NDF          ! NDF index of input file
      DOUBLE PRECISION IN_RA_CEN       ! apparent RA of input file map centre
                                       ! (radians)
      INTEGER          IN_RA1_PTR      ! pointer to .SCUCD.RA1
      INTEGER          IN_RA2_PTR      ! pointer to .SCUCD.RA2
      CHARACTER*(DAT__SZLOC) IN_REDSX_LOC
                                       ! locator to REDS extension in input
                                       ! file
      DOUBLE PRECISION IN_ROTATION     ! angle between apparent N and N of
                                       ! input coord system (radians)
      CHARACTER*(DAT__SZLOC) IN_SCUBAX_LOC
                                       ! locator to SCUBA extension in input
                                       ! file
      CHARACTER*(DAT__SZLOC) IN_SCUCDX_LOC
                                       ! locator to SCUCD extension in input
                                       ! file
      DOUBLE PRECISION IN_UT1          ! UT1 at start of an input observation,
                                       ! expressed as modified Julian day
      INTEGER          ITEMP           ! scratch integer
      INTEGER          JIGGLE_COUNT    ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT   ! number of times jiggle pattern is
                                       ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                       ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                       ! y jiggle offsets (arcsec)
      INTEGER          LAST_EXP        ! exposure during which abort
                                       ! occurred
      INTEGER          LAST_INT        ! integration during which abort
                                       ! occurred
      INTEGER          LAST_MEAS       ! measurement during which abort
                                       ! occurred
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      INTEGER          MEASUREMENT     ! Counter in do loop
      INTEGER          MEAS_S (SCUBA__MAX_MEAS) ! array containing 1 for
                                          ! measurement selected by
                                          ! data-spec, 0 otherwise
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NERR            ! Number of errors from VEC_
      INTEGER          NREC            ! number of history records in input
                                       ! file
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
      INTEGER          N_FITS          ! number of items in FITS array
      INTEGER          N_INTEGRATIONS  ! number of integrations per measurement
                                       ! in input file
      INTEGER          N_MEASUREMENTS  ! number of measurements in input file
      INTEGER          N_POINT         ! dimension of pointing correction 
                                       ! array in input file
      INTEGER          N_SWITCHES      ! number of switches per exposure in
                                       ! input file
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      REAL             POINT_DAZ (SCUBA__MAX_POINT)
                                       ! azimuth pointing corrections (radians)
      REAL             POINT_DEL (SCUBA__MAX_POINT)
                                       ! elevation pointing corrections
                                       ! (radians)
      DOUBLE PRECISION POINT_LST (SCUBA__MAX_POINT)
                                       ! LST of pointing corrections (radians)
      LOGICAL          POS_SELECTED       ! .TRUE. if selected data were
                                          ! specified by P=....
      INTEGER          POS_S_END          ! end of VM holding POS_S
      INTEGER          POS_S_PTR          ! start of VM holding POS_S
      LOGICAL          REBIN           ! .TRUE. if REBIN application has 
                                       ! been run on input file
      LOGICAL          REDUCE_SWITCH   ! .TRUE. if REDUCE_SWITCH application
                                       ! has been run on input file
      LOGICAL          RESTORE         ! Was the SCAN data restored
      REAL             RTEMP           ! scratch real
      INTEGER          RUN_NUMBER      ! run number of input file
      CHARACTER*15     SAMPLE_COORDS   ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE     ! sample mode of input file
      REAL             SAMPLE_PA       ! position angle of sample x axis
                                       ! relative to x axis of SAMPLE_COORDS
                                       ! system
      LOGICAL          SCAN_REVERSAL   ! Are we using SCAN_REVERSAL?
      CHARACTER*80     SCUCD_STATE     ! 'state' of SCUCD at the end of
                                       ! the observation
      LOGICAL          STATE           ! Is an NDF component there or not
      CHARACTER*80     STEMP           ! scratch string
      LOGICAL          SWITCH_EXPECTED ! Should the section include SWITCH(NO)
      INTEGER          SWITCH_S (SCUBA__MAX_SWITCH)
                                          ! array that has 1 for
                                          ! switches selected by
                                          ! data-spec, 0 otherwise
      LOGICAL          USE_INTS        ! How to use the specified ints
      REAL             WAVELENGTH      ! the wavelength of the map (microns)
*.

      IF (STATUS .NE. SAI__OK) RETURN

*      print *,'In REABIN_NDF'

*     get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC,
     :     STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ', 
     :     IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUCD', 'READ', 
     :     IN_SCUCDX_LOC, STATUS)

*      print *,'Reading Extensions'

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_XLOC (IN_NDF, 'REDS', 'READ', IN_REDSX_LOC,
     :        STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            IN_REDSX_LOC = DAT__NOLOC
         END IF
      END IF

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: input '//
     :           'file contains too many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS, 
     :     N_FITS, STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

      CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'RUN', RUN_NUMBER, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'OBJECT', OBJECT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MODE', OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'SAM_MODE', SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)

      CALL MSG_SETC ('OBJECT', OBJECT)
      CALL MSG_SETC ('MODE', OBSERVING_MODE)
      CALL MSG_SETI ('RUN', RUN_NUMBER)
      CALL MSG_SETC ('SAMPLE', SAMPLE_MODE)
      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF(MSG__NORM, ' ', '^PKG: run ^RUN was a '//
     :     '^MODE observation of ^OBJECT with ^SAMPLE sampling', 
     :     STATUS)

      IF ((OBSERVING_MODE .NE. 'MAP')      .AND.
     :     (OBSERVING_MODE .NE. 'FOCUS')    .AND.
     :     (OBSERVING_MODE .NE. 'ALIGN')    .AND.
     :     (OBSERVING_MODE .NE. 'POINTING')) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: the file '//
     :           'does not contain map data', STATUS)
            RETURN
         END IF
      END IF

*     check that the history of the file is OK

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (IN_NDF, NREC, STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            NREC = 0
         END IF

         REDUCE_SWITCH = .FALSE.
         EXTINCTION = .FALSE.
         FLATFIELD = .FALSE.
         REBIN = .FALSE.
         RESTORE = .FALSE.

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION', 
     :              I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               ELSE IF (STEMP .EQ. 'FLATFIELD') THEN
                  FLATFIELD = .TRUE.
               ELSE IF (STEMP .EQ. 'REBIN') THEN
                  REBIN = .TRUE.
               ELSE IF (STEMP .EQ. 'RESTORE') THEN
                  RESTORE = .TRUE.
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN

            IF (.NOT. REDUCE_SWITCH) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :              'REDUCE_SWITCH application has not been run '//
     :              'on the input file. Please try again.', STATUS)
               RETURN
            END IF

            IF (.NOT. EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ','^TASK: '//
     :              'the input data has not been corrected for '//
     :              'EXTINCTION. Please try again.', STATUS)
               RETURN
            END IF

            IF (.NOT. FLATFIELD) THEN
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_OUTIF(MSG__QUIET, ' ', '^TASK: '//
     :              'WARNING: the FLATFIELD application has not'//
     :              ' been run on the input file.', STATUS)
            END IF

            IF (REBIN) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :              'REBIN application has already been run on '//
     :              'the input file. Please try again.', STATUS)
               RETURN
            END IF
         END IF
      END IF

*     If this data is SCAN and has not been restored then I 
*     need to implment SCAN_REVERSAL

      SCAN_REVERSAL = .FALSE.
      IF (SAMPLE_MODE .EQ. 'RASTER' .AND. .NOT. RESTORE) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_GET_FITS_L(SCUBA__MAX_FITS, N_FITS, FITS,
     :           'SCAN_REV', SCAN_REVERSAL, STATUS)

*     If I couldnt find the SCAN_REV keyword assume that SCAN_REVERSAL
*     is true (always was initially).
            IF (STATUS .NE. SAI__OK) THEN
               SCAN_REVERSAL = .TRUE.
               CALL ERR_ANNUL(STATUS)
            END IF
         END IF

      END IF

*     get the sub-instrument and wavelength of the data, check for consistency

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SUB_1', STEMP, STATUS)
      IF (N_FILE .EQ. 1) THEN
         SUB_INSTRUMENT = STEMP
      ELSE
         IF (SUB_INSTRUMENT .NE. STEMP) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               CALL MSG_SETC ('SUB', STEMP)
               CALL MSG_SETC ('SUB1', SUB_INSTRUMENT)
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :              'file contains data for ^SUB but previous '//
     :              'file(s) held data for ^SUB1', STATUS)
            END IF
         END IF
      END IF

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'WAVE_1', RTEMP, STATUS)
      IF (N_FILE .EQ. 1) THEN
         WAVELENGTH = RTEMP
      ELSE
         IF (WAVELENGTH .NE. RTEMP) THEN
            IF (STATUS .EQ. SAI__OK) THEN
               CALL MSG_SETR ('WAVE', RTEMP)
               CALL MSG_SETR ('WAVE1', WAVELENGTH)
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: the '//
     :              'file contains data for wavelength ^WAVE but '//
     :              'previous file(s) held data for ^WAVE1',
     :              STATUS)
            END IF
         END IF
      END IF

*     coords of telescope centre

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'CENT_CRD', IN_CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (IN_CENTRE_COORDS)

      IF (OUT_COORDS .EQ. 'PL' .AND. 
     :     IN_CENTRE_COORDS .NE. 'PLANET') THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('TASK', TSKNAME)
         CALL ERR_REP(' ', '^TASK: You have chosen the PL '//
     :        'coordinate system but the source is not moving',
     :        STATUS)
      END IF


      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'LAT', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT_RAD, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'LONG', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG_RAD, STATUS)

      IF (IN_CENTRE_COORDS .EQ. 'PLANET') THEN
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LAT2', STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT2_RAD, STATUS)
         CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LONG2', STEMP, STATUS)
         CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG2_RAD,
     :        STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MJD1', IN_MJD1, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS, 
     :        'MJD2', IN_MJD2, STATUS)
      ELSE
         IN_LAT2_RAD = 0.0D0
         IN_LONG2_RAD = 0.0D0
         IN_MJD1 = 0.0D0
         IN_MJD2 = 0.0D0
      END IF

      IF ((IN_CENTRE_COORDS .NE. 'AZ') .AND.
     :     (IN_CENTRE_COORDS .NE. 'GA') .AND.
     :     (IN_CENTRE_COORDS .NE. 'NA')) THEN
         IN_LONG_RAD = IN_LONG_RAD * 15.0D0
         IN_LONG2_RAD = IN_LONG2_RAD * 15.0D0
      END IF

*     offset from telescope centre

      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS, 
     :     'MAP_X', MAP_X, STATUS)
      MAP_X = MAP_X / REAL (R2AS)
      CALL SCULIB_GET_FITS_R (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MAP_Y', MAP_Y, STATUS)
      MAP_Y = MAP_Y / REAL (R2AS)

*     the UT of the observation expressed as modified Julian day

      CALL SCULIB_GET_MJD(N_FITS, FITS, IN_UT1, RTEMP, STATUS)

*     the time of the first file read in will be the one for which the
*     apparent RA,Decs of all the input data will be calculated
*     This routine returns this information for all files and relies on
*     higher routines to select the correct one

      MJD_STANDARD = IN_UT1 
      SOBJECT = OBJECT          ! Store first object name

*     These are only needed to inform user of UT for RD rebinning
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS, 
     :     'UTDATE', SUTDATE, STATUS)
      CALL SCULIB_GET_FITS_C (N_FITS, N_FITS, FITS, 
     :     'UTSTART', SUTSTART, STATUS)
      
*     search for pointing correction structure in the REDS extension, if there
*     is one read in the corrections

      IF (STATUS .EQ. SAI__OK) THEN

         CALL CMP_GET1D(IN_REDSX_LOC,'POINT_LST',SCUBA__MAX_POINT,
     :        POINT_LST, N_POINT, STATUS)
         CALL CMP_GET1R(IN_REDSX_LOC,'POINT_DAZ',SCUBA__MAX_POINT,
     :        POINT_DAZ, N_POINT, STATUS)
         CALL CMP_GET1R(IN_REDSX_LOC,'POINT_DEL',SCUBA__MAX_POINT,
     :        POINT_DEL, N_POINT, STATUS)
         
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            N_POINT = 0
         END IF
      END IF

*     map the various components of the data array and check the data
*     dimensions

      CALL NDF_DIM (IN_NDF, MAX_DIM, DIM, NDIM, STATUS)

      CALL NDF_SQMF(.TRUE., IN_NDF, STATUS)
      CALL NDF_MAP (IN_NDF, 'DATA', '_REAL', 'READ', 
     :     FILE_DATA_PTR, ITEMP, STATUS)

*     Need to check if FIGARO has removed the VARIANCE array

      CALL NDF_STATE(IN_NDF, 'VARIANCE', STATE, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (STATE) THEN
            CALL NDF_MAP (IN_NDF, 'VARIANCE', '_REAL', 'READ',
     :           FILE_VARIANCE_PTR, ITEMP, STATUS)
         ELSE
            CALL MSG_SETC('TASK', TSKNAME)
            CALL MSG_OUTIF(MSG__QUIET, ' ','WARNING! ^TASK: '//
     :           'Variance array is missing. Using dummy array',
     :           STATUS)
            CALL SCULIB_MALLOC(DIM(1)*DIM(2)*VAL__NBR,
     :           DUMMY_VARIANCE_PTR, DUMMY_ENDVAR_PTR,
     :           STATUS)
            ITEMP = DIM(1) * DIM(2)
            RTEMP = 1.0e-6
            CALL SCULIB_CFILLR(ITEMP, RTEMP,
     :           %VAL(DUMMY_VARIANCE_PTR))
            FILE_VARIANCE_PTR = DUMMY_VARIANCE_PTR
         END IF               
      END IF
      
      IF (STATUS .EQ. SAI__OK) THEN
         IF ((NDIM .NE. 2)    .OR.
     :        (DIM(1) .LT. 1)  .OR.
     :        (DIM(2) .LT. 1)) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI ('NDIM', NDIM)
            CALL MSG_SETI ('DIM1', DIM(1))
            CALL MSG_SETI ('DIM2', DIM(2))
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: data array '//
     :           'has bad dimensions (^NDIM) ^DIM1, ^DIM2', STATUS)
         END IF
      END IF

      N_BOL = DIM (1)
      N_POS = DIM (2)

*     map the DEM_PNTR and LST arrays and check their dimensions

      CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS, 
     :     N_MEASUREMENTS, STATUS)

*     Check LST_STRT
      CALL SCULIB_GET_LST_STRT(IN_SCUCDX_LOC, IN_LST_STRT_PTR,
     :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*     find if the observation was aborted

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'STATE', SCUCD_STATE, STATUS)
      CALL CHR_UCASE (SCUCD_STATE)
      ABORTED = .FALSE.
      IF (INDEX(SCUCD_STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF


*     Print out information on observation

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      IF (.NOT. ABORTED) THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: file contains data for ^N_E '//
     :        'exposure(s) in ^N_I integrations(s) in ^N_M '//
     :        'measurement(s)', STATUS)
      ELSE

*     get the exposure, integration, measurement numbers at which the abort
*     occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'EXP_NO', LAST_EXP, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'INT_NO', LAST_INT, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MEAS_NO', LAST_MEAS, STATUS)

         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        '^PKG: the observation should have '//
     :        'had ^N_E exposure(s) in ^N_I integration(s) in '//
     :        '^N_M measurement(s)', STATUS)
         CALL MSG_SETI ('N_E', LAST_EXP)
         CALL MSG_SETI ('N_I', LAST_INT)
         CALL MSG_SETI ('N_M', LAST_MEAS)
         CALL MSG_OUTIF(MSG__NORM, ' ', 
     :        ' - However, the observation was '//
     :        'ABORTED during exposure ^N_E of integration '//
     :        '^N_I of measurement ^N_M', STATUS)
      END IF

*     calculate the apparent RA and Dec of the map centre at IN_UT1

      CALL SCULIB_CALC_APPARENT (IN_LONG_RAD, IN_LAT_RAD,
     :     IN_LONG2_RAD, IN_LAT2_RAD, DBLE(MAP_X), DBLE(MAP_Y), 
     :     IN_CENTRE_COORDS, %VAL(IN_LST_STRT_PTR), IN_UT1,
     :     IN_MJD1, IN_MJD2, IN_RA_CEN, IN_DEC_CEN, IN_ROTATION,
     :     STATUS)

*     set the default map centre to that of the first component observation
*     Always return the RA and DEC of the map. Routines higher up
*     decide on which to use.

      OUT_RA_CEN  = IN_RA_CEN
      OUT_DEC_CEN = IN_DEC_CEN

*     get the bolometer description arrays

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOL, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)

*     now read in data specific to the sample mode of the observation

      IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN

         CALL SCULIB_GET_JIGGLE(IN_SCUCDX_LOC, SCUBA__MAX_JIGGLE,
     :        N_FITS, FITS, JIGGLE_COUNT, JIGGLE_REPEAT, 
     :        JIGGLE_P_SWITCH, SAMPLE_PA, SAMPLE_COORDS, JIGGLE_X,
     :        JIGGLE_Y, STATUS)

*     likewise for raster maps

      ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN

         CALL SCULIB_GET_RASTER(IN_SCUCDX_LOC, N_SWITCHES,
     :        N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :        IN_RA1_PTR, IN_RA2_PTR, IN_DEC1_PTR, IN_DEC2_PTR,
     :        STATUS)

      END IF

*     copy data into scratch memory

      CALL SCULIB_MALLOC (N_POS * N_BOL * VAL__NBR,
     :     DATA_PTR, DATA_END, STATUS)
      CALL SCULIB_MALLOC (N_POS * N_BOL * VAL__NBR,
     :     VARIANCE_PTR, VARIANCE_END, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL VEC_RTOR(.FALSE., N_POS * N_BOL,
     :        %VAL(FILE_DATA_PTR), %VAL(DATA_PTR), IERR,
     :        NERR, STATUS)
         CALL VEC_RTOR(.FALSE., N_POS * N_BOL,
     :        %VAL(FILE_VARIANCE_PTR), 
     :        %VAL(VARIANCE_PTR), IERR, NERR, STATUS)

      END IF

*     SCUBA SECTION
*     If there a SCUBA section has been specified then we need to apply it
      IF (NSPEC .GT. 0) THEN

*     Get some memory

         POS_S_PTR = 0
         CALL SCULIB_MALLOC (N_POS * VAL__NBI, POS_S_PTR, 
     :        POS_S_END, STATUS)

*     decode the data specification
         
         SWITCH_EXPECTED = .FALSE.
         
         CALL SCULIB_DECODE_SPEC (NSPEC,DATA_SPEC,%val(IN_DEM_PNTR_PTR),
     :        1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS, 
     :        N_POS, N_BOL, SWITCH_EXPECTED,
     :        POS_SELECTED, %val(POS_S_PTR), SWITCH_S, EXP_S, 
     :        INT_S, MEAS_S, BOL_S, STATUS)

*     Is section good or bad

         IF (SECPAR) THEN
            print *,'Using value from text file for USE_INTS'
            USE_INTS = USE_SECTION
         ELSE
            CALL PAR_GET0L ('USE_SECTION', USE_INTS, STATUS)
            CALL PAR_CANCL ('USE_SECTION', STATUS)
         END IF

*     If we are using the section we actually want the inverse
*     section to be set bad and not the section itself.
         USE_INTS = .NOT.USE_INTS

*     and set quality for the selected bolometers and positions

         CALL SCULIB_SET_DATA (USE_INTS, %val(DATA_PTR), 
     :        N_BOL, N_POS, 1, BOL_S, %val(POS_S_PTR), 
     :        VAL__BADR, STATUS)
         
         CALL SCULIB_FREE ('POS_S', POS_S_PTR, POS_S_END, STATUS)

      END IF


*     Get some memory for the bolometer positions

      CALL SCULIB_MALLOC (N_POS * N_BOL * VAL__NBD,
     :     BOL_RA_PTR, BOL_RA_END, STATUS)
      CALL SCULIB_MALLOC (N_POS * N_BOL * VAL__NBD,
     :     BOL_DEC_PTR, BOL_DEC_END, STATUS)

*     Loop through bolometers and find apparent RA/Dec
      IF (STATUS .EQ. SAI__OK) THEN

         CALL SCULIB_PROCESS_BOLS(.FALSE., .FALSE.,1, N_BOL,
     :        N_POS, N_SWITCHES, N_EXPOSURES, 
     :        N_INTEGRATIONS, N_MEASUREMENTS,
     :        1, N_EXPOSURES, 1, N_INTEGRATIONS, 1,N_MEASUREMENTS,
     :        N_FILE, N_FITS, FITS,
     :        %VAL(IN_DEM_PNTR_PTR), %VAL(IN_LST_STRT_PTR),
     :        IN_ROTATION, SAMPLE_MODE,
     :        SAMPLE_COORDS, OUT_COORDS, JIGGLE_REPEAT,
     :        JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y, JIGGLE_P_SWITCH,
     :        IN_RA_CEN, IN_DEC_CEN,
     :        %VAL(IN_RA1_PTR), %VAL(IN_RA2_PTR), 
     :        %VAL(IN_DEC1_PTR), %VAL(IN_DEC2_PTR), MJD_STANDARD,
     :        IN_UT1, IN_MJD1, IN_LONG_RAD, IN_LAT_RAD, IN_MJD2, 
     :        IN_LONG2_RAD, IN_LAT2_RAD,
     :        N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :        SCUBA__NUM_CHAN, SCUBA__NUM_ADC, BOL_ADC, BOL_CHAN,
     :        BOL_DU3, BOL_DU4, SCAN_REVERSAL, 0.0, 0.0, 0.0, 0.0,
     :        %VAL(BOL_DEC_PTR), %VAL(BOL_RA_PTR),
     :        %VAL(DATA_PTR), 0.0, 
     :        STATUS)

      END IF

*     Store pointers to start of each integration in this map

      N_INTS = N_MEASUREMENTS * N_INTEGRATIONS

      DATA_OFFSET = 1

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS

            CALL SCULIB_FIND_SWITCH(%VAL(IN_DEM_PNTR_PTR),
     :           1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :           N_POS, 1, 1, INTEGRATION,MEASUREMENT,
     :           INT_LIST(N_FILE, DATA_OFFSET), ITEMP, STATUS)

            DATA_OFFSET = DATA_OFFSET + 1

         END DO
      END DO

*     Also store int+1 so that I can easily calculate end of data
      INT_LIST(N_FILE, DATA_OFFSET) = N_POS + 1

*     annul locators and array identifiers and close the file

      CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL CMP_UNMAP (IN_SCUCDX_LOC, 'LST_STRT', STATUS)

      IF (SAMPLE_MODE .EQ. 'RASTER') THEN
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA1', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA2', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC1', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC2', STATUS)
      END IF

 1    CONTINUE                  ! Jump here if error before mapping ARY

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
      IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)
      END IF



      END
