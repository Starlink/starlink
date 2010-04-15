      SUBROUTINE SURF_READ_REBIN_NDF( IN_NDF, MAX_FILE, NSPEC,
     :     DATA_SPEC, OUT_COORDS, N_FILE, USE_SECTION,
     :     N_BOL, N_POS, N_INTS, N_MEAS, N_BEAMS, MJD_STANDARD, IN_UT1,
     :     OUT_RA_CEN, OUT_DEC_CEN, FITS, N_FITS, WAVELENGTH,
     :     SUB_INSTRUMENT, SOBJECT, SUTDATE, SUTSTART,
     :     BOL_ADC, BOL_CHAN,
     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
     :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
     :     QMF, QUALITY_PTR, QUALITY_END, BADBITS,
     :     USE_LST, LST_PTR, ANG_INT, ANG_MEAS,INT_LIST, MEAS_LIST,
     :     BOLWT, STATUS)
*+
*  Name:
*     SURF_READ_REBIN_NDF

*  Purpose:
*     Read an NDF into memory prior to regridding

*  Language:
*     Starlink Fortran 77

*  Invocation:
*      CALL SURF_READ_REBIN_NDF( IN_NDF, MAX_FILE, NSPEC,
*     :     DATA_SPEC, OUT_COORDS, N_FILE, USE_SECTION,
*     :     N_BOL, N_POS, N_INTS, N_BEAMS, MJD_STANDARD, IN_UT1,
*     :     OUT_RA_CEN, OUT_DEC_CEN, FITS, N_FITS, WAVELENGTH,
*     :     SUB_INSTRUMENT, SOBJECT, SUTDATE, SUTSTART,
*     :     BOL_ADC, BOL_CHAN,
*     :     BOL_RA_PTR, BOL_RA_END, BOL_DEC_PTR,
*     :     BOL_DEC_END, DATA_PTR, DATA_END, VARIANCE_PTR, VARIANCE_END,
*     :     QMF, QUALITY_PTR, QUALITY_END, BADBITS,
*     :     USE_LST, LST_PTR, ANG_INT, ANG_MEAS, INT_LIST, MEAS_LIST, BOLWT,
*     :     STATUS)


*  Description:
*     This routines reads all necessary information from an NDF (via
*     the given NDF identifier) for rebinning. Optionally, returns
*     the quality array (needed for despiking).

*  Arguments:
*     IN_NDF = INTEGER (Given)
*        NDF identifier of input NDF
*     MAX_FILE = INTEGER (Given)
*        Max number of files allowed [used for INT_LIST/MEAS only]
*     NSPEC = INTEGER (Given)
*        Number of SCUBA sections in DATA_SPEC
*     DATA_SPEC( NSPEC ) = CHAR (Given)
*        SCUBA sections
*     OUT_COORDS = CHAR (Given)
*        Output coordinates system. (Passed into SURF_READ_REBIN_NDF)
*     N_FILE = INTEGER (Given)
*        Current file number (less than MAX_FILE and greater than 0).
*     USE_SECTION = LOGICAL (Given)
*        Determines whether we are using the section or the invers
*     N_BOL = INTEGER (Returned)
*        Number of bolometers associated with this observation
*     N_POS = INTEGER (Returned)
*        Number of samples in observation
*     N_INTS = INTEGER (Returned)
*        Total Number of integrations in observation (INT*MEAS)
*     N_MEAS = INTEGER (Returned)
*        Number of measurements in observation
*     N_BEAMS = INTEGER (Given & Returned)
*        Number of beams requested in the positions arrays.
*        N_BEAMS = 1  will return the middle-beam (standard position)
*        N_BEAMS = 2 will return L and R beams for RASTER and JIGGLE
*        N_BEAMS = 3 will return L, M and R beams (as M,L,R) for JIGGLE
*                   data that has been reduce_switched and for SCAN
*                   data. N_BEAM is set to 2 if a single switch of a
*                   JIGGLE map is requested.
*     MJD_STANDARD = DOUBLE (Given & Returned)
*        Modified Julian data of observation. Returned if N_FILE=1.
*     IN_UT1 = DOUBLE (Returned)
*        Modified Julian data of observation UT1 at start of an input
*        observation, expressed as modified Julian day
*     IN_RA_CEN = DOUBLE (Returned)
*        RA of centre
*     IN_DEC_CEN = DOUBLE (Returned)
*        Dec of centre
*     FITS ( SCUBA_MAX_FITS ) = CHARACTER *80 (Returned)
*        FITS header entries
*     N_FITS = INTEGER (Returned)
*        Number of FITS entries
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
*     QMF = LOGICAL (Given)
*        Flag to decide whether quality is being stored (.FALSE.) or
*        being folded into the data array (.true.). See NDF_SQMF
*     QUALITY_PTR = INTEGER (Returned)
*        Pointer to quality array
*     QUALITY_END = INTEGER (Returned)
*        Pointer to end of quality array
*     BADBITS = BYTE (Returned)
*        Bad bits mask for quality array
*     USE_LST = LOGICAL (Given)
*        Governs whether we want an LST array returned
*     LST_PTR( 2 ) = INTEGER (Returned)
*        Array of pointers (begin and end)  to array of LSTs
*     ANG_INT( MAX_FILE, SCUBA__MAX_INT,2)  = REAL (Returned)
*        Array containing the polarimetry angles for each integration
*        The 2 dimensions are for WPLATE and ANGROT
*     ANG_MEAS( MAX_FILE, SCUBA__MAX_MEAS,2) = REAL (Returned)
*        Array containing the pol angles for each measurement
*        The 2 dimensions are for WPLATE and ANGROT
*     INT_LIST( MAX_FILE, SCUBA__MAX_INT+1) = INTEGER (Returned)
*        Position of integrations in each data file
*     MEAS_LIST(MAX_FILE, SCUBA__MAX_MEAS+1) = INTEGER (Returned)
*        Position of measurements in each data file
*     BOLWT ( N_BOL ) = REAL (Returned)
*        Relative weights of each bolometer
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Notes:

*  Authors:
*     TIMJ: Tim Jenness (JACH)
*     JFL: John Lightfoot (RoE)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     1997 May 12 (TIMJ)
*       Initial version removed from reds_wtfn_rebin.f
*     $Log$
*     Revision 1.26  2005/03/19 01:41:02  timj
*     Propogate focal station from app level to calc_bol_coords
*
*     Revision 1.25  2005/03/18 06:27:33  timj
*     initialise some variables
*
*     Revision 1.24  2004/11/18 20:37:14  timj
*     initialise some more pointers
*
*     Revision 1.23  2004/09/08 02:03:33  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.22  2000/06/27 02:44:38  timj
*     Allow for a small range of WAVElengths
*
*     Revision 1.21  2000/06/16 01:25:15  timj
*     Use new-format SCULIB_GET_MJD
*
*     Revision 1.20  1999/08/19 03:37:43  timj
*     Header tweaks to ease production of SSN72 documentation.
*
*     Revision 1.19  1999/08/03 20:01:36  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     Revision 1.18  1999/07/14 20:13:29  timj
*     Pass LAT_OBS into SCULIB_CALC_APPARENT rather than having it as
*     a parameter.
*
*     Revision 1.17  1999/05/15 01:50:00  timj
*     Finalise support for POLMAP/POLPHOT observing modes.
*     Only check first few characters of history app name
*     now that we are writing version number to this string.
*     POLPHOT is synonym for PHOTOM.
*     Read units from file.
*     Initialise ANG_INT and ANG_MEAS in this routine.
*
*     Revision 1.16  1999/02/27 04:39:00  timj
*     Read polarimeter extensions.
*     Return back pointers to measurements as well as integrations.
*
*     Revision 1.15  1998/04/28 20:03:12  timj
*     Return the FITS array.
*
*     Revision 1.14  1998/04/28 02:22:24  timj
*     Add BOLWT
*
*     Revision 1.13  1998/03/18 22:55:48  timj
*     Add support for multiple beams
*
*     Revision 1.12  1997/11/04 23:21:12  timj
*     Add support for LOCAL_COORDS and MAP_X/Y
*
*     Revision 1.11  1997/10/21 19:30:06  timj
*     Read BADBITS mask
*
*     Revision 1.10  1997/10/20 21:13:16  timj
*     Pass through quality arrays if needed.
*     Free memory allocated to DUMMY_VARIANCE_PTR
*

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'SURF_PAR'         ! REDS
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'MSG_PAR'          ! MSG__ constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Local Constants:
      INTEGER          MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      CHARACTER*25     TSKNAME              ! Name of task
      PARAMETER (TSKNAME = 'READ_REBIN_NDF')

*  Arguements Given:
      CHARACTER*(*)    DATA_SPEC(SCUBA__MAX_SECT)
      INTEGER          IN_NDF
      INTEGER          MAX_FILE
      INTEGER          N_FILE
      INTEGER          NSPEC
      CHARACTER*(*)    OUT_COORDS
      LOGICAL          QMF
      LOGICAL          USE_SECTION
      LOGICAL          USE_LST

*  Arguments Given & Returned:
      DOUBLE PRECISION MJD_STANDARD
      INTEGER          N_BEAMS
      CHARACTER*(*)    SUB_INSTRUMENT
      REAL             WAVELENGTH

*  Arguments Returned:
      REAL             ANG_INT(MAX_FILE,SCUBA__MAX_INT, 2)
      REAL             ANG_MEAS(MAX_FILE,SCUBA__MAX_MEAS, 2)
      BYTE             BADBITS
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
      INTEGER          BOL_DEC_END
      INTEGER          BOL_DEC_PTR
      INTEGER          BOL_RA_END
      INTEGER          BOL_RA_PTR
      REAL             BOLWT( SCUBA__NUM_CHAN * SCUBA__NUM_ADC )
      INTEGER          DATA_END
      INTEGER          DATA_PTR
      CHARACTER*(80)   FITS (SCUBA__MAX_FITS)
      INTEGER          INT_LIST(MAX_FILE, SCUBA__MAX_INT + 1)
      DOUBLE PRECISION IN_UT1
      INTEGER          LST_PTR( 2 )
      INTEGER          MEAS_LIST(MAX_FILE, SCUBA__MAX_MEAS + 1)
      INTEGER          N_BOL
      INTEGER          N_FITS
      INTEGER          N_INTS
      INTEGER          N_MEAS
      INTEGER          N_POS
      DOUBLE PRECISION OUT_RA_CEN
      DOUBLE PRECISION OUT_DEC_CEN
      INTEGER          QUALITY_END
      INTEGER          QUALITY_PTR
      CHARACTER*(*)    SOBJECT
      CHARACTER*(*)    SUTDATE
      CHARACTER*(*)    SUTSTART
      INTEGER          VARIANCE_END
      INTEGER          VARIANCE_PTR

*  Status
      INTEGER          STATUS

*  Local Variables:
      LOGICAL          ABORTED         ! .TRUE. if an observation has been
                                       ! aborted
      INTEGER          ANG_PTR         ! Mapped ANGROT NDF (for POL data)
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU4 Nasmyth coord of bolometers
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! bolometer types
      BYTE             BTEMP           ! Temporary byte
      INTEGER          DATA_OFFSET     ! Offset for pointer arrays
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      INTEGER          DUMMY_ENDQ_PTR  ! Pointer to end of dummy quality
      INTEGER          DUMMY_ENDVAR_PTR
                                       ! Pointer to end of dummy var
      INTEGER          DUMMY_QUALITY_PTR ! Pointer to dummy quality
      INTEGER          DUMMY_VARIANCE_PTR
                                       ! Pointer to dummy variance
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      REAL             FAST_AXIS       ! Fast axis direction (POL data)
      INTEGER          FILE_DATA_PTR   ! pointer to main data array in input
                                       ! file
      INTEGER          FILE_QUALITY_PTR ! pointer to quality array in input file
      INTEGER          FILE_VARIANCE_PTR
                                       ! pointer to variance array in input file
      LOGICAL          FLATFIELD       ! .TRUE. if the FLATFIELD application
                                       ! has been run on the input file
      CHARACTER*10     FOCAL_STATION   ! Where is the instrument located?
      INTEGER          I               ! DO loop index
      INTEGER          IERR            ! Position of error from VEC_
      INTEGER          INTEGRATION     ! integration index in DO loop
      CHARACTER*20     INSTRUMENT      ! Name of instrument
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
      DOUBLE PRECISION LAT_OBS         ! Latitude of observatory
      INTEGER          LBND(MAX_DIM)   ! Lower bounds of NDF section
      CHARACTER*(15)   LOCAL_COORDS    ! Coordinate system of MAP_X and MAP_Y
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      INTEGER          MEASUREMENT     ! Counter in do loop
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NERR            ! Number of errors from VEC_
      INTEGER          NREC            ! number of history records in input
                                       ! file
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
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
      INTEGER          SECNDF          ! Section identifier
      LOGICAL          SCAN_REVERSAL   ! Are we using SCAN_REVERSAL?
      CHARACTER*80     SCUCD_STATE     ! 'state' of SCUCD at the end of
                                       ! the observation
      LOGICAL          STATE           ! Is an NDF component there or not
      CHARACTER*80     STEMP           ! scratch string
      LOGICAL          SWITCH_EXPECTED ! Should the section include SWITCH(NO)
      CHARACTER*20     TELESCOPE       ! Name of telescope
      LOGICAL          THERE           ! Is a component present
      INTEGER          TNDF(2)         ! Temporary NDF identifiers
      INTEGER          UBND(MAX_DIM)   ! Upper bounds of NDF section
      CHARACTER*15     UNITS           ! Units from the NDF Char component
      LOGICAL          USE_INTS        ! How to use the specified ints
      INTEGER          WP_PTR          ! Mapped WPLATE NDF (for POL data)

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Initialise all the pointers
      BOL_RA_PTR = 0
      BOL_RA_END = 0
      BOL_DEC_PTR = 0
      BOL_DEC_END = 0
      DUMMY_VARIANCE_PTR = 0
      DUMMY_ENDVAR_PTR = 0
      DUMMY_ENDQ_PTR = 0
      DUMMY_QUALITY_PTR = 0
      DATA_PTR = 0
      DATA_END = 0
      VARIANCE_END =0
      VARIANCE_PTR = 0
      QUALITY_PTR = 0
      QUALITY_END = 0
      FILE_DATA_PTR = 0
      FILE_VARIANCE_PTR = 0
      IN_RA1_PTR = 0
      IN_RA2_PTR = 0
      IN_DEC1_PTR = 0
      IN_DEC2_PTR = 0


*     get some general descriptive parameters of the observation

      CALL NDF_XLOC (IN_NDF, 'FITS', 'READ', IN_FITSX_LOC,
     :     STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUBA', 'READ',
     :     IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (IN_NDF, 'SCUCD', 'READ',
     :     IN_SCUCDX_LOC, STATUS)

*     The REDS extension may not be present
      CALL NDF_XSTAT(IN_NDF, 'REDS', THERE, STATUS)

      IF (THERE) THEN
         CALL NDF_XLOC (IN_NDF, 'REDS', 'READ', IN_REDSX_LOC,
     :        STATUS)
      ELSE
         IN_REDSX_LOC = DAT__NOLOC
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
     :     (OBSERVING_MODE .NE. 'PHOTOM')    .AND.
     :     (OBSERVING_MODE .NE. 'POLPHOT')    .AND.
     :     (OBSERVING_MODE .NE. 'POLMAP')    .AND.
     :     (OBSERVING_MODE .NE. 'POINTING')) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL ERR_REP (' ', '^TASK: the file '//
     :           'does not contain map data', STATUS)
            RETURN
         END IF
      END IF

      IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :     OBSERVING_MODE .EQ. 'POLPHOT') THEN
         CALL MSG_SETC('PKG', PACKAGE)
         CALL MSG_SETC('MODE', OBSERVING_MODE)
         CALL MSG_OUTIF(MSG__QUIET, ' ', '^PKG: WARNING! The ^MODE '//
     :        'data will not provide a fully-sampled image', STATUS)
      END IF


*     check that the history of the file is OK

      RESTORE = .FALSE.

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

         IF (NREC .GT. 0) THEN
            DO I = 1, NREC
               CALL NDF_HINFO (IN_NDF, 'APPLICATION',
     :              I, STEMP, STATUS)
               CALL CHR_UCASE (STEMP)
               IF (STEMP(:13) .EQ. 'REDUCE_SWITCH') THEN
                  REDUCE_SWITCH = .TRUE.
               ELSE IF (STEMP(:10) .EQ. 'EXTINCTION') THEN
                  EXTINCTION = .TRUE.
               ELSE IF (STEMP(:9) .EQ. 'FLATFIELD') THEN
                  FLATFIELD = .TRUE.
               ELSE IF (STEMP(:5) .EQ. 'REBIN') THEN
                  REBIN = .TRUE.
               ELSE IF (STEMP(:7) .EQ. 'RESTORE') THEN
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
*     Check to make sure the wavelength is okay. Allow a 20 micron
*     error.
         IF (ABS(WAVELENGTH - RTEMP) .GT. 20.0) THEN
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

*     Get telescope and instrument information...
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'INSTRUME', INSTRUMENT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'TELESCOP', TELESCOPE, STATUS)

*     ...And calculate the focal station
      CALL SURFLIB_GET_FOCAL_STATION( TELESCOPE, INSTRUMENT,
     :     SUB_INSTRUMENT, FOCAL_STATION, STATUS )

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

*     and the coordinate frame of these offsets
*     not sure whether old files have this parameter so test for status
*     If it is not available then assume it is CENTRE_COORDS

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LOCL_CRD', LOCAL_COORDS, STATUS)

         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            LOCAL_COORDS = IN_CENTRE_COORDS
         END IF
      END IF

*     Read the latitude of the observatory
      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

*     the time of the first file read in will be the one for which the
*     apparent RA,Decs of all the input data will be calculated
*     This routine returns this information for all files and relies on
*     higher routines to select the correct one

      IF (N_FILE .EQ. 1) MJD_STANDARD = IN_UT1

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

*     Find the dimensions of the data array

      CALL NDF_DIM (IN_NDF, MAX_DIM, DIM, NDIM, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (OBSERVING_MODE .EQ. 'PHOTOM' .OR.
     :        OBSERVING_MODE .EQ. 'POLPHOT') THEN
            IF ((NDIM .NE. 3)                  .OR.
     :          (DIM(1) .LT. 1)         .OR.
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
            IF ((NDIM .NE. 2)    .OR.
     :           (DIM(1) .LT. 1)  .OR.
     :           (DIM(2) .LT. 1)) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETI ('NDIM', NDIM)
               CALL MSG_SETI ('DIM1', DIM(1))
               CALL MSG_SETI ('DIM2', DIM(2))
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ', '^TASK: data array '//
     :              'has bad dimensions (^NDIM) ^DIM1, ^DIM2', STATUS)
            END IF
         END IF
      END IF

      N_BOL = DIM (1)
      N_POS = DIM (2)


*     Get an NDF identifier for an NDF section
*     Do this so that I can deal with photometry 'images'

*     Define a base section
      LBND(1) = 1
      LBND(2) = 1
      UBND(1) = N_BOL
      UBND(2) = N_POS
      UBND(3) = 2
      LBND(3) = 2   ! Use middle beam data

*      NDIM = 2
*      IF (OBSERVING_MODE .EQ. 'PHOTOM') NDIM = 3

*     Get the section
      CALL NDF_SECT(IN_NDF, NDIM, LBND, UBND, SECNDF, STATUS)

*     Map the data and variance

      CALL NDF_SQMF(QMF, IN_NDF, STATUS)
      CALL NDF_SQMF(QMF, SECNDF, STATUS)
      CALL NDF_MAP (SECNDF, 'DATA', '_REAL', 'READ',
     :     FILE_DATA_PTR, ITEMP, STATUS)

*     Need to check if FIGARO has removed the VARIANCE array

      CALL NDF_STATE(IN_NDF, 'VARIANCE', STATE, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         IF (STATE) THEN
            CALL NDF_MAP (SECNDF, 'VARIANCE', '_REAL', 'READ',
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
     :           %VAL(CNF_PVAL(DUMMY_VARIANCE_PTR)))
            FILE_VARIANCE_PTR = DUMMY_VARIANCE_PTR
         END IF
      END IF

*     Handle the quality array if necessary
*     Deal with the possibility that FIGARO has removed it!

      IF (.NOT.QMF) THEN

         CALL NDF_STATE(IN_NDF, 'QUALITY', STATE, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN
            IF (STATE) THEN
               CALL NDF_MAP (SECNDF, 'QUALITY', '_UBYTE', 'READ',
     :              FILE_QUALITY_PTR, ITEMP, STATUS)
               CALL NDF_BB(SECNDF, BADBITS, STATUS)
            ELSE
               CALL MSG_SETC('TASK', TSKNAME)
               CALL MSG_OUTIF(MSG__QUIET, ' ','WARNING! ^TASK: '//
     :              'Quality array is missing. Using dummy array',
     :              STATUS)
               CALL SCULIB_MALLOC(DIM(1)*DIM(2)*VAL__NBUB,
     :              DUMMY_QUALITY_PTR, DUMMY_ENDQ_PTR,
     :              STATUS)
               ITEMP = DIM(1) * DIM(2)
               BTEMP = 0
               CALL SCULIB_CFILLB(ITEMP, BTEMP,
     :              %VAL(CNF_PVAL(DUMMY_QUALITY_PTR)))
               FILE_QUALITY_PTR = DUMMY_QUALITY_PTR
               BADBITS = VAL__BADUB
            END IF
         END IF

      END IF



*     map the DEM_PNTR and LST arrays and check their dimensions

      CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*     Check LST_STRT
      CALL SCULIB_GET_LST_STRT(IN_SCUCDX_LOC, IN_LST_STRT_PTR,
     :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*  UT at which observation was made expressed as modified Julian day

      CALL SCULIB_GET_MJD(N_FITS, FITS, %VAL(CNF_PVAL(IN_LST_STRT_PTR)),
     :                    IN_UT1,
     :     RTEMP, RTEMP, STATUS)

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

*     Deal with bolometer weights

      IF (STATUS .EQ. SAI__OK) THEN

*     Read in the bolometer weights if they are there

         CALL CMP_GET1R(IN_REDSX_LOC, 'BOLWT', N_BOL,
     :        BOLWT, ITEMP, STATUS)

*     If there was an error this means that the BOLWT extension is not there
*     therefore fill up the weights array with ones
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            DO I = 1, N_BOL
               BOLWT(I) = 1.0
            END DO

*     Make sure we read in the correct number of values
*     Raise an error if not
         ELSE IF (ITEMP .NE. N_BOL) THEN

            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK', TSKNAME)
            CALL MSG_SETI('NB', ITEMP)
            CALL MSG_SETI('NBOL', N_BOL)
            CALL ERR_REP(' ','^TASK: The number of entries in the '//
     :           'BOLWT extension (^NB) does not match the number '//
     :           'of bolometers in the file (^NBOL)', STATUS)
         END IF

      END IF

*     calculate the apparent RA and Dec of the map centre at IN_UT1
*     Cannot add MAP_X and MAP_Y here since
*       1. This routine does not support LOCAL_COORDS
*       2. The tracking centre moves if LOCAL_COORDS is AZ for
*          a fixed RA,Dec centre.
*     The reference centre will always be the map centre and not the
*     offset map centre.

      CALL SCULIB_CALC_APPARENT (LAT_OBS, IN_LONG_RAD, IN_LAT_RAD,
     :     IN_LONG2_RAD, IN_LAT2_RAD, 0.0D0, 0.0D0,
     :     IN_CENTRE_COORDS, %VAL(CNF_PVAL(IN_LST_STRT_PTR)), IN_UT1,
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
     :        %VAL(CNF_PVAL(FILE_DATA_PTR)), %VAL(CNF_PVAL(DATA_PTR)),
     :        IERR,
     :        NERR, STATUS)
         CALL VEC_RTOR(.FALSE., N_POS * N_BOL,
     :        %VAL(CNF_PVAL(FILE_VARIANCE_PTR)),
     :        %VAL(CNF_PVAL(VARIANCE_PTR)), IERR, NERR, STATUS)

      END IF

*     Copy quality if needed

      IF (.NOT.QMF) THEN

         CALL SCULIB_MALLOC (N_POS * N_BOL * VAL__NBUB,
     :        QUALITY_PTR, QUALITY_END, STATUS)

         IF (STATUS .EQ. SAI__OK) THEN

            CALL VEC_UBTOUB(.FALSE., N_POS * N_BOL,
     :           %VAL(CNF_PVAL(FILE_QUALITY_PTR)),
     :           %VAL(CNF_PVAL(QUALITY_PTR)), IERR,
     :           NERR, STATUS)

         END IF
      END IF

*     Free up the dummy variance and quality arrays if they were used

      IF (DUMMY_VARIANCE_PTR .NE. 0) THEN
         CALL SCULIB_FREE ('DUMMY_VAR', DUMMY_VARIANCE_PTR,
     :        DUMMY_ENDVAR_PTR, STATUS)
      END IF

      IF (DUMMY_QUALITY_PTR .NE. 0) THEN
         CALL SCULIB_FREE ('DUMMY_QUAL', DUMMY_QUALITY_PTR,
     :        DUMMY_ENDQ_PTR, STATUS)
      END IF



*     Annul the NDF section
      CALL NDF_UNMAP(SECNDF, '*', STATUS)
      CALL NDF_ANNUL(SECNDF, STATUS)

*     SCUBA SECTION
*     If a SCUBA section has been specified then we need to apply it
      IF (NSPEC .GT. 0) THEN

*     If we are using the section we actually want the inverse
*     section to be set bad and not the section itself.
         USE_INTS = .NOT.USE_SECTION

*     decode the data specifications

         SWITCH_EXPECTED = .FALSE.

         CALL SCULIB_MASK_DATA(USE_INTS, 'REAL', NSPEC, DATA_SPEC,
     :        %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :        1, N_EXPOSURES, N_INTEGRATIONS,
     :        N_MEASUREMENTS, N_POS, N_BOL, 1, SWITCH_EXPECTED,
     :        VAL__BADR, BTEMP, 0, .TRUE., DATA_PTR,
     :        STATUS)

      END IF


*     Get some memory for the bolometer positions
*     Note that we have 1 position per beam

      CALL SCULIB_MALLOC (N_POS * N_BOL * N_BEAMS * VAL__NBD,
     :     BOL_RA_PTR, BOL_RA_END, STATUS)
      CALL SCULIB_MALLOC (N_POS * N_BOL * N_BEAMS * VAL__NBD,
     :     BOL_DEC_PTR, BOL_DEC_END, STATUS)

*     Get some memory for the LST information
      IF (USE_LST) THEN
         CALL SCULIB_MALLOC (N_POS * VAL__NBD,
     :        LST_PTR(1), LST_PTR(2), STATUS)

*     Fill it with Bad values
         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_CFILLD(N_POS, VAL__BADD,
     :                         %VAL(CNF_PVAL(LST_PTR(1))))
         END IF

      END IF

*     Loop through bolometers and find apparent RA/Dec
      IF (STATUS .EQ. SAI__OK) THEN

         CALL SURFLIB_PROCESS_BOLS(TSKNAME,1, N_BOL,
     :        N_POS, N_BEAMS, N_SWITCHES, N_EXPOSURES,
     :        N_INTEGRATIONS, N_MEASUREMENTS,
     :        1, N_EXPOSURES, 1, N_INTEGRATIONS, 1,N_MEASUREMENTS,
     :        N_FILE, N_FITS, FITS,
     :        %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :        %VAL(CNF_PVAL(IN_LST_STRT_PTR)),
     :        IN_ROTATION, SAMPLE_MODE,
     :        SAMPLE_COORDS, OUT_COORDS, JIGGLE_REPEAT,
     :        JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y, JIGGLE_P_SWITCH,
     :        FOCAL_STATION, IN_RA_CEN, IN_DEC_CEN,
     :        %VAL(CNF_PVAL(IN_RA1_PTR)), %VAL(CNF_PVAL(IN_RA2_PTR)),
     :        %VAL(CNF_PVAL(IN_DEC1_PTR)), %VAL(CNF_PVAL(IN_DEC2_PTR)),
     :        MJD_STANDARD,
     :        IN_UT1, IN_MJD1, IN_LONG_RAD, IN_LAT_RAD, IN_MJD2,
     :        IN_LONG2_RAD, IN_LAT2_RAD,
     :        LOCAL_COORDS, DBLE(MAP_X), DBLE(MAP_Y),
     :        N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :        SCUBA__NUM_CHAN, SCUBA__NUM_ADC, BOL_ADC, BOL_CHAN,
     :        BOL_DU3, BOL_DU4, SCAN_REVERSAL, 0.0D0, 0.0D0, 0.0, 0.0,
     :        %VAL(CNF_PVAL(BOL_DEC_PTR)), %VAL(CNF_PVAL(BOL_RA_PTR)),
     :        %VAL(CNF_PVAL(DATA_PTR)), 0, USE_LST,
     :        %VAL(CNF_PVAL(LST_PTR(1))),
     :        0, 0, STATUS)

      END IF

*     Store pointers to start of each integration in this map
*     This will crash if SCUBA__MAX_INT is defined as the
*     maximum number of integrations *PER* measurement
*     rather than the total number of integrations.

      N_INTS = N_MEASUREMENTS * N_INTEGRATIONS

      DATA_OFFSET = 1

      DO MEASUREMENT = 1, N_MEASUREMENTS
         DO INTEGRATION = 1, N_INTEGRATIONS

            CALL SCULIB_FIND_SWITCH(%VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :           1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :           N_POS, 1, 1, INTEGRATION,MEASUREMENT,
     :           INT_LIST(N_FILE, DATA_OFFSET), ITEMP, STATUS)

            DATA_OFFSET = DATA_OFFSET + 1

         END DO
      END DO

*     Also store int+1 so that I can easily calculate end of data
*     This must be zero if the preceeding value is zero
*     since this indicates an aborted data set.
      INT_LIST(N_FILE, DATA_OFFSET) = N_POS + 1

      IF (DATA_OFFSET .EQ. 1) INT_LIST(N_FILE, DATA_OFFSET) = 0
      IF (DATA_OFFSET .GT. 1) THEN
         IF (INT_LIST(N_FILE, DATA_OFFSET-1) .EQ. 0) THEN
            INT_LIST(N_FILE, DATA_OFFSET) = 0
         END IF
      END IF


*     Store pointers to start of each measurement in this map (for MEASREBIN)

      N_MEAS = N_MEASUREMENTS

      DATA_OFFSET = 1

      DO MEASUREMENT = 1, N_MEASUREMENTS

            CALL SCULIB_FIND_SWITCH(%VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :           1, N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :           N_POS, 1, 1, 1, MEASUREMENT,
     :           MEAS_LIST(N_FILE, DATA_OFFSET), ITEMP, STATUS)

            DATA_OFFSET = DATA_OFFSET + 1

      END DO

      MEAS_LIST(N_FILE, DATA_OFFSET) = N_POS + 1

*     Now look for the polarimetry arrays in the REDS extension

*     Note that at some point we may decide that it is easier to:
*       1 - Read the WPLATE data directly from the SCUCD extension
*           (ie run SURFLIB_FILL_WPLATE again)
*       2 - Read ANGROT directly from SURFLIB_PROCESS_BOLS and
*           average it here rather than reading it from the
*           file again.
*     I still think it is somewhat easier to rely on REMIP
*     being run first and storing the values there

*     Fill ANG_MEAS and ANG_INT with bad values -- this is required
*     since they will not be initialised at all if there are no
*     ANGROT or WPLATE extensions.

      DO I = 1, SCUBA__MAX_INT
         ANG_INT(N_FILE, I, 1) = VAL__BADR
         ANG_INT(N_FILE, I, 2) = VAL__BADR
      END DO

      DO I = 1, SCUBA__MAX_MEAS
         ANG_MEAS(N_FILE, I, 1) = VAL__BADR
         ANG_MEAS(N_FILE, I, 2) = VAL__BADR
      END DO

*     Only fill in the arrays if the WPLATE and ANGROT arrays
*     are present

      IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN

*     Search for a WPLATE NDF
         CALL DAT_THERE(IN_REDSX_LOC, 'WPLATE', THERE, STATUS)

         IF (THERE) THEN

*     Search for the fast axis
            FAST_AXIS = 0.0
            CALL DAT_THERE(IN_REDSX_LOC, 'FAST_AXIS', THERE, STATUS)
            IF (THERE) THEN
               CALL CMP_GET0R(IN_REDSX_LOC, 'FAST_AXIS', FAST_AXIS,
     :              STATUS)
            ELSE
               CALL MSG_OUTIF(MSG__QUIET, ' ','WARNING! Waveplate '//
     :              'angle is present without a fast axis offset. '//
     :              'Using an offset of 0.0 degrees', STATUS)
            END IF

*     Search for the ANGROT NDF

            CALL DAT_THERE(IN_REDSX_LOC, 'ANGROT', THERE, STATUS)

            IF (THERE) THEN

*     Okay, we have found WPLATE and ANGROT.
*     Get NDF identifiers
               CALL NDF_FIND(IN_REDSX_LOC, 'WPLATE', TNDF(1), STATUS)
               CALL NDF_FIND(IN_REDSX_LOC, 'ANGROT', TNDF(2), STATUS)

*     Map the data arrays
               CALL NDF_MAP(TNDF(1), 'DATA','_REAL','READ',
     :              WP_PTR, ITEMP, STATUS)
               CALL NDF_MAP(TNDF(2), 'DATA','_REAL','READ',
     :              ANG_PTR, ITEMP, STATUS)

*     Call the routine to copy the data from the NDFs to the
*     ANG_INT,ANG_MEAS arrays (because of all the pointers)

               CALL SURFLIB_FILL_POLPACK_ANGLES(MAX_FILE,
     :              SCUBA__MAX_INT, SCUBA__MAX_MEAS, N_FILE,
     :              N_INTEGRATIONS,
     :              N_MEASUREMENTS, %VAL(CNF_PVAL(WP_PTR)),
     :              %VAL(CNF_PVAL(ANG_PTR)),
     :              FAST_AXIS, ANG_INT, ANG_MEAS, STATUS)

*     Shut down
               CALL NDF_UNMAP(TNDF(1),'*', STATUS)
               CALL NDF_UNMAP(TNDF(2),'*', STATUS)
               CALL NDF_ANNUL(TNDF(1), STATUS)
               CALL NDF_ANNUL(TNDF(2), STATUS)

            END IF

         END IF

      END IF

*     Read the Units from the NDF and store them in the FITS airlock
*     use this to make sure that Units are propogated to the output
*     NDF (could also be used to check units for consistency).
*     Would not need to do this if we always propogated the output NDF
*     from the input rather than simply doing that when there is only
*     a single input file.

      CALL NDF_CGET(IN_NDF, 'Units', UNITS, STATUS)
      CALL SCULIB_PUT_FITS_C(SCUBA__MAX_FITS, N_FITS, FITS,
     :     'NDFUNITS', UNITS, 'Units as read from the NDF', STATUS)


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
