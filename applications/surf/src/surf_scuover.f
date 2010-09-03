      SUBROUTINE SURF_SCUOVER (STATUS)
*+
*  Name:
*     SCUOVER

*  Purpose:
*     Routine to overlay the bolometer names onto a rebinned image

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SURF_SCUOVER ( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status

*  Description:
*     This routine is used to overlay the array layout onto a rebinned
*     SCUBA image. The displayed image is read from the graphics database
*     unless a command line value is given. In order to calculate the bolometer
*     positions it is also necessary to read in the extinction corrected
*     data file that was used to regrid the data (in fact any extinction
*     corrected file can be used, possibly with strange results).
*     By default the position of the bolometers at the start of
*     the first integration and zero jiggle offset is plotted. Optionally,
*     it is possible to plot the bolometer positions at any point during
*     the observation (still with zero jiggle offset).

*  Usage:
*     scuover

*  ADAM parameters:
*     DEVICE = DEVICE (Read)
*        The graphics device on which the bolometers are to be drawn.
*        The global default (set with KAPPA GDSET) will be used unless
*        specified otherwise.
*     EXPOSURE = INTEGER (Read)
*        Ues the bolometer positions at the specified exposure within the
*        specified INTEGRATION and MEASUREMENT. For SCAN/MAP data the
*        middle of an exposure (ie scan) is used. Default is exposure 1.
*     EXT = NDF (Read)
*        The name of the extinction corrected data from which the bolometer
*        positions should be taken.
*     INTEGRATION = INTEGER (Read)
*        Use the bolometer positions at the specified integration. Default
*        is measurement 1.
*     MEASUREMENT = INTEGER (Read)
*        Use the bolometer positions at the specified exposure. Default
*        is measurement 1.
*     MSG_FILTER = CHAR (Read)
*        Message filter level. Default is NORM.
*     NDF = NDF (Read)
*        The name of the regridded data set (taken from the AGI graphics
*        database).
*     NAME = LOGICAL (Read)
*        Label with bolometer name if true, else bolometer number. The default
*        is true. If a null response is given the bolometer label is not
*        drawn.
*     STYLE = LITERAL (Read)
*        Plotting style to be used for the bolometers. The relevant key
*        to use for adjusting the plotting style is 'bolometer'. This
*        is a synonym for curve and can be abbreviated to 'bol'. The most
*        useful attribute to change is the colour. Explicit control
*        of the style of the text labels is provided using the 'text'
*        attribute. If this attribute is not supplied all plotting
*        styles are inherited from the 'bol' style.

*  Examples:
*     scuover
*        The bolometer names will be overlaid using the default colour.
*     scuover style='colour(bol)=red' noname
*        This command will overlay bolometer numbers over the image in red.
*     scuover style='colour(bol)=yellow,font(text)=3,colour(text)=cyan'
*        Draw yellow circles but draw the labels in cyan using font 3
*        (italics).
*     scuover integration=2 name=!
*        Overlay the bolometer positions at the start of the second
*        integration but do not label them

*  Notes:
*     - An image must have already been displayed before using SCUOVER.
*     - The image must have been displayed using an AST aware application
*     (such as KAPPA DISPLAY). Otherwise the alignment will not occur.
*     - The array position is always shown with zero jiggle offset.
*     - This routine does not take into account the use of SHIFT_DX or
*     SHIFT_DY in REBIN. (the relevant information is not stored in the
*     rebinned image).
*     - Pointing shifts are taken into account.
*     - It is assumed that the displayed image id regridded to the
*     default map centre. SCUOVER aligns the image using AXIS coordinates
*     and not SKY coordinates therefore there will be an error if incorrect
*     sky coordinates are assumed.

*  Related Applications:
*     SURF: REBIN, SCUPA
*     KAPPA: DISPLAY, GDSET

*  Authors:
*     JFL: J.Lightfoot (ROE)
*     TIMJ: T. Jenness (timj@jach.hawaii.edu)


*  Copyright:
*     Copyright (C) 1995,1996,1997,1998,1999 Particle Physics and Astronomy
*     Research Council. All Rights Reserved.

*  History:
*     $Log$
*     Revision 1.22  2005/03/23 03:46:10  timj
*     - Some variable initialisations when status is bad
*     - Fix size of maximum filename read from fits header
*
*     Revision 1.21  2005/03/19 01:41:02  timj
*     Propogate focal station from app level to calc_bol_coords
*
*     Revision 1.20  2004/11/18 20:40:10  timj
*     CNF_PVAL brackets were in the wrong place
*
*     Revision 1.19  2004/09/08 02:03:34  timj
*     Add CNF_PVAL where appropriate
*
*     Revision 1.18  2004/07/14 21:46:59  timj
*     Remove obsolete GKS_PAR reference
*
*     Revision 1.17  2000/07/07 03:24:16  timj
*     Doc fixes
*
*     Revision 1.16  2000/06/16 01:24:38  timj
*     Plot text in different style to circles
*
*     Revision 1.15  2000/06/03 03:14:45  timj
*     Replace SGS with AST/PGPLOT
*
*     Revision 1.14  1999/08/03 20:01:42  timj
*     Add copyright message to header.
*     Minor fixes to header style.
*
*     1996 December 17 (TIMJ)
*        Original version
*     {note_history_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global constants:
      INCLUDE 'AST_PAR'                ! AST__ constants
      INCLUDE 'DAT_PAR'                ! DAT__ constants
      INCLUDE 'NDF_PAR'                ! for NDF__xxxx constants
      INCLUDE 'PRM_PAR'                ! for VAL__xxxx constants
      INCLUDE 'PAR_ERR'                ! for PAR__ constants
      INCLUDE 'PAR_PAR'                ! for PAR__ constants
      INCLUDE 'SURF_PAR'               ! SURF definitions
      INCLUDE 'SAE_PAR'                ! SSE global definitions
      INCLUDE 'MSG_PAR'                ! MSG__ constants
      INCLUDE 'CNF_PAR'                ! For CNF_PVAL function

*  Status:
      INTEGER STATUS

*  External references:
      INTEGER CHR_LEN                  ! CHR used-string-length function
      DOUBLE PRECISION SLA_EPJ2D       ! Convert from Julian Epoch to MJD

*  Local Constants:
      INTEGER     MAX_DIM              ! max number of dims in array
      PARAMETER (MAX_DIM = 4)
      REAL AR                          ! A `normal' text aspect ratio
      PARAMETER ( AR = 0.66667 )
      CHARACTER * 7 TSKNAME            ! SCUOVER name
      PARAMETER (TSKNAME = 'SCUOVER')

*  Local variables:
      LOGICAL          ABORTED         ! .TRUE. if an observation has been
                                       ! aborted
      LOGICAL          ALIGN           ! were the two frames aligned
      DOUBLE PRECISION ATTRS1( 20 )    ! PGPLOT CURVES attributes on entry
      DOUBLE PRECISION ATTRS2( 20 )    ! PGPLOT TEXT attributes on entry
      INTEGER          AX2GRMAP        ! Mapping between AXIS and GRAPHICS frm
      CHARACTER*(1)    B1              ! Name of bolometer (1 char)
      CHARACTER*(2)    B2              ! Name of bolometer (2 char)
      CHARACTER*(3)    BOL             ! Name of bolometer
      LOGICAL          BOLNAME         ! Show true bolometer name?
      INTEGER          BOL_ADC (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! A/D numbers of bolometers measured in
                                       ! input file
      INTEGER          BOL_CHAN (SCUBA__NUM_CHAN * SCUBA__NUM_ADC)
                                       ! channel numbers of bolometers measured
                                       ! in input file
      INTEGER          BOL_DEC_END
                                       ! pointer to end of BOL_DEC_PTR scratch
                                       ! space
      INTEGER          BOL_DEC_PTR
                                       ! pointer to scratch space holding
                                       ! apparent Dec / y offset positions of
                                       ! measured points in input file (radians)
      DOUBLE PRECISION BOL_DIST        ! Separation of bols
      REAL             BOL_DIST_WORLD  ! Separation of bols in WORLD coords
      REAL             BOL_DU3 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU3 Nasmyth coord of bolometers
      REAL             BOL_DU4 (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! dU4 Nasmyth coord of bolometers
      INTEGER          BOL_RA_END
                                       ! pointer to end of BOL_RA_PTR scratch
                                       ! space
      INTEGER          BOL_RA_PTR
                                       ! pointer to scratch space holding
                                       ! apparent RA / x offset positions of
                                       ! measured points in input file (radians)
      CHARACTER*20     BOL_TYPE (SCUBA__NUM_CHAN, SCUBA__NUM_ADC)
                                       ! bolometer types
      DOUBLE PRECISION BOX             ! Dummy for KPG1_PLOT
      LOGICAL          DEVCAN          ! The device parameter is to be cancelled?
      INTEGER          DIM (MAX_DIM)   ! array dimensions
      DOUBLE PRECISION DTEMP           ! Temp double
      DOUBLE PRECISION DXTEMP          ! Temp double for X coord
      DOUBLE PRECISION DYTEMP          ! Temp double for Y coord
      INTEGER          END_EXP         ! Last exposure
      INTEGER          END_INT         ! Last integration
      INTEGER          END_MEAS        ! Last measurement
      LOGICAL          EXTINCTION      ! .TRUE. if EXTINCTION application has
                                       ! been run on input file
      INTEGER          EXT_NDF         ! NDF id of extinction file
      CHARACTER*80     FILENAME        ! default Name of extinction file
                                       ! names of input files read
      CHARACTER*80     FITS (SCUBA__MAX_FITS)
                                       ! array of FITS keywords
      LOGICAL          FLATFIELD       ! .TRUE. if the FLATFIELD application
                                       ! has been run on the input file
      CHARACTER*10     FOCAL_STATION   ! Where is the instrument located?
      INTEGER          FSET            ! AST_FRAMESET object for 2D AXIS frame
      LOGICAL          GOTLOC          ! A locator to the NDF has been
                                       ! obtained?
      LOGICAL          GOTNAM          ! A reference name of the NDF has been
                                       ! obtained?
      REAL             HGT             ! Height for key text
      INTEGER          I               ! DO loop index
      INTEGER          IAXFRM          ! Index of AXIS domain in AST frameset
      INTEGER          IERR            ! Position of error from VEC_
      CHARACTER*20     INSTRUMENT       ! Name of instrument
      CHARACTER*15     IN_CENTRE_COORDS! coord system of telescope centre in
                                       ! an input file
      DOUBLE PRECISION IN_DEC_CEN      ! apparent Dec of input file map centre
                                       ! (radians)
      INTEGER          IN_DEC1_PTR     ! pointer to .SCUCD.DEC1
      INTEGER          IN_DEC2_PTR     ! pointer to .SCUCD.DEC2
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
                                       ! was at IN_LAT,IN_LONG for PLANET
                                       ! centre coordinate system
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
      DOUBLE PRECISION IN_UT1          ! UT1 at start of an input observation,
                                       ! expressed as modified Julian day
      DOUBLE PRECISION INXPOS( 2 )     ! X positions of test bolometer
      DOUBLE PRECISION INYPOS( 2 )     ! Y positions of test bolometer
      INTEGER          IPIC            ! AGI idents from KPG1_PLOT [not used]
      INTEGER          IPICD           ! AGI ident of new DATA picture
      INTEGER          IPICF           ! AGI ident of new FRAME picture
      INTEGER          IPLOT           ! AST pointer to new plot
      INTEGER          ITEMP           ! scratch integer
      INTEGER          IWCS            ! AST FrameSet
      INTEGER          IWCSNEW         ! New AST FrameSet
      INTEGER          JIGGLE_COUNT    ! number of jiggles in pattern
      INTEGER          JIGGLE_P_SWITCH ! number of jiggles per switch
      INTEGER          JIGGLE_REPEAT   ! number of times jiggle pattern is
                                       ! repeated in a switch
      REAL             JIGGLE_X (SCUBA__MAX_JIGGLE)
                                       ! x jiggle offsets (arcsec)
      REAL             JIGGLE_Y (SCUBA__MAX_JIGGLE)
                                       ! y jiggle offsets (arcsec)
      DOUBLE PRECISION LAT_OBS         ! Latitude of observatory
      CHARACTER * ( DAT__SZLOC ) LOCI  ! Locator for input data structure
      CHARACTER*(15)   LOCAL_COORDS    ! Coordinate system of MAP_X and MAP_Y
      REAL             MAP_X           ! x offset of map centre from telescope
                                       ! centre (radians)
      REAL             MAP_Y           ! y offset of map centre from telescope
                                       ! centre (radians)
      REAL             MARGIN( 4 )     ! Dummy for KPG1_PLOT call
      DOUBLE PRECISION MJD_STANDARD    ! date for which apparent RA,Decs of all
                                       ! measured positions are calculated
      INTEGER          NDIM            ! the number of dimensions in an array
      INTEGER          NDF             ! NDF id of input image
      INTEGER          NERR            ! Number of errors from VEC_
      INTEGER          NFRM            ! AXIS frame number
      INTEGER          NREC            ! number of history records in input file
      INTEGER          N_BOL           ! number of bolometers measured in input
                                       ! files
      INTEGER          N_EXPOSURES     ! number of exposures per integration
                                       ! in input file
      INTEGER          N_FITS          ! number of items in FITS array
      INTEGER          N_INTEGRATIONS  ! number of integrations per measurement
                                       ! in input file
      INTEGER          N_MEASUREMENTS  ! number of measurements in input file
      INTEGER          N_POINT         ! dimension of pointing correction
                                       ! array in input file
      INTEGER          N_POS           ! number of positions measured in input
                                       ! files
      INTEGER          N_SWITCHES      ! number of switches per exposure in
                                       ! input file
      CHARACTER*40     OBJECT          ! name of object
      CHARACTER*40     OBSERVING_MODE  ! observing mode of input file
      CHARACTER*10     OUT_COORDS      ! coordinate system of output map
      CHARACTER*10     OUTCRDS         ! Dummy output coords variable
      DOUBLE PRECISION OUT_DEC_CEN     ! apparent Dec of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_LAT         ! longitude of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_LONG        ! longitude of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_RA_CEN      ! apparent RA of output map centre
                                       ! (radians)
      DOUBLE PRECISION OUT_ROTATION    ! angle between apparent N and N of
                                       ! output coord system (radians)
      LOGICAL          PLOTNAME        ! Plot the name of the bolometer
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
      CHARACTER * ( 256 ) REFNAM       ! Reference name
      REAL             RDTEMP          ! scratch real
      REAL             RTEMP           ! scratch real
      INTEGER          RUN_NUMBER      ! run number of input file
      CHARACTER*15     SAMPLE_COORDS   ! coordinate system of sample offsets
      CHARACTER*15     SAMPLE_MODE     ! sample mode of input file
      REAL             SAMPLE_PA       ! position angle of sample x axis
                                       ! relative to x axis of SAMPLE_COORDS
                                       ! system
      REAL             SHIFT_DX
                                       ! x shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      REAL             SHIFT_DY
                                       ! y shift to be applied to component map
                                       ! in OUTPUT_COORDS frame (radians)
      CHARACTER*80     STATE           ! 'state' of SCUCD at the end of
                                       ! the observation
      INTEGER          START_EXP       ! First exposure
      INTEGER          START_INT       ! First integration
      INTEGER          START_MEAS      ! First measurement
      CHARACTER*80     STEMP           ! scratch string
      CHARACTER*20     SUB_INSTRUMENT  ! Sub-instrument name
      CHARACTER*20     TELESCOPE       ! Name of telescope
      DOUBLE PRECISION XTEMP(2)        ! X pos of first two bols
      DOUBLE PRECISION YTEMP(2)        ! Y pos of first two bols

*.

      IF (STATUS .NE. SAI__OK) RETURN

*     Start up the error system
      CALL ERR_BEGIN(STATUS)

      DEVCAN = .FALSE.
      GOTNAM = .FALSE.
      GOTLOC = .FALSE.

*     Initialise PLOTNAME logical
      PLOTNAME = .TRUE.




*     Begin an AST Context.
      CALL AST_BEGIN( STATUS )

*     Begin an NDF context.
      CALL NDF_BEGIN

*     Get the AST plot
*     ================

*     Create a frameset with a AXIS domain
*     that we can use for alignment purposes
      FSET = AST_FRAMESET( AST_FRAME( 2, 'Domain=AXIS', STATUS ),
     :     ' ', STATUS)

*     Open the graphics device and workstation. An error is reported if no
*     existing DATA picture can be found. An AST Plot is returned which
*     can be used to draw in the existing Plot.
      MARGIN(1) = 0
      MARGIN(2) = 0
      MARGIN(3) = 0
      MARGIN(4) = 0
      CALL KPG1_PLOT( FSET, 'OLD', 'SURF_SCUOVER',
     :     ' ', MARGIN, 0, ' ', ' ', 0.0, 1.0, 'PIXEL', BOX,
     :     IPICD, IPICF, IPIC, IPLOT, NFRM, ALIGN, STATUS)

*     Obtain a reference to the NDF.
*     ==============================
      CALL KPG1_AGREF( IPICD, 'READ', GOTNAM, REFNAM, STATUS )

*     See whether the reference is a name or locator.  The latter should
*     be phased out, but there may be some old databases and software in
*     circulation.
      CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
      IF ( GOTLOC ) LOCI = REFNAM

*     End immediately if there was an error.
      IF ( STATUS .NE. SAI__OK ) THEN
         DEVCAN = .TRUE.
         GOTO 980
      END IF

*     Obtain the NDF.
*     ===============

*     Begin an NDF context
      CALL NDF_BEGIN

*     Obtain the NDF.  If the name is given on the command line it will be
*     used.  If not, the database data reference is used, if there is one.
*     Otherwise, the user is prompted.
      CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, NDF, STATUS )


*     Read in FITS header
*     ===================

      CALL NDF_XLOC (NDF, 'FITS', 'READ', IN_FITSX_LOC,
     :     STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK',TSKNAME)
            CALL ERR_REP (' ', '^TASK: input '//
     :           'file contains too many FITS items', STATUS)
         END IF
      END IF

      CALL DAT_GET1C (IN_FITSX_LOC, SCUBA__MAX_FITS, FITS,
     :     N_FITS, STATUS)
      CALL DAT_ANNUL (IN_FITSX_LOC, STATUS)

*     Read in SCUPROJ and FILE_1

      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SCUPROJ', OUT_COORDS, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'FILE_1', FILENAME, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MJD-OBS', MJD_STANDARD, STATUS)

*     If could not find it, try reading from AST
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL(STATUS)
            CALL NDF_GTWCS(NDF, IWCS, STATUS)
            IF (STATUS .NE. SAI__OK) THEN
               CALL ERR_REP(' ','Error reading WCS/AST in NDF'//
     :              ' whilst looking for MJD-OBS',STATUS)
            ELSE IF (IWCS .EQ. AST__NULL) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','Read null AST component whilst '//
     :              'looking for MJD-OBS', STATUS)
            ELSE IF ( AST_GETC( IWCS, 'Class', STATUS)
     :              .NE.'FrameSet' ) THEN
               STATUS = SAI__ERROR
               CALL ERR_REP(' ','WCS read correctly but was not a '//
     :              'FrameSet', STATUS)
            ELSE

*     Select a sky frame
*     This should set the sky frame to the current frame
*     if it isnt already
               IF (.NOT. AST_ISASKYFRAME(IWCS, STATUS) ) THEN
                  IWCSNEW = AST_FINDFRAME( IWCS,
     :                 AST_SKYFRAME( ' ', STATUS ), ' ', STATUS )
               END IF

*     Read the epoch
               DTEMP = AST_GETD(IWCS, 'Epoch', STATUS)

*     Convert to MJD
               IF (STATUS .EQ. SAI__OK) THEN
                  MJD_STANDARD = SLA_EPJ2D( DTEMP )
               END IF

            END IF

         END IF

      END IF


      IF (OUT_COORDS.NE.'AZ' .AND. OUT_COORDS.NE.'NA'
     :     .AND. OUT_COORDS.NE.'PL') THEN
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LAT', OUT_LAT, STATUS)
         CALL SCULIB_GET_FITS_D (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'LONG', OUT_LONG, STATUS)
      ELSE
         OUT_LAT = 0.0D0
         OUT_LONG = 0.0D0
      END IF

*  Tidy up
      CALL NDF_ANNUL(NDF, STATUS)

*  Abort if STATUS is bad
      IF (STATUS .NE. SAI__OK) THEN
         CALL NDF_END(STATUS)
         DEVCAN = .TRUE.
         GO TO 980
      END IF


*     Open EXTINCTION file
*     ====================

      CALL PAR_DEF0C('EXT', FILENAME, STATUS)

      CALL NDF_ASSOC('EXT', 'READ', EXT_NDF, STATUS)

*     Check HISTORY to make sure it is a SCUBA extinction corrected
*     demodulated data file

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_HNREC (EXT_NDF, NREC, STATUS)
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
               CALL NDF_HINFO (EXT_NDF, 'APPLICATION',
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
               END IF
            END DO
         END IF

         IF (STATUS .EQ. SAI__OK) THEN
            IF (.NOT. EXTINCTION) THEN
               STATUS = SAI__ERROR
               CALL MSG_SETC('TASK', TSKNAME)
               CALL ERR_REP (' ','^TASK: '//
     :              'the input data has not been corrected for '//
     :              'EXTINCTION. Please try again.', STATUS)
            END IF
         END IF
      END IF


*     Get the Bolometer NA coordinates
*     ================================

      CALL NDF_DIM (EXT_NDF, MAX_DIM, DIM, NDIM, STATUS)
      N_BOL = DIM (1)
      N_POS = DIM (2)


*  Get locators

      CALL NDF_XLOC (EXT_NDF, 'SCUBA', 'READ',
     :     IN_SCUBAX_LOC, STATUS)
      CALL NDF_XLOC (EXT_NDF, 'SCUCD', 'READ',
     :     IN_SCUCDX_LOC, STATUS)

      IF (STATUS .EQ. SAI__OK) THEN
         CALL NDF_XLOC (EXT_NDF, 'REDS', 'READ', IN_REDSX_LOC,
     :        STATUS)
         IF (STATUS .NE. SAI__OK) THEN
            CALL ERR_ANNUL (STATUS)
            IN_REDSX_LOC = DAT__NOLOC
         END IF
      END IF

*  get the bolometer description arrays

      CALL SCULIB_GET_BOL_DESC(IN_SCUBAX_LOC, SCUBA__NUM_CHAN,
     :     SCUBA__NUM_ADC, N_BOL, BOL_TYPE, BOL_DU3,
     :     BOL_DU4, BOL_ADC, BOL_CHAN, STATUS)

*     Read in FITS header from EXT
*     ============================

      CALL NDF_XLOC (EXT_NDF, 'FITS', 'READ', IN_FITSX_LOC,
     :     STATUS)

      CALL DAT_SIZE (IN_FITSX_LOC, ITEMP, STATUS)
      IF (ITEMP .GT. SCUBA__MAX_FITS) THEN
         IF (STATUS .EQ. SAI__OK) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC('TASK',TSKNAME)
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
      OBSERVING_MODE = ' '
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'MODE', OBSERVING_MODE, STATUS)
      CALL CHR_UCASE (OBSERVING_MODE)
      SAMPLE_MODE = ' '
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'SAM_MODE', SAMPLE_MODE, STATUS)
      CALL CHR_UCASE (SAMPLE_MODE)

*     Telescope and instrument name and sub-instrument name
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'INSTRUME', INSTRUMENT, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'TELESCOP', TELESCOPE, STATUS)

*     We know there can only be one sub instrument and that it is
*     in the BOL_TYPE array
      IF (STATUS .EQ. SAI__OK) THEN
         SUB_INSTRUMENT = BOL_TYPE( BOL_CHAN(1), BOL_ADC(1) )
      END IF

*     And calculate the focal station
      CALL SURFLIB_GET_FOCAL_STATION( TELESCOPE, INSTRUMENT,
     :     SUB_INSTRUMENT, FOCAL_STATION, STATUS )

*     coords of telescope centre

      IN_CENTRE_COORDS = ' '
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'CENT_CRD', IN_CENTRE_COORDS, STATUS)
      CALL CHR_UCASE (IN_CENTRE_COORDS)


      IF (OUT_COORDS .EQ. 'PL' .AND.
     :     IN_CENTRE_COORDS .NE. 'PLANET') THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC('TASK', TSKNAME)
         CALL ERR_REP(' ', '^TASK: The image was rebinned with '//
     :        'PL coordinates but the extinction corrected file '//
     :        'does not have a moving coordinate system.',
     :        STATUS)
      END IF

      IN_LONG_RAD = 0.0D0
      IN_LAT_RAD = 0.0D0
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LAT', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LAT_RAD, STATUS)
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS,
     :     'LONG', STEMP, STATUS)
      CALL SCULIB_DECODE_ANGLE (STEMP, IN_LONG_RAD, STATUS)

      IN_LONG2_RAD = 0.0D0
      IN_LAT2_RAD = 0.0D0
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
      END IF

      IF ((IN_CENTRE_COORDS .NE. 'AZ') .AND.
     :     (IN_CENTRE_COORDS .NE. 'GA') .AND.
     :     (IN_CENTRE_COORDS .NE. 'PL') .AND.
     :     (IN_CENTRE_COORDS .NE. 'NA')) THEN
         IN_LONG_RAD = IN_LONG_RAD * 15.0D0
         IN_LONG2_RAD = IN_LONG2_RAD * 15.0D0
      END IF

*     offset from telescope centre

      MAP_X = 0.0
      MAP_Y = 0.0
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
      LAT_OBS = 0.0D0
      CALL SCULIB_GET_FITS_D (N_FITS, N_FITS, FITS,
     :     'LAT-OBS', LAT_OBS, STATUS)
      LAT_OBS = LAT_OBS * PI / 180.0D0

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

*     Find the start LST
*     map the DEM_PNTR and LST arrays and check their dimensions

      CALL SCULIB_GET_DEM_PNTR(3, IN_SCUBAX_LOC,
     :     IN_DEM_PNTR_PTR, ITEMP, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*  Check LST_STRT
      CALL SCULIB_GET_LST_STRT(IN_SCUCDX_LOC, IN_LST_STRT_PTR,
     :     N_SWITCHES, N_EXPOSURES, N_INTEGRATIONS,
     :     N_MEASUREMENTS, STATUS)

*  UT at which observation was made expressed as modified Julian day

      CALL SCULIB_GET_MJD(N_FITS, FITS, %VAL(CNF_PVAL(IN_LST_STRT_PTR)),
     :                    IN_UT1,
     :     RTEMP, RTEMP, STATUS)

*  see if the observation completed normally or was aborted

      STATE = ' '
      CALL SCULIB_GET_FITS_C (SCUBA__MAX_FITS, N_FITS, FITS, 'STATE',
     :  STATE, STATUS)
      CALL CHR_UCASE (STATE)
      ABORTED = .FALSE.
      IF (INDEX(STATE,'ABORTING') .NE. 0) THEN
         ABORTED = .TRUE.
      END IF

*     Write out the standard info concerning number of int/meas/exp
* Print out information on observation

      IF (ABORTED) THEN

*  get the exposure, integration, measurement numbers at which the abort
*  occurred

         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'EXP_NO', N_EXPOSURES, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'INT_NO', N_INTEGRATIONS, STATUS)
         CALL SCULIB_GET_FITS_I (SCUBA__MAX_FITS, N_FITS, FITS,
     :        'MEAS_NO', N_MEASUREMENTS, STATUS)

      END IF

      CALL MSG_SETI ('N_E', N_EXPOSURES)
      CALL MSG_SETI ('N_I', N_INTEGRATIONS)
      CALL MSG_SETI ('N_M', N_MEASUREMENTS)

      CALL MSG_SETC ('PKG', PACKAGE)
      CALL MSG_OUTIF (MSG__NORM, ' ',
     :     '^PKG: file contains data for ^N_E '//
     :     'exposure(s) in ^N_I integration(s) in '//
     :     '^N_M measurement(s)', STATUS)


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

*     now read in data specific to the sample mode of the observation

      IF (SAMPLE_MODE .EQ. 'JIGGLE') THEN

         CALL SCULIB_GET_JIGGLE(IN_SCUCDX_LOC, SCUBA__MAX_JIGGLE,
     :        N_FITS, FITS, JIGGLE_COUNT, JIGGLE_REPEAT,
     :        JIGGLE_P_SWITCH, SAMPLE_PA, SAMPLE_COORDS, JIGGLE_X,
     :        JIGGLE_Y, STATUS)

*     Actually don't want any jiggling for the overlay so
         DO I = 1, JIGGLE_COUNT
            JIGGLE_X(I) = 0.0
            JIGGLE_Y(I) = 0.0
         END DO

*     likewise for raster maps

      ELSE IF (SAMPLE_MODE .EQ. 'RASTER') THEN

         CALL SCULIB_GET_RASTER(IN_SCUCDX_LOC, N_SWITCHES,
     :        N_EXPOSURES, N_INTEGRATIONS, N_MEASUREMENTS,
     :        IN_RA1_PTR, IN_RA2_PTR, IN_DEC1_PTR, IN_DEC2_PTR,
     :        STATUS)

      END IF

*     If I am doing a AZ/NA raster overlay I need to get the output
*     longitude and latitude from the extinction FITS header
*     Note that I rebin in tangent plane centred on RD coordinates.

      IF (SAMPLE_MODE .EQ. 'RASTER' .AND.
     :     (OUT_COORDS .EQ. 'NA' .OR. OUT_COORDS .EQ. 'AZ')) THEN

         OUTCRDS = 'RD'
         CALL SCULIB_CALC_OUTPUT_COORDS (IN_RA_CEN, IN_DEC_CEN,
     :        MJD_STANDARD, OUTCRDS, OUT_LONG, OUT_LAT, STATUS)

      END IF

*     Ask for the integration, exposure, and measurement

      START_MEAS = 1
      START_EXP = 1
      START_INT = 1

      IF (N_MEASUREMENTS .GT. 1) THEN
         CALL PAR_DEF0I('MEASUREMENT', START_MEAS, STATUS)
         CALL PAR_MINI('MEASUREMENT', 1, STATUS)
         CALL PAR_MAXI('MEASUREMENT', N_MEASUREMENTS, STATUS)
         CALL PAR_GET0I('MEASUREMENT', START_MEAS, STATUS)
      END IF
      IF (N_INTEGRATIONS .GT. 1) THEN
         CALL PAR_DEF0I('INTEGRATION', START_INT, STATUS)
         CALL PAR_MINI('INTEGRATION', 1, STATUS)
         CALL PAR_MAXI('INTEGRATION', N_INTEGRATIONS, STATUS)
         CALL PAR_GET0I('INTEGRATION', START_INT, STATUS)
      END IF
      IF (N_EXPOSURES .GT. 1) THEN
         CALL PAR_DEF0I('EXPOSURE', START_EXP, STATUS)
         CALL PAR_MINI('EXPOSURE', 1, STATUS)
         CALL PAR_MAXI('EXPOSURE', N_EXPOSURES, STATUS)
         CALL PAR_GET0I('EXPOSURE', START_EXP, STATUS)
      END IF

      END_MEAS = START_MEAS
      END_EXP = START_EXP
      END_INT = START_INT

*     Allocate some memory
      BOL_RA_PTR = 0
      BOL_RA_END = 0
      BOL_DEC_PTR = 0
      BOL_DEC_END = 0

      CALL SCULIB_MALLOC (N_BOL * VAL__NBD,
     :     BOL_RA_PTR, BOL_RA_END, STATUS)
      CALL SCULIB_MALLOC (N_BOL * VAL__NBD,
     :     BOL_DEC_PTR, BOL_DEC_END, STATUS)

*     Only want position for first exposure, integration and measurement
*     calculate position of each bolometer at first measurement

      CALL SURFLIB_PROCESS_BOLS(TSKNAME, 0, N_BOL,
     :     N_POS, 1, N_SWITCHES, N_EXPOSURES,
     :     N_INTEGRATIONS,N_MEASUREMENTS,
     :     START_EXP, END_EXP, START_INT, END_INT, START_MEAS, END_MEAS,
     :     0, N_FITS, FITS,
     :     %VAL(CNF_PVAL(IN_DEM_PNTR_PTR)),
     :     %VAL(CNF_PVAL(IN_LST_STRT_PTR)),
     :     IN_ROTATION, SAMPLE_MODE,
     :     SAMPLE_COORDS, OUT_COORDS, JIGGLE_REPEAT,
     :     JIGGLE_COUNT, JIGGLE_X, JIGGLE_Y, JIGGLE_P_SWITCH,
     :     FOCAL_STATION, IN_RA_CEN, IN_DEC_CEN,
     :     %VAL(CNF_PVAL(IN_RA1_PTR)), %VAL(CNF_PVAL(IN_RA2_PTR)),
     :     %VAL(CNF_PVAL(IN_DEC1_PTR)), %VAL(CNF_PVAL(IN_DEC2_PTR)),
     :     MJD_STANDARD,
     :     IN_UT1,IN_MJD1, IN_LONG_RAD, IN_LAT_RAD, IN_MJD2,
     :     IN_LONG2_RAD, IN_LAT2_RAD,
     :     LOCAL_COORDS, DBLE(MAP_X), DBLE(MAP_Y),
     :     N_POINT, POINT_LST, POINT_DAZ, POINT_DEL,
     :     SCUBA__NUM_CHAN, SCUBA__NUM_ADC, BOL_ADC, BOL_CHAN,
     :     BOL_DU3, BOL_DU4, .FALSE., 0.0D0, 0.0D0, 0.0, 0.0,
     :     %VAL(CNF_PVAL(BOL_DEC_PTR)), %VAL(CNF_PVAL(BOL_RA_PTR)),
     :     0.0, 0.0, .FALSE., 0, 0, 0, STATUS)

*     annul locators and array identifiers and close the file

      CALL CMP_UNMAP (IN_SCUBAX_LOC, 'DEM_PNTR', STATUS)
      CALL CMP_UNMAP (IN_SCUCDX_LOC, 'LST_STRT', STATUS)

      IF (SAMPLE_MODE .EQ. 'RASTER') THEN
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA1', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'RA2', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC1', STATUS)
         CALL CMP_UNMAP(IN_SCUCDX_LOC, 'DEC2', STATUS)
      END IF

      CALL DAT_ANNUL (IN_SCUBAX_LOC, STATUS)
      CALL DAT_ANNUL (IN_SCUCDX_LOC, STATUS)
      IF (IN_REDSX_LOC .NE. DAT__NOLOC) THEN
         CALL DAT_ANNUL (IN_REDSX_LOC, STATUS)
      END IF

      CALL NDF_ANNUL (EXT_NDF, STATUS)

*     END NDF CONTEXT
      CALL NDF_END(STATUS)

*     OK, all the data required should have been read in by now, check that
*     there is some input data

*     Now just need to plot the bolometer positions

*     Nasmyth rebin doesn't need a coordinate frame

      IF (OUT_COORDS.NE.'NA'.AND.OUT_COORDS.NE.'AZ'.AND.
     :     OUT_COORDS .NE. 'PL') THEN

*     calculate the apparent RA,Dec of the selected output centre

         CALL SCULIB_CALC_APPARENT (LAT_OBS, OUT_LONG, OUT_LAT, 0.0D0,
     :        0.0D0, 0.0D0, 0.0D0, OUT_COORDS, 0.0, MJD_STANDARD, 0.0D0,
     :        0.0D0, OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION, STATUS)


*     convert the RA,Decs of the observed points to tangent plane offsets
*     from the chosen output centre

         SHIFT_DX = 0.0         ! dont support shift yet
         SHIFT_DY = 0.0

         IF (STATUS .EQ. SAI__OK) THEN
            CALL SCULIB_APPARENT_2_TP (N_BOL,
     :           %VAL(CNF_PVAL(BOL_RA_PTR)),
     :           %VAL(CNF_PVAL(BOL_DEC_PTR)),
     :           OUT_RA_CEN, OUT_DEC_CEN, OUT_ROTATION,
     :           DBLE(SHIFT_DX), DBLE(SHIFT_DY), STATUS)
         END IF

      END IF

      IF (STATUS .EQ. SAI__OK) THEN

*     Find the frame associated with the AXIS domain
*     ==============================================

         CALL KPG1_ASFFR( IPLOT, 'AXIS', IAXFRM, STATUS)

*     Set up a mapping between this frame and the GRAPHICS frame
*     This will allow us to translate bolometer positions to
*     graphics coordinates
         AX2GRMAP = AST_GETMAPPING( IPLOT, IAXFRM, AST__BASE, STATUS )

*     Get information on line style and colour.
*     =========================================

*     Set up Bolometer as a synonym for curve
         CALL KPG1_ASPSY( '(BOL*OMETER)', '(CURVES)', STATUS )

*     Set the plotting style.
         CALL KPG1_ASSET( 'SURF_SCUOVER', 'STYLE', IPLOT, STATUS )

*     Set the appearance of lines drawn using PGPLOT so that they mimic
*     curves produced using astCurves.
         CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTRS1, STATUS )

*     Calculate the distance between bolometers. If we only have one
*     bolometer just assume a distance of 24 arcsec (a random number)

         IF (N_BOL .LE. 1) THEN

            BOL_DIST = 15.0D0

         ELSE

*     Need to find half distance between bolometers
            CALL VEC_DTOD(.TRUE., 2, %VAL(CNF_PVAL(BOL_RA_PTR)),
     :           XTEMP, IERR, NERR, STATUS)
            CALL VEC_DTOD(.TRUE., 2, %VAL(CNF_PVAL(BOL_DEC_PTR)),
     :           YTEMP, IERR, NERR, STATUS)

            BOL_DIST = SQRT((XTEMP(2)-XTEMP(1))**2 +
     :           (YTEMP(2)-YTEMP(1))**2)
            BOL_DIST = BOL_DIST * R2AS * 0.5D0

         END IF

*     Now calculate (half) this distance in World coordinates
*     (I am assuming dx=dy for simplicity)

         INXPOS(1) = 0.0D0
         INYPOS(1) = 0.0D0
         INXPOS(2) = 0.0D0
         INYPOS(2) = BOL_DIST
         CALL AST_TRAN2( AX2GRMAP, 2, INXPOS, INYPOS, .TRUE.,
     :        XTEMP, YTEMP, STATUS)

         BOL_DIST_WORLD = REAL(ABS(YTEMP(2) - YTEMP(1)))

*     Now that we have converted bolometer separation to world coordinates
*     for textht, we need to calculate the required size relative to
*     the default PGPLOT TEXT HEIGHT. This is because the standard
*     font size is defined a 1/40 the height of the view surface!!!!
*     Luckily, there is a kAPPA routine that emulates SGS_SHTX :-)

*     This is the height of the text in graphics coordinates
*     We want the text to be readable but still fit inside the
*     circles
         HGT = 0.85 * BOL_DIST_WORLD

*     Now scale by the view surface
         CALL KPG1_PGSHT( HGT, STATUS )

*     Which type of label do we want
         IF (STATUS .EQ. SAI__OK) THEN
            CALL PAR_GET0L('NAME', BOLNAME, STATUS)

*     A null response indicates no name at all
            IF (STATUS .EQ. PAR__NULL) THEN
               PLOTNAME = .FALSE.
               CALL ERR_ANNUL( STATUS )
            END IF
         END IF

*     Set the Fill-Area mode to outline so we can see the text
*     in the circles
         CALL PGSFS( 2 )

*     We are going to do this loop twice. Once to draw the circles
*     and again to write the labels. This is so that we can change
*     style between plots and can therefore draw the labels in
*     a different colour to the circles
*     Not very elegant since we end up calculating the positions
*     twice. Do not want to store the positions since will need
*     to MALLOC some memory for that.

*     =============== CIRCLES ==================
*     Loop through all bolometers
         DO I = 1, N_BOL

            CALL VEC_DTOD(.TRUE., 1,
     :                    %VAL(CNF_PVAL(BOL_RA_PTR)+(I-1)*VAL__NBD),
     :           DXTEMP, IERR, NERR, STATUS)
            CALL VEC_DTOD(.TRUE., 1,
     :                    %VAL(CNF_PVAL(BOL_DEC_PTR)+(I-1)*VAL__NBD),
     :           DYTEMP, IERR, NERR, STATUS)

            DXTEMP = DXTEMP * R2AS
            DYTEMP = DYTEMP * R2AS

*     Transform to world coordinates
*     It is probably better to do this outside the loop since
*     AST_TRAN2 can convert many coordinates at once
            CALL AST_TRAN2( AX2GRMAP, 1, DXTEMP, DYTEMP,
     :           .TRUE., DXTEMP, DYTEMP, STATUS)
            RTEMP = SNGL( DXTEMP )
            RDTEMP = SNGL( DYTEMP )

*     Plot the circle using PGPLOT directly
            CALL PGCIRC( RTEMP, RDTEMP, BOL_DIST_WORLD * 0.85 )

         END DO

*     ========== CHANGE STYLE =============

*     Clear previous synonyms
         CALL KPG1_ASPSY( ' ', ' ', STATUS )

*     Set up BOLometer as a synonym for textlab
         CALL KPG1_ASPSY( '(BOL*OMETER)', '(TEXTLAB)', STATUS )

*     Set the appearance of fonts drawn using PGPLOT so that they mimic
*     curves produced using astText.
         CALL KPG1_PGSTY( IPLOT, 'TEXTLAB', .TRUE., ATTRS2, STATUS )


*     ============= LABELS ==============
*     Loop through all bolometers and plot labels
         IF (PLOTNAME) THEN
            DO I = 1, N_BOL

               CALL VEC_DTOD(.TRUE., 1,
     :                       %VAL(CNF_PVAL(BOL_RA_PTR)+(I-1)*VAL__NBD),
     :              DXTEMP, IERR, NERR, STATUS)
               CALL VEC_DTOD(.TRUE., 1,
     :              %VAL(CNF_PVAL(BOL_DEC_PTR)+(I-1)*VAL__NBD),
     :              DYTEMP, IERR, NERR, STATUS)

               DXTEMP = DXTEMP * R2AS
               DYTEMP = DYTEMP * R2AS

*     Transform to world coordinates
*     It is probably better to do this outside the loop since
*     AST_TRAN2 can convert many coordinates at once
               CALL AST_TRAN2( AX2GRMAP, 1, DXTEMP, DYTEMP,
     :              .TRUE., DXTEMP, DYTEMP, STATUS)
               RTEMP = SNGL( DXTEMP )
               RDTEMP = SNGL( DYTEMP )

*     Convert to proper bol name if necessary
               IF (BOLNAME) THEN
                  CALL SCULIB_BOLNAME(BOL_ADC(I), BOL_CHAN(I), BOL,
     :                 STATUS)
               ELSE
                  CALL CHR_ITOC(I, BOL, ITEMP)
               END IF

*     Now plot on map (Need to call 3 times depending on length of string)
*     Need to offset the Y value by half the character height so that it
*     can be centred in Y as well as centre justified in X
*     Divide by 2.5 since 2.0 seems to add too much of a correction
               CALL PGPTXT(RTEMP, RDTEMP - (HGT/2.5), 0.0, 0.5, BOL)

            END DO

         END IF

      END IF

*     Ensure that the previous synonyms are cleared
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*     Reset the plot attributes to their entry values
      CALL KPG1_PGSTY( IPLOT, 'TEXTLAB', .FALSE., ATTRS2, STATUS )
      CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTRS1, STATUS )

*     Free memory

      CALL SCULIB_FREE ('BOL_RA', BOL_RA_PTR, BOL_RA_END, STATUS)
      CALL SCULIB_FREE ('BOL_DEC', BOL_DEC_PTR, BOL_DEC_END, STATUS)

 980  CONTINUE

*  Tidy up the locators.
      IF( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
      CALL DAT_VALID( LOCI, GOTLOC, STATUS )
      IF( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )

*  Shutdown PGPLOT and the graphics database.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  End the AST Context.
      CALL AST_END( STATUS )

*  End the error context
      CALL ERR_END(STATUS)

      END
