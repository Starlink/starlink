      SUBROUTINE SIMCRDD( STATUS )
*+
*  Name:
*     SIMCRDD

*  Purpose:
*     Produce simulated CRDD based on a supplied image of the sky.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SIMCRDD( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine generates a group of artificial CRDD files
*     containing simulated data derived by convolving the IRAS detector
*     point spread functions with a supplied sky image. A group of
*     input CRDD files are used to define the scan geometry required
*     for the simulated scans. Note, no noise or positional errors are
*     included in the simulated CRDD files. No VARIANCE or QUALITY 
*     components are included in the output CRDD files.

*  Usage:
*     SIMCRDD IN SKY OUT

*  ADAM Parameters:
*     HISTORY = _LOGICAL (Read)
*        Determines if history information is to be stored within the
*        output CRDD files. See help on "History_in_IRAS90" for more
*        information on history. The history information will contain
*        the names of the input CRDD files, trial sky image, and 
*        detector Point Spread Functions.      [current history setting]
*     IN = NDF (Read)
*        Specifies a group of input CRDD files. This should be in the
*        form of a group expression (see help on "Group_expressions").
*        The input CRDD files can be in any of the standard IRAS90
*        systems of units.
*     MSG_FILTER = LITERAL (Read)
*        The level of information displayed on the users screen. This
*        should take one of the values QUIET, NORMAL or VERBOSE (see
*        help on "Message_filtering").
*                                       [current message filter setting]
*     OUT = NDF (Write)
*        A group of output CRDD files corresponding one-for-one with
*        the list of input CRDD files given for parameter IN.  This
*        should be in the form of a group expression (see help on
*        "Group_expressons"). Expressions such as "*_SIM" are expanded
*        by replacing the "*" character with each input CRDD file in
*        turn. The output CRDD files are in the same units as the input
*        CRDD files.
*     PSF = LITERAL (Read)
*        A string which is used to specify the NDFs holding the
*        detector point spread functions. NDF names are formed by
*        appending the detector number. For instance, the PSF for
*        detector #12 is assumed to reside in an NDF with name <PSF>12
*        (where <PSF> is replaced by the value supplied for parameter
*        PSF). The default value causes the standard PSFs supplied as
*        part of the IRAS90 package to be used. See the help on
*        "Point_Spread_Functions" for more information on the origin and
*        format of these files.                                       []
*     SKY = NDF (Read)
*        The IRAS90 image to use as the trial sky. This can be in any
*        of the standard IRAS90 system of units.

*  Examples:
*     SIMCRDD ZCMA_*_DS NEWSKY *_SIM
*        This causes an artificial CRDD file to be created for each
*        file in the current directory which satisfies the wild-card
*        template ZCMA_*_DS. Each created CRDD file has the same name
*        as the input except that the string "_SIM" is appended to the
*        end. The input CRDD files determine the sky position and
*        detector number to use for each simulated CRDD value. The CRDD
*        values are formed by convolving the standard detector PSFs with
*        the image contained in NDF NEWSKY. Each pair of input and
*        output CRDD files can be directly compared, value for value.
*     SIMCRDD M51_B1S1 MODEL MODEL_CRDD PSF=MYPSF
*        The CRDD file M51_B1S1 is simulated, using the trial sky image
*        MODEL, and the results are stored in MODEL_CRDD. The point
*        spread functions used to generate the simulated data are
*        contained in NDFs MYPSF1, MYPSF2, (etc) to MYPSF62. If any of
*        these NDFs do not exist, then the application continues but
*        bad values are stored for the corresponding detectors in the
*        output CRDD files.

*  Notes:
*     -  The waveband index of the data contained in the trial sky 
*     image is recorded in the IRAS extension of each output CRDD file,
*     within an integer component called SKYBAND.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'PAR_ERR'          ! PAR error constants
      INCLUDE 'MSG_PAR'          ! MSG constants
      INCLUDE 'GRP_PAR'          ! GRP constants
      INCLUDE 'IRI_PAR'          ! IRI constants
      INCLUDE 'I90_DAT'          ! IRAS90 data

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER CHR_LEN            ! Returns used length of a string.
      LOGICAL CHR_SIMLR          ! True if 2 strings are equal apart 
                                 ! from case.

*  Local Constants:
      INTEGER NX                 ! No. of pixel weight grid offsets in
                                 ! the X direction.
      PARAMETER ( NX = 3 )

      INTEGER NY                 ! No. of pixel weight grid offsets in
                                 ! the Y direction.
      PARAMETER ( NY = 3 )

*  Local Variables:
      CHARACTER COMC*1           ! Comment character for output group.
      CHARACTER INSTRM*(IRI__SZINS)! Instrument from which input sky
                                 ! image was derived.
      CHARACTER LOC1*(DAT__SZLOC)! Locator to IMAGE_INFO structure in
                                 ! sky NDF.
      CHARACTER NAME*(GRP__SZNAM)! Name of current output CRDD file.
      CHARACTER PSFPRE*(GRP__SZNAM)! Prefix for PSF NDFs.
      CHARACTER TEXT*(GRP__SZNAM)! Text to replace a bad output name.
      CHARACTER TYPE1*(IRI__SZTYP)! Type of sky image.
      CHARACTER UNITS1*(IRI__SZUNI)! Units from sky image.


      DOUBLE PRECISION DEC0      ! DEC of the centre of the sky image.
      DOUBLE PRECISION PIXSIZ(2) ! Nominal pixel dimensions in the sky
                                 ! image.
      DOUBLE PRECISION RA0       ! RA of the centre of the sky image.
      DOUBLE PRECISION XC        ! X image coordinate at the centre of
                                 ! the sky image.
      DOUBLE PRECISION YC        ! Y image coordinate at the centre of
                                 ! the sky image.


      INTEGER BAND1              ! Waveband index of sky image.
      INTEGER DGOOD              ! No. of detectors simulated.
      INTEGER DUSED( I90__DETS ) ! List of detectors simulated.
      INTEGER EL1                ! No. of elements in mapped sky image.
      INTEGER IDA1               ! IRA identifier for astrometry
                                 ! information for the sky image.
      INTEGER IGRPA              ! Input group identifier.
      INTEGER IGRPB              ! Output group identifier.
      INTEGER IGRPC              ! PSF group identifier.
      INTEGER I                  ! Group index to current input NDF.
      INTEGER INDF1              ! Input sky NDF identifier.
      INTEGER INDF3              ! Input CRDD file NDF identifier.
      INTEGER INDF4              ! Output CRDD file NDF identifier.
      INTEGER IP1                ! Pointer to mapped sky image.
      INTEGER IPPWG( NX*NY )     ! Pointers to pixel weight grids.
      INTEGER IPPWG2             ! Pointers to a work array the same
                                 ! size as the pixel weight grids.
      INTEGER LBND1( 2 )         ! Lower bounds of input sky image.
      INTEGER NCRDDF             ! No. of input CRDD files.
      INTEGER NCOUT              ! No. of output CRDD files.
      INTEGER NDIM1              ! No. of dimensions in input sky image.
      INTEGER NOUT               ! No. of good outputs.
      INTEGER PWGSZX             ! Initial no. of pixels per row in a
                                 ! pixel weight grid.
      INTEGER PWGSZY             ! Initial no. of rows in a pixel weight
                                 ! grid.
      INTEGER UBND1( 2 )         ! Upper bounds of input sky image.

      REAL SCALE1                ! Factor for converting input sky
                                 ! data values to Jy/sr.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get a value for parameter MSG_FILTER, and use it to establish the
*  conditional message filter level.
      CALL MSG_IFGET( 'MSG_FILTER', STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  Initialise the IRC_ and IRA_ systems.
      CALL IRC_INIT( STATUS )
      CALL IRA_INIT( STATUS )

*  Get a group containing the names of all the input CRDD files.
      CALL IRM_RDNDF( 'IN', 0, 1, 'Give more input CRDD file names',
     :                IGRPA, NCRDDF, STATUS )

*  Get a single NDF giving the sky image.
      CALL NDF_ASSOC( 'SKY', 'READ', INDF1, STATUS )

*  Get an IRA identifier for the astrometry information held within the
*  IRAS extension of the supplied NDF.
      CALL IRA_IMPRT( INDF1, IDA1, STATUS )

*  Check that the image is an IRAS90 image, and get the mandatory items
*  from the IMAGE_INFO structure.
      CALL IRI_OLD( INDF1, INSTRM, BAND1, TYPE1, UNITS1, LOC1, STATUS )

*  Annul the locator to the IMAGE_INFO structure.
      CALL DAT_ANNUL( LOC1, STATUS )

*  Get the conversion factor for converting input sky values to units of
*  Jy/sr.
      IF( CHR_SIMLR( UNITS1, IRI__JPS ) ) THEN
         SCALE1 = 1.0      

      ELSE IF( CHR_SIMLR( UNITS1, IRI__MJPS ) ) THEN
         SCALE1 = 1.0E6

*  For any other units...
      ELSE

*  If this is a CPC image, report an error.
         IF( INSTRM .EQ. 'CPC' .AND. STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'U', IRI__MJPS )
            CALL ERR_REP( 'SIMCRDD_ERR1',
     :       'SIMCRDD: Cannot handle CPC images in units other than ^U',
     :                    STATUS )
            GO TO 999
         END IF

*  Get the nominal pixel size in the sky image.
         CALL IRA_PIXSZ( IDA1, PIXSIZ, STATUS )

*  Find the factor which converts input sky values into units of Jy/sr.
         CALL IRM_UNTIV( UNITS1, IRI__JPS, BAND1,
     :                   PIXSIZ( 1 )*PIXSIZ( 2 ), SCALE1, STATUS )

      END IF

*  Get the bounds of the sky image.
      CALL NDF_BOUND( INDF1, 2, LBND1, UBND1, NDIM1, STATUS )

*  Get the RA and DEC (B1950) of the centre of the sky image.
      XC = DBLE( LBND1( 1 ) + UBND1( 1 ) )/2.0D0
      YC = DBLE( LBND1( 2 ) + UBND1( 2 ) )/2.0D0
      CALL IRA_TRANS( 1, XC, YC, .TRUE., 'EQU(B1950)', IDA1, RA0, DEC0,
     :                STATUS )

*  Map the data array of the sky NDF.
      CALL NDF_MAP( INDF1, 'DATA', '_REAL', 'READ', IP1, EL1, STATUS )

*  Get a group containing the names of all the output CRDD files
*  and store the comment character for the output group.
      CALL IRM_WRNDF( 'OUT', IGRPA, NCRDDF, NCRDDF,
     :                'Give more output CRDD file names', IGRPB,
     :                NCOUT, STATUS )
      CALL GRP_GETCC( IGRPB, 'COMMENT', COMC, STATUS )

*  Get the prefix to use when constructing the names of NDFs holding 
*  detector PSF. The supplied prefix is appended with the number of 
*  each detector (1 to 62) to get the name of the .SDF file holding
*  that detectors PSF.
      CALL PAR_GET0C( 'PSF', PSFPRE, STATUS )

*  Create a group in which element N holds the name of the NDF 
*  containing the PSF for detector N.
      CALL SIMCA0( PSFPRE, IGRPC, STATUS )

*  Get workspace to hold the pixel weight grids. The size of each grid
*  is expanded as required.
      PWGSZX = 50
      PWGSZY = 50

      DO I = 1, NX*NY
         CALL PSX_CALLOC( PWGSZX*PWGSZY, '_REAL', IPPWG( I ), STATUS )
      END DO

*  Allocate space for a real work array the same size as the pixel
*  weight grids.
      CALL PSX_CALLOC( PWGSZX*PWGSZY, '_REAL', IPPWG2, STATUS )

*  Initialise the arrays used to store information describing the PSFs.
      CALL SIMCB1( 0, 0, 0, 0, 0, 0, STATUS )

*  Abort if an error has occured.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Loop round each input CRDD file.
      NOUT = 0
      DO I = 1, NCRDDF
         CALL MSG_BLANKIF( MSG__NORM, STATUS )

*  Get an NDF identifier for the input NDF.
         CALL NDG_NDFAS( IGRPA, I, 'READ', INDF3, STATUS )

*  Tell the user which input NDF is currently being procesed.
         CALL NDF_MSG( 'NDF', INDF3 )
         CALL MSG_OUTIF( MSG__NORM, 'SIMCRDD_MSG1',
     :                   '  Processing ^NDF...', STATUS )

*  Obtain an identifier the output NDF, propagating all components from
*  the input to the output except DATA and VARIANCE (the HISTORY,
*  LABEL, TITLE and all extensions are propagated by default).
         CALL NDG_NDFPR( INDF3, 'UNITS,QUALITY,AXIS', IGRPB, I, INDF4,
     :                   STATUS )

*  Generate the simulated CRDD and store in the output NDF.
         CALL SIMCA1( LBND1, UBND1, IP1, IDA1, SCALE1, IGRPC, INDF3, 
     :                INDF4, RA0, DEC0, PWGSZX, PWGSZY, NX, NY, IPPWG, 
     :                IPPWG2, DGOOD, DUSED, STATUS )

*  Store the waveband of the sky image in a component called SKYBAND of
*  the IRAS extension.
         CALL NDF_XPT0I( BAND1, INDF4, 'IRAS', 'SKYBAND', STATUS )

*  Update the HISTORY structure in the output NDF.
         CALL SIMCA2( 'HISTORY', INDF4, INDF3, INDF1, IGRPC, DGOOD, 
     :                 DUSED, STATUS )

*  Annul the NDF identifier for the input NDF.
         CALL NDF_ANNUL( INDF3, STATUS )

*  If an error has occurred, delete the output NDF, otherwise just 
*  annul its identifier.
         IF( STATUS .NE. SAI__OK ) THEN
            CALL NDF_DELET( INDF4, STATUS )
         ELSE
            CALL NDF_ANNUL( INDF4, STATUS )
         END IF

*  If any error has occured, then
         IF ( STATUS .NE. SAI__OK ) THEN

*  Flush the error message, and reset STATUS to SAI__OK.
            CALL ERR_FLUSH( STATUS )

*  Warn the user that no output CRDD file will be created for the
*  current input CRDD file.
            CALL GRP_GET( IGRPB, I, 1, NAME, STATUS )
            CALL MSG_SETC( 'NDF', NAME )
            CALL MSG_OUTIF( MSG__QUIET, 'SIMCRDD_MSG2',
     :                      'WARNING: ^NDF cannot be produced',
     :                      STATUS )

*  Overwrite the current output NDF name with a string which will be
*  interpreted as a comment by later applications.
            TEXT = COMC//'          '//NAME( : CHR_LEN( NAME ) )//
     :             ' could not be produced'
            CALL GRP_PUT( IGRPB, 1, TEXT, I, STATUS )

*  If a good output was produced, increment the count of good outputs.
         ELSE
            NOUT = NOUT + 1
         ENDIF

*  Do the next pair of input and output CRDD files.
      END DO

*  Produce a text file holding the names of all the output CRDD files.
*  The user can specify this text file within a group expression for a
*  subsequent stage of the IRAS90 data processing. Note, the names of
*  output CRDD files which were not succesfully created are replaced by
*  strings starting with a comment character and identifying the output
*  CRDD file which could not be produced.
      IF( NOUT .GT. 0 ) CALL IRM_LISTN( 'NDFLIST', IGRPB, 'SIMCRDD', 
     :                                   STATUS )      

*  Release resources used to hold information describing the PSFs.
 999  CONTINUE
      CALL SIMCB1( 0, -1, 0, 0, 0, 0, STATUS )

*  Release the memory used to hold the pixel weight grids, and work
*  space.

      DO I = 1, NX*NY
         CALL PSX_FREE( IPPWG( I ), STATUS )
      END DO

      CALL PSX_FREE( IPPWG2, STATUS )

*  Delete the groups used to hold input and output CRDD file names, and
*  PSF names.
      CALL GRP_DELET( IGRPA, STATUS )
      CALL GRP_DELET( IGRPB, STATUS )
      CALL GRP_DELET( IGRPC, STATUS )

*  Close down the IRA, and IRC systems.
      CALL IRA_CLOSE( STATUS )
      CALL IRC_CLOSE( STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  If a parameter null or abort error exists, annul it.
      IF( STATUS .EQ. PAR__NULL .OR. STATUS .EQ. PAR__ABORT ) THEN
         CALL ERR_ANNUL( STATUS )
      END IF

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SIMCRDD_ERR2',
     :   'SIMCRDD: Error creating simulated CRDD files.',
     :   STATUS )
      END IF

      END
