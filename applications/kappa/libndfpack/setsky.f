      SUBROUTINE SETSKY( STATUS )
*+
*  Name:
*     SETSKY

*  Purpose:
*     Makes an IRAS astrometry extension.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL SETSKY( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application makes an IRAS astrometry extension within a
*     two-dimensional NDF, therefore allowing sky co-ordinate
*     information to be stored with an arbitrary image.  This
*     information is used by certain IRAS90 applications (those with
*     the SKY prefix) to perform various astrometric operations.  These
*     include annotation of a displayed image with a grid of celestial
*     co-ordinates, marking the location of a given celestial position
*     given a pixel position, and aligning a group of images.  See
*     SUN/163 for details.
*
*     The astrometry is determined either by you supplying explicit
*     values for certain projection parameters, or by you providing the
*     sky and corresponding image co-ordinates for a set of positions
*     (see parameter POSITIONS).  In the latter case, the projection
*     parameters are determined automatically by searching through
*     parameter space in order to minimise the sum of the squared
*     residuals between the supplied pixel co-ordinates and the
*     transformed sky co-ordinates.  You may force particular
*     projection parameters to take certain values by assigning an
*     explicit value to the corresponding application parameter listed
*     below.  The individual residuals at each position can be written
*     out to a logfile so that you can identify any aberrant points.
*     The RMS residual (in pixels) implied by the best-fitting
*     parameters is displayed.
*
*  Usage:
*     setsky ndf positions coords epoch [projtype] [lon] [lat]
*            [refcode] [pixelsize] [orient] [tilt] [logfile]

*  ADAM Parameters:
*     COORDS = LITERAL (Read)
*        The sky co-ordinate system to use.  Valid values include
*        "Ecliptic" (IAU 1980), "Equatorial" (FK4 and FK5), and
*        "Galactic" (IAU 1958).  Ecliptic and equatorial co-ordinates
*        are referred to the mean equinox of a given epoch.  This epoch
*        is specified by appending it to the system name, in
*        parentheses, for example, "Equatorial(1994.5)".  The epoch may
*        be preceded by a single character, "B" or "J", indicating that
*        the epoch is Besselian or Julian respectively.  If this letter
*        is missing, a Besselian epoch is assumed.
*     EPOCH = DOUBLE PRECISION (Read)
*        The Julian epoch at which the observation was made (e.g.
*        "1994.0").
*     LAT = LITERAL (Read)
*        The latitude of the reference point, in the co-ordinate system
*        specified by parameter COORDS.  For example, if COORDS is
*        "Equatorial", LAT is the Declination.  See SUN/163, Section
*        4.7.2 for full details of the allowed syntax for specifying
*        this position.  For convenience here are some examples how you
*        may specify the Declination -45 degrees, 12 arcminutes: "-45 12
*        00", "-45 12", "-45d 12m", "-45.2d", "-451200", "-0.78888r".
*        The last of these is a radians value.  A null value causes the
*        latitude of the reference point to be estimated automatically
*        from the data supplied for parameter POSITIONS. [!]
*     LOGFILE = FILENAME (Read)
*        Name of the text file to log the final projection parameter
*        values and the residual at each supplied position.  If null,
*        there will be no logging.  This parameter is ignored if a null
*        value is given to parameter POSITIONS. [!]
*     LON= LITERAL (Read)
*        The longitude of the reference point, in the co-ordinate
*        system specified by parameter COORDS.  For example, if COORDS
*        is "Equatorial", LON is the Right Ascension.  See SUN/163,
*        Section 4.7.2 for full details of the allowed syntax for
*        specifying this position.  For convenience here are some
*        examples how you may specify the Right Ascension 11 hours, 34
*        minutes, and 56.2 seconds: "11 34 56.2", "11h 34m 56.2s", "11
*        34.9366", "11.58228", "113456.2".  See parameter LAT for
*        examples of specifying a non-equatorial longitude.  A null
*        value causes the longitude of the reference point to be
*        estimated automatically from the data supplied for parameter
*        POSITIONS. [!]
*     NDF = NDF (Read and Write)
*        The NDF that is to have an IRAS astrometry extension.
*     ORIENT = LITERAL (Read)
*        The position angle of the NDF's y axis on the celestial
*        sphere, measured from north through east.  North is defined as
*        the direction of increasing sky latitude, and east is the
*        direction of increasing sky longitude.  Values are constrained
*        to the range 0 to two-pi radians.  A null value causes the
*        position angle to be estimated automatically from the data
*        supplied for parameter POSITIONS. [!]
*     PIXELREF( 2 ) = REAL (Read)
*        The pixel co-ordinates of the reference pixel (x then y).
*        This parameter is ignored unless REFCODE = "Pixel".  Remember
*        that the centre of a pixel at indices i,j is (i-0.5,j-0.5).  A
*        null value causes the pixel co-ordinates of the reference
*        point to be estimated automatically from the data supplied for
*        parameter POSITIONS. [!]
*     PIXELSIZE( 2 ) = _REAL (Read)
*        The x and y pixel sizes at the reference position.  If only
*        one value is given, the pixel is deemed to be square.  Values
*        may be given in a variety of units (see parameter LAT).  For
*        example, 0.54 arcseconds could be specified as "0.54s" or
*        "0.009m" or "2.618E-6r".  A null value causes the pixel
*        dimensions to be estimated automatically from the data
*        supplied for parameter POSITIONS. [!]
*     POSITIONS = LITERAL (Read)
*        A list of sky co-ordinates and corresponding image
*        co-ordinates for the set of positions which are to be used to
*        determine the astrometry.  If a null value is given then the
*        astrometry is determined by the explicit values you supply for
*        each of the other parameters.  Each position is defined by
*        four values, the sky longitude (in the same format as for
*        parameter LON), the sky latitude (in the same format as for
*        parameter LAT), the image pixel x co-ordinate and the image
*        pixel y co-ordinate (both decimal values).  These should be
*        supplied (in the order stated) for each position.  These
*        values are given in the form of a `group expression' (see
*        SUN/150).  This means that values can be either typed in
*        directly or supplied in a text file.  If typed in directly,
*        the items in the list should be separated by commas, and you
*        are re-prompted for further values if the last supplied value
*        ends in a minus sign.  If conveyed in a text file, they should
*        again be separated by commas, but can be split across lines.
*        The name of the text file is given in response to the prompt,
*        preceded by an `up arrow' symbol (^).
*     PROJTYPE = LITERAL (Read)
*        The type of projection to use.  The options are:
*           "Aitoff"         - Aitoff equal-area,
*           "Gnomonic"       - Gnomonic or tangent plane,
*           "Lambert"        - Lambert normal equivalent cylindrical,
*           "Orthographic"   - Orthographic.
*
*        The following synonyms are also recognised:
*            "All_sky"       - Aitoff,
*            "Cylindrical"   - Lambert,
*            "Tangent_plane" - Gnomonic.
*
*        See SUN/163 for descriptions of these projections.  A null
*        value causes the projection to be determined automatically
*        from the data supplied for parameter POSITIONS. [!]
*     REFCODE = LITERAL (Read)
*        The code for the reference pixel.  If it has value "Pixel"
*        this requests that pixel co-ordinates for the reference point
*        be obtained through parameter PIXELREF.  The other options are
*        locations specified by two characters, the first corresponding
*        to the vertical position and the second the horizontal.  For
*        the vertical, valid positions are T(op), B(ottom), or
*        C(entre); and for the horizontal the options are L(eft),
*        R(ight), or C(entre).  Thus REFCODE = "CC" means the reference
*        position is at the centre of the NDF image, and "BL" specifies
*        that the reference position is at the centre of the
*        bottom-left pixel in the image.  A null value causes the pixel
*        co-ordinates of the reference point to be estimated
*        automatically from the data supplied for parameter POSITIONS.
*        [!]
*     TILT = LITERAL (Read)
*        The angle through which the celestial sphere is to be rotated
*        prior to doing the projection.  The axis of rotation is a
*        radius passing through the reference point.  The rotation is
*        in an anti-clockwise sense when looking from the reference
*        point towards the centre of the celestial sphere.  In common
*        circumstances this can be set to zero.  Values may be given in
*        a variety of units (see parameter LAT).  Values are
*        constrained to the range 0 to two-pi radians.  A null value
*        causes the latitude of the reference point to be estimated
*        automatically from the data supplied for parameter POSITIONS.
*        ["0.0"]

*  Examples:
*     setsky m51 ^stars.lis ecl(j1994.0) 1994.0 logfile=m51.log 
*        This creates an astrometry extension within the two-dimensional
*        NDF called m51.  The values for parameters PROJTYPE, LON, LAT,
*        PIXELREF, PIXELSIZE and ORIENT are determined automatically so
*        that they minimised the sum of the squared residuals (in
*        pixels) at each of the positions specified in the file
*        stars.lis.  This file contains a line for each position, each
*        line containing an ecliptic longitude and latitude, followed
*        by a pair of image co-ordinates.  These values should be
*        separated by commas.  The ecliptic co-ordinates were
*        determined at Julian epoch 1994.0, and are referred to the
*        mean equinox at Julian epoch 1994.0.  The determined parameter
*        values together with the residual at each position are logged
*        to file m51.log.
*     setsky m51 ^stars.lis ecl(j1994.0) 1994.0 orient=0 projtype=orth
*        This creates an astrometry extension within the
*        two-dimensional NDF called m51.  The values for parameters
*        PROJTYPE, LON, LAT, PIXELREF and PIXELSIZE are determined
*        automatically as in the previous example.  In this example
*        however, an Orthographic projection is forced, and the value
*        zero is assigned to parameter ORIENT, resulting in north being
*        `upwards' in the image.
*     setsky virgo "!" eq(j2000.0) 1989.3 gn "12 29" "+12 30" bl 1.1s
*            0.0d
*        This creates an astrometry extension within the two-dimensional
*        NDF called virgo.  It is a gnomonic projection in the
*        equatorial system at Julian epoch 2000.0.  The bottom-left
*        pixel of the image is located at Right Ascension 12 hours 29
*        minutes, Declination +12 degrees 30 minutes.  A pixel at that
*        position is square and has angular size of 1.1 arcseconds.
*        The image was observed at epoch 1989.3.  At the bottom-left of
*        the image, north is at the top, parallel to the y-axis of the
*        image.
*     setsky map "!" galactic(1950.0) 1993.8 aitoff 90 0 cc
*       [0.5d,0.007r] 180.0d
*        This creates an astrometry extension within the two-dimensional
*        NDF called map.  It is an Aitoff projection in the galactic
*        system at Besselian epoch 1950.0.  The centre of the image is
*        located at galactic longitude 90 degrees, latitude 0 degrees.
*        A pixel at that position is rectangular and has angular size
*        of 0.5 degrees by 0.007 radians.  The image was made at epoch
*        1993.8.  At the image centre, south is at the top and is
*        parallel to the y-axis of the image.
*     setsky zodiac "!" ec 1983.4 or 10.3 -5.6 Pixel 20m 0.3d
*       pixelref=[9.5,-11.2]
*        This creates an astrometry extension within the
*        two-dimensional NDF called zodiac.  It is an orthographic
*        projection in the Ecliptic system at Besselian epoch 1950.0.
*        The reference point at pixel co-ordinates (9.5,-11.2)
*        corresponds to ecliptic longitude 10.3 degrees, latitude
*        -5.6 degrees.  A pixel at that position is square and has
*        angular size of 20 arcminutes.  The image was observed at
*        epoch 1983.4.  At the reference point the y-axis of the image
*        points to 0.3 degrees east of north.

*  Related Applications:
*     ASTROM; IRAS90: SKYALIGN, SKYBOX, SKYGRID, SKYLINE, SKYMARK,
*     SKYPOS, SKYWRITE. 

*  Notes:
*     -  SETSKY overwrites an existing astrometry extension within
*     the NDF.
*     -  WARNING: As is standard for NDF extensions, the transformation
*     stored in the NDF will be propagated to new NDFs derived from it.
*     However, certain operations will invalidate the transformation.
*     These include configuration change, a shift of origin, and
*     resampling.  Once there is a standard astrometry extension, KAPPA
*     applications will be made to process that extension correctly, by
*     modifying it where that's possible otherwise not copying it.

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*  {enter_new_authors_here}

*  History:
*     1994 April 13 (MJC):
*        Original version.
*     28-OCT-1994 (DSB):
*        Added parameters POSITIONS and LOGFILE.  Modified to determine
*        projection parameters automatically from the data suplied for
*        POSITIONS.
*     1995 February 13 (MJC):
*        Corrected typo's, errors in the examples, output the fitted
*        pixel size in arcseconds if appropriate.  Validated the
*        extension locator before annulling it.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'NDF_PAR'          ! NDF_ public constants
      INCLUDE 'IRA_PAR'          ! IRA_ public constants
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT public constants
      INCLUDE 'GRP_PAR'          ! GRP public constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Number of dimensions
      PARAMETER ( NDIM = 2 )
      INTEGER NP                 ! Number of projection parameters
      PARAMETER ( NP = 8 )

*  Local Variables:
      INTEGER ADDED              ! No. of elements added to a GRP group
      CHARACTER * ( DAT__SZNAM ) ASNAME ! Name of astrometry structure
      CHARACTER * ( IRA__SZFSC ) ATEXT ! Best ref longitude
      CHARACTER * ( IRA__SZFSC ) BTEXT ! Best ref latitude
      CHARACTER * ( IRA__SZFSC ) CPIXEL( 2 ) ! Pixel sizes as sky
                                 ! co-ordinate strings
      CHARACTER * ( VAL__SZR ) CXREF ! X-position of reference pixel
      CHARACTER * ( VAL__SZR ) CYREF ! Y-position of reference pixel
      DOUBLE PRECISION EPOCH     ! Julian epoch
      LOGICAL FLAG               ! Was group expression flagged?
      INTEGER IDA                ! Astrometry identifier
      INTEGER IGRP               ! ID for GRP group holding co-ord data
      INTEGER IGRP0              ! ID for GRP group holding co-ord data
      INTEGER LBND( NDIM )       ! Lower bounds of the NDF
      CHARACTER * ( DAT__SZLOC ) LOC ! Locator to the IRAS extension
      LOGICAL LOG                ! Log search results to a file?
      INTEGER LOGFD              ! FIO log file descriptor
      INTEGER MDIM               ! Actual no. of dimensions of the NDF
      INTEGER NCX                ! No. of signif. characters in CXREF
      INTEGER NCY                ! No. of signif. characters in CYREF
      INTEGER NDF                ! NDF identifier
      INTEGER NPOS               ! No. of supplied sky positions
      LOGICAL ORIENT             ! Has value been supplied for ORIENT?
      CHARACTER * ( IRA__SZFSC ) PATEXT ! Best image orientation
      DOUBLE PRECISION P( NP )   ! Parameters defining the projection
      CHARACTER * ( 26 + 2 * VAL__SZR ) PRMSUF ! String to complete the
                                 ! prompt string for LON and LAT
      CHARACTER * ( IRA__SZPRJ ) PROJEC ! Chosen projection type
      CHARACTER * ( IRA__SZPLS ) PROTYP ! List of supported projection
                                 ! types
      REAL PIXREF( 2 )           ! Reference pixel co-ordinates 
      REAL PIXSIZ( 2 )           ! Pixel sizes
      LOGICAL PROJ               ! Was a value supplied for PROJTYPE?
      LOGICAL PSIZE              ! Has value been supplied for PIXSIZE?
      LOGICAL REFIMG             ! Were ref. pos. image co-ords given?
      CHARACTER * ( 5 ) REFPOS   ! Reference-position code
      LOGICAL REFSKY             ! Were ref. pos. sky co-ords given?
      REAL    RMS                ! Smallest RMS positional error found
      CHARACTER * ( IRA__SZSCS ) SCS ! Chosen sky co-ordinate system
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      LOGICAL SEARCH             ! Search in parameter space?
      INTEGER SIZE               ! Current size of GRP group.
      LOGICAL TILT               ! Has value been supplied for TILT?
      LOGICAL THERE              ! If true there is already an IRAS
                                 ! extension in the NDF
      CHARACTER * ( IRA__SZFSC ) TTEXT ! Best celestial sphere tilt
      INTEGER UBND( NDIM )       ! Upper bounds of the NDF
      LOGICAL VALID              ! Locator is valid or not
      CHARACTER * ( DAT__SZNAM ) XNAME ! Name of extn. holding
                                 ! astrometry data

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF and find its shape.
*  ==================================

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the NDF.
      CALL NDF_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

*  The astrometry expects a two-dimensional NDF.  It cannot handle
*  planes from higher dimensions.  So first check that the
*  dimensionality is 2.
      CALL NDF_BOUND( NDF, NDIM, LBND, UBND, MDIM, STATUS )
      
*  Find whether both of these dimensions is significant (>1 element).
*  An error condition will result if there are not two significant
*  dimensions.
      CALL KPG1_SGDIM( NDF, NDIM, SDIM, STATUS )

*  Read in corresponding sky and image co-ordinate positions.
*  ==========================================================
*
*  Create a new GRP group to hold the co-ordinate data.
      CALL GRP_NEW( 'SETSKY co-ordinate data', IGRP0, STATUS )

*  Abort if an error has occurred.  This will ensure that the status
*  check following the next subroutine call only picks up errors
*  generated in that subroutine.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  Get the input co-ordinate data and store it in the GRP group.  Loop
*  round until a group expression is supplied which does not end with a
*  flag character (minus sign).
      FLAG = .TRUE.
      DO WHILE( FLAG .AND. STATUS .EQ. SAI__OK )
         CALL GRP_GROUP( 'POSITIONS', GRP__NOID, IGRP0, SIZE, ADDED,
     :                   FLAG, STATUS )
      END DO

*  If some data was supplied, indicate that a search is to be made in
*  parameter space to determine the mapping between the supplied sky
*  co-ordinates and image co-ordinates.  Otherwise, indicate that this
*  will not be done (in which case the user must supply all the values
*  required to define the mapping), annul the error, and delete the
*  group.
      IF ( STATUS .EQ. SAI__OK ) THEN      
         SEARCH = .TRUE.

*  Report an error if no values were supplied
         IF ( SIZE .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSKY_ERR1', 'No values were supplied '/
     :                    /'for parameter ''%POSITIONS''.', STATUS )
            GOTO 999
         END IF
            
*  Create a copy of the group from which any blanks have been removed.
*  Delete the original group.
         CALL GRP_REMOV( IGRP0, ' ', IGRP, STATUS )
         CALL GRP_DELET( IGRP0, STATUS )

*  Each position is defined by 4 values (sky longitude and latitude,
*  image X and Y).  If the number of elements in the group is not a
*  multiple of 4, report an error.
         CALL GRP_GRPSZ( IGRP, SIZE, STATUS )
         NPOS = SIZE/4
         IF ( 4*NPOS .NE. SIZE ) THEN
            CALL MSG_SETI( 'N', SIZE )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSKY_ERR2', 'An incomplete list of '/
     :        /'positions was supplied for parameter ''%POSITIONS''. '/
     :        /' ^N values supplied.', STATUS )
            GOTO 999
         END IF

            
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         SEARCH = .FALSE.
         CALL ERR_ANNUL( STATUS )
         CALL GRP_DELET( IGRP0, STATUS )
         IGRP = GRP__NOID
      END IF
      

*  Set up IRA.
*  ===========

*  Initialise the IRA system.
      CALL IRA_INIT( STATUS )

*  If an IRA astrometry structure already exists in the NDF, use the
*  same location for the new one.
      CALL IRA_FIND( NDF, THERE, XNAME, ASNAME, LOC, STATUS )
      IF ( THERE ) THEN
         CALL IRA_LOCAT( XNAME, ASNAME, STATUS )

*  Otherwise, use extension IRAS and component ASTROMETRY.
      ELSE
         CALL IRA_LOCAT( 'IRAS', 'ASTROMETRY', STATUS )

*  Check whether the extension already exists.
         CALL NDF_XSTAT( NDF, 'IRAS', THERE, STATUS )

*  Create it if it is not present.
         IF ( .NOT. THERE ) CALL NDF_XNEW( NDF, 'IRAS',
     :                                     'IRAS_EXTENSION',
     :                                     0, 0, LOC, STATUS )

      END IF

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices.
      IF ( STATUS .NE. SAI__OK ) GOTO 999


*  Obtain details of the transformation.
*  =====================================

*  Find the available projection types.
      CALL IRA_IPROJ( PROTYP, STATUS )

*  Abort if an error has already occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  Obtain the projection type. 
      CALL PAR_CHOIC( 'PROJTYPE', 'GNOMONIC', PROTYP, .FALSE., PROJEC,
     :                STATUS )

*  A null value is OK if the mapping is to be based on supplied
*  co-ordinate data.  In this case, the best projection will be found
*  and used.
      IF ( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         PROJ = .FALSE.
      ELSE
         PROJ = .TRUE.
      END IF
      
*  Obtain the sky co-ordinate system to use.  The dynamic default is
*  equatorial, J2000.
      SCS = 'EQUATORIAL(J2000)'
      CALL IRA_GTSCS( 'COORDS', .TRUE., SCS, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999      
      
*  Get the reference co-ordinates. 
      CALL PAR_CHOIC( 'REFCODE', 'CC', 'Pixel,TL,BL,CL,TR,BR,CR,TC,'/
     :                /'BC,CC', .FALSE., REFPOS, STATUS )

*  If the reference co-ordinates are pixel, the user wants to nominate
*  some arbitrary co-ordinates for the reference pixel.  Get the value
*  and create the prompt suffix for obtaining the longitude and
*  latitude.
      IF ( REFPOS .EQ. 'PIXEL' ) THEN
         CALL PAR_EXACR( 'PIXELREF', 2, PIXREF, STATUS )
         CALL CHR_RTOC( PIXREF( 1 ), CXREF, NCX )
         CALL CHR_RTOC( PIXREF( 2 ), CYREF, NCY )
         PRMSUF = ' at pixel co-ordinates ('//CXREF( :NCX )//','/
     :               /CYREF( :NCY )//')'

*  Assign other prompt suffices and reference pixel co-ordinates for
*  each of the other position codes.
*
*  The centre
      ELSE IF ( REFPOS .EQ. 'CC' ) THEN
         PIXREF( 1 ) = 0.5 * REAL( UBND( 1 ) + LBND( 1 ) )
         PIXREF( 2 ) = 0.5 * REAL( UBND( 2 ) + LBND( 2 ) )
         PRMSUF = ' of the centre.'
           
*  The top-left corner
      ELSE IF ( REFPOS .EQ. 'TL' ) THEN
         PIXREF( 1 ) = REAL( LBND( 1 ) )
         PIXREF( 2 ) = REAL( UBND( 2 ) )
         PRMSUF = ' of the top-left corner.'
            
*  The bottom-left corner
      ELSE IF ( REFPOS .EQ. 'BL' ) THEN
         PIXREF( 1 ) = REAL( LBND( 1 ) )
         PIXREF( 2 ) = REAL( LBND( 2 ) )
         PRMSUF = ' of the bottom-left corner.'
            
*  Centre-left
      ELSE IF ( REFPOS .EQ. 'CL' ) THEN
         PIXREF( 1 ) = REAL( LBND( 1 ) )
         PIXREF( 2 ) = 0.5 * REAL( UBND( 2 ) + LBND( 2 ) )
         PRMSUF = ' of midway along the left side.'
            
*  The top-right corner
      ELSE IF ( REFPOS .EQ. 'TR' ) THEN
         PIXREF( 1 ) = REAL( UBND( 1 ) )
         PIXREF( 2 ) = REAL( UBND( 2 ) )
         PRMSUF = ' of the top-right corner.'
            
*  The bottom-right corner
      ELSE IF ( REFPOS .EQ. 'BR' ) THEN
         PIXREF( 1 ) = REAL( UBND( 1 ) )
         PIXREF( 2 ) = REAL( LBND( 2 ) )
         PRMSUF = ' of the bottom-right corner.'
            
*  The centre-right
      ELSE IF ( REFPOS .EQ. 'CR' ) THEN
         PIXREF( 1 ) = REAL( UBND( 1 ) )
         PIXREF( 2 ) = 0.5 * REAL( UBND( 2 ) + LBND( 2 ) )
         PRMSUF = ' of midway along the right side.'
            
*  The top-centre
      ELSE IF ( REFPOS .EQ. 'TC' ) THEN
         PIXREF( 1 ) = 0.5 * REAL( UBND( 1 ) + LBND( 1 ) )
         PIXREF( 2 ) = REAL( UBND( 2 ) )
         PRMSUF = ' of midway along the top side.'
            
*  The bottom-centre
      ELSE IF ( REFPOS .EQ. 'BC' ) THEN
         PIXREF( 1 ) = 0.5 * REAL( UBND( 1 ) + LBND( 1 ) )
         PIXREF( 2 ) = REAL( LBND( 2 ) )
         PRMSUF = ' of midway along the bottomside.'
            
      END IF

*  A null value is OK for REFCODE or PIXELREF if the mapping is to be
*  based on supplied co-ordinate data.  In this case, the best image
*  co-ordinates for the reference position will be found and used.
      IF ( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         REFIMG = .FALSE.
         PRMSUF = ' of the reference point.'
         
      ELSE
         REFIMG = .TRUE.
      
*  IRA routine needs double precision.
         P( 3 ) = DBLE( PIXREF( 1 ) )
         P( 4 ) = DBLE( PIXREF( 2 ) )

      END IF
         
*  Obtain the reference longitude and latitude, and convert them to
*  radians.
      CALL IRA_GETCO( 'LON', 'LAT', PRMSUF, SCS, .TRUE., P( 1 ),
     :                P( 2 ), STATUS )

*  A null value is OK for LON or LAT if the mapping is to be based on
*  supplied co-ordinate data.  In this case, the best sky co-ordinates
*  for the reference position will be found and used.
      IF ( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         REFSKY = .FALSE.
      ELSE
         REFSKY = .TRUE.
      END IF
         
*  Abort if an error has already occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999      
      
*  Obtain the x and y pixel sizes as strings.  If just one value is
*  supplied assume that the x and y sizes are the same.
      CALL PAR_GETVC( 'PIXELSIZE', NDIM, CPIXEL, MDIM, STATUS )
      IF ( MDIM .EQ. 1 ) CPIXEL( 2 ) = CPIXEL( 1 )
      IF ( CPIXEL( 1 ) .EQ. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'INVPIXSIZE',
     :     'SETSKY: The pixelsize is undefined.', STATUS )
      END IF

*  A null value for the pixel size is OK if the mapping is to be based
*  on supplied co-ordinate data.  Annul the error and set a flag to
*  indicate that pixel size has not yet been determined.
      IF ( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN      
         CALL ERR_ANNUL( STATUS )               
         PSIZE = .FALSE.

*  If a pixel size was supplied, flag that pixel size has been
*  determined.
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         PSIZE = .TRUE.

*  Convert the formatted sky co-ordinate specifying the pixelsize
*  (usually arcsec or radians) into double-precision values.
         CALL IRA_CTOD1( CPIXEL( 1 ), SCS, 2, P( 5 ), STATUS )
         CALL IRA_CTOD1( CPIXEL( 2 ), SCS, 2, P( 6 ), STATUS )

      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999      
      
*  Obtain the position angle of the NDF's Y axis on the celestial
*  sphere, measured from north through east.  Since this is a pure
*  angle we don't want units of hours, thus galactic longitude
*  co-ordinates are used (a value in degrees, arcminutes, arcseconds,
*  or radians will be accepted).  This restricts the values to 0 to
*  two-pi radians.  Set the default to be 0.0, i.e. up is north.
      P( 7 ) = 0.0D0
      CALL IRA_GTCO1( 'ORIENT', ' ', 'GALACTIC', 1, .TRUE., P( 7 ),
     :                STATUS )

*  A null value for ORIENT is OK if the mapping is to be based on
*  supplied co-ordinate data.  In this case, annul the error and set a
*  flag to indicate that the orientation of the map is still to be
*  determined.
      IF ( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         ORIENT = .FALSE.

*  Otherwise, indicate that the orientation of the map has been fixed.
      ELSE 
         ORIENT = .TRUE.
      END IF

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999
      
*  Obtain the tilt of the celestial sphere prior to projection.  Set
*  the default to be 0.0, i.e. there is no tilt.  Since this is a pure
*  angle we don't want units of hours, thus galactic longitude
*  co-ordinates are used (a value in degrees, arcminutes, arcseconds,
*  or radians will be accepted).  This restricts the values to 0 to
*  two-pi radians.
      P( 8 ) = 0.0D0
      CALL IRA_GTCO1( 'TILT', ' ', 'GALACTIC', 1, .TRUE., P( 8 ),
     :                STATUS )

*  A null value for TILT is OK if the mapping is to be based on
*  supplied co-ordinate data.  In this case, annul the error and set a
*  flag to indicate that the TILT of the celestial sphere is still to be
*  determined.
      IF ( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         TILT = .FALSE.

*  Otherwise, indicate that the tilt of the celestiual sphere has been
*  fixed.
      ELSE 
         TILT = .TRUE.
      END IF

*  Obtain the epoch.
      CALL PAR_GET0D( 'EPOCH', EPOCH, STATUS )

*  Abort if an error has occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  If a search is to be done, get the name of a log file in which to 
*  store the parameter values best fit residuals.  Annul the error if 
*  a null value is supplied.
      IF ( SEARCH ) THEN

         CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 0, LOGFD, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
            LOG = .FALSE.
         ELSE
            LOG = .TRUE.
         END IF

      ELSE
         LOG = .FALSE.

      END IF
 
*  Create and store the astrometry extension.
*  ==========================================

*  If the mapping is to be based on supplied co-ordinate data, determine
*  the best projection parameters.  Otherwise the supplied parameter
*  values will be used.
      IF ( SEARCH ) THEN
         CALL KPS1_SKYFT( LOG, LOGFD, IGRP, NPOS, SCS, EPOCH, PROJ, 
     :                    TILT, ORIENT, PSIZE, REFIMG, REFSKY, NP, 
     :                    PROJEC, P, RMS, STATUS )

*  Give a title for the parameter values which are to be displayed.
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETR( 'RMS', RMS )
         CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG1',
     :     '  These parameter values give an RMS positional error of '/
     :     /'^RMS pixels ... ', STATUS )

      ELSE
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG2',
     :     '  The following parameter values will be stored... ',
     :     STATUS )
      END IF

*  Display the parameter values, etc, to be stored in the NDF.
      CALL MSG_SETC( 'PRJ', PROJEC )
      CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG3',
     :  '    Projection type                      : ^PRJ', STATUS )

      CALL IRA_DTOC( P( 1 ), P( 2 ), SCS, 0, ATEXT, BTEXT, STATUS )
      CALL MSG_SETC( 'A', ATEXT )
      CALL MSG_SETC( 'B', BTEXT )
      CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG4',
     :  '    Sky co-ordinates of reference point  : ^A, ^B', STATUS )
      
      CALL MSG_SETR( 'X', REAL( P( 3 ) ) )
      CALL MSG_SETR( 'Y', REAL( P( 4 ) ) )
      CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG5',
     : '    Image co-ordinates of reference point: (^X,^Y)', STATUS )

* Use arcminutes or arcseconds as appropriate.
      PIXSIZ( 1 ) = REAL( IRA__R2AM * P( 5 ) )
      PIXSIZ( 2 ) = REAL( IRA__R2AM * P( 6 ) )
      IF ( PIXSIZ( 1 ) .LT. 1.0 .AND. PIXSIZ( 2 ) .LT. 1.0 ) THEN
         CALL MSG_SETR( 'DX', PIXSIZ( 1 ) * 60.0 )
         CALL MSG_SETR( 'DY', PIXSIZ( 2 ) * 60.0 )
         CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG6',
     :     '    Pixel dimensions                     : (^DX,^DY) '/
     :     /'arcsecs', STATUS )
      ELSE
         CALL MSG_SETR( 'DX', PIXSIZ( 1 ) )
         CALL MSG_SETR( 'DY', PIXSIZ( 2 ) )
         CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG6',
     :     '    Pixel dimensions                     : (^DX,^DY) '/
     :     /'arcmins', STATUS )
      END IF

      CALL IRA_DTOC1( P( 7 ), 'GAL', 1, 2, PATEXT, STATUS )
      CALL MSG_SETC( 'PA', PATEXT )
      CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG7',
     :  '    Position angle of image Y axis       : ^PA', STATUS )

      CALL IRA_DTOC1( P( 8 ), 'GAL', 1, 2, TTEXT, STATUS )
      CALL MSG_SETC( 'TILT', TTEXT )
      CALL MSG_OUTIF( MSG__NORM, 'SETSKY_MSG8',
     :  '    Tilt of celestial sphere             : ^TILT', STATUS )

      CALL MSG_BLANK( STATUS )

*  Create the astrometry information.
      CALL IRA_CREAT( PROJEC, NP, P, SCS, EPOCH, NDF, IDA, STATUS )

*  Annul the IRA identifier.
      CALL IRA_ANNUL( IDA, STATUS )      
      
*  Close the IRA system.
      CALL IRA_CLOSE( STATUS )

*  Jump to here if an error occurs.
  999 CONTINUE

*  If required, annul the log file descriptor.
      IF ( LOG ) CALL FIO_ANNUL( LOGFD, STATUS )

*  Annul locator to the NDF extension holding the astrometry structure,
*  if the locator exists.
      CALL DAT_VALID( LOC, VALID, STATUS )
      IF ( VALID ) CALL DAT_ANNUL( LOC, STATUS )

*  If a GRP group was used to hold co-ordinate data, delete the group.
      IF ( IGRP .NE. GRP__NOID ) CALL GRP_DELET( IGRP, STATUS )

*  Close the NDF system.
      CALL NDF_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETSKY_ERR',
     :     'SETSKY: Unable to add astrometry structure to IRAS '/
     :     /'extension.', STATUS )
      END IF

      END

