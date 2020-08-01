      SUBROUTINE SETSKY( STATUS )
*+
*  Name:
*     SETSKY

*  Purpose:
*     Stores new  WCS information within an NDF.

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
*     This application adds WCS information describing a celestial sky
*     co-ordinate system to a two-dimensional NDF. This information can
*     be stored either in the form of a standard NDF WCS component, or
*     in the form of an "IRAS90 astrometry structure" (see Parameter
*     IRAS90).
*
*     The astrometry is determined either by you supplying explicit
*     values for certain projection parameters, or by you providing the
*     sky and corresponding image co-ordinates for a set of positions
*     (see Parameter POSITIONS).  In the latter case, the projection
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
*        is missing, a Besselian epoch is assumed if the epoch is less
*        than 1984.0, and a Julian epoch is assumed otherwise.
*     EPOCH = DOUBLE PRECISION (Read)
*        The Julian epoch at which the observation was made (e.g.
*        "1994.0").
*     IRAS90 = _LOGICAL (Read)
*        If a TRUE value is supplied, then the WCS information will be
*        stored in the form of an IRAS90 astrometry structure. This is the
*        form used by the IRAS90 package (see SUN/163). In this case, any
*        existing IRAS90 astrometry structure will be over-written. See
*        the "Notes:" section below for warnings about using this form.
*
*        If a FALSE value is supplied, then the WCS information will be
*        stored in the form of a standard NDF WCS component which will be
*        recognized, used and updated correctly by most other Starlink
*        software.
*
*        If a null value (!) is supplied, then a TRUE value will be used
*        if the supplied NDF already has an IRAS90 extension. Otherwise a
*        FALSE value will be used. [!]
*     LAT = LITERAL (Read)
*        The latitude of the reference point, in the co-ordinate system
*        specified by Parameter COORDS.  For example, if COORDS is
*        "Equatorial", LAT is the Declination.  See SUN/163, Section
*        4.7.2 for full details of the allowed syntax for specifying
*        this position.  For convenience here are some examples how you
*        may specify the Declination -45 degrees, 12 arcminutes: "-45 12
*        00", "-45 12", "-45d 12m", "-45.2d", "-451200", "-0.78888r".
*        The last of these is a radians value.  A null value causes the
*        latitude of the reference point to be estimated automatically
*        from the data supplied for Parameter POSITIONS. [!]
*     LOGFILE = FILENAME (Read)
*        Name of the text file to log the final projection parameter
*        values and the residual at each supplied position.  If null,
*        there will be no logging.  This parameter is ignored if a null
*        value is given to Parameter POSITIONS. [!]
*     LON= LITERAL (Read)
*        The longitude of the reference point, in the co-ordinate
*        system specified by Parameter COORDS.  For example, if COORDS
*        is "Equatorial", LON is the Right Ascension.  See SUN/163,
*        Section 4.7.2 for full details of the allowed syntax for
*        specifying this position.  For convenience here are some
*        examples how you may specify the Right Ascension 11 hours, 34
*        minutes, and 56.2 seconds: "11 34 56.2", "11h 34m 56.2s", "11
*        34.9366", "11.58228", "113456.2".  See Parameter LAT for
*        examples of specifying a non-equatorial longitude.  A null
*        value causes the longitude of the reference point to be
*        estimated automatically from the data supplied for Parameter
*        POSITIONS. [!]
*     NDF = NDF (Read and Write)
*        The NDF in which to store the WCS information.
*     ORIENT = LITERAL (Read)
*        The position angle of the NDF's y axis on the celestial
*        sphere, measured from north through east.  North is defined as
*        the direction of increasing sky latitude, and east is the
*        direction of increasing sky longitude.  Values are constrained
*        to the range 0 to two-pi radians.  A null value causes the
*        position angle to be estimated automatically from the data
*        supplied for Parameter POSITIONS. [!]
*     PIXELREF( 2 ) = REAL (Read)
*        The pixel co-ordinates of the reference pixel (x then y).
*        This parameter is ignored unless REFCODE = "Pixel".  Remember
*        that the centre of a pixel at indices i,j is (i-0.5,j-0.5).  A
*        null value causes the pixel co-ordinates of the reference
*        point to be estimated automatically from the data supplied for
*        Parameter POSITIONS. [!]
*     PIXELSIZE( 2 ) = _REAL (Read)
*        The x and y pixel sizes at the reference position.  If only
*        one value is given, the pixel is deemed to be square.  Values
*        may be given in a variety of units (see Parameter LAT).  For
*        example, 0.54 arcseconds could be specified as "0.54s" or
*        "0.009m" or "2.618E-6r".  A null value causes the pixel
*        dimensions to be estimated automatically from the data
*        supplied for Parameter POSITIONS. [!]
*     POSITIONS = LITERAL (Read)
*        A list of sky co-ordinates and corresponding image
*        co-ordinates for the set of positions which are to be used to
*        determine the astrometry.  If a null value is given then the
*        astrometry is determined by the explicit values you supply for
*        each of the other parameters.  Each position is defined by
*        four values, the sky longitude (in the same format as for
*        Parameter LON), the sky latitude (in the same format as for
*        Parameter LAT), the image pixel x co-ordinate and the image
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
*           "Gnomonic"       - Gnomonic (i.e. tangent plane),
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
*        from the data supplied for Parameter POSITIONS. [!]
*     REFCODE = LITERAL (Read)
*        The code for the reference pixel.  If it has value "Pixel"
*        this requests that pixel co-ordinates for the reference point
*        be obtained through Parameter PIXELREF.  The other options are
*        locations specified by two characters, the first corresponding
*        to the vertical position and the second the horizontal.  For
*        the vertical, valid positions are T(op), B(ottom), or
*        C(entre); and for the horizontal the options are L(eft),
*        R(ight), or C(entre).  Thus REFCODE = "CC" means the reference
*        position is at the centre of the NDF image, and "BL" specifies
*        that the reference position is at the centre of the
*        bottom-left pixel in the image.  A null value causes the pixel
*        co-ordinates of the reference point to be estimated
*        automatically from the data supplied for Parameter POSITIONS.
*        [!]
*     TILT = LITERAL (Read)
*        The angle through which the celestial sphere is to be rotated
*        prior to doing the projection.  The axis of rotation is a
*        radius passing through the reference point.  The rotation is
*        in an anti-clockwise sense when looking from the reference
*        point towards the centre of the celestial sphere.  In common
*        circumstances this can be set to zero.  Values may be given in
*        a variety of units (see Parameter LAT).  Values are
*        constrained to the range 0 to two-pi radians.  A null value
*        causes the latitude of the reference point to be estimated
*        automatically from the data supplied for Parameter POSITIONS.
*        ["0.0"]

*  Examples:
*     setsky m51 ^stars.lis ecl(j1994.0) 1994.0 logfile=m51.log
*        This creates a WCS component to a two-dimensional NDF called
*        m51.  The values for Parameters PROJTYPE, LON, LAT, PIXELREF,
*        PIXELSIZE, and ORIENT are determined automatically so
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
*        This creates a WCS component within the two-dimensional NDF
*        called m51.  The values for Parameters PROJTYPE, LON, LAT,
*        PIXELREF, and PIXELSIZE are determined automatically as in
*        the previous example.  In this example however, an Orthographic
*        projection is forced, and the value zero is assigned to
*        Parameter ORIENT, resulting in north being `upwards' in the image.
*     setsky virgo "!" eq(j2000.0) 1989.3 gn "12 29" "+12 30" bl 1.1s 0.0d
*        This creates a WCS component within the two-dimensional
*        NDF called virgo.  It is a gnomonic projection in the
*        equatorial system at Julian epoch 2000.0.  The bottom-left
*        pixel of the image is located at Right Ascension 12 hours 29
*        minutes, Declination +12 degrees 30 minutes.  A pixel at that
*        position is square and has angular size of 1.1 arcseconds.
*        The image was observed at epoch 1989.3.  At the bottom-left of
*        the image, north is at the top, parallel to the y-axis of the
*        image.
*     setsky map "!" galactic(1950.0) 1993.8 aitoff 90 0 cc [0.5d,0.007r] 180.0d
*        This creates a WCS component within the two-dimensional
*        NDF called map.  It is an Aitoff projection in the galactic
*        system at Besselian epoch 1950.0.  The centre of the image is
*        located at galactic longitude 90 degrees, latitude 0 degrees.
*        A pixel at that position is rectangular and has angular size
*        of 0.5 degrees by 0.007 radians.  The image was made at epoch
*        1993.8.  At the image centre, south is at the top and is
*        parallel to the y-axis of the image.
*     setsky zodiac "!" ec 1983.4 or 10.3 -5.6 Pixel 20m 0.3d pixelref=[9.5,-11.2] IRAS90=YES
*        This creates an IRAS90 astrometry extension within the
*        two-dimensional NDF called zodiac.  It is an orthographic
*        projection in the Ecliptic system at Besselian epoch 1950.0.
*        The reference point at pixel co-ordinates (9.5,-11.2)
*        corresponds to ecliptic longitude 10.3 degrees, latitude -5.6
*        degrees.  A pixel at that position is square and has
*        angular size of 20 arcminutes.  The image was observed at
*        epoch 1983.4.  At the reference point the y-axis of the image
*        points to 0.3 degrees east of north.

*  Notes:
*     - The GAIA image display tool (SUN/214) provides various interactive
*     tools for storing new WCS information within an NDF.
*     - This application was written to supply the limited range of WCS
*     functions required by the IRAS90 package. For instance, it does not
*     support the complete range or projections or sky co-ordinate systems
*     which may be represented by the more general NDF WCS component.
*     - If WCS information is stored in the form of an IRAS90 astrometry
*     structure (see Parameter IRAS90), it will in general be invalidated
*     by any subsequent KAPPA commands which modify the transformation
*     between sky and pixel coordinates. For instance, if the image is
*     moved using SLIDE (for example), then the IRAS90 astrometry
*     structure will no longer correctly describe the sky co-ordinates
*     associated with each pixel. For this reason (amongst others) it is
*     better to set Parameter IRAS90 to a false value.

*  Related Applications:
*     ASTROM; IRAS90: SKYALIGN, SKYBOX, SKYGRID, SKYLINE, SKYMARK,
*     SKYPOS, SKYWRITE.

*  Copyright:
*     Copyright (C) 1994 Science & Engineering Research Council.
*     Copyright (C) 1995, 2001 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1994 April 13 (MJC):
*        Original version.
*     28-OCT-1994 (DSB):
*        Added Parameters POSITIONS and LOGFILE.  Modified to determine
*        projection parameters automatically from the data supplied for
*        POSITIONS.
*     1995 February 13 (MJC):
*        Corrected typo's, errors in the examples, output the fitted
*        pixel size in arcseconds if appropriate.  Validated the
*        extension locator before annulling it.
*     26-NOV-2001 (DSB):
*        Added Parameter IRAS90.
*     {enter_further_changes_here}

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
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PAR_ERR'          ! PAR error constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER NDIM               ! Number of dimensions
      PARAMETER ( NDIM = 2 )
      INTEGER NP                 ! Number of projection parameters
      PARAMETER ( NP = 8 )

*  Local Variables:
      CHARACTER ASNAME*( DAT__SZNAM ) ! Name of astrometry structure
      CHARACTER ATEXT*( IRA__SZFSC ) ! Best ref longitude
      CHARACTER BTEXT*( IRA__SZFSC ) ! Best ref latitude
      CHARACTER CPIXEL( 2 )*( IRA__SZFSC ) ! Pixel sizes as sky co-ordinate strings
      CHARACTER CXREF*( VAL__SZR ) ! X-position of reference pixel
      CHARACTER CYREF*( VAL__SZR ) ! Y-position of reference pixel
      CHARACTER LOC*( DAT__SZLOC ) ! Locator to the IRAS extension
      CHARACTER PATEXT*( IRA__SZFSC ) ! Best image orientation
      CHARACTER PRMSUF*( 26 + 2 * VAL__SZR ) ! String to complete the prompt string for LON and LAT
      CHARACTER PROJEC*( IRA__SZPRJ ) ! Chosen projection type
      CHARACTER PROTYP*( IRA__SZPLS ) ! List of supported projection types
      CHARACTER REFPOS*( 5 ) ! Reference-position code
      CHARACTER SCS*( IRA__SZSCS ) ! Chosen sky co-ordinate system
      CHARACTER TTEXT*( IRA__SZFSC ) ! Best celestial sphere tilt
      CHARACTER XNAME*( DAT__SZNAM ) ! Name of extn. holding astrometry data
      DOUBLE PRECISION EPOCH     ! Julian epoch
      DOUBLE PRECISION P( NP )   ! Parameters defining the projection
      INTEGER ADDED              ! No. of elements added to a GRP group
      INTEGER IDA                ! Astrometry identifier
      INTEGER IGRP               ! ID for GRP group holding co-ord data
      INTEGER IGRP0              ! ID for GRP group holding co-ord data
      INTEGER IPIX               ! Index of PIXEL Frame in IWCS
      INTEGER IRAFRM             ! AST Frame describing IRA sky co-ords
      INTEGER IRAMAP             ! AST Mapping from IRA "image" to sky co-ords
      INTEGER IWCS               ! WCS FrameSet
      INTEGER LBND( NDIM )       ! Lower bounds of the NDF
      INTEGER LOGFD              ! FIO log file descriptor
      INTEGER MDIM               ! Actual no. of dimensions of the NDF
      INTEGER NCX                ! No. of signif. characters in CXREF
      INTEGER NCY                ! No. of signif. characters in CYREF
      INTEGER NDF                ! NDF identifier
      INTEGER NPOS               ! No. of supplied sky positions
      INTEGER SDIM( NDF__MXDIM ) ! Significant NDF dimensions
      INTEGER SIZE               ! Current size of GRP group.
      INTEGER UBND( NDIM )       ! Upper bounds of the NDF
      LOGICAL FLAG               ! Was group expression flagged?
      LOGICAL IRAS90             ! Store WCS info as an IRA structure?
      LOGICAL LOG                ! Log search results to a file?
      LOGICAL NEWIRA             ! Has the IRA structure been created by this app?
      LOGICAL ORIENT             ! Has value been supplied for ORIENT?
      LOGICAL PROJ               ! Was a value supplied for PROJTYPE?
      LOGICAL PSIZE              ! Has value been supplied for PIXSIZE?
      LOGICAL REFIMG             ! Were ref. pos. image co-ords given?
      LOGICAL REFSKY             ! Were ref. pos. sky co-ords given?
      LOGICAL SEARCH             ! Search in parameter space?
      LOGICAL THERE              ! If true there is already an IRAS extension in the NDF
      LOGICAL TILT               ! Has value been supplied for TILT?
      LOGICAL VALID              ! Locator is valid or not
      REAL RMS                   ! Smallest RMS positional error found
      REAL PIXREF( 2 )           ! Reference pixel co-ordinates
      REAL PIXSIZ( 2 )           ! Pixel sizes

*.

*  Check inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Access the NDF and find its shape.
*  ==================================

*  Start an AST context.
      CALL AST_BEGIN( STATUS )

*  Start an NDF context.
      CALL NDF_BEGIN

*  Obtain the NDF.
      CALL LPG_ASSOC( 'NDF', 'UPDATE', NDF, STATUS )

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
      IF( STATUS .NE. SAI__OK ) GO TO 999

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
      IF( STATUS .EQ. SAI__OK ) THEN
         SEARCH = .TRUE.

*  Report an error if no values were supplied
         IF( SIZE .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSKY_ERR1', 'No values were supplied '/
     :                    /'for Parameter ''%POSITIONS''.', STATUS )
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
         IF( 4*NPOS .NE. SIZE ) THEN
            CALL MSG_SETI( 'N', SIZE )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSKY_ERR2', 'An incomplete list of '/
     :        /'positions was supplied for Parameter ''%POSITIONS''. '/
     :        /' ^N values supplied.', STATUS )
            GOTO 999
         END IF


      ELSE IF( STATUS .EQ. PAR__NULL ) THEN
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
      THERE = .FALSE.
      NEWIRA = .FALSE.
      CALL IRA_FIND( NDF, THERE, XNAME, ASNAME, LOC, STATUS )
      IF( THERE ) THEN
         CALL IRA_LOCAT( XNAME, ASNAME, STATUS )

*  Otherwise, use extension IRAS and component ASTROMETRY.
      ELSE
         XNAME = 'IRAS'
         ASNAME = 'ASTROMETRY'
         CALL IRA_LOCAT( XNAME, ASNAME, STATUS )

*  Check whether the extension already exists.
         CALL NDF_XSTAT( NDF, XNAME, THERE, STATUS )

*  Create it if it is not present.
         IF( .NOT. THERE ) THEN
            NEWIRA = .TRUE.
            CALL NDF_XNEW( NDF, XNAME, 'IRAS_EXTENSION', 0, 0, LOC,
     :                     STATUS )
         END IF

      END IF

*  Exit if an error occurred.  This is needed because the significant
*  dimensions are used as array indices (and also because of the
*  following check for a null parameter value).
      IF( STATUS .NE. SAI__OK ) GOTO 999

*  See if the IRAS90 extension is to be deleted at the end.
      CALL PAR_GET0L( 'IRAS90', IRAS90, STATUS )

*  A null value means "Keep it only if the input NDF has an IRAS90
*  extension."
      IF( STATUS .NE. SAI__OK ) THEN
         IRAS90 = THERE
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

*  Obtain details of the transformation.
*  =====================================

*  Find the available projection types.
      CALL IRA_IPROJ( PROTYP, STATUS )

*  Abort if an error has already occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the projection type.
      CALL PAR_CHOIC( 'PROJTYPE', 'GNOMONIC', PROTYP, .FALSE., PROJEC,
     :                STATUS )

*  A null value is OK if the mapping is to be based on supplied
*  co-ordinate data.  In this case, the best projection will be found
*  and used.
      IF( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
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
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the reference co-ordinates.
      CALL PAR_CHOIC( 'REFCODE', 'CC', 'Pixel,TL,BL,CL,TR,BR,CR,TC,'/
     :                /'BC,CC', .FALSE., REFPOS, STATUS )

*  If the reference co-ordinates are pixel, the user wants to nominate
*  some arbitrary co-ordinates for the reference pixel.  Get the value
*  and create the prompt suffix for obtaining the longitude and
*  latitude.
      IF( REFPOS .EQ. 'PIXEL' ) THEN
         CALL PAR_EXACR( 'PIXELREF', 2, PIXREF, STATUS )
         CALL CHR_RTOC( PIXREF( 1 ), CXREF, NCX )
         CALL CHR_RTOC( PIXREF( 2 ), CYREF, NCY )
         PRMSUF = ' at pixel co-ordinates ('//CXREF( :NCX )//','/
     :               /CYREF( :NCY )//')'

*  Assign other prompt suffices and reference pixel co-ordinates for
*  each of the other position codes.
*
*  The centre
      ELSE IF( REFPOS .EQ. 'CC' ) THEN
         PIXREF( 1 ) = 0.5 * REAL( UBND( 1 ) + LBND( 1 ) )
         PIXREF( 2 ) = 0.5 * REAL( UBND( 2 ) + LBND( 2 ) )
         PRMSUF = ' of the centre.'

*  The top-left corner
      ELSE IF( REFPOS .EQ. 'TL' ) THEN
         PIXREF( 1 ) = REAL( LBND( 1 ) )
         PIXREF( 2 ) = REAL( UBND( 2 ) )
         PRMSUF = ' of the top-left corner.'

*  The bottom-left corner
      ELSE IF( REFPOS .EQ. 'BL' ) THEN
         PIXREF( 1 ) = REAL( LBND( 1 ) )
         PIXREF( 2 ) = REAL( LBND( 2 ) )
         PRMSUF = ' of the bottom-left corner.'

*  Centre-left
      ELSE IF( REFPOS .EQ. 'CL' ) THEN
         PIXREF( 1 ) = REAL( LBND( 1 ) )
         PIXREF( 2 ) = 0.5 * REAL( UBND( 2 ) + LBND( 2 ) )
         PRMSUF = ' of midway along the left side.'

*  The top-right corner
      ELSE IF( REFPOS .EQ. 'TR' ) THEN
         PIXREF( 1 ) = REAL( UBND( 1 ) )
         PIXREF( 2 ) = REAL( UBND( 2 ) )
         PRMSUF = ' of the top-right corner.'

*  The bottom-right corner
      ELSE IF( REFPOS .EQ. 'BR' ) THEN
         PIXREF( 1 ) = REAL( UBND( 1 ) )
         PIXREF( 2 ) = REAL( LBND( 2 ) )
         PRMSUF = ' of the bottom-right corner.'

*  The centre-right
      ELSE IF( REFPOS .EQ. 'CR' ) THEN
         PIXREF( 1 ) = REAL( UBND( 1 ) )
         PIXREF( 2 ) = 0.5 * REAL( UBND( 2 ) + LBND( 2 ) )
         PRMSUF = ' of midway along the right side.'

*  The top-centre
      ELSE IF( REFPOS .EQ. 'TC' ) THEN
         PIXREF( 1 ) = 0.5 * REAL( UBND( 1 ) + LBND( 1 ) )
         PIXREF( 2 ) = REAL( UBND( 2 ) )
         PRMSUF = ' of midway along the top side.'

*  The bottom-centre
      ELSE IF( REFPOS .EQ. 'BC' ) THEN
         PIXREF( 1 ) = 0.5 * REAL( UBND( 1 ) + LBND( 1 ) )
         PIXREF( 2 ) = REAL( LBND( 2 ) )
         PRMSUF = ' of midway along the bottomside.'

      END IF

*  A null value is OK for REFCODE or PIXELREF if the mapping is to be
*  based on supplied co-ordinate data.  In this case, the best image
*  co-ordinates for the reference position will be found and used.
      IF( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
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
      IF( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         REFSKY = .FALSE.
      ELSE
         REFSKY = .TRUE.
      END IF

*  Abort if an error has already occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the x and y pixel sizes as strings.  If just one value is
*  supplied assume that the x and y sizes are the same.
      CALL PAR_GETVC( 'PIXELSIZE', NDIM, CPIXEL, MDIM, STATUS )
      IF( MDIM .EQ. 1 ) CPIXEL( 2 ) = CPIXEL( 1 )
      IF( CPIXEL( 1 ) .EQ. ' ' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'INVPIXSIZE',
     :     'SETSKY: The pixelsize is undefined.', STATUS )
      END IF

*  A null value for the pixel size is OK if the mapping is to be based
*  on supplied co-ordinate data.  Annul the error and set a flag to
*  indicate that pixel size has not yet been determined.
      IF( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         PSIZE = .FALSE.

*  If a pixel size was supplied, flag that pixel size has been
*  determined.
      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         PSIZE = .TRUE.

*  Convert the formatted sky co-ordinate specifying the pixelsize
*  (usually arcsec or radians) into double-precision values.
         CALL IRA_CTOD1( CPIXEL( 1 ), SCS, 2, P( 5 ), STATUS )
         CALL IRA_CTOD1( CPIXEL( 2 ), SCS, 2, P( 6 ), STATUS )

      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

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
      IF( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         ORIENT = .FALSE.

*  Otherwise, indicate that the orientation of the map has been fixed.
      ELSE
         ORIENT = .TRUE.
      END IF

*  Abort if an error has occurred.
      IF( STATUS .NE. SAI__OK ) GO TO 999

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
      IF( SEARCH .AND. STATUS .EQ. PAR__NULL ) THEN
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
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  If a search is to be done, get the name of a log file in which to
*  store the parameter values best fit residuals.  Annul the error if
*  a null value is supplied.
      IF( SEARCH ) THEN

         CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 0, LOGFD, STATUS )

         IF( STATUS .EQ. PAR__NULL ) THEN
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
      IF( SEARCH ) THEN
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
      IF( PIXSIZ( 1 ) .LT. 1.0 .AND. PIXSIZ( 2 ) .LT. 1.0 ) THEN
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

*  If requested, we now convert the IRA structure into an equivalent
*  NDF WCS component.
      IF( .NOT. IRAS90 ) THEN

*  Create an AST Frame describing the sky co-ordinates in the IRA
*  structure, and a Mapping from IRA "image" co-ordinates to sky
*  co-ordinates.
         CALL KPG1_ASIRA( IDA, IRAFRM, IRAMAP, STATUS )

*  Only proceed if both Mapping and Frame were produced.
         IF( IRAFRM .NE. AST__NULL .AND. IRAMAP .NE. AST__NULL ) THEN

*  Get the current WCS FrameSet from the NDF.
            CALL NDF_GTWCS( NDF, IWCS, STATUS )

*  Find the index of the PIXEL Frame.
            CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )

*  Add the IRA Frame into the FrameSet, using the IRA Mapping to join the
*  existing PIXEL Frame to the IRA Frame. This makes PIXEL co-ords and IRA
*  "image" co-ords identical.
            CALL AST_ADDFRAME( IWCS, IPIX, IRAMAP, IRAFRM, STATUS )

*  Save the new WCS FrameSet.
            CALL NDF_PTWCS( IWCS, NDF, STATUS )

*  Report an error if the WCS component could not be created.
         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'SETSKY_ERR', 'Could not convert the IRAS90'//
     :                    ' astrometry information into a standard '//
     :                    'NDF WCS component.', STATUS )
         END IF

      END IF

*  Annul the IRA identifier.
      CALL IRA_ANNUL( IDA, STATUS )

*  Close the IRA system.
      CALL IRA_CLOSE( STATUS )

*  Jump to here if an error occurs.
  999 CONTINUE

*  If required, delete the IRAS90 extension.
      IF( .NOT. IRAS90 .OR. ( STATUS .NE. SAI__OK .AND. NEWIRA ) ) THEN
         CALL ERR_BEGIN( STATUS )
         CALL NDF_XDEL( NDF, XNAME, STATUS )
         CALL ERR_END( STATUS )
      END IF

*  If required, annul the log file descriptor.
      IF( LOG ) CALL FIO_ANNUL( LOGFD, STATUS )

*  Annul locator to the NDF extension holding the astrometry structure,
*  if the locator exists.
      CALL DAT_VALID( LOC, VALID, STATUS )
      IF( VALID ) CALL DAT_ANNUL( LOC, STATUS )

*  If a GRP group was used to hold co-ordinate data, delete the group.
      IF( IGRP .NE. GRP__NOID ) CALL GRP_DELET( IGRP, STATUS )

*  Close the NDF system.
      CALL NDF_END( STATUS )

*  Close the AST context.
      CALL AST_END( STATUS )

*  If an error occurred, then report a contextual message.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SETSKY_ERR', 'SETSKY: Unable to add '//
     :                 'astrometry information to an NDF.', STATUS )
      END IF

      END
