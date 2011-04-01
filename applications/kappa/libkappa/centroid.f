      SUBROUTINE CENTROID ( STATUS )
*+
*  Name:
*     CENTROID

*  Purpose:
*     Finds the centroids of star-like features in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL CENTROID( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This routine takes an NDF and returns the co-ordinates of the
*     centroids of features in its data array given approximate initial
*     co-ordinates.  A feature is a set of connected pixels which are
*     above or below the surrounding background region.  For example, a
*     feature could be a star or galaxy on the sky, although the
*     applications is not restricted to two-dimensional NDFs.
*
*     Four methods are available for obtaining the initial positions,
*     selected using Parameter MODE:
*
*     - from the parameter system (see Parameter INIT);
*
*     - using a graphics cursor to indicate the feature in a previously
*     displayed data array (see Parameter DEVICE);
*
*     - from a specified positions list (see Parameter INCAT); or
*
*     - from a simple text file containing a list of co-ordinates (see
*     Parameter COIN).
*
*     In the first two modes the application loops, asking for new
*     feature co-ordinates until it is told to quit or encounters an
*     error.
*
*     The results may optionally be written to an output positions list
*     which can be used to pass the positions on to another application
*     (see Parameter OUTCAT), or to a log file geared more towards human
*     readers, including details of the input parameters (see Parameter
*     LOGFILE).
*
*     The uncertainty in the centroid positions may be estimated if
*     variance values are available within the supplied NDF (see
*     Parameter CERROR).

*  Usage:
*     centroid ndf [mode] { init   [search] [maxiter] [maxshift] [toler]
*                         { coin=?
*                         { incat=?
*                       mode

*  ADAM Parameters:
*     CATFRAME = LITERAL (Read)
*        A string determining the co-ordinate Frame in which positions
*        are to be stored in the output catalogue associated with
*        Parameter OUTCAT.  The string supplied for CATFRAME can be one
*        of the following options.
*
*        - A Domain name such as SKY, AXIS, PIXEL, etc.
*
*        - An integer value giving the index of the required Frame.
*
*        - An IRAS90 Sky Co-ordinate System (SCS) values such as
*        EQUAT(J2000) (see SUN/163).
*
*        If a null (!) value is supplied, the positions will be stored
*        in the current Frame. [!]
*     CATEPOCH = DOUBLE PRECISION (Read)
*        The epoch at which the sky positions stored in the output
*        catalogue were determined.  It will only be accessed if an
*        epoch value is needed to qualify the co-ordinate Frame
*        specified by COLFRAME.  If required, it should be given as a
*        decimal years value, with or without decimal places ("1996.8"
*        for example).  Such values are interpreted as a Besselian epoch
*        if less than  1984.0 and as a Julian epoch otherwise.
*     CENTRE = LITERAL (Write)
*        The formatted co-ordinates of the last centroid position, in
*        the current Frame of the NDF.
*     CERROR = _LOGICAL (Read)
*        If TRUE, errors in the centroided position will be calculated.
*        The input NDF must contain a VARIANCE component in order to
*        compute errors.  [FALSE]
*     COIN = FILENAME (Read)
*        Name of a text file containing the initial guesses at the
*        co-ordinates of features to be centroided.  It is only accessed
*        if Parameter MODE is given the value "File".  Each line should
*        contain the formatted axis values for a single position, in the
*        current Frame of the NDF. Axis values can be separated by
*        spaces, tabs or commas.  The file may contain comment lines
*        with the first character # or !.
*     DESCRIBE = LOGICAL (Read)
*        If TRUE, a detailed description of the co-ordinate Frame in
*        which  the centroided positions will be reported is displayed
*        before the positions themselves.  [current value]
*     DEVICE = DEVICE (Read)
*        The graphics device which is to be used to give the initial
*        guesses at the centroid positions.  It is only accessed if
*        Parameter MODE is given the value "Cursor".
*        [Current graphics device]
*     ERROR = LITERAL (Write)
*        The errors associated with the position written to Parameter
*        CENTRE.
*     GUESS = _LOGICAL (Read)
*        If TRUE, then the supplied guesses for the centroid positions
*        will be included in the screen and log file output, together
*        with the accurate positions.  [current value]
*     INCAT = FILENAME (Read)
*        A catalogue containing a positions list giving the initial
*        guesses at the centroid positions, such as produced by
*        applications CURSOR, LISTMAKE, etc.  It is only accessed if
*        Parameter MODE is given the value "Catalogue".
*     INIT = LITERAL (Read)
*        An initial guess at the co-ordinates of the next feature to be
*        centroided, in the current co-ordinate Frame of the NDF
*        (supplying a colon ":" will display details of the current
*        co-ordinate Frame).  The position should be supplied as a
*        list of formatted axis values separated by spaces or commas.
*        INIT is only accessed if Parameter MODE is given the value
*        "Interface".  If the initial co-ordinates are supplied on the
*        command line only one centroid will be found; otherwise the
*        application will ask for further guesses, which may be
*        terminated by supplying the null value (!).
*     LOGFILE = FILENAME (Read)
*        Name of the text file to log the results.  If null, there
*        will be no logging.  Note this is intended for the human reader
*        and is not intended for passing to other applications.  [!]
*     MARK = LITERAL (Read)
*        Only accessed if Parameter MODE is given the value "Cursor".
*        It indicates which positions are to be marked on the screen
*        using the marker type given by Parameter MARKER.  It can take
*        any of the following values.
*
*        - "Initial"  -- The position of the cursor when the mouse
*                        button is pressed is marked.
*
*        - "Centroid" -- The corresponding centroid position is marked.
*
*        - "None"     -- No positions are marked.
*
*        [current value]
*     MARKER = INTEGER (Read)
*        This parameter is only accessed if Parameter MARK is set TRUE.
*        It specifies the type of marker with which each cursor position
*        should be marked, and should be given as an integer PGPLOT
*        marker type.  For instance, 0 gives a box, 1 gives a dot, 2
*        gives a cross, 3 gives an asterisk, 7 gives a triangle.  The
*        value must be larger than or equal to -31.  [current value]
*     MAXITER = _INTEGER (Read)
*        Maximum number of iterations to be used in the search.  It must
*        be in the range 1--9.  The dynamic default is 3.  [9]
*     MAXSHIFT() = _REAL (Read)
*        Maximum shift in each dimension allowed between the guess and
*        output positions in pixels.  Each must lie in the range
*        0.0--26.0.  If only a single value is given, then it will be
*        duplicated to all dimensions.  The dynamic default is half of
*        SEARCH + 1.  [9.0]
*     MODE = LITERAL (Read)
*        The mode in which the initial co-ordinates are to be obtained.
*        The supplied string can be one of the following values.
*
*        - "Interface" -- positions are obtained using Parameter INIT.
*
*        - "Cursor"    -- positions are obtained using the graphics
*                         cursor of the device specified by Parameter
*                         DEVICE.
*
*        - "Catalogue" -- positions are obtained from a positions list
*                         using Parameter INCAT.
*
*        - "File"      -- positions are obtained from a text file using
*                         Parameter COIN.
*
*        [current value]
*     NDF = NDF (Read)
*        The NDF structure containing the data array to be analysed.  In
*        cursor mode (see Parameter MODE), the run-time default is the
*        displayed data, as recorded in the graphics database.  In other
*        modes, there is no run-time default and the user must supply a
*        value.  []
*     NSIM = _INTEGER (Read)
*        The number of simulations or realisations using the variance
*        information in order to estimate the error in the centroid
*        position.  The uncertainty in the centroid error decreases
*        as one over the square root of NSIM.  The range of acceptable
*        values is 3--10000.  [100]
*     OUTCAT = FILENAME (Write)
*        The output catalogue in which to store the centroided
*        positions.  If a null value (!) is supplied, no output
*        catalogue is produced.  See also Parameter CATFRAME.  [!]
*     PLOTSTYLE = GROUP (Read)
*        A group of attribute settings describing the style to use when
*        drawing the graphics markers specified by Parameter MARK.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text
*        file preceded by an up-arrow character "^".  Such text files
*        should contain further comma-separated lists which will be read
*        and interpreted in the same manner.  Attribute settings are
*        applied in the order in which they occur within the list, with
*        later settings overriding any earlier settings given for the
*        same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!)---the initial default---is
*        supplied.  To apply changes of style to only the current
*        invocation, begin these attributes with a plus sign.  A mixture
*        of persistent and temporary style changes is achieved by
*        listing all the persistent attributes followed by a plus sign
*        then the list of temporary attributes.
*
*        See section "Plotting Attributes" in SUN/95 for a description
*        of the available attributes.  Any unrecognised attributes are
*        ignored (no error is reported).  [current value]
*     POSITIVE = _LOGICAL (Read)
*        TRUE, if array features are positive above the background.
*        [TRUE]
*     SEARCH() = _INTEGER (Read)
*        Size in pixels of the search box to be used. If only a single
*        value is given, then it will be duplicated to all dimensions
*        so that a square, cube or hypercube region is searched.
*        Each value must be odd and lie in the range 3--51.  [9]
*     TITLE = LITERAL (Read)
*        A title to store with the output catalogue specified by
*        Parameter OUTCAT, and to display before the centroid positions
*        are listed.  If a null (!) value is supplied, the title is
*        taken from any input catalogue specified by Parameter INCAT, or
*        is a fixed string including the name of the NDF.  [!]
*     TOLER = _REAL (Read)
*        Accuracy in pixels required in centroiding.  Iterations will
*        stop when the shift between successive centroid positions
*        is less than the accuracy.  The accuracy must lie in the range
*        0.0--2.0.  [0.05]
*     XCEN = LITERAL (Write)
*         The formatted X co-ordinate of the last centroid position, in
*         the current co-ordinate Frame of the NDF.
*     XERR = LITERAL (Write)
*         The error associated with the value written to Parameter XCEN.
*     YCEN = LITERAL (Write)
*         The formatted Y co-ordinate of the last centroid position, in
*         the current co-ordinate Frame of the NDF.
*     YERR = LITERAL (Write)
*         The error associated with the value written to Parameter YCEN.

*  Examples:
*     centroid cluster cu
*        This finds the centroids in the NDF called cluster via the
*        graphics cursor on the current graphics device.
*     centroid cluster cu search=21 mark=ce plotstyle='colour=red'
*        This finds the centroids in the NDF called cluster via the
*        graphics cursor on the current graphics device.  The search
*        box for the centroid is 21 pixels in each dimension.  The
*        centroid positions are marked using a red symbol.
*     centroid cluster i "21.7,5007.1"
*        This finds the centroid of the object in the two-dimensional
*        NDF called cluster around the current Frame co-ordinate
*        (21.7,5007.1).
*     centroid arp244(6,,) i "40,30" toler=0.01
*        This finds the two-dimensional centroid of the feature near
*        pixel (6,40,30) in the three-dimensional NDF called arp244
*        (assuming the current co-ordinate Frame of the NDF is PIXEL).
*        The centroid must be found to 0.01 pixels.
*     centroid cluster cu xcen=(xp) ycen=(yp)
*        This finds the centroid of an object in the two-dimensional NDF
*        called cluster using a graphics cursor, and writes the
*        centroid co-ordinates to ICL variables XP and YP for use in
*        other applications.
*     centroid cluster mode=file coin=objects.dat logfile=centroids.log
*        This finds the centroids in the NDF called cluster.  The
*        initial positions are given in the text file objects.dat in
*        the current co-ordinate Frame.  A log of the input parameter
*        values, initial and centroid positions is written to the text
*        file centroids.log.
*     centroid cluster mode=cat incat=a outcat=b catframe=ecl
*        This example reads the initial guess positions from the
*        positions list in file a.FIT, and writes the accurate centroid
*        positions to positions list file b.FIT, storing the output
*        positions in ecliptic co-ordinates.  The input file may, for
*        instance, have been created using the application CURSOR.

*  Notes:
*     -  All positions are supplied and reported in the current
*     co-ordinate Frame of the NDF.  A description of the co-ordinate
*     Frame being used  is given if Parameter DESCRIBE is set to a TRUE
*     value.  Application WCSFRAME can be used to change the current
*     co-ordinate Frame of the NDF before running this application if
*     required.
*     -  In Cursor or Interface mode, only the first 200 supplied
*     positions will be stored in the output catalogue.  Any further
*     positions will be displayed on the screen but not stored in the
*     output catalogue.
*     -  The centroid positions are not displayed on the screen when the
*     message filter environment variable MSG_FILTER is set to QUIET.
*     The creation of output parameters and files is unaffected by
*     MSG_FILTER.

*  Related Applications:
*     KAPPA: PSF, CURSOR, LISTSHOW, LISTMAKE.

*  Implementation Status:
*     -  The processing of bad pixels and all non-complex numeric types
*     is supported.

*  Estimation of Centroid Positions:
*     Each centroid position is obtained by projecting the data values
*     within a search box centred on the supplied position, on to each
*     axis in turn.  This forms a set of profiles for the feature, one
*     for each axis.  An estimate of the background at each point in
*     these profiles is made and subtracted from the profile.  This
*     flattens the profile backgrounds, removing any slope in the data.
*     Once the profiles have been flattened in this way, and estimate of
*     the background noise in each is made.  The centroid of the feature
*     is then found using only the data above the noise level.
*
*     Successive estimates of the centroid position are made by using
*     the previous estimate of the centroid as the initial position for
*     another estimation. This loop is repeated up to a maximum number
*     of iterations, though it normally terminates when a desired
*     accuracy has been achieved.
*
*     The achieved accuracy is affected by noise, and the presence of
*     non-Gaussian or overlapping features, but typically an accuracy
*     better than 0.1 pixel is readily attainable for stars.  The error
*     in the centroid position may be estimated by a Monte-Carlo method
*     using the data variance to generate realisations of the data about
*     the feature (see Parameter CERROR).  Each realisation is processed
*     identically to the actual data, and statistics are formed to
*     derive the standard deviations.

*  Copyright:
*     Copyright (C) 1991, 1992, 1998-2001 Central Laboratory of
*         the Research Councils
*     Copyright (C) 2004-2006 Particle Physics and Astronomy Research
*     Council.
*     Copyright (C) 2009-2010 Science and Technology Facilities Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     1991 March 24 (MJC):
*        Original NDF version based on the pre-0.8 version.
*     1991 July 12 (MJC):
*        Added COOUT file.
*     1992 March 3 (MJC):
*        Replaced AIF parameter-system calls by the extended PAR
*        library.
*     1992 April 14 (MJC):
*        Handles arbitrary user-defined sections.
*     1992 November 30 (MJC):
*        Does not use non-monotonic axis centres.
*     5-JUN-1998 (DSB):
*        Report an error if an even value is supplied for SEARCH.
*     25-JUN-1999 (DSB):
*        Many major changes to make use of AST/PGPLOT.
*     13-DEC-2001 (DSB):
*        Added Parameters CATFRAME and CATEPOCH.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     27-DEC-2005 (TIMJ):
*        Use KPG1_NDFNAM.
*     03-MAY-2006 (TIMJ):
*        Protect some sections if status is bad. Prevents SEGV.
*        Initialise some variables.
*     2009 July 24 (MJC):
*        Remove QUIET parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     2010 October 14 (MJC):
*        Permit temporary style attributes.
*     1-APR-2011 (DSB):
*        Use KPG_GDFND in place of KPG1_AGFND in case the most recent
*        data picture had no WCS.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE            ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! SSE global definitions
      INCLUDE 'DAT_PAR'         ! Data-system constants
      INCLUDE 'AST_PAR'         ! AST constants and functions
      INCLUDE 'NDF_PAR'         ! NDF definitions
      INCLUDE 'SUBPAR_PAR'      ! SUBPAR constants
      INCLUDE 'PAR_ERR'         ! Parameter-system errors
      INCLUDE 'CNF_PAR'         ! For CNF_PVAL function
      INCLUDE 'MSG_PAR'         ! Message-system constants

*  Status:
      INTEGER  STATUS

*  Local Constants:
      INTEGER MXPOS              ! Max. no. of positions which can be
      PARAMETER ( MXPOS=200 )    ! given in interface mode

*  Local Variables:
      CHARACTER BUFOUT*132      ! Buffer for writing the logfile
      CHARACTER LOCI*(DAT__SZLOC)! Locator for input data structure
      CHARACTER MARK*8          ! Positions to mark
      CHARACTER MODE*10         ! Mode for getting initial co-ords
      CHARACTER NDFNAM*256      ! Name of input IMAGE
      CHARACTER REFNAM*256      ! Reference name
      CHARACTER TITLE*80        ! Title for output positions list
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      INTEGER CFRM              ! Pointer to the Current Frame of NDF
      INTEGER DIMS( NDF__MXDIM )! Dimensions of NDF
      INTEGER FDL               ! File description of logfile
      INTEGER FDO               ! File description of output co-ord list
      INTEGER I                 ! Loop counter
      INTEGER ID0               ! Identifier for first output position
      INTEGER IMARK             ! PGPLOT marker type
      INTEGER INDF              ! Input NDF identifier
      INTEGER IPIC              ! AGI identifier for last data picture
      INTEGER IPIC0             ! AGI id. for original current picture
      INTEGER IPID              ! Pointer to array of pos'n identifiers
      INTEGER IPIX              ! Index of PIXEL Frame in IWCS
      INTEGER IPLOT             ! Plot obtained from graphics database
      INTEGER IPIN              ! Pointer to array of supplied positions
      INTEGER IPOUT             ! Pointer to array of output positions
      INTEGER IPW1              ! Pointer to work space
      INTEGER IPW2              ! Pointer to work space
      INTEGER IPW3              ! Pointer to work space
      INTEGER IPW4              ! Pointer to work space
      INTEGER IWCS              ! WCS FrameSet from input NDF
      INTEGER IWCSG             ! FrameSet read from input catalogue
      INTEGER J                 ! Loop counter
      INTEGER MAP1              ! Mapping from PIXEL Frame to Current Frame
      INTEGER MAP2              ! Mapping from supplied Frame to Current Frame
      INTEGER MAP3              ! Mapping from supplied Frame to PIXEL Frame
      INTEGER MPOS              ! Size of output positions list
      INTEGER MXITER            ! Maximum number of iterations to be used
      INTEGER NAXC              ! No. of axes in current NDF Frame
      INTEGER NAXIN             ! No. of axes in supplied Frame
      INTEGER NC                ! Character column counter
      INTEGER NCI               ! Character column counter of image names
      INTEGER NDIM              ! Actual number of dimensions of NDF
      INTEGER NDIMS             ! Number of significant dimensions of the NDF
      INTEGER NPOS              ! No. of supplied positions
      INTEGER NSIM              ! Number of simulations
      INTEGER NVAL              ! Number of values return from a parameter system
      INTEGER SDIM( NDF__MXDIM )! Significant dimensions of the NDF
      INTEGER SEARCH( NDF__MXDIM )! Size of search region to be used
      INTEGER SLBND( NDF__MXDIM )! Significant lower bounds of the image
      INTEGER STATE             ! State of Parameter INIT
      INTEGER SUBND( NDF__MXDIM )! Significant upper bounds of the image
      LOGICAL CAT               ! Catalogue mode was selected
      LOGICAL CERROR            ! Errors to be calculated
      LOGICAL CURSOR            ! Cursor mode was selected
      LOGICAL DESC              ! Describe the current Frame?
      LOGICAL FILE              ! File mode was selected
      LOGICAL GOTLOC            ! A locator to the NDF has been obtained
      LOGICAL GOTNAM            ! A reference name of the NDF has been obtained
      LOGICAL GUESS             ! Display original gueses?
      LOGICAL INTERF            ! Interface mode was selected
      LOGICAL LOGPOS            ! A log of the positions is written to a text file
      LOGICAL OUTCO             ! The output co-ordinate file was opened successfully
      LOGICAL POSTVE            ! Array features are positive in sign
      LOGICAL QUIET             ! Suppress screen output?
      REAL MXSHFT( NDF__MXDIM ) ! Maximum shifts allowed between guess and output
      REAL TOLER                ! Accuracy required in centroiding
*.

*  Check the inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Initialise
      GOTLOC = .FALSE.
      GOTNAM = .FALSE.
      OUTCO = .FALSE.
      LOGPOS = .FALSE.
      FILE = .FALSE.
      NPOS = 0
      MARK = ' '

*  Initialise pointers for valgrind
      IPW1 = 0
      IPW2 = 0
      IPOUT = 0
      IPID = 0

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  See if we are to run quietly., i.e not at NORMAL or lower priority.
      QUIET = .NOT. MSG_FLEVOK( MSG__NORM, STATUS )

*  Abort if an error occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  Attempt to open a log file to store the results for human readers.
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FDL, STATUS )

*  Annul the error if a null value was given, and indicate that a log
*  file is not to be created.
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

      ELSE IF( STATUS .EQ. SAI__OK ) THEN
         LOGPOS = .TRUE.

      END IF

*  Remind the user about the log file, if required.
      IF( LOGPOS .AND. .NOT. QUIET ) CALL MSG_OUT( 'LOG', '  Logging '//
     :                                           'to $LOGFILE', STATUS )

*  Attempt to open an output co-ordinate text file.  This facility is
*  retained for compatibility with the pre-AST version of this
*  application, but is no longer documented.  It is expected that people
*  will now use Parameter OUTCAT instead of COOUT.  Only use the
*  parameter if it was given on the command line.
      CALL LPG_STATE( 'COOUT', STATE, STATUS )
      IF( STATE .EQ. SUBPAR__ACTIVE ) THEN

*  Abort if an error occured.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Get the file name and open the file.
         CALL FIO_ASSOC( 'COOUT', 'WRITE', 'LIST', 132, FDO, STATUS )

*  Annul the error if a null value was given, and indicate that an
*  output co-ordinate text file is not to be created.
         IF( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )

         ELSE IF( STATUS .EQ. SAI__OK ) THEN
            OUTCO = .TRUE.
         END IF
      END IF

*  Indicate we have not yet got a title for the output positions list.
      TITLE = ' '

*  Find where the initial guess positions are to be obtained from.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,Cursor,'//
     :                'Catalogue,File', .TRUE., MODE, STATUS )

*  Set convenience flags for the various values of MODE.
      CURSOR = MODE .EQ. 'CURSOR'
      CAT = MODE .EQ. 'CATALOGUE'
      FILE = MODE .EQ. 'FILE'
      INTERF = MODE .EQ. 'INTERFACE'

*  Abort if an error occured.
      IF( STATUS .NE. SAI__OK ) GO TO 999

*  No initialization needed for "File" mode.  We cannot read the
*  contents of a file yet, because we do not yet have an NDF and so do
*  not know how many columns the file must contain.
      IF( FILE ) THEN

*  In "Catalogue" mode, open a positions list catalogue and read its
*  contents.  A pointer to a FrameSet is returned, together with
*  pointers to positions and identifiers, and a title.  The positions
*  are returned in the Base Frame of this FrameSet.
      ELSE IF( CAT ) THEN
         IWCSG = AST__NULL
         CALL KPG1_RDLST( 'INCAT', .FALSE., IWCSG, NPOS, NAXIN, IPIN,
     :                    IPID, TITLE, ' ', STATUS )

*  In "Cursor" mode, open and prepare the graphics device.
      ELSE IF( CURSOR ) THEN

*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
         CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC0, STATUS )

*  Find the most recent DATA picture which has WCS.
         CALL KPG1_GDFND( 'DATA', IPIC, STATUS )

*  Report the name, comment, and label, if one exists, for the current
*  picture.
         CALL KPG1_AGATC( STATUS )

*  Set the PGPLOT viewport and AST Plot for this DATA picture.  The
*  PGPLOT viewport is set equal to the selected picture, with world
*  co-ordinates giving millimetres form the bottom-left corner of the
*  view surface.  The returned Plot may include a Frame with Domain
*  AGI_DATA representing AGI DATA co-ordinates (defined by a TRANSFORM
*  structure stored with the picture in the database).
         CALL KPG1_GDGET( IPIC, AST__NULL, .TRUE., IPLOT, STATUS )

*  See what markers are to be drawn.
         CALL PAR_CHOIC( 'MARK', 'Centroid', 'Centroid,Initial,None',
     :                   .FALSE., MARK, STATUS )

*  If so, get the marker type, and set the plotting style.
*  The plus sign requests support of temporary attributes.
         IF( MARK .NE. 'NONE' ) THEN
            CALL PAR_GDR0I( 'MARKER', 2, -31, 10000, .FALSE., IMARK,
     :                      STATUS )
            CALL KPG1_ASSET( 'KAPPA_CENTROID', '+PLOTSTYLE', IPLOT,
     :                       STATUS )

*  Set the current PGPLOT marker attributes (size, colour, etc) so that
*  they are the same as the marker attributes specified in the Plot.
*  The pre-existing PGPLOT attribute values are saved in ATTR.
            CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR, STATUS )

         END IF

*  Abort if an error has occurred.
         IF( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain a reference to the NDF.
         CALL KPG1_AGREF( IPIC, 'READ', GOTNAM, REFNAM, STATUS )

*  See whether the reference is a name or locator.  The latter should be
*  phased out, but there may be some old databases and software
*  in circulation.
         CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
         IF( GOTLOC ) LOCI = REFNAM

*  End immediately if there an error.
         IF( STATUS .NE. SAI__OK ) GO TO 999


*  No initialisation needed in "Interface" mode.
      ELSE IF( INTERF ) THEN

      END IF

*  Obtain the NDF. If the name is given on the command line it will be
*  used.  If not, the database data reference is used, if there is one.
*  Otherwise, the user is prompted.
      CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, INDF, STATUS )

*  Determine whether or not errors are to be estimated, and if so how
*  many simulations to perform.  Since a standard deviation is to be
*  calculated the minimum number of simulations is 3.  Check that there
*  is variance present in the NDF.
      NSIM = 1
      CALL PAR_GET0L( 'CERROR', CERROR, STATUS )
      IF( CERROR ) THEN
         CALL NDF_STATE( INDF, 'Variance', CERROR, STATUS )
         IF( .NOT. CERROR ) THEN
            CALL NDF_MSG( 'NDF', INDF )
            CALL MSG_OUT( ' ', 'Cannot produce error estimates since '//
     :                    'there is no Variance information in '//
     :                    '''^NDF''.', STATUS )
         ELSE
            CALL PAR_GDR0I( 'NSIM', 100, 3, 10000, .TRUE., NSIM,
     :                      STATUS )
         END IF
      END IF

*  We need to know how many significant axes there are (i.e. pixel axes
*  spanning more than a single pixel), so count them.  We ignore
*  insignificant axes since the user will probably not be interested in
*  them (and the centroiding routines cannot handle axes spanning only
*  a single pixel).
      CALL NDF_DIM( INDF, NDF__MXDIM, DIMS, NDIM, STATUS )
      NDIMS = 0
      DO I = 1, NDIM
         IF( DIMS( I ) .GT. 1 ) NDIMS = NDIMS + 1
      END DO

*  Now get the WCS FrameSet from the NDF.
      CALL KPG1_ASGET( INDF, NDIMS, .TRUE., .FALSE., .FALSE., SDIM,
     :                 SLBND, SUBND, IWCS, STATUS )

*  Get a pointer to the Current Frame in the NDF.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Save the number of Current Frame axes.
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )

*  Get the Mapping from PIXEL to Current Frame in the NDF. First find
*  the index of the PIXEL Frame, and then get the Mapping.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
      MAP1 = AST_SIMPLIFY( AST_GETMAPPING( IWCS, IPIX, AST__CURRENT,
     :                                     STATUS ), STATUS )

*  We now get the AST Mapping from the Frame in which the positions are
*  supplied to the Current Frame of the NDF.
      IF( CURSOR ) THEN

*  In cursor mode, the positions will be supplied in GRAPHICS
*  co-ordinates (i.e. millimetres from the bottom-left corner of the
*  screen).  Merge the Plot read from the graphics database with the
*  FrameSet read from the NDF aligning them in some suitable Frame.
         CALL KPG1_ASMRG( IPLOT, IWCS, ' ', QUIET, 0, STATUS )

*  Get the Mapping.
         MAP2 = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, AST__BASE,
     :                                        AST__CURRENT, STATUS ),
     :                        STATUS )

*  In catalogue mode, the positions are supplied in the Base Frame of
*  the FrameSet stored in the catalogue.  Merge this FrameSet with the
*  FrameSet read from the NDF aligning them in some suitable Frame.
      ELSE IF( CAT ) THEN
         CALL KPG1_ASMRG( IWCSG, IWCS, ' ', QUIET, 0, STATUS )

*  Get the Mapping.
         MAP2 = AST_SIMPLIFY( AST_GETMAPPING( IWCSG, AST__BASE,
     :                                        AST__CURRENT, STATUS ),
     :                        STATUS )

*  In the other modes, the positions are supplied in the Current Frame.
      ELSE
         MAP2 = AST_UNITMAP( NAXC, ' ', STATUS )
      END IF

*  Save the number of axes in the Frame in which the positions are
*  supplied.
      NAXIN = AST_GETI( MAP2, 'NIN', STATUS )

*  We need the Mapping from the Frame in which the positions are
*  supplied, to the PIXEL Frame of the NDF.  We get this Mapping by
*  concatenating the Mapping from input Frame to Current Frame, with
*  the Mapping from Current Frame to PIXEL Frame (obtained by
*  temporarily inverting the Mapping from PIXEL to Current Frame).
      CALL AST_INVERT( MAP1, STATUS )
      MAP3 = AST_SIMPLIFY( AST_CMPMAP( MAP2, MAP1, .TRUE., ' ',
     :                                 STATUS ), STATUS )
      CALL AST_INVERT( MAP1, STATUS )

*  See if a description of the NDFs current Frame is required.
      CALL PAR_GET0L( 'DESCRIBE', DESC, STATUS )

*  If so, give a detailed description of the Frame in which positions
*  will be reported if required.
      IF( DESC .AND. .NOT. QUIET ) THEN
         CALL KPG1_DSFRM( CFRM, 'Positions will be reported in the '//
     :                    'following co-ordinate Frame:', AST__BAD,
     :                    AST__BAD, .TRUE., STATUS )
      END IF

*  If we are in "File" mode, obtain the file and read the positions,
*  interpreting them as positions within the Current Frame of the NDF.
*  A pointer to memory holding the positions is returned.  Store a safe
*  value for the IPID pointer.  Identifiers are generated automatically
*  in File mode instead of being read from the file, and so we do not
*  have a pointer to an array of identifiers at this point.
      IF( FILE ) THEN
         CALL KPG1_ASFIL( 'COIN', ' ', CFRM, NPOS, IPIN, ' ', STATUS )
         IPID = IPIN
      END IF

*  Allocate work space to hold the output positions list.  These
*  positions are given in the Current Frame of the NDF.  There is a
*  limit of MXPOS positions in interactive modes (Cursor, and
*  Interface).
      IF( CAT .OR. FILE ) THEN
         MPOS = NPOS
      ELSE
         MPOS = MXPOS
      END IF
      CALL PSX_CALLOC( NAXC*MPOS, '_DOUBLE', IPOUT, STATUS )

*  Get a title for the output positions list. Use the title from the
*  input positions list as the default, or construct a new title if none
*  is available.
      IF( TITLE .EQ. ' ' ) THEN

*  Get the NDF name in a token and convert it to a string.
         CALL KPG1_NDFNM( INDF, NDFNAM, NCI, STATUS )

*  Construct the new title.
         IF( STATUS .EQ. SAI__OK ) THEN
            NC = 0
            TITLE = ' '
            CALL CHR_APPND( 'Feature positions in ', TITLE, NC )
            NC = NC + 1
            CALL CHR_APPND( NDFNAM( :NCI ), TITLE, NC )
         END IF

      END IF

      IF( STATUS .EQ. SAI__OK ) THEN
         CALL PAR_DEF0C( 'TITLE', TITLE, STATUS )
         CALL PAR_GET0C( 'TITLE', TITLE, STATUS )
         IF( STATUS .EQ. PAR__NULL ) CALL ERR_ANNUL( STATUS )
      END IF

*  Obtain the search region sizes, duplicating the value if only a
*  single value is given.  Each size must be a positive odd number.
      CALL PAR_GDRVI( 'SEARCH', NDIMS, 1, 51, SEARCH, NVAL, STATUS )
      IF( STATUS .EQ. SAI__OK .AND. NVAL .LT. NDIMS ) THEN
         DO  I = NVAL + 1, NDIMS
            SEARCH( I ) = SEARCH( 1 )
         END DO
      END IF

*  Constrain the search area to be odd and no bigger than the image.
      DO  I = 1, NDIMS

         IF( MOD( SEARCH( I ), 2  ) .EQ. 0 .AND.
     :      STATUS .EQ. SAI__OK ) THEN
            STATUS = SAI__ERROR
            CALL MSG_SETI( 'S', SEARCH( I ) )
            CALL ERR_REP( 'CENTROID_EVN', 'CENTROID: Even value ^S '//
     :                    'supplied for Parameter %SEARCH - must '//
     :                    'be odd.', STATUS )
            GO TO 999
         END IF

         SEARCH( I ) = MIN( DIMS( I ), SEARCH( I ) )

      END DO

      CALL PAR_GDR0I( 'MAXITER', 3, 1, 9, .TRUE., MXITER, STATUS )
      CALL PAR_GET0L( 'POSITIVE', POSTVE, STATUS )

*  Obtain the maximum shifts, duplicating the value if only a single
*  value  is given.  Each size must be a positive odd number.
      CALL PAR_DEF1R( 'MAXSHIFT', 1, REAL( SEARCH( 1 ) ) * 0.5 + 1.0,
     :                STATUS )

      CALL PAR_GDRVR( 'MAXSHIFT', NDIMS, 0.0, 26.0, MXSHFT, NVAL,
     :                STATUS )

      IF( STATUS .EQ. SAI__OK .AND. NVAL .LT. NDIMS ) THEN
         DO  I = NVAL + 1, NDIMS
            MXSHFT( I ) = MXSHFT( 1 )
         END DO
      END IF

*  Constrain the maximum shifts.
      DO  I = 1, NDIMS
         MXSHFT( I ) = MIN( MXSHFT( I ), REAL( DIMS( I ) ) )
      END DO

*  Obtain the tolerance of the iterations to find the centroid.
      CALL PAR_GDR0R( 'TOLER', 0.05, 0.0, 2.0, .TRUE., TOLER, STATUS )

*  See if original positions are to be displayed.
      CALL PAR_GET0L( 'GUESS', GUESS, STATUS )

*  Record what is going on in the log file.
      IF( LOGPOS .AND. STATUS .EQ. SAI__OK ) THEN

*  Form string for the search areas.
         BUFOUT = 'Search area in pixels = '
         NC = 25
         DO  J = 1, NDIMS
            CALL CHR_PUTI( SEARCH( J ), BUFOUT, NC )
            IF( J. LT. NDIMS ) CALL CHR_PUTC( ', ', BUFOUT, NC )
         END DO
         CALL FIO_WRITE( FDL, BUFOUT( :NC ), STATUS )

         WRITE( BUFOUT, '(''Maximum number of iterations = '',I2)' )
     :          MXITER
         CALL FIO_WRITE( FDL, BUFOUT( :33), STATUS )

         IF( POSTVE ) THEN
            CALL FIO_WRITE( FDL, 'Positive features', STATUS )
         ELSE
            CALL FIO_WRITE( FDL, 'Negative features', STATUS )
         END IF

*  Form string for the search areas.
         BUFOUT = 'Maximum shift in pixels = '
         NC = 27
         DO  J = 1, NDIMS
            CALL CHR_PUTR( MXSHFT( J ), BUFOUT, NC )
            IF( J. LT. NDIMS ) CALL CHR_PUTC( ', ', BUFOUT, NC )
         END DO
         CALL FIO_WRITE( FDL, BUFOUT( :NC ), STATUS )

         WRITE( BUFOUT, '(''Tolerance in pixels = '', G11.5)' ) TOLER
         CALL FIO_WRITE( FDL, BUFOUT( :33 ), STATUS )
         CALL FIO_WRITE( FDL, ' ', STATUS )

         IF( CERROR ) THEN
            WRITE( BUFOUT, '(''Number of simulations = '', I4)' ) NSIM
            CALL FIO_WRITE( FDL, BUFOUT, STATUS )
            CALL FIO_WRITE( FDL, ' ', STATUS )
         END IF

      END IF

*  Process all the supplied centroids together as a single batch in
*  non-interactive modes.
      IF( CAT .OR. FILE ) THEN

*  Allocate work arrays.
         CALL PSX_CALLOC( NPOS * NAXC, '_DOUBLE', IPW1, STATUS )
         CALL PSX_CALLOC( NPOS * NDIMS, '_DOUBLE', IPW2, STATUS )
         IF( CERROR ) THEN
            CALL PSX_CALLOC( NSIM * NPOS * NDIMS, '_DOUBLE', IPW3,
     :                       STATUS )
            CALL PSX_CALLOC( NSIM * NPOS * NAXC, '_DOUBLE', IPW4,
     :                       STATUS )
         ELSE
            IPW3 = IPW1
            IPW4 = IPW1
         END IF

*  Find the centroids and errors, and display them.  Sets the hardwired
*  Parameters CENTRE, XCEN, YCEN, XERR, YERR, and ERROR.
         CALL KPS1_CENBT( INDF, CERROR, MAP3, MAP1, MAP2, CFRM, NPOS,
     :                    NAXC, NAXIN, %VAL( CNF_PVAL( IPIN ) ), CAT,
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    LOGPOS, FDL, QUIET, NSIM, NDIMS, SLBND, SUBND,
     :                    SEARCH, POSTVE, GUESS, MXSHFT, MXITER, OUTCO,
     :                    FDO, TOLER, TITLE, NSIM * NPOS,
     :                    %VAL( CNF_PVAL( IPW1 ) ),
     :                    %VAL( CNF_PVAL( IPW2 ) ),
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    %VAL( CNF_PVAL( IPW3 ) ),
     :                    %VAL( CNF_PVAL( IPW4 ) ), STATUS )

*  Free the work arrays.
         CALL PSX_FREE( IPW1, STATUS )
         CALL PSX_FREE( IPW2, STATUS )

         IF( CERROR ) THEN
            CALL PSX_FREE( IPW3, STATUS )
            CALL PSX_FREE( IPW4, STATUS )
         END IF

*  In interactive modes, find each centroid individually, waiting for
*  the user to supply a new one before continuing each time.
      ELSE

*  Allocate work arrays.
         IF( CERROR ) THEN
            CALL PSX_CALLOC( NSIM * NDIMS, '_DOUBLE', IPW1, STATUS )
            CALL PSX_CALLOC( NSIM * NAXC, '_DOUBLE', IPW2, STATUS )
         END IF

*  Find the centroids and errors, and display them.  Sets the hardwired
*  Parameters CENTRE, XCEN, YCEN, XERR, YERR, and ERROR.
         CALL KPS1_CENSG( INDF, CERROR, MAP3, MAP1, MAP2, CFRM, 'INIT',
     :                    CURSOR, MARK, IMARK, NAXC, NAXIN, LOGPOS, FDL,
     :                    QUIET, NSIM, NDIMS, SLBND, SUBND, SEARCH,
     :                    POSTVE, GUESS, MXSHFT, MXITER, OUTCO, FDO,
     :                    TOLER, TITLE, MPOS, %VAL( CNF_PVAL( IPOUT ) ),
     :                    NPOS, %VAL( CNF_PVAL( IPW1 ) ),
     :                    %VAL( CNF_PVAL( IPW2 ) ), STATUS )

*  Free the work arrays.
         IF( CERROR ) THEN
            CALL PSX_FREE( IPW1, STATUS )
            CALL PSX_FREE( IPW2, STATUS )
         END IF

      END IF

*  Decide where the position identifiers for the output positions list
*  will come from.  In Catalogue mode they are copied from the input
*  positions list, which should already have been stored in an array
*  pointed to by IPID. Set ID0 to zero to tell KPG1_WRLST to use the
*  IPID array.
      IF( CAT ) THEN
         ID0 = 0

*  In other modes, no position identifiers are available, so we create
*  new ones. The identifier for the first position (1) is stored in ID0.
*  This causes KPG1_WRLST to ignore the IPID array and create its own
*  monotonically increasing identifiers starting at the value of ID0.
*  However, we need to make sure that the IPID variable contains a safe
*  pointer value even though it is not used.  This is to avoid a risk of
*  an access violation when the pointer is passed using %VAL.  So we
*  assign it the value of IPOUT.
      ELSE
         ID0 = 1
         IPID = IPOUT
      END IF

*  Create the output positions list if there are any positions ot
*  output.
      IF( NPOS .GT. 0 ) THEN
         CALL KPG1_WRLST( 'OUTCAT', MPOS, NPOS, NAXC,
     :                    %VAL( CNF_PVAL( IPOUT ) ),
     :                    AST__CURRENT, IWCS, TITLE, ID0,
     :                    %VAL( CNF_PVAL( IPID ) ),
     :                    .TRUE., STATUS )
      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  Close any open files.
      IF( OUTCO ) CALL FIO_ANNUL( FDO, STATUS )
      IF( LOGPOS ) CALL FIO_ANNUL( FDL, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Release the dynamic arrays holding the input positions and
*  identifiers in catalogue mode.
      IF( CAT ) THEN
         CALL PSX_FREE( IPID, STATUS )
         CALL PSX_FREE( IPIN, STATUS )

*  Release the dynamic arrays holding the input positions and
*  identifiers in file mode.
      ELSE IF( FILE ) THEN
         CALL PSX_FREE( IPIN, STATUS )

*  Do cursor mode tidying...
      ELSE IF( CURSOR ) THEN

*  Annul the locator to the reference object.
         IF( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
         CALL DAT_VALID( LOCI, GOTLOC, STATUS )
         IF( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )

*  Re-instate any changed PGPLOT marker attributes.
         IF( MARK .NE. 'NONE' ) CALL KPG1_PGSTY( IPLOT, 'MARKERS',
     :                                           .FALSE., ATTR, STATUS )

*  Close the graphics database and device.
         CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CENTROID_ERR', 'CENTROID: Failed to find a '//
     :                 'set of centroided feature positions.', STATUS )
      END IF

      END
