      SUBROUTINE BEAMFIT ( STATUS )
*+
*  Name:
*     BEAMFIT

*  Purpose:
*     Fits beam features in an NDF.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL BEAMFIT( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This fits Gaussians to beam features within the data array of a
*     two-dimensional NDF given approximate initial co-ordinates.  It
*     uses an unconstrained least-squares minimisation using the
*     residuals and a modified Levenberg-Marquardt algorithm.  The
*     beam feature is a set of connected pixels which are either
*     above or below the surrounding background region.  The errors in
*     the fitted coefficients are also calculated.
*
*     You may apply various constraints.  These are either fixed, or
*     relative.   Fixed values include the FWHM or background level.
*     Relative constraints define the properties of secondary beam
*     features with respect to the primary (first given) feature, and
*     can specify amplitude ratios, and beam separations.  
*
*     Four methods are available for obtaining the initial positions, 
*     selected using parameter MODE:
*
*     - from the parameter system (see parameter INIT);
*
*     - using a graphics cursor to indicate the feature in a previously
*     displayed data array (see parameter DEVICE);
*
*     - from a specified positions list (see parameter INCAT); or
*
*     - from a simple text file containing a list of co-ordinates (see
*     parameter COIN).
*
*     In the first two modes the application loops, asking for new
*     feature co-ordinates until it is told to quit or encounters an
*     error or the maximum number of features is reached.  The last is
*     five, unless parameter SEPARATION defines the location of the 
*     secondary beams and then only the primary beam's position is 
*     demanded.
*
*     The fit coefficients and their errors are stored in results
*     parameters, which can be used to pass them coefficients on to 
*     another application.  Also a lsiting of the fit may be written
*     to a log file geared more towards human readers, including details
*     of the input parameters (see parameter LOGFILE).  
*
*  Usage:
*     beamfit ndf [mode] [beams] init1

*  ADAM Parameters:
*     AMP( 2 ) = _DOUBLE (Write)
*        The primary beam position's amplitude and its error.
*     AMPRATIO( ) = _REAL (Read)
*        If number of beam positions given by BEAMS is more than one,
*        this specifies the ratio of the amplitude of the secondary
*        beams to the primary.  Thus you should supply one fewer value 
*        than the number of beams.  If you give fewer than that the last
*        ratio  is copied to the missing values.  The ratios would
*        normally be negative, usually -1 or -0.5.  AMPRATIO is ignored
*        when there is only one beam feature to fit.
*     BACK( 2 ) = _DOUBLE (Write)
*        The primary beam position's background level and its error.
*     BEAMS = _INTEGER (Read)
*        The number of beam positions to fit.  This will normally be 1, 
*        unless a chopped observation is supplied, when there may be two
*        or three beam positions.  This parameter is ignored  for "File" 
*        and "Catalogue" modes, where the number comes from the number 
*        of beam positions read from the files; and for "Interface" mode
*        when the beam position is supplied on the command line.
*        In all modes there is a maximum of five positions, which for
*        "File" or "Catalogue" modes will be the first five.  [1]
*     CENTRE( 2 ) = LITERAL (Write)
*        The formatted co-ordinates and their errors of the primary
*        beam in the current co-ordinate Frame of the NDF.  
*     COIN =  FILENAME (Read)
*        Name of a text file containing the initial guesses at the 
*        co-ordinates of beams to be fitted.  It is only accessed if 
*        parameter MODE is given the value "File".  Each line should
*        contain the formatted axis values for a single position, in the
*        current Frame of the NDF.  Axis values can be separated by 
*        spaces, tabs or commas.  The file may contain comment lines 
*        with the first character # or !. 
*     DESCRIBE = LOGICAL (Read)
*        If TRUE, a detailed description of the co-ordinate Frame in 
*        which the beam positions will be reported is displayed before 
*        the positions themselves.  [current value]
*     DEVICE = DEVICE (Read)
*        The graphics device which is to be used to give the initial
*        guesses at the beam positions.  Only accessed if parameter 
*        MODE is given the value "Cursor".  [Current graphics device]
*     FIXAMP = _DOUBLE (Read)
*        This specifies the fixed amplitude of the first beam. 
*        Secondary sources arising from chopped data use FIXAMP 
*        multiplied by the AMPRATIO.  A null value indicates that
*        the amplitude should be fitted.  [!]
*     FITAREA() = _INTEGER (Read)
*        Size in pixels of the fitting area to be used.  If only a 
*        single value is given, then it will be duplicated to all 
*        dimensions so that a square region is fitted.  Each value must 
*        be greater than 9.  A null value requests that the full data 
*        array is used.  [!]
*     FIXBACK = _DOUBLE (Read)
*        If a non-null value is supplied then the model fit will use
*        that value as the constant background level otherwise the
*        background is a free parameter of the fit.  [!]
*     FIXFWHM = LITERAL (Read)
*        If a non-null value is supplied then the model fit will use
*        that value as the full-width half-maximum value for the beam
*        and assumes that the beam is circular.  If two values are 
*        supplied then these are the fixed major- and minor-axis 
*        full-width half maxima.
*        
*        If the current co-ordinate Frame of the NDF is a SKY Frame 
*        (e.g. right ascension and declination), then the value should 
*        be supplied as an increment of celestial latitude (e.g.
*        declination).  Thus, "5.7" means 5.7 arcseconds, "20:0" would
*        mean 20 arcminutes, and "1:0:0" would mean 1 degree.  If the 
*        current co-ordinate Frame is not a SKY Frame, then the widths
*        should be specified as an increment along Axis 1 of the 
*        current co-ordinate Frame.  Thus, if the Urrent Frame is 
*        PIXEL, the value should be given simply as a number of pixels.
*
*        Null indicates that the FWHM values are free parameters of the
*        fit.  [!]
*     FIXPOS = _LOGICAL (Read)
*        If TRUE, the supplied position of each beam is used and
*        the centre co-ordinates of the beam are not fit.  FALSE means
*        that the centre is to be fitted.  It is advisable not to use 
*        this option in the inaccurate "Cursor" mode.  [FALSE]
*     INCAT = FILENAME (Read)
*        A catalogue containing a positions list giving the initial
*        guesses at the beam positions, such as produced by applications
*        CURSOR, LISTMAKE, etc.  It is only accessed if parameter MODE 
*        is given the value "Catalogue". 
*     INIT1-INIT5 = LITERAL (Read)
*        An initial guess at the co-ordinates of the next beam to be 
*        fitted, in the current co-ordinate Frame of the NDF (supplying 
*        a colon ":" will display details of the current co-ordinate 
*        Frame).  The primary feature's position is given by INIT1, and 
*        any secondary beams by INIT2 through INIT5, the parameter name
*        incrementing by 1 for each subsequent beam feature.  The total
*        number required is equal to the value of parameter BEAMS.
*
*        A position should be supplied as a list of formatted WCS axis
*        values separated by spaces or commas.  INIT1-INIT5 parameters
*        are only accessed if parameter MODE is given the value 
*        "Interface".  If the initial co-ordinates are supplied on the
*        command line only one beam will be fit; otherwise the 
*        application will ask for further beams that may be terminated
*        by supplying the null value (!). 
*     LOGFILE = FILENAME (Read)
*        Name of the text file to log the results.  If null, there
*        will be no logging.  Note this is intended for the human reader
*        and is not intended for passing to other applications.  [!]
*     MAJFWHM( 2 ) = _DOUBLE (Write)
*         The primary beam position's major-axis FWHM and its error, 
*         measured in the current co-ordinate Frame of the NDF.
*     MARK = LITERAL (Read)
*        Only accessed if parameter MODE is given the value "Cursor". 
*        It indicates which positions are to be marked on the screen 
*        using the marker type given by parameter MARKER.  It can take 
*        any of the following values.
*
*        - "Initial" -- The position of the cursor when the mouse 
*       button is pressed is marked.
*
*        - "Fit" -- The corresponding fit position is marked.
*
*        - "None" -- No positions are marked.
*
*        [current value]
*     MARKER = INTEGER (Read)
*        This parameter is only accessed if parameter MARK is set TRUE.
*        It specifies the type of marker with which each cursor 
*        position should be marked, and should be given as an integer
*        PGPLOT marker type.  For instance, 0 gives a box, 1 gives a
*        dot, 2 gives a cross, 3 gives an asterisk, 7 gives a triangle. 
*        The value must be larger than or equal to -31.  [current value]
*     MINFWHM( 2 ) = _DOUBLE (Write)
*        The primary beam position's minor-axis FWHM and its error,
*        measured in the current co-ordinate Frame of the NDF.
*     MODE = LITERAL (Read)
*        The mode in which the initial co-ordinates are to be obtained. 
*        The supplied string can be one of the following values.
*
*        - "Interface" -- positions are obtained using parameter INIT.
*
*        - "Cursor" -- positions are obtained using the graphics cursor
*        of the device specified by parameter DEVICE.
*
*        - "Catalogue" -- positions are obtained from a positions list
*        using parameter INCAT.
*
*        -  "File" -- positions are obtained from a text file using 
*        parameter COIN. 
*        [current value]
*     NDF = NDF (Read)
*        The NDF structure containing the data array to be analysed.  In
*        cursor mode (see parameter MODE), the run-time default is the 
*        displayed data, as recorded in the graphics database.  In other
*        modes, there is no run-time default and the user must supply a
*        value.  []
*     ORIENT( 2 ) = _DOUBLE (Write)
*        The primary beam position's orientation and its error, measured
*        in degrees.  If the current WCS frame is a SKY Frame, the angle
*        is measured from North through East.  For other Frames the 
*        angle is from the X-axis through Y.
*     PLOTSTYLE = LITERAL (Read)
*        A group of attribute settings describing the style to use when
*        drawing the graphics markers specified by parameter MARK.  
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text 
*        file preceded by an up-arrow character "^".  Such text files 
*        should contain further comma-separated lists which will be 
*        read and interpreted in the same manner.  Attribute settings 
*        are applied in the order in which they occur within the list, 
*        with later settings overriding any earlier settings given for 
*        the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value>
*        is the value to assign to the attribute.  Default values will
*        be used for any unspecified attributes.  All attributes will be
*        defaulted if a null value (!) is supplied.  See section 
*        "Plotting Attributes" in SUN/95 for a description of the 
*        available attributes.  Any unrecognised attributes are ignored
*        (no error is reported).  [current value]
*     QUIET = _LOGICAL (Read)
*        If TRUE then the fit parameters are not displayed on the 
*        screen.  Output parameters and files are still created.  
*        [FALSE]
*     RMS = _DOUBLE (Write)
*        The primary beam position's root mean-squared deviation from
*        the fit.
*     SEP1-SEP4 = LITERAL (Read)
*        If number of beam positions given by BEAMS is more than one,
*        each parameter specifies fixed separations along each axis 
*        of a secondary beam position to that of the primary.  Thus you 
*        should supply one fewer offset pairs than the number of beam 
*        positions via these parameters.  SEP1 applies to the first 
*        secondary beam, SEP2 is for the second secondary beam, and so
*        on.
*
*        Each distance should be given as a single literal string 
*        containing a space- or comma-separated list of axis values 
*        measured in the current co-ordinate Frame of the NDF.  The
*        allowed formats depends on the class of the current Frame.
*        Supplying a single colon ":" will display details of the
*        current Frame, together with an indication of the format 
*        required for each axis value, and a new parameter value is then
*        obtained.

*        A null value means fit to all beam positions.  [!]
*     VARIANCE = _LOGICAL (Read)
*        If TRUE, then any VARIANCE component present within the input
*        NDF will be used to weight the fit; the weight used for each
*        data value is the reciprocal of the variance.  If set to FALSE 
*        or there is no VARIANCE present, all points will be given equal
*        weight.  [FALSE]

*  Examples:
*     beamfit mars_3pos i 1 "5.0,-3.5"
*        This finds the Gaussian coefficients of the primary beam
*        feature in the NDF called mars_3pos, using the supplied
*        co-ordinates (5.0,-3.5) for the initial guess for the
*        beam;s centre.  The co-ordinates are measured in the NDF's
*        current co-ordinate Frame.  In this case they offsets in
*        arcseconds.
*     beamfit ndf=mars_3pos mode=interface beams=1 init1="5.0,-3.5"
*             fixback=0.0
*        As above but now the background is fixed to be zero.
*     beamfit ndf=mars_3pos mode=interface beams=1 fixfwhm=16.5
*        As above but now the Gaussian is constrained to have
*        a FWHM of 16.5 arcseconds and be circular.
*     beamfit mars_3pos in beams=1 fixfwhm=16.5 fitarea=51
*        As above but now the fitted data is restricted to areas
*        51x51 pixels about the initial guess positions.  All
*        the other examples use the full array.
*     beamfit mars_3pos int 3 "5.0,-3.5" ampratio=-0.5
*        As the first example except this finds the Gaussian 
*        coefficients of the primary beam feature and two secondary 
*        features.  The secondary features have fixed amplitudes that 
*        are half that of the primary feature and of the opposite
*        polarity.
*     beamfit mars_3pos int 2 "5.0,-3.5" sep1="-60.5,0.6" fixpos
*        This finds the Gaussian coefficients of the primary beam
*        feature and a secondary feature in the NDF called mars_3pos.
*        The supplied co-ordinates (5.0,-3.5) define the centre, i.e.
*        they are not fitted.  Also the secondary beam is fixed at
*        (-55.5,-2.9).
*     beamfit mode=cu beams=1
*        This finds the Gaussian coefficients of the primary beam
*        feature of an NDF, using the graphics cursor on the current 
*        graphics device to indicate the approximate centre of the 
*        feature.  The NDF being analysed comes from the graphics
*        database.
*     beamfit mode=cu beams=3 sep1="-60.5,0.6" sep2="59.5,-1.7"
*        This is like the previous example except it finds the Gaussian
*        coefficients of the primary beam feature and two secondary 
*        features in an NDF.  The graphics cursor indicates the 
*        approximate centre of the primary feature.  The secondary
*        features have fixed offsets from the primary, measured in
*        the current Frame of the NDF displayed.
*     beamfit jupiter cu 2 mark=ce plotstyle='colour=red' marker=3
*        This fits to two beam feaures in the NDF called jupiter 
*        via the graphics cursor on the current graphics device.  The
*        beam positions are marked using a red asterisk.
*     beamfit jupiter file 4 coin=features.dat logfile=jupiter.log
*        This fits to the beam features in the NDF called jupiter.  The
*        initial positions are given in the text file features.dat in
*        the current co-ordinate Frame.  Only the first four positions
*        will be used.  A log of selected input parameter values, 
*        and the fitted coefficents and errors is written to the text
*        file jupiter.log.
*     beamfit jupiter mode=cat incat=jove
*        This example reads the initial guess positions from the
*        positions list in file jove.FIT.  The number of beam
*        features fit is the number of positions in the catalogue
*        subject to a maximum of five.  The input file may, for
*        instance, have been created using the application CURSOR.

*  Notes:
*     -  All positions are supplied and reported in the current 
*     co-ordinate Frame of the NDF.  A description of the co-ordinate 
*     Frame being used is given if parameter DESCRIBE is set to a TRUE
*     value.  Application WCSFRAME can be used to change the current
*     co-ordinate Frame of the NDF before running this application if 
*     required.
*     -  In Cursor or Interface mode, only the first 200 supplied 
*     positions will be stored in the output catalogue.  Any further
*     positions will be displayed on the screen but not stored in the
*     output catalogue.
*     -  The uncertainty in the positions are estimated iteratively
*     using the curvature matrix derived from the Jacobian, itself
*     determined by a forward-difference approximation.

*  Related Applications:
*     KAPPA: PSF, CENTROID, CURSOR, LISTSHOW, LISTMAKE; ESP: FITGAUSS.

*  Implementation Status:
*     -  The processing of bad pixels and all non-complex numeric types
*     is supported.

*  Copyright:
*     Copyright (C) 2007 Particle Physics and Astronomy Research 
*     Council.  All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE.  See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public
*     License along with this program; if not, write to the Free
*     Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
*     MA 02111-1307, USA.

*  Authors:
*     MJC: Malcolm J. Currie  (STARLINK)
*     {enter_new_authors_here}

*  History:
*     2007 January 17 (MJC):
*        Original version based on CENTROID.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT  NONE             ! No implicit typing allowed

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! SSE global definitions
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'AST_PAR'          ! AST constants and functions
      INCLUDE 'NDF_PAR'          ! NDF definitions
      INCLUDE 'MSG_PAR'          ! Message-system constants
      INCLUDE 'SUBPAR_PAR'       ! SUBPAR constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function
      INCLUDE 'PRM_PAR'          ! PRIMDAT constants
      INCLUDE 'BF_PAR'           ! Used for array size constants

*  Status:
      INTEGER  STATUS

*  Local Constants:
      INTEGER MXPOS              ! Maximum number of beam positions
      PARAMETER ( MXPOS = 5 )

      INTEGER MXCOEF              ! Maximum number of fit coefficients
      PARAMETER ( MXCOEF = BF__NCOEF * BF__MXPOS )

*  Local Variables:
      REAL AMPRAT( BF__MXPOS - 1 ) ! Amplitude ratios
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      DOUBLE PRECISION BC( NDF__MXDIM )! Dummy base co-ordinates
      CHARACTER*132  BUFOUT      ! Buffer for writing the logfile
      LOGICAL CAT                ! Catalogue mode was selected
      INTEGER CFRM               ! Pointer to Current Frame of the NDF
      LOGICAL CURSOR             ! Cursor mode was selected
      LOGICAL DESC               ! Describe the current Frame?
      INTEGER DIMS( BF__NDIM )   ! Dimensions of NDF
      LOGICAL FAREA              ! Full data area to be used?
      INTEGER FDL                ! File description of logfile
      LOGICAL FILE               ! File mode was selected?
      INTEGER FITREG( BF__NDIM ) ! Size of fitting region to be used
      DOUBLE PRECISION FPAR( MXCOEF ) ! Stores fixed parameter values
      LOGICAL FIXCON( BF__NCON ) ! Constraints set?
      DOUBLE PRECISION FWHM( BF__NDIM ) ! Fixed FWHM
      LOGICAL FXFWHM             ! FWHM of the Gaussian fixed?
      LOGICAL GOTLOC             ! Locator to the NDF obtained?
      LOGICAL GOTNAM             ! Reference name of the NDF obtained?
      LOGICAL HASVAR             ! Errors to be calculated
      INTEGER I                  ! Loop counter
      INTEGER ID0                ! Identifier for first output position
      INTEGER IMARK              ! PGPLOT marker type
      INTEGER INDF               ! Input NDF identifier
      LOGICAL INTERF             ! Interface mode selected?
      INTEGER IPIC               ! AGI identifier for last data picture
      INTEGER IPIC0              ! AGI identifier for original current 
                                 ! picture
      INTEGER IPID               ! Pointer to array of position 
                                 ! identifiers
      INTEGER IPIX               ! Index of PIXEL Frame in IWCS
      INTEGER IPLOT              ! Plot obtained from graphics database
      INTEGER IPIN               ! Pointer to array of supplied 
                                 ! positions
      INTEGER IPOUT              ! Pointer to array of output positions
      INTEGER IPW1               ! Pointer to work space
      INTEGER IPW2               ! Pointer to work space
      INTEGER IWCS               ! WCS FrameSet from input NDF
      INTEGER IWCSG              ! FrameSet read from input catalogue
      INTEGER J                  ! Loop counter and index
      CHARACTER*(DAT__SZLOC) LOCI ! Locator for input data structure
      LOGICAL LOGF               ! Write log of positions to text file?
      INTEGER MAP1               ! Mapping from PIXEL Frame to Current 
                                 ! Frame
      INTEGER MAP2               ! Mapping from supplied Frame to 
                                 ! Current Frame
      INTEGER MAP3               ! Mapping from supplied Frame to PIXEL 
                                 ! Frame
      CHARACTER*8 MARK           ! Positions to mark
      CHARACTER*10 MODE          ! Mode for getting initial co-ords 
      LOGICAL MORE               ! Obtain another separation?
      INTEGER MSGFIL             ! Initial message-system filter level
      INTEGER NAMP               ! Number of amplitude ratios supplied
      INTEGER NAXC               ! Number of axes in current NDF Frame
      INTEGER NAXIN              ! Number of axes in supplied Frame
      INTEGER NC                 ! Character column counter
      INTEGER NCI                ! Character column counter of image 
                                 ! names
      INTEGER NCOEF              ! Number of fit coefficients
      CHARACTER*256 NDFNAM       ! Name of input IMAGE
      INTEGER NDIMS              ! Number of significant dimensions of 
                                 ! the NDF
      INTEGER NPOS               ! Number of supplied beam positions
      INTEGER NVAL               ! Number of values returned for a
                                 ! parameter
      DOUBLE PRECISION OFF1( NDF__MXDIM ) ! Separation for one position
      DOUBLE PRECISION OFFSET( BF__MXPOS - 1, NDF__MXDIM )! Separations
      LOGICAL POSC               ! Centre fixed at supplied position?
      LOGICAL QUIET              ! Suppress screen output?
      CHARACTER*256 REFNAM       ! Reference name
      DOUBLE PRECISION S2FWHM    ! Standard deviation to FWHM
      INTEGER SDIM( BF__NDIM )   ! Significant dimensions of the NDF
      CHARACTER*4 SEPAR          ! SEPn parameter name
      INTEGER SLBND( BF__NDIM )  ! Significant lower bounds of the image
      INTEGER STATE              ! State of parameter INIT
      INTEGER SUBND( BF__NDIM )  ! Significant upper bounds of the image
      CHARACTER*80 TITLE         ! Title for output positions list
      LOGICAL VAR                ! Use variance for weighting
      INTEGER WAX                ! Index of axis measuring fixed FWHMs

*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise.
      GOTLOC = .FALSE.
      GOTNAM = .FALSE.
      LOGF = .FALSE.
      FILE = .FALSE.
      NPOS = 0
      MARK = ' '

*  Initialise pointers for valgrind.
      IPW1 = 0
      IPW2 = 0
      IPOUT = 0
      IPIN = 0
      IPID = 0

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Begin an NDF context.
      CALL NDF_BEGIN

*  See if we are to run quietly.
      CALL PAR_GET0L( 'QUIET', QUIET, STATUS )

*  Abort if an error occurred.
      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Silent running, but first record the exisitng reporting level.
      CALL MSG_IFLEV( MSGFIL )
      IF ( QUIET ) CALL MSG_IFSET( MSG__QUIET, STATUS )

*  Attempt to open a log file to store the results for human readers.  
      CALL FIO_ASSOC( 'LOGFILE', 'WRITE', 'LIST', 80, FDL, STATUS )

*  Annul the error if a null value was given, and indicate that a log
*  file is not to be created.
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )

      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         LOGF = .TRUE.

      END IF

*  Remind the user about the log file, if required.
      IF ( LOGF ) CALL MSG_OUTIF( MSG__NORM, 'LOG', 
     :                            '  Logging to $LOGFILE', STATUS )

*  Select & initialise beam-selection mode
*  =======================================

*  Find where the initial guess positions are to be obtained from.
      CALL PAR_CHOIC( 'MODE', 'Interface', 'Interface,Cursor,'//
     :                'Catalogue,File', .TRUE., MODE, STATUS )

*  Set convenience flags for the various values of MODE.
      CURSOR = MODE .EQ. 'CURSOR'
      CAT = MODE .EQ. 'CATALOGUE'
      FILE = MODE .EQ. 'FILE'
      INTERF = MODE .EQ. 'INTERFACE'
      TITLE = ' '

*  Abort if an error occurred.
      IF ( STATUS .NE.  SAI__OK ) GO TO 999

      NPOS = -1

*  No initialization needed for "File" mode.  We cannot read the 
*  contents of a file yet, because we do not yet have an NDF and so do
*  not know how many columns the file must contain.
      IF ( FILE ) THEN

*  In "Catalogue" mode, open a positions list catalogue and read its
*  contents.  A pointer to a FrameSet is returned, together with 
*  pointers to positions and identifiers, and a title.  The positions 
*  are returned in the Base Frame of this FrameSet.
      ELSE IF ( CAT ) THEN
         IWCSG = AST__NULL
         CALL KPG1_RDLST( 'INCAT', .FALSE., IWCSG, NPOS, NAXIN, IPIN, 
     :                    IPID, TITLE, ' ', STATUS )
         NPOS = MIN( NPOS, BF__MXPOS )

*  In "Cursor" mode, open and prepare the graphics device.
      ELSE IF ( CURSOR ) THEN

*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
         CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC0, STATUS )

*  Find the most recent DATA picture.
         CALL KPG1_AGFND( 'DATA', IPIC, STATUS )

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
         CALL PAR_CHOIC( 'MARK', 'Fit', 'Fit,Initial,None',
     :                   .FALSE., MARK, STATUS )

*  If so, get the marker type, and set the plotting style.
         IF ( MARK .NE.  'NONE' ) THEN
            CALL PAR_GDR0I( 'MARKER', 2, -31, 10000, .FALSE., IMARK, 
     :                      STATUS )
            CALL KPG1_ASSET( 'KAPPA_BEAMFIT', 'PLOTSTYLE', IPLOT, 
     :                       STATUS )

*  Set the current PGPLOT marker attributes (size, colour, etc.) so 
*  that they are the same as the marker attributes specified in the
*  Plot.  The pre-existing PGPLOT attribute values are saved in ATTR.
            CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR, STATUS )
         END IF

*  Abort if an error has occurred.
         IF ( STATUS .NE. SAI__OK ) GO TO 999 

*  Obtain a reference to the NDF.
         CALL KPG1_AGREF( IPIC, 'READ', GOTNAM, REFNAM, STATUS )

*  See whether the reference is a name or locator.  The latter should be
*  phased out, but there may be some old databases and software
*  in circulation.
         CALL DAT_VALID( REFNAM, GOTLOC, STATUS )
         IF ( GOTLOC ) LOCI = REFNAM

*  End immediately if there an error.
         IF ( STATUS .NE. SAI__OK ) GO TO 999

*  "Interface" mode.
      ELSE IF ( INTERF ) THEN

*  If the initial co-ordinates are supplied on the command line we 
*  know that only one beam is to be fitted.
         CALL LPG_STATE( 'INIT1', STATE, STATUS )
         IF ( STATE .EQ. SUBPAR__ACTIVE ) THEN
            NPOS = 1
         END IF
      END IF

*  Obtain the NDF & WCS Frame
*  ==========================

*  Obtain the NDF.  If the name is given on the command line it will be 
*  used.  If not, the database data reference is used, if there is one. 
*  Otherwise, the user is prompted.
      CALL KPG1_ASREF( 'NDF', 'READ', GOTNAM, REFNAM, INDF, STATUS )

*  Check that there is variance present in the NDF.
      CALL NDF_STATE( INDF, 'Variance', HASVAR, STATUS )

*  If all input have variance components, see if input variances are to 
*  be used as weights.
      IF ( HASVAR ) THEN
         CALL PAR_GET0L( 'VARIANCE', VAR, STATUS )
      ELSE
         VAR = .FALSE.
      END IF      

*  We need to know how many significant axes there are (i.e. pixel axes
*  spanning more than a single pixel), and there must not be more than
*  two.
      CALL KPG1_SGDIM( INDF, BF__NDIM, SDIM, STATUS )

*  Now get the WCS FrameSet from the NDF.
      CALL KPG1_ASGET( INDF, BF__NDIM, .TRUE., .FALSE., .FALSE., SDIM, 
     :                 SLBND, SUBND, IWCS, STATUS )
      DO I = 1, BF__NDIM
         DIMS( I ) = SUBND( I ) - SLBND( I ) + 1
      END DO

*  In case the data have come from a cube with an insignificant axis
*  recording the co-ordinate of the plane from which the two-dimensional
*  array was derived, we want to extract a Current Frame that has no
*  insignificant axes.
      CALL KPG1_ASSIG( IWCS, BF__NDIM, SLBND, SUBND, STATUS )

*  Get a pointer to the possibly new Current Frame in the NDF.
      CFRM = AST_GETFRAME( IWCS, AST__CURRENT, STATUS )

*  Save the number of Current Frame axes.  This should be BF__NDIM.
*  Leave it parameterised in case the alloweddimensionality is 
*  extended.
      NAXC = AST_GETI( CFRM, 'NAXES', STATUS )

*  Get the Mapping from the Current Frame to PIXEL in the NDF.  First
*  find the index of the PIXEL Frame, and then get the Mapping.
      CALL KPG1_ASFFR( IWCS, 'PIXEL', IPIX, STATUS )
      MAP1 = AST_SIMPLIFY( AST_GETMAPPING( IWCS, IPIX, AST__CURRENT, 
     :                                     STATUS ), STATUS )

      IF ( STATUS .NE. SAI__OK ) GO TO 999

*  Obtain the AST Frame of input beam co-ordinates.
*  ================================================

*  We now get the AST Mapping from the Frame in which the positions are
*  supplied to the Current Frame of the NDF.
      IF ( CURSOR ) THEN

*  In cursor mode, the positions will be supplied in GRAPHICS 
*  co-ordinates (i.e.  millimetres from the bottom-left corner of the 
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
      ELSE IF ( CAT ) THEN
         CALL KPG1_ASMRG( IWCSG, IWCS, ' ', QUIET, 0, STATUS )

*  Get the Mapping.
         MAP2 = AST_SIMPLIFY( AST_GETMAPPING( IWCSG, AST__BASE, 
     :                                        AST__CURRENT, STATUS ),
     :                        STATUS )


*  In the other modes, the positions are supplied in the Current Frame.
      ELSE
         MAP2 = AST_UNITMAP( NAXC, ' ', STATUS )
      END IF

*  Find the Mapping from input Frame to PIXEL co-ordinates
*  =======================================================

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
      IF ( DESC .AND. .NOT. QUIET ) THEN
         CALL KPG1_DSFRM( CFRM, 'Positions will be reported in the '//
     :                    'following co-ordinate Frame:', .TRUE.,
     :                    STATUS )
      END IF

*  If we are in "File" mode, obtain the file and read the positions,
*  interpreting them as positions within the Current Frame of the NDF.
*  A pointer to memory holding the positions is returned.  Store a safe
*  value for the IPID pointer.  Identifiers are generated automatically 
*  in File mode instead of being read from the file, and so we do not
*  have a pointer to an array of identifiers at this point.
      IF ( FILE ) THEN
         CALL KPG1_ASFIL( 'COIN', ' ', CFRM, NPOS, IPIN, ' ', STATUS )
         IF ( NPOS .EQ. 0 ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BEAMFIT_EMPTYFILE', 'No data records of '//
     :                    'formatted positions found.', STATUS )
            GO TO 999
         END IF
         IPID = IPIN
      END IF

      IF ( FILE .OR. CAT ) THEN
         IF ( NPOS .GT. 1 ) THEN
            CALL MSG_SETI( 'NPOS', NPOS )
            CALL MSG_OUTIF( MSG__NORM, ' ', 
     :                      '  ^NPOS beam positions read', STATUS )
         ELSE
            CALL MSG_OUTIF( MSG__NORM, ' ', 
     :                      '  One beam position read', STATUS )
         END IF
      END IF

*  Obtain the number of beam positions if it's not been determined
*  already.
      IF ( NPOS .EQ. -1 ) CALL PAR_GDR0I( 'BEAMS', 1, 1, BF__MXPOS, 
     :                                    .FALSE., NPOS, STATUS )

*  Additional Parameters
*  =====================

*  Initialise in case there may be more.
      DO I = 1, MXCOEF
         FPAR( I ) = VAL__BADD
      END DO

*  We need to specify which axis to use for the widths and separations.
*  By convention this is the latitude axis of a SkyFrame or the first 
*  axis otherwise.
      IF ( AST_ISASKYFRAME( CFRM, STATUS ) ) THEN
         WAX = AST_GETI( CFRM, 'LATAXIS', STATUS )
      ELSE
         WAX = 1
      END IF

*  Fit area
*  --------
*  Obtain the fitting region sizes, duplicating the value if only a 
*  single value is given.
      FAREA = .FALSE.
      CALL PAR_GDRVI( 'FITAREA', BF__NDIM, 9, 99999, FITREG, NVAL, 
     :                STATUS )
      IF ( STATUS .EQ. SAI__OK .AND. NVAL .LT. BF__NDIM ) THEN
         DO I = NVAL + 1, BF__NDIM
            FITREG( I ) = FITREG( 1 )
         END DO
      ELSE IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         FAREA = .TRUE.
         DO I = 1, BF__NDIM
            FITREG( I ) = DIMS( I )
         END DO
      END IF

*  Constrain the search area to be no bigger than the image.
      DO  I = 1, BF__NDIM
         FITREG( I ) = MIN( DIMS( I ), FITREG( I ) )
      END DO

*  Is the amplitude fixed?
*  -----------------------
      CALL PAR_GDR0D( 'FIXAMP', 1.0D0, VAL__MIND, VAL__MAXD, .FALSE., 
     :                FPAR( 6 ), STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         FIXCON( 1 ) = .FALSE.
      ELSE
         FIXCON( 1 ) = .TRUE.
         IF ( NPOS .GT. 1 ) THEN
            DO I = 2, NPOS
               FPAR( 6 + ( I - 1 ) * BF__NCOEF ) = FPAR( 6 )
            END DO
         END IF
      END IF

*  Is the background fixed?
*  ------------------------
      CALL PAR_GDR0D( 'FIXBACK', 0.0D0, VAL__MIND, VAL__MAXD, .FALSE., 
     :                FPAR( 7 ), STATUS )

      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         FIXCON( 2 ) = .FALSE.
      ELSE
         FIXCON( 2 ) = .TRUE.
         IF ( NPOS .GT. 1 ) THEN
            DO I = 2, NPOS
               FPAR( 7 + ( I - 1 ) * BF__NCOEF ) = FPAR( 7 )
            END DO
         END IF
      END IF

*  Is the FWHM fixed?
*  ------------------
      S2FWHM = SQRT( 8.D0 * LOG( 2.D0 ) )

*  Obtain the FIXFWHM parameter value(s).  There is no dynamic default.
      FWHM( 1 ) = AST__BAD
      CALL KPG1_GTAXV( 'FIXFWHM', BF__NDIM, .FALSE., CFRM, WAX, FWHM,
     :                 NVAL, STATUS )
      
      IF ( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         FIXCON( 3 ) = .FALSE.
      ELSE

*  Assign the single (circular) fixed value.
         FIXCON( 3 ) = .TRUE.
         IF ( NVAL .EQ. 1 ) THEN
            DO I = 1, NPOS
               J = ( I - 1 ) * BF__NCOEF

*  Note we calculate the standard deviations of the Gaussian, so apply
*  the standard scaling.
               FPAR( 3 + J ) = FWHM( 1 ) / S2FWHM
               FPAR( 4 + J ) = FPAR( 3 + J )
               FPAR( 5 + J ) = 0.0D0
            END DO

*  We've been supplied with the major and minor axes.
         ELSE
            DO I = 1, NPOS
               J = ( I - 1 ) * BF__NCOEF

*  Note we calculate the standard deviations of the Gaussian, so apply
*  the standard scaling.
               FPAR( 3 + J ) = MAX( FWHM( 1 ), FWHM( 2 ) ) / S2FWHM
               FPAR( 4 + J ) = MIN( FWHM( 1 ), FWHM( 2 ) ) / S2FWHM
            END DO
         END IF
      END IF

*  Use the initial co-ordinates rather than fitting.
      CALL PAR_GET0L( 'FIXPOS', FIXCON( 4 ), STATUS )

*  Amplitude ratios
*  ----------------
      FIXCON( 5 ) = .FALSE.
      IF ( NPOS .GT. 1 ) THEN
         CALL PAR_GDRVR( 'AMPRATIO', NPOS - 1, -2.0, 2.0, AMPRAT, 
     :                   NAMP, STATUS )

         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            FIXCON( 5 ) = .TRUE.
            IF ( NAMP .LT. NPOS - 1 ) THEN
               DO I = NAMP, NPOS - 1
                  AMPRAT( I ) = AMPRAT( NAMP )
               END DO
            END IF
         END IF
      END IF

*  Are the separations fixed?
*  --------------------------
      FIXCON( 6 ) = .FALSE.
      IF ( NPOS .GT. 1 ) THEN
         I = 1
         DO WHILE ( MORE )

*  Obtain the SEPn parameter value(s).  There is no dynamic default.
            WRITE ( SEPAR, '(''SEP'',I1)' ) I
            OFF1( 1 ) = AST__BAD            
            CALL KPG1_GTPOS( SEPAR, IWCS, .FALSE., OFF1, BC, STATUS )
      
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )

*  All valid offsets must be supplied.  So after an error disregard
*  any earlier valid values.
               FIXCON( 6 ) = .FALSE.
               MORE = .FALSE.
            ELSE
               FIXCON( 6 ) = .TRUE.
               DO J = 1, BF__NDIM
                  OFFSET( I, J ) = OFF1( J )
               END DO
               I = I + 1
               MORE = I .LT. NPOS
            END IF
         END DO
      END IF

*  Record input data in the log file.
*  ==================================
      IF ( LOGF .AND. STATUS .EQ. SAI__OK ) THEN

*  NDF name
*  --------
*  Store the NDF name in the logfile, aligning with the rest of the
*  output.
         CALL NDF_MSG( 'NAME', INDF )
         CALL MSG_LOAD( 'DATASET', '    NDF             : ^NAME',
     :                  BUFOUT, NC, STATUS )
         CALL FIO_WRITE( FDL, BUFOUT( : NC ), STATUS )
      END IF

*  Display the header.
      CALL KPS1_BFHD( CFRM, LOGF, FDL, NAXC, TITLE, STATUS )
         
*  Fitting region
*  --------------
      IF ( LOGF ) THEN

*  Form string for the search areas.
         BUFOUT = '    Fitting area    : '
         NC = 22
         DO  J = 1, BF__NDIM
            CALL CHR_PUTI( FITREG( J ), BUFOUT, NC )
            IF ( J .LT. BF__NDIM ) CALL CHR_PUTC( ', ', BUFOUT, NC )
         END DO
         CALL CHR_PUTC( ' pixels',  BUFOUT, NC )
         CALL FIO_WRITE( FDL, BUFOUT( :NC ), STATUS )

      END IF

*  Do the fitting
*  ==============

*  Process all the supplied beams together as a single batch in
*  non-interactive modes.
      IF ( CAT .OR. FILE ) THEN

*  Find the beam parameters and determine errors, and report them.
         CALL KPS1_BFFIL( INDF, MAP3, MAP1, MAP2, CFRM, VAR, NPOS, 
     :                    NAXC, NAXIN, %VAL( CNF_PVAL( IPIN ) ), CAT, 
     :                    %VAL( CNF_PVAL( IPID ) ), LOGF, FDL, FIXCON,
     :                    AMPRAT, OFFSET, MXCOEF, FPAR, SLBND, SUBND, 
     :                    FAREA, FITREG, STATUS )

*  In interactive modes, find each beam individually, waiting for the
*  user to supply a new one before continuing each time.
      ELSE

*  Fit the beams obtained interactively, and determine errors. 
*  Display the results.
         CALL KPS1_BFINT( INDF, MAP3, MAP1, MAP2, CFRM, VAR, NPOS,
     :                    'INIT', CURSOR, MARK, IMARK, NAXC, NAXIN,
     :                    LOGF, FDL, FIXCON, AMPRAT, OFFSET, MXCOEF,
     :                    FPAR, SLBND, SUBND, FAREA, FITREG, STATUS )

      END IF

*  Tidy up.
*  ========
 999  CONTINUE

*  Return to previous reporting level.
      IF ( QUIET ) CALL MSG_IFSET( MSGFIL, STATUS )

*  Close any open files.
      IF ( LOGF ) CALL FIO_ANNUL( FDL, STATUS )

*  End the NDF context.
      CALL NDF_END( STATUS )

*  Release the dynamic arrays holding the input positions and 
*  identifiers in catalogue mode.
      IF ( CAT ) THEN
         CALL PSX_FREE( IPID, STATUS )
         CALL PSX_FREE( IPIN, STATUS )

*  Release the dynamic arrays holding the input positions and 
*  identifiers in file mode.
      ELSE IF ( FILE ) THEN
         CALL PSX_FREE( IPIN, STATUS )

*  Do cursor mode tidying...
      ELSE IF ( CURSOR ) THEN

*  Annul the locator to the reference object.
         IF ( GOTLOC ) CALL REF_ANNUL( LOCI, STATUS )
         CALL DAT_VALID( LOCI, GOTLOC, STATUS )
         IF ( GOTLOC ) CALL DAT_ANNUL( LOCI, STATUS )

*  Re-instate any changed PGPLOT marker attributes.
         IF ( MARK .NE. 'NONE' ) CALL KPG1_PGSTY( IPLOT, 'MARKERS', 
     :                                           .FALSE., ATTR, STATUS )

*  Close the graphics database and device.
         CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

      END IF

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'BEAMFIT_ERR', 'BEAMFIT: Failed to fit to '//
     :                 'the beams.', STATUS )
      END IF

      END
