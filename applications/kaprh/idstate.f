      SUBROUTINE IDSTATE( STATUS )
*+
*  Name:
*     IDSTATE

*  Purpose:
*     Shows the current status of a graphics device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IDSTATE( STATUS )

*  Usage:
*     idstate [device] [frame]

*  Description:
*     This application displays information about the current graphics
*     database picture on a graphics device, including the extreme axis
*     values in any requested co-ordinate Frame (see parameter FRAME).
*     Information is written to various output parameters for use by
*     other applications, and is also written to the screen by default
*     (see parameter REPORT). An outline may be drawn around the current
*     picture if required (see parameter OUTLINE).

*  ADAM Parameters:
*     COMMENT = LITERAL (Write)
*        The comment of the current picture.  Up to 132 characters
*        will be written.
*     DESCRIBE = _LOGICAL (Read)
*        If TRUE, a detailed description is displayed of the co-ordinate
*        Frame in which the picture bounds are reported (see parameter
*        FRAME). [current value]
*     DEVICE = DEVICE (Read)
*        Name of the graphics device about which information is
*        required. [Current graphics device]
*     DOMAIN = LITERAL (Write)
*        The Domain name of the current co-ordinate Frame for the current
*        picture.
*     EPOCH = _DOUBLE (Read)
*        If a "Sky Co-ordinate System" specification is supplied (using
*        parameter FRAME) for a celestial co-ordinate system, then an
*        epoch value is needed to qualify it. This is the epoch at
*        which the displayed sky co-ordinates were determined. It should
*        be given as a decimal years value, with or without decimal places
*        ("1996.8" for example). Such values are interpreted as a Besselian
*        epoch if less than 1984.0 and as a Julian epoch otherwise.
*     FRAME = LITERAL (Read)
*        A string determining the co-ordinate Frame in which the bounds
*        of the current picture are to be reported. When a picture is
*        created by an application such as PICDEF, DISPLAY, etc, WCS
*        information describing the available co-ordinate systems are stored
*        with the picture in the graphics database. This application can
*        report bounds in any of the co-ordinate Frames stored with the
*        current picture. The string supplied for FRAME can be one of the
*        following:
*
*        - A domain name such as SKY, AXIS, PIXEL, BASEPIC, CURPIC, etc. The
*        special domain AGI_WORLD is used to refer to the world co-ordinate
*        system stored in the AGI graphics database. This can be useful if
*        no WCS information was store with the picture when it was created.
*
*        - An integer value giving the index of the required Frame.
*
*        - A "Sky Co-ordinate System" (SCS) value such as EQUAT(J2000) (see
*        section "Sky Co-ordinate Systems" in SUN/95).
*
*        If a null value (!) is supplied, bounds are reported in the
*        co-ordinate Frame which was current when the picture was created.
*        [!]
*     LABEL = LITERAL (Write)
*        The label of the current picture.  It is blank if there is no
*        label.
*     NAME = LITERAL (Write)
*        The name of the current picture.
*     OUTLINE = _LOGICAL (Read)
*        If OUTLINE is TRUE, then an outline will be drawn around the
*        current picture to indicate its position. [FALSE]
*     REFNAM = LITERAL (Write)
*        The reference object associated with the current picture.  It
*        is blank if there is no reference object.  Up to 132 characters
*        will be written.
*     REPORT = _LOGICAL (Read)
*        If this is FALSE the state of the graphics device is not
*        reported, merely the results are written to the output
*        parameters.  It is intended for use within procedures. [TRUE]
*     STYLE = LITERAL (Read)
*        A group of attribute settings describing the plotting style to use
*        when drawing the outline (see parameter OUTLINE). The format
*        of the axis values reported on the screen may also be controlled.
*
*        A comma-separated list of strings should be given in which each
*        string is either an attribute setting, or the name of a text file
*        preceded by an up-arrow character "^". Such text files should
*        contain further comma-separated lists which will be read and
*        interpreted in the same manner. Attribute settings are applied in
*        the order in which they occur within the list, with later settings
*        over-riding any earlier settings given for the same attribute.
*
*        Each individual attribute setting should be of the form:
*
*           <name>=<value>
*
*        where <name> is the name of a plotting attribute, and <value> is
*        the value to assign to the attribute. Default values will be
*        used for any unspecified attributes. All attributes will be
*        defaulted if a null value (!) is supplied. See section "Plotting
*        Attributes" in SUN/95 for a description of the available
*        attributes. Any unrecognised attributes are ignored (no error is
*        reported).
*
*        The appearance of the outline is controlled by the attributes
*        Colour(Border), Width(Border), etc (the synonym "Outline" may be
*        used in place of "Border"). In addition, the following attributes
*        may be set in order to control the appearance of the formatted axis
*        values reported on the screen: Format, Digits, Symbol, Unit. These
*        may be suffixed with an axis number (e.g. "Digits(2)") to refer to
*        the values displayed for a specific axis. [current value]
*     X1 = LITERAL (Write)
*        The lowest value found within the current picture for axis 1 of the
*        requested co-ordinate Frame (see parameter FRAME).
*     X2 = LITERAL (Write)
*        The highest value found within the current picture for axis 1 of the
*        requested co-ordinate Frame (see parameter FRAME).
*     Y1 = LITERAL (Write)
*        The lowest value found within the current picture for axis 2 of the
*        requested co-ordinate Frame (see parameter FRAME).
*     Y2 = LITERAL (Write)
*        The highest value found within the current picture for axis 2 of the
*        requested co-ordinate Frame (see parameter FRAME).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     idstate
*        Shows the status of the current graphics device. The bounds of
*        the picture are displayed in the current co-ordinate Frame of
*        the picture.
*     idstate ps_l basepic
*        Shows the status of the ps_l device. The bounds of the picture
*        are displayed in the BASEPIC Frame (normalised device co-ordinates
*        in which the short of the two dimensions of the display surface
*        has length 1.0).
*     idstate outline frame=pixel style="'colour=red,width=3'"
*        Shows the status of the current graphics device and draws a
*        thick, red outline around the current database picture. The
*        bounds of the picture are displayed in the PIXEL co-ordinate
*        Frame (if available).
*     idstate refnam=(ndfname)
*        Shows the status of the current graphics device.  If there
*        is a reference data object, its name is written to the ICL
*        variable NDFNAME.
*     idstate x1=(x1) x2=(x2) y1=(y1) y2=(y2) frame=basepic
*        Shows the status of the current graphics device.  The bounds
*        of the current picture in normalised device co-ordinates
*        are written to the ICL variables: X1, X2, Y1, Y2.

*  Notes:
*     -  The displayed bounds are the extreme axis values found anywhere
*     within the current picture. In some situations these extreme
*     values may not occur on the edges of the picture. For instance, if
*     the current picture represents a region including the north
*     celestial pole, then displaying the picture bounds in celestial
*     co-ordinates will give a declination upper limit of +90 degrees,
*     whilst the RA limits will be 0 hours and (close to) 24 hours.
*     -  Previous versions of this application reported bounds in
*     "Normalised Device Co-ordinates". Similar functionality is now
*     provided by setting parameter FRAME to "BASEPIC". Be aware though,
*     that "Normalised Device Co-ordinates" were normalised so that the
*     longer of the two axes had a length of 1.0, but BASEPIC co-ordinates
*     are normalised so that the shorter of the two axes has length 1.0.

*  Related Applications:
*     KAPPA: IDSTATE.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-DEC-1989 (RFWS):
*        Original version.
*     13-DEC-1989 (RFWS):
*        Added call to MSG_SYNC to synchonise message system and
*        graphical output.
*     1990 Jan 16 (MJC):
*        Added output of the current picture's label, if it exists.
*     1991 February 8 (MJC):
*        Added output of the current picture's reference object, if it
*        exists.
*     1991 March 24 (MJC):
*        Converted to SST prologue.
*     1991 August 20 (MJC):
*        Added output parameters and REPORT parameter.
*     7-JAN-2000 (DSB):
*        Big changes for the first AST/PGPLOT version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'AST_PAR'          ! GNS constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER ATTR*10          ! Buffer for attribute name
      CHARACTER COM*80           ! Picture comment
      CHARACTER DOM0*30          ! Original Current Frame Domain
      CHARACTER LABEL*( DAT__SZNAM ) ! Picture label
      CHARACTER LFMT*80          ! Buffer for formatted lower axis value
      CHARACTER NAME*( DAT__SZNAM ) ! Picture name
      CHARACTER REFNAM*132       ! Reference object's name
      CHARACTER SYM*30           ! Buffer for an axis symbol string
      CHARACTER TEXT*256         ! Buffer for a line of output text
      CHARACTER UFMT*80          ! Buffer for formatted upper axis value
      DOUBLE PRECISION GLBND     ! Lower axis bound in requested Frame
      DOUBLE PRECISION GUBND     ! Upper axis bound in requested Frame
      DOUBLE PRECISION LBNDG( 2 )! Lower bounds of picture in GRAPHICS Frame
      DOUBLE PRECISION UBNDG( 2 )! Upper bounds of picture in GRAPHICS Frame
      DOUBLE PRECISION XL        ! Position of low bound in GRAPHICS Frame
      DOUBLE PRECISION XU        ! Position of high bound in GRAPHICS Frame
      INTEGER IAT                ! Used length of TEXT string
      INTEGER IAXIS              ! Axis index
      INTEGER ICURR              ! Index of original current Frame
      INTEGER IPIC               ! AGI current picture ID
      INTEGER IPICB              ! AGI BASE picture ID
      INTEGER IPLOT              ! Pointer to picture's AST Plot
      INTEGER JAT                ! Used length of ATTR string
      INTEGER MAP                ! Pointer to Mapping from Base to Current
      INTEGER NCREF              ! Number of characters in reference
      LOGICAL DESC               ! Give description of requested Frame?
      LOGICAL OUTLIN             ! Draw an outline around the current picture?
      LOGICAL REFOBJ             ! Is there a reference object?
      LOGICAL REPORT             ! Are the results to be reported?
      LOGICAL USECUR             ! Use original Current Frame?
      LOGICAL VALID              ! Is the reference object a locator?
      REAL X1                    ! Lower picture X bound
      REAL X2                    ! Upper picture X bound
      REAL Y1                    ! Lower picture Y bound
      REAL Y2                    ! Upper picture Y bound
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Begin an AST context.
      CALL AST_BEGIN( STATUS )

*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC, STATUS )

*  Get the Plot associated with the current picture. If no Plot is stored
*  with the picture (i.e. if it was created by a non-AST based
*  application), then a default Plot will be created containing a GRAPHICS
*  Frame, a Frame representing AGI world co-ordinates, and BASEPIC and
*  CURPIC Frames representing normalised co-ordinates in the BASE and
*  current pictures. The world co-ordinates in the PGPLOT window is set to
*  millimetres from the bottom left corner of the view surface, which
*  corresponds to the Base (GRAPHICS) Frame in the returned Plot.
      CALL KPG1_GDGET( IPIC, AST__NULL, .FALSE., IPLOT, STATUS )

*  Save the index of the Current Frame in the Plot.
      ICURR = AST_GETI( IPLOT, 'CURRENT', STATUS )

*  Get the Domain of the Current Frame.
      DOM0 = AST_GETC( IPLOT, 'DOMAIN', STATUS )

*  Select the Frame to be reported.
      CALL MSG_SETC( 'OBJ', 'current graphics database picture' )
      CALL KPG1_ASFRM( 'FRAME', 'EPOCH', IPLOT, ' ', ' ', .TRUE.,
     :                 '^OBJ', STATUS )

*  Establish synonyms for AST graphical element names to be recognised
*  during the following call to KPG1_ASSET.
      CALL KPG1_ASPSY( '(OUT*LINE)', '(BORDER)', STATUS )

*  Set the plotting and formatting attributes.
      CALL KPG1_ASSET( 'KAPPA_IDSTATE', 'STYLE', IPLOT, STATUS )

*  Set a flag indicating whether the required Frame is the original
*  Current Frame.
      USECUR = AST_GETI( IPLOT, 'CURRENT', STATUS ) .EQ. ICURR

*  Get the bounds of the current PGPLOT window (i.e. the current picture),
*  and store double precision equivalents. These values are in GRAPHICS
*  co-ordinates (i.e. mm from the bottom left corner of the display surface).
      CALL PGQWIN( X1, X2, Y1, Y2 )
      LBNDG( 1 ) = DBLE( X1 )
      UBNDG( 1 ) = DBLE( X2 )
      LBNDG( 2 ) = DBLE( Y1 )
      UBNDG( 2 ) = DBLE( Y2 )

*  See if the current AGI picture is to be outlined for identification.
      OUTLIN = .FALSE.
      CALL PAR_GET0L( 'OUTLINE', OUTLIN, STATUS )

*  If required, draw the outline. Make it a bit smaller than the picture
*  to avoid PGPLOT clipping.
      IF( OUTLIN ) THEN

*  Get an AGI identifier for the BASE picture.
         CALL AGI_IBASE( IPICB, STATUS )

*  Draw the outline, temporarily making the BASE picture current to avoid
*  PGPLOT clipping.
         CALL KPG1_ASGRD( IPLOT, IPICB, .FALSE., STATUS )

*  Annul the AGI identifier for the BASE picture.
         CALL AGI_ANNUL( IPICB, STATUS )

      END IF

*  See whether reporting is required or not.
      CALL PAR_GET0L( 'REPORT', REPORT, STATUS )

*  Start the display with a blank line, followed by the name of the device.
      IF ( REPORT ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'IDSTATE_MSG1', 'Status of the $DEVICE '//
     :                 'graphics device...', STATUS )

      END IF

*  Obtain the current AGI picture name and display it.  Write it to an
*  output parameter.
      CALL AGI_INAME( NAME, STATUS )
      IF ( REPORT ) THEN
         CALL MSG_BLANK( STATUS )
         CALL MSG_SETC( 'PNAME', NAME )
         CALL MSG_OUT( 'IDSTATE_MSG2', '   The current picture is a '//
     :                 '^PNAME picture.', STATUS )
      END IF
      CALL PAR_PUT0C( 'NAME', NAME, STATUS )

*  Obtain the comment string associated with the picture and display it.
*  Write it to an output parameter.
      CALL AGI_ICOM( COM, STATUS )
      IF ( REPORT ) THEN
         CALL MSG_SETC( 'COM', COM )
         CALL MSG_OUT( 'IDSTATE_MSG3', '   Comment: ^COM', STATUS )
      END IF
      CALL PAR_PUT0C( 'COMMENT', COM, STATUS )

*  Obtain the label associated with the picture, if a label exists.
*  Write it to an output parameter.  A dummy is required if it doesn't
*  so that the old value is overwritten.
      CALL AGI_ILAB( IPIC, LABEL, STATUS )
      IF ( LABEL( 1:1 ) .NE. ' ' ) THEN
         IF ( REPORT ) THEN
            CALL MSG_SETC( 'PLABEL', LABEL )
            CALL MSG_OUT( 'IDSTATE_MSG4', '   Label: ^PLABEL', STATUS )
         END IF
         CALL PAR_PUT0C( 'LABEL', LABEL, STATUS )
      ELSE
         CALL PAR_PUT0C( 'LABEL', ' ', STATUS )
      END IF

*  Determine whether or not there is a reference object associated with
*  the current picture.
      CALL KPG1_AGREF( IPIC, 'READ', REFOBJ, REFNAM, STATUS )

*  If one exists translate its locator to a token containing the path
*  name and file name, and tidy the reference locator; or if the
*  reference is just a name, write it to a token.  Write a message
*  containing the reference object. Note that the token is renewed if
*  it is to be used twice.
      IF ( REFOBJ ) THEN
         CALL DAT_VALID( REFNAM( :DAT__SZLOC ), VALID, STATUS )
         IF ( VALID ) THEN
            CALL KPG1_HMSG( 'RNAME', REFNAM( :DAT__SZLOC ) )
            CALL REF_ANNUL( REFNAM( :DAT__SZLOC ), STATUS )
            CALL MSG_LOAD( 'REFNAME', '^RNAME', REFNAM, NCREF, STATUS )
         ELSE
            CALL MSG_SETC( 'RNAME', REFNAM )
         END IF

         IF ( REPORT ) THEN
            CALL MSG_RENEW
            CALL MSG_OUT( 'GDTSTATE_MSG5', '   Reference data '//
     :                    'object: ^RNAME', STATUS )
         END IF

*  Write the reference name to a parameter, using a dummy if there is
*  no object so that a previous name associated with another picture is
*  overwritten.
         CALL PAR_PUT0C( 'REFNAM', REFNAM, STATUS )
      ELSE
         CALL PAR_PUT0C( 'REFNAM', ' ', STATUS )
      END IF

*  If required, report a description of the Frame being used.
      IF( REPORT ) THEN

*  See if a description of the Frame being used is required.
         CALL PAR_GET0L( 'DESCRIBE', DESC, STATUS )

*  Display the Domain of the original Current Frame unless this will be
*  done below.
         IF( .NOT. ( DESC .AND. USECUR ) ) THEN
            CALL MSG_SETC( 'DOM', DOM0 )
            CALL MSG_OUT( 'IDSTATE_MSG6', '   Current co-ordinate '//
     :                    'Frame: ^DOM', STATUS )
         END IF

*  If so, display it, indicating if this is the pictures current Frame or
*  not.
         IF( DESC ) THEN
            IF( USECUR ) THEN
               CALL KPG1_DSFRM( IPLOT, '   Current co-ordinate Frame:',
     :                          STATUS )
            ELSE
               CALL KPG1_DSFRM( IPLOT, '   Requested co-ordinate '//
     :                          'Frame:', STATUS )
            END IF

         ELSE
            CALL MSG_BLANK( STATUS )
         END IF

      END IF

*  Write the Domain of the Current Frame to an output parameter.
      CALL PAR_PUT0C( 'DOMAIN', DOM0, STATUS )

*  Get the Mapping from the Base (GRAPHICS) Frame in the Plot to the
*  current (requested) Frame.
      MAP = AST_SIMPLIFY( AST_GETMAPPING( IPLOT, AST__BASE,
     :                                    AST__CURRENT, STATUS ),
     :                    STATUS )

*  Give a heading for the axis bounds.
      IF( REPORT ) THEN
         CALL MSG_SETC( 'DOM', AST_GETC( IPLOT, 'DOMAIN', STATUS ) )
         CALL MSG_OUT( 'IDSTATE_MSG7', '   Picture bounds in the '//
     :                 '^DOM Frame:', STATUS )
      END IF

*  Loop round each axis in the requested Frame.
      DO IAXIS = 1, AST_GETI( IPLOT, 'NAXES', STATUS )

*  Use the Mapping to determine the bounds of the PGPLOT window
*  along this axis of the requested Frame.
         CALL AST_MAPBOX( MAP, LBNDG, UBNDG, .TRUE., IAXIS, GLBND,
     :                    GUBND, XL, XU, STATUS )

*  Construct a string holding the axis number.
         TEXT = '      Axis '
         IAT = 11
         CALL CHR_PUTI( IAXIS, TEXT, IAT )

*  Construct a string holding the name of the Symbol attribute for this
*  axis.
         ATTR = 'SYMBOL('
         JAT = 7
         CALL CHR_PUTI( IAXIS, ATTR, JAT )
         CALL CHR_APPND( ')', ATTR, JAT )

*  Get the symbol string.
         SYM = AST_GETC( IPLOT, ATTR( : JAT ), STATUS)

*  Remove any PGPLOT escape sequences.
         CALL KPG1_PGESC( SYM, STATUS )

*  Add the axis symbol in parenthesise to the output text if not blank.
         IF( SYM .NE. ' ' ) THEN
            CALL CHR_APPND( ' (', TEXT, IAT )
            CALL CHR_APPND( SYM, TEXT, IAT )
            CALL CHR_APPND( ') :', TEXT, IAT )
         ELSE
            CALL CHR_APPND( ' :', TEXT, IAT )
         END IF

         IAT = IAT + 1

*  Format and append the lower bound value.
         LFMT = AST_FORMAT( IPLOT, IAXIS, GLBND, STATUS )
         CALL CHR_APPND( LFMT, TEXT, IAT )

*  Add a delimiter string
         CALL CHR_APPND( ' to', TEXT, IAT )
         IAT = IAT + 1

*  Format and append the upper bound value.
         UFMT = AST_FORMAT( IPLOT, IAXIS, GUBND, STATUS )
         CALL CHR_APPND( UFMT, TEXT, IAT )

*  Display the text for this axis, if required.
         IF( REPORT ) CALL MSG_OUT( 'IDSTATE_MSG8', TEXT( : IAT ),
     :                              STATUS )

*  Store the first two axis values in the output parameters.
         IF( IAXIS .EQ. 1 ) THEN
            CALL PAR_PUT0C( 'X1', LFMT, STATUS )
            CALL PAR_PUT0C( 'X2', UFMT, STATUS )

         ELSE IF( IAXIS .EQ. 2 ) THEN
            CALL PAR_PUT0C( 'Y1', LFMT, STATUS )
            CALL PAR_PUT0C( 'Y2', UFMT, STATUS )

         END IF

      END DO

*  Add a blank line to the report.
      IF( REPORT ) CALL MSG_BLANK( STATUS )

*  Tidy up.
 999  CONTINUE

*  Clear the AST attribute synonyms.
      CALL KPG1_ASPSY( ' ', ' ', STATUS )

*  Close the graphics database and device.
      CALL KPG1_PGCLS( 'DEVICE', .FALSE., STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IDSTATE_ERR', 'IDSTATE: Failed to display '//
     :                 'information about the current graphics '//
     :                 'database picture.', STATUS )
      END IF

      END
