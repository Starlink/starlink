      SUBROUTINE IDSTATE( STATUS )
*+
*  Name:
*     IDSTATE

*  Purpose:
*     Shows the current status of an image display.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL IDSTATE( STATUS )

*  Usage:
*     idstate [device]

*  Description:
*     This application displays the current status of an image-display
*     device, including details of the current graphics-database
*     picture (e.g. its co-ordinate system and position on the display
*     surface), and the reserved colour table.

*  ADAM Parameters:
*     COMMENT = LITERAL (Write)
*        The comment of the current picture.  Up to 132 characters
*        will be written.
*     DEVICE = DEVICE (Read)
*        Name of the image-display device about which information is
*        required. [Current image-display device]
*     LABEL = LITERAL (Write)
*        The label of the current picture.  It is blank if there is no
*        label.
*     NAME = LITERAL (Write)
*        The name of the current picture.
*     NCX1 = _REAL (Write)
*        The lower x normalised device co-ordinate of the current
*        picture.
*     NCX2 = _REAL (Write)
*        The upper x normalised device co-ordinate of the current
*        picture.
*     NCY1 = _REAL (Write)
*        The lower y normalised device co-ordinate of the current
*        picture.
*     NCY2 = _REAL (Write)
*        The upper y normalised device co-ordinate of the current
*        picture.
*     OUTLINE = _LOGICAL (Read)
*        If OUTLINE is TRUE, then an outline will be drawn around the
*        current picture to indicate its position. [FALSE]
*     REFNAM = LITERAL (Write)
*        The reference object associated with the current picture.  It
*        is blank if there is no reference object.  Up to 132 characters
*        will be written.
*     REPORT = _LOGICAL (Read)
*        If this is FALSE the state of the image-display device is not
*        reported, merely the results are written to the output
*        parameters.  It is intended for use within procedures. [TRUE]
*     WCX1 = _REAL (Write)
*        The lower x world co-ordinate of the current picture.
*     WCX2 = _REAL (Write)
*        The upper x world co-ordinate of the current picture.
*     WCY1 = _REAL (Write)
*        The lower y world co-ordinate of the current picture.
*     WCY2 = _REAL (Write)
*        The upper y world co-ordinate of the current picture.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Examples:
*     idstate
*        Shows the status of the current image-display device.
*     idstate xwindows
*        Shows the status of the xwindows device.
*     idstate outline
*        Shows the status of the current image-display device and draws
*        an outline around the current database picture.
*     idstate refnam=(ndfname)
*        Shows the status of the current image-display device.  If there
*        is a reference data object, its name is written to the ICL
*        variable NDFNAME.
*     gdstate wcx1=(x1) wcx2=(x2) wcy1=(y1) wcy2=(y2)
*        Shows the status of the current image-display device.  The
*        bounds of the current picture in world co-ordinates are
*        written to the ICL variables: X1, X2, Y1, Y2.

*  Notes:
*     -  If a channel to the image-display device cannot be opened, then
*     this application will still execute without error, but a reduced
*     amount of information will be displayed and an outline around the
*     current picture (if requested) will not be drawn.

*  Algorithm:
*     -  See if the current picture is to be outlined.
*     -  Obtain an AGI identifier for the current picture on the
*     image-display device, using the appropriate access mode.
*     -  Activate the SGS plotting package.
*     -  Obtain an SGS zone for the current picture. If this fails,
*     then a channel to the image-display device cannot be opened. Note
*     this fact, but carry on regardless.
*     -  If the device is open, then enquire the GKS workstation ID for
*     the current SGS zone.
*     -  Open GNS for use with GKS and enquire the class and physical
*     device name of the workstation. Close GNS.
*     -  Display the image-display device name and class.
*     -  If the device is not open, simply display its name.
*     -  Display the physical device name, if available.
*     -  Obtain the name of the current AGI picture and display it.
*     -  Similarly display the current picture's comment string and
*     label if it has one.
*     -  Obtain the picture's world co-ordinates from AGI and display
*     them.
*     -  If the device is open, then enquire the current normalisation
*     transformation number from GKS, handling any errors.
*     -  Enquire the GKS viewport and window bounds for the
*     normalisation transformation, thereby giving the normalised
*     device co-ordinates (NDC) of the current AGI picture. Handle any
*     errors.
*     -  Display the NDC values.
*     -  If the current picture is to be outlined, then use SGS to draw
*     a box around it.
*     -  Deactivate SGS.
*     -  Annul the AGI picture.
*     -  If an error occurred, then report context information.

*  Related Applications:
*     KAPPA: GDSTATE.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 July 10 (RFWS and MJC):
*        Original version based on GDSTATE.
*     1991 August 20 (MJC):
*        Added output parameters and REPORT parameter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'GNS_PAR'          ! GNS constants
      INCLUDE 'CTM_PAR'          ! Colour-table management constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZCOM              ! Length of picture comment
      PARAMETER ( SZCOM = 80 )

*  Local Variables:
      CHARACTER * ( 80 ) BUFFER  ! Buffer for creating table of palette
                                 ! colours
      CHARACTER * ( GNS__SZDEV ) DEV ! Device name
      CHARACTER * ( GNS__SZKEY ) CLASS ! GNS workstation class
      CHARACTER * ( SZCOM ) COM  ! Picture comment
      CHARACTER * ( DAT__SZNAM ) LABEL ! Picture label
      CHARACTER * ( DAT__SZNAM ) NAME ! Picture name
      CHARACTER * ( 18 ) PALCOL( 0:CTM__RSVPN - 1 ) ! Palette colours
      CHARACTER * ( 132 ) REFNAM ! Reference object's name 
      INTEGER CTNR               ! GKS current normalisation trans. no.
      INTEGER GSTAT              ! GKS error value
      INTEGER IPIC               ! AGI picture ID
      INTEGER IPIXX              ! Maximum number of columns of pixels
                                 ! of the image display (not used)
      INTEGER IPIXY              ! Maximum number of lines of pixels
                                 ! of the image display (not used)
      INTEGER IWRK               ! SGS current GKS workstation ID
      INTEGER IZON               ! SGS zone ID
      INTEGER J                  ! Loop counter
      INTEGER K                  ! Loop counter
      INTEGER LDEV               ! Length of device name
      INTEGER NCREF              ! Number of characters in reference
      INTEGER NINTS              ! Total number of greyscale intensities
                                 ! available on the image display
      LOGICAL OPN                ! Whether the device can be opened
      LOGICAL OUTLIN             ! Whether to outline current picture
      LOGICAL REFOBJ             ! Whether there is a reference object
      LOGICAL REPORT             ! Whether the results are to be
                                 ! reported
      REAL PALETT( 3 )           ! Palette RGB intensities
      LOGICAL VALID              ! Whether reference object is a locator
      REAL VIEWPT( 4 )           ! GKS viewport bounds in NDC
      REAL WINDOW( 4 )           ! GKS window bounds in world co-ords
      REAL X1                    ! Lower picture X bound
      REAL X2                    ! Upper picture X bound
      REAL Y1                    ! Lower picture Y bound
      REAL Y2                    ! Upper picture Y bound

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the current AGI picture is to be outlined for identification.
      OUTLIN = .FALSE.
      CALL PAR_GET0L( 'OUTLINE', OUTLIN, STATUS )

*  See whether reporting is required or not.
      CALL PAR_GET0L( 'REPORT', REPORT, STATUS )

*  Start an AGI scope.
      CALL AGI_BEGIN

*  Obtain an identifier for the current AGI picture on the image-display
*  device for which information is required. Use the appropriate access
*  mode.
      IF ( OUTLIN ) THEN
         CALL AGI_ASSOC( 'DEVICE', 'UPDATE', IPIC, STATUS )
      ELSE
         CALL AGI_ASSOC( 'DEVICE', 'READ', IPIC, STATUS )
      END IF

*  Activate the SGS plotting package for use with AGI.
      CALL AGS_ACTIV( STATUS )
      OPN = .FALSE.
      IF ( STATUS .EQ. SAI__OK ) THEN

*  Obtain an SGS zone for the current picture. Note if the workstation
*  cannot be opened, but carry on regardless.
         CALL ERR_MARK
         CALL AGS_NZONE( IZON, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            OPN = .TRUE.
         END IF
         CALL ERR_RLSE
      END IF

*  Start the display with a blank line.
      IF ( REPORT ) CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  If the device is open, then enquire the GKS workstation ID for the
*  current SGS zone.
      IF ( STATUS .EQ. SAI__OK ) THEN
         IF ( OPN ) THEN
            CALL SGS_ICURW( IWRK )

*  Open GNS for use with GKS and enquire the class and physical device
*  name of the workstation. Then close GNS.
            CALL GNS_START( 'GKS', STATUS )
            CALL GNS_IWCG( IWRK, 'CLASS', CLASS, STATUS )
            LDEV = 0
            CALL GNS_IDNG( IWRK, DEV, LDEV, STATUS )
            CALL GNS_STOP( 'GKS', STATUS )

*  Display the image-display device name and class.
            CALL CHR_LCASE( CLASS )
            IF ( REPORT ) THEN
               CALL MSG_SETC( 'CLASS', CLASS )
               CALL MSG_OUT( 'HEADER',
     :          'Status of the $DEVICE ^CLASS image-display device...',
     :          STATUS )
            END IF

*  If the device is not open, then just display its name.
         ELSE IF ( REPORT ) THEN
            CALL MSG_OUT( 'HEADER_1',
     :       'Status of the $DEVICE image-display device...', STATUS )
         END IF
      END IF

*  Display the physical device name, if available.
      IF ( OPN .AND. ( LDEV .GT. 0 ) .AND. REPORT ) THEN
         CALL MSG_SETC( 'DEV', DEV( : LDEV ) )
         CALL MSG_OUT( 'DEV', '   Physical device: ^DEV', STATUS )
      END IF

*  Display the number of reserved palette colour indices.
      IF ( REPORT ) THEN
         CALL MSG_SETI( 'RESPEN', CTM__RSVPN )
         CALL MSG_OUT( 'PALIND', '   Total palette colours: ^RESPEN',
     :                 STATUS )
      END IF

      IF ( OPN ) THEN

*  Obtain the number of colour indices and the maximum display
*  surface.
         CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

*  Display the number of unreserved colour indices.
         IF ( REPORT ) THEN
            CALL MSG_SETI( 'CTPEN', NINTS - CTM__RSVPN )
            CALL MSG_OUT( 'CTIND', '   Number of non-palette colour '/
     :        /'indices: ^CTPEN', STATUS )
         END IF

*  Inquire the realised palette colour indices.  See if within GKS an
*  error has occurred.  Find the names of the palette colours, or
*  nearest equivalents.

         IF ( REPORT ) THEN
            DO  J = 0, CTM__RSVPN - 1
               CALL GQCR( IWRK, J, 1, GSTAT, PALETT( 1 ),
     :                    PALETT( 2 ), PALETT( 3 ) )
               CALL GKS_GSTAT( STATUS )
               CALL KPG1_COLNM( PALETT( 1 ), PALETT( 2 ),
     :                          PALETT( 3 ), PALCOL( J ), STATUS )
            END DO

*  Display the named colours, neatly formatted three per line.
            DO  J = 0, CTM__RSVPN - 1, 3
               WRITE( BUFFER, '('' '',3(I5,'': '',A18))' )
     :           ( K, PALCOL( K ), K = J, MIN( CTM__RSVPN - 1, J + 2 ) )
               CALL MSG_SETC( 'PALETTE', BUFFER )
               CALL MSG_OUT( 'PALCOLS', '^PALETTE', STATUS )
            END DO
         END IF
      END IF

*  Obtain the current AGI picture name and display it.  Write it to an
*  output parameter.
      CALL AGI_INAME( NAME, STATUS )
      IF ( REPORT ) THEN
         CALL MSG_OUT( 'BLANK', ' ', STATUS )
         CALL MSG_SETC( 'PNAME', NAME )
         CALL MSG_OUT( 'PICNAME',
     :     '   The current picture is a ^PNAME picture.', STATUS )
      END IF
      CALL PAR_PUT0C( 'NAME', NAME, STATUS )

*  Obtain the comment string associated with the picture and display it.
*  Write it to an output parameter.
      CALL AGI_ICOM( COM, STATUS )
      IF ( REPORT ) THEN
         CALL MSG_SETC( 'COM', COM )
         CALL MSG_OUT( 'PICCOMMENT',
     :     '   Comment: ^COM', STATUS )
      END IF
      CALL PAR_PUT0C( 'COMMENT', COM, STATUS )

*  Obtain the label associated with the picture, if a label exists.
*  Write it to an output parameter.  A dummy is required if it doesn't
*  so that the old value is overwritten.
      CALL AGI_ILAB( IPIC, LABEL, STATUS )
      IF ( LABEL( 1:1 ) .NE. ' ' ) THEN
         IF ( REPORT ) THEN
            CALL MSG_SETC( 'PLABEL', LABEL )
            CALL MSG_OUT( 'PICLABEL',
     :       '   Label: ^PLABEL', STATUS )
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
            CALL MSG_OUT( 'REFNAME', '   Reference data object: ^RNAME',
     :                    STATUS )
         END IF

*  Write the reference name to a parameter, using a dummy if there is
*  no object so that a previous name associated with another picture is
*  overwritten.
         CALL PAR_PUT0C( 'REFNAM', REFNAM, STATUS )
      ELSE
         CALL PAR_PUT0C( 'REFNAM', ' ', STATUS )
      END IF

*  Obtain the picture's world co-ordinates and display them.
      CALL AGI_IWOCO( X1, X2, Y1, Y2, STATUS )
      IF ( REPORT ) THEN
         CALL MSG_OUT( 'WORLD', '   World co-ordinates:', STATUS )
         CALL MSG_SETR( 'X1', X1 )
         CALL MSG_SETR( 'X2', X2 )
         CALL MSG_OUT( 'WORLD_X',
     :     '               X = ^X1 to ^X2', STATUS )
         CALL MSG_SETR( 'Y1', Y1 )
         CALL MSG_SETR( 'Y2', Y2 )
         CALL MSG_OUT( 'WORLD_Y',
     :     '               Y = ^Y1 to ^Y2', STATUS )
      END IF

*  Write the results to output parameters.
      CALL PAR_PUT0R( 'WCX1', X1, STATUS )
      CALL PAR_PUT0R( 'WCX2', X2, STATUS )
      CALL PAR_PUT0R( 'WCY1', Y1, STATUS )
      CALL PAR_PUT0R( 'WCY2', Y2, STATUS )

*  If the device is open, enquire the current GKS normalisation
*  transformation number. Report an error if necessary.
      IF ( OPN ) THEN
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL GQCNTN( GSTAT, CTNR )
            CALL GKS_GSTAT( STATUS )
         END IF

*  Enquire the GKS window and viewport bounds for this transformation.
*  The viewport then gives the bounds of the current SGS zone (and
*  hence the AGI picture) in normalised device units. Report an error
*  if necessary.
         IF ( STATUS .EQ. SAI__OK ) THEN
            CALL GQNT( CTNR, GSTAT, WINDOW, VIEWPT )
            CALL GKS_GSTAT( STATUS )
         END IF

*  Display the NDC bounds of the current picture.
         IF ( REPORT ) THEN
            CALL MSG_OUT( 'NDC', '   Normalised device co-ordinates:',
     :                    STATUS )
            CALL MSG_SETR( 'X1', VIEWPT( 1 ) )
            CALL MSG_SETR( 'X2', VIEWPT( 2 ) )
            CALL MSG_OUT( 'NDC_X',
     :        '               X = ^X1 to ^X2', STATUS )
            CALL MSG_SETR( 'Y1', VIEWPT( 3 ) )
            CALL MSG_SETR( 'Y2', VIEWPT( 4 ) )
            CALL MSG_OUT( 'NDC_Y',
     :        '               Y = ^Y1 to ^Y2', STATUS )
         END IF

*  Write the results to output parameters.
         CALL PAR_PUT0R( 'NCX1', VIEWPT( 1 ), STATUS )
         CALL PAR_PUT0R( 'NCX2', VIEWPT( 2 ), STATUS )
         CALL PAR_PUT0R( 'NCY1', VIEWPT( 3 ), STATUS )
         CALL PAR_PUT0R( 'NCY2', VIEWPT( 4 ), STATUS )

*  If the picture is to be outlined, then draw a box around it.
         IF ( ( STATUS .EQ. SAI__OK ) .AND. OUTLIN ) THEN
            CALL MSG_SYNC( STATUS )
            CALL SGS_BOX( X1, X2, Y1, Y2 )
         END IF
      END IF
      IF ( REPORT ) CALL MSG_OUT( 'BLANK', ' ', STATUS )

*  Deactivate SGS (which also releases the current SGS zone ID if the
*  device is open).
      CALL AGS_DEACT( STATUS )

*  Annul the current AGI picture ID.
      CALL AGI_ANNUL( IPIC, STATUS )

*  End the AGI scope.
      CALL AGI_END( -1, STATUS )

*  If an error occurred, then report context information.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'IDSTATE_ERR', 'IDSTATE: Error showing ' //
     :   'current state of image-display device.', STATUS )
      END IF

      END
