      SUBROUTINE GDSTATE( STATUS )
*+
*  Name:
*     GDSTATE

*  Purpose:
*     Shows the current status of a graphics device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL GDSTATE( STATUS )

*  Usage:
*     gdstate [device]

*  Description:
*     This application displays the current status of a graphics
*     device, including details of the current graphics-database
*     picture (e.g. its co-ordinate system and position on the display
*     surface).

*  ADAM Parameters:
*     COMMENT = LITERAL (Write)
*        The comment of the current picture.  Up to 132 characters
*        will be written.
*     DEVICE = DEVICE (Read)
*        Name of the graphics device about which information is
*        required. [Current graphics device]
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
*        If this is FALSE the state of the graphics device is not
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
*     gdstate
*        Shows the status of the current graphics device.
*     gdstate ps_l
*        Shows the status of the ps_l device.
*     gdstate outline
*        Shows the status of the current graphics device and draws an
*        outline around the current database picture.
*     gdstate refnam=(ndfname)
*        Shows the status of the current graphics device.  If there
*        is a reference data object, its name is written to the ICL
*        variable NDFNAME.
*     gdstate ncx1=(x1) ncx2=(x2) ncy1=(y1) ncy2=(y2)
*        Shows the status of the current graphics device.  The bounds
*        of the current picture in normalised device co-ordinates
*        are written to the ICL variables: X1, X2, Y1, Y2.

*  Notes:
*     -  If a channel to the graphics device cannot be opened, then
*     this application will still execute without error, but a reduced
*     amount of information will be displayed and an outline around the
*     current picture (if requested) will not be drawn.

*  Algorithm:
*     -  See if the current picture is to be outlined.
*     -  Obtain an AGI identifier for the current picture on the
*     graphics device, using the appropriate access mode.
*     -  Activate the SGS plotting package.
*     -  Obtain an SGS zone for the current picture. If this fails, then
*     a channel to the graphics device cannot be opened. Note this fact,
*     but carry on regardless.
*     -  If the device is open, then enquire the GKS workstation ID for
*     the current SGS zone.
*     -  Open GNS for use with GKS and enquire the class and physical
*     device name of the workstation. Close GNS.
*     -  Display the graphics device name and class.
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
*     KAPPA: IDSTATE.

*  Authors:
*     RFWS: R.F. Warren-Smith (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZCOM              ! Length of picture comment
      PARAMETER ( SZCOM = 80 )

*  Local Variables:
      CHARACTER * ( GNS__SZDEV ) DEV ! Device name
      CHARACTER * ( GNS__SZKEY ) CLASS ! GNS workstation class
      CHARACTER * ( SZCOM ) COM  ! Picture comment
      CHARACTER * ( DAT__SZNAM ) LABEL ! Picture label
      CHARACTER * ( DAT__SZNAM ) NAME ! Picture name
      CHARACTER * ( 132 ) REFNAM ! Reference object's name 
      INTEGER CTNR               ! GKS current normalisation trans. no.
      INTEGER GSTAT              ! GKS error value
      INTEGER IPIC               ! AGI picture ID
      INTEGER IWRK               ! SGS current GKS workstation ID
      INTEGER IZON               ! SGS zone ID
      INTEGER LDEV               ! Length of device name
      INTEGER NCREF              ! Number of characters in reference
      LOGICAL OPN                ! Whether the device can be opened
      LOGICAL OUTLIN             ! Whether to outline current picture
      LOGICAL REFOBJ             ! Whether there is a reference object
      LOGICAL REPORT             ! Whether the results are to be
                                 ! reported
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

*  Obtain an identifier for the current AGI picture on the graphics
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

*  Display the graphics device name and class.
            CALL CHR_LCASE( CLASS )
            IF ( REPORT ) THEN
               CALL MSG_SETC( 'CLASS', CLASS )
               CALL MSG_OUT( 'HEADER', 'Status of the $DEVICE ^CLASS '/
     :           /'graphics device...', STATUS )
            END IF

*  If the device is not open, then just display its name.
         ELSE IF ( REPORT ) THEN
            CALL MSG_OUT( 'HEADER_1',
     :       'Status of the $DEVICE graphics device...', STATUS )
         END IF
      END IF

*  Display the physical device name, if available.
      IF ( OPN .AND. ( LDEV .GT. 0 ) .AND. REPORT ) THEN
         CALL MSG_SETC( 'DEV', DEV( : LDEV ) )
         CALL MSG_OUT( 'DEV', '   Physical device: ^DEV', STATUS )
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
     :    '   Comment: ^COM', STATUS )
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
         CALL ERR_REP( 'GDSTATE_ERR', 'GDSTATE: Error showing '/
     :   /'current state of graphics device.', STATUS )
      END IF

      END
