      SUBROUTINE PICIN( STATUS )
*+
*  Name:
*     PICIN

*  Purpose:
*     Finds the attributes of a picture interior to the current picture.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICIN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application finds the attributes of a picture, selected by
*     name, that was created since the current picture and lies within
*     the bounds of the current picture.  The search starts from the
*     most-recent picture, unless the current picture is included,
*     whereupon the current picture is tested first.
*
*     The attributes reported are the name, comment, label, name of the
*     reference data object, the bounds in world, raster, and normalised
*     device co-ordinates.

*  Usage:
*     picin [name] [device]

*  ADAM Parameters:
*     COMMENT = LITERAL (Write)
*        The comment of the current picture.  Up to 132 characters
*        will be written.
*     CURRENT = _LOGICAL (Read)
*        If this is {\tt TRUE}, the current picture is compared against the
*        chosen name before searching from the most-recent picture
*        within the current picture. [FALSE]
*     DEVICE = DEVICE (Read)
*        Name of the graphics device about which information is
*        required. [Current graphics device]
*     LABEL = LITERAL (Write)
*        The label of the current picture.  It is blank if there is no
*        label.
*     NAME = LITERAL (Read)
*        The name of the picture to be found within the current picture.
*        If it is null (!), the first interior picture is selected.
*        ["DATA"]
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
*     PNAME = LITERAL (Write)
*        The name of the current picture.
*     RCX1 = _REAL (Write)
*        The lower x raster co-ordinate of the current picture.  A
*        value of -1 signifies that the value could not be determined
*        because the device is not of the raster type.
*     RCX2 = _REAL (Write)
*        The upper x raster co-ordinate of the current picture.  A
*        value of -1 signifies that the value could not be determined
*        because the device is not of the raster type.
*     RCY1 = _REAL (Write)
*        The lower y raster co-ordinate of the current picture.  A
*        value of -1 signifies that the value could not be determined
*        because the device is not of the raster type.
*        picture.
*     RCY2 = _REAL (Write)
*        The upper y raster co-ordinate of the current picture.  A
*        value of -1 signifies that the value could not be determined
*        because the device is not of the raster type.
*     REFNAM = LITERAL (Write)
*        The reference object associated with the current picture.  It
*        is blank if there is no reference object.  Up to 132 characters
*        will be written.
*     REPORT = _LOGICAL (Read)
*        If this is FALSE details of the interior picture are not
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
*     picin
*        This reports the attributes of the last DATA picture within
*        the current picture for the current graphics device.
*     picin frame graphon
*        This reports the attributes of the last FRAME picture within
*        the current picture for the Graphon device.
*     picin refnam=(object) current
*        This reports the attributes of the last data picture within
*        the current picture for the current graphics device.  If there
*        is a reference data object, its name is written to the ICL
*        variable OBJECT.  The search includes the current picture.
*     picin ncx1=(x1) ncx2=(x2) ncy1=(y1) ncy2=(y2)
*        This reports the attributes of the last DATA picture within
*        the current picture for the current graphics device.  The
*        bounds of the current picture in normalised device
*        co-ordinates are written to the ICL variables: X1, X2, Y1, Y2.

*  Notes:
*     This application is intended for use within procedures.  Also if
*     a DATA picture is selected and the current picture is included in
*     the search, this application informs about the same picture that
*     an application that works in a cursor interaction mode would
*     select, and so acts as a check that the correct picture will be
*     accessed.

*  Related Applications:
*     KAPPA: GDSTATE, PICDEF, PICLIST, PICTRANS, PICXY.

*  [optional_A_task_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1991 August 20 (MJC):
*        Original version.
*     1993 August 19 (MJC):
*        Added raster co-ordinates.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! Data-system constants
      INCLUDE 'PAR_ERR'          ! Parameter system errors

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CTNR               ! GKS current normalisation trans. no.  
      LOGICAL CURRNT             ! Select the current picture if it
                                 ! matches the name?
      REAL DEVCO( 4 )            ! GKS window bounds in device co-ords
      INTEGER GSTAT              ! GKS error value
      LOGICAL HASLAB             ! The picture has a label?
      INTEGER IPIXX              ! Maximum number of device column
                                 ! pixels of the device
      INTEGER IPIXY              ! Maximum number of device line pixels
                                 ! of the device
      CHARACTER * ( DAT__SZNAM ) LABEL ! Picture label
      CHARACTER * ( DAT__SZNAM ) NAME ! Only picture with this name is to
                                 ! be selected
      INTEGER NCREF              ! Number of characters in reference
                                 ! name
      INTEGER NINTS              ! Number of greyscale intensities
                                 ! available on the chosen device
      CHARACTER * ( 132 ) PICCOM ! Picture comment
      INTEGER PICID              ! Current picture identifier
      INTEGER PICIDI             ! Interior picture identifier
      INTEGER PICIDN             ! Next picture identifier
      CHARACTER * ( 132 ) REFNAM ! Reference object's name 
      LOGICAL REFOBJ             ! There is a reference object
                                 ! associated with the current picture?
      LOGICAL REPORT             ! The results are to be reported?
      LOGICAL VALID              ! Reference object is a locator?
      REAL VIEWPT( 4 )           ! GKS viewport bounds in NDC
      REAL WINDOW( 4 )           ! GKS window bounds in world co-ords
      REAL X1                    ! Lower x bound of the picture
      REAL X2                    ! Upper x bound of the picture
      REAL Y1                    ! Lower y bound of the picture
      REAL Y2                    ! Upper y bound of the picture
      INTEGER ZONE               ! SGS zone of current picture
      INTEGER ZONEI              ! SGS zone of interior picture

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See whether reporting is required or not.
      CALL PAR_GET0L( 'REPORT', REPORT, STATUS )

*  Find the name, defaulted to DATA.  A null means a null string and
*  so can be annulled.
      CALL ERR_MARK
      CALL PAR_DEF0C( 'NAME', 'DATA', STATUS )
      CALL PAR_GET0C( 'NAME', NAME, STATUS )
      IF ( STATUS .EQ. PAR__NULL ) THEN
         NAME = ' '
         CALL ERR_ANNUL( STATUS )
      END IF
      CALL ERR_RLSE

*  Should the current picture be included in the search?
      CALL PAR_GET0L( 'CURRENT', CURRNT, STATUS )

*  Associate image display and start database activity.  Update access
*  is used to be able to mark the plot to indicate which images have
*  been measured.
      CALL AGS_ASSOC( 'DEVICE', 'READ', ' ', PICID, ZONE, STATUS )

*  Exclude the current picture if requested to do so by skipping on to
*  the next picture in the database.
      IF ( .NOT. CURRNT ) THEN
         CALL AGI_RCS( ' ', PICID, PICIDN, STATUS )
         CALL AGI_SELP( PICIDN, STATUS )
      END IF

*  Find the last nominated picture that lies within the current picture,
*  and was created subsequently to the current picture.
      CALL KPG1_AGFND( NAME, PICIDI, STATUS )

*  Obtain the SGS zone identifier for the current DATA picture.
      CALL AGS_NZONE( ZONEI, STATUS )

*  Obtain the current AGI picture name and display it.
      CALL AGI_INAME( NAME, STATUS )
      IF ( REPORT ) CALL MSG_OUT( 'BLANK', ' ', STATUS )
      CALL MSG_SETC( 'INAME', NAME )
      CALL PAR_PUT0C( 'PNAME', NAME, STATUS )

*  Get the comment of the picture.
      CALL AGI_ICOM( PICCOM, STATUS )
      CALL MSG_SETC( 'PICCOM', PICCOM )
      CALL PAR_PUT0C( 'COMMENT', PICCOM, STATUS )

*  Obtain the label associated with the picture, if a label exists.
*  Write it to an output parameter.  A dummy is required if it doesn't
*  so that the old value is overwritten.
      HASLAB = .FALSE.
      CALL AGI_ILAB( PICIDI, LABEL, STATUS )
      IF ( LABEL( 1:1 ) .NE. ' ' ) THEN
         CALL MSG_SETC( 'LABEL', LABEL )
         CALL PAR_PUT0C( 'LABEL', LABEL, STATUS )
         HASLAB = .TRUE.
      ELSE
         CALL PAR_PUT0C( 'LABEL', ' ', STATUS )
      END IF

*  Report it to the user.
      IF ( HASLAB .AND. REPORT ) THEN
         CALL MSG_OUT( 'INTPIC', 'Interior picture has name: ^INAME, '/
     :     /'comment: ^PICCOM, label: ^LABEL.', STATUS )
      ELSE IF ( REPORT ) THEN
         CALL MSG_OUT( 'INTPIC', 'Interior picture has name: ^INAME, '/
     :     /'comment: ^PICCOM.', STATUS )
      END IF

*  Determine whether or not there is a reference object associated
*  with the current picture.
      CALL KPG1_AGREF( PICIDI, 'READ', REFOBJ, REFNAM, STATUS )

*  If one exists translate its locator to a token containing the path
*  name and file name, and tidy the reference locator; or if the
*  reference is just a name, write it to a token.  Note that the token
*  is renewed if it is to be used twice.  Write a message containing
*  the reference object.
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
            CALL MSG_OUT( 'REFNAME', 'Reference data object: ^RNAME',
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
         CALL MSG_BLANK( STATUS )
      END IF

*  Write the results to output parameters.
      CALL PAR_PUT0R( 'WCX1', X1, STATUS )
      CALL PAR_PUT0R( 'WCX2', X2, STATUS )
      CALL PAR_PUT0R( 'WCY1', Y1, STATUS )
      CALL PAR_PUT0R( 'WCY2', Y2, STATUS )

*  Enquire the current GKS normalisation transformation number.  Report
*  an error if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL GQCNTN( GSTAT, CTNR )
         CALL GKS_GSTAT( STATUS )
      END IF

*  Enquire the GKS window and viewport bounds for this transformation.
*  The viewport then gives the bounds of the current SGS zone (and
*  hence the AGI picture) in normalised device units.  Report an error
*  if necessary.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL GQNT( CTNR, GSTAT, WINDOW, VIEWPT )
         CALL GKS_GSTAT( STATUS )
      END IF

*  Display the NDC bounds of the current picture.
      IF ( REPORT ) THEN
         CALL MSG_OUT( 'NDC', '   Normalised device co-ordinates:',
     :                 STATUS )
         CALL MSG_SETR( 'X1', VIEWPT( 1 ) )
         CALL MSG_SETR( 'X2', VIEWPT( 2 ) )
         CALL MSG_OUT( 'NDC_X',
     :     '               X = ^X1 to ^X2', STATUS )
         CALL MSG_SETR( 'Y1', VIEWPT( 3 ) )
         CALL MSG_SETR( 'Y2', VIEWPT( 4 ) )
         CALL MSG_OUT( 'NDC_Y',
     :     '               Y = ^Y1 to ^Y2', STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

*  Write the results to output parameters.
      CALL PAR_PUT0R( 'NCX1', VIEWPT( 1 ), STATUS )
      CALL PAR_PUT0R( 'NCX2', VIEWPT( 2 ), STATUS )
      CALL PAR_PUT0R( 'NCY1', VIEWPT( 3 ), STATUS )
      CALL PAR_PUT0R( 'NCY2', VIEWPT( 4 ), STATUS )

*  Obtain the maximum display surface size in pixels, if the device is
*  raster.  There will be an error if it is not, so start a new error
*  context.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL ERR_MARK
         CALL KPG1_QIDAT( 'DEVICE', 'SGS', NINTS, IPIXX, IPIXY, STATUS )

         IF ( STATUS .EQ. SAI__OK ) THEN

*  Find the device co-ordinates by scaling the normalised device
*  co-ordinates.  Display the device-co-ordinate bounds of the current
*  picture.
            DEVCO( 1 ) = VIEWPT( 1 ) * REAL( IPIXX )
            DEVCO( 2 ) = VIEWPT( 2 ) * REAL( IPIXX )
            DEVCO( 3 ) = VIEWPT( 3 ) * REAL( IPIXY )
            DEVCO( 4 ) = VIEWPT( 4 ) * REAL( IPIXY )

*  Display the DC bounds of the current picture.
            IF ( REPORT ) THEN
               CALL MSG_OUT( 'DC', '   Device co-ordinates:',
     :                       STATUS )
               CALL MSG_SETR( 'X1', DEVCO( 1 ) )
               CALL MSG_SETR( 'X2', DEVCO( 2 ) )
               CALL MSG_OUT( 'NDC_X',
     :           '               X = ^X1 to ^X2', STATUS )
               CALL MSG_SETR( 'Y1', DEVCO( 3 ) )
               CALL MSG_SETR( 'Y2', DEVCO( 4 ) )
               CALL MSG_OUT( 'NDC_Y',
     :           '               Y = ^Y1 to ^Y2', STATUS )
               CALL MSG_BLANK( STATUS )
            END IF

*  The device does not have the raster attribute, so there are no
*  co-ordinates to report.  So set the values to indicate that fact.
*  Minus 1 is convenient as it cannot be a genuine value.  Any scripts
*  using the data can test for it easily.
         ELSE
            CALL ERR_ANNUL( STATUS )
            DEVCO( 1 ) = -1.0
            DEVCO( 2 ) = -1.0
            DEVCO( 3 ) = -1.0
            DEVCO( 4 ) = -1.0
         END IF

*  End the new error context.
         CALL ERR_RLSE

*  Write the results to output parameters.
         CALL PAR_PUT0R( 'RCX1', DEVCO( 1 ), STATUS )
         CALL PAR_PUT0R( 'RCX2', DEVCO( 2 ), STATUS )
         CALL PAR_PUT0R( 'RCY1', DEVCO( 3 ), STATUS )
         CALL PAR_PUT0R( 'RCY2', DEVCO( 4 ), STATUS )
      END IF

*  Close down the database and device.  Restore the current picture.
      CALL AGS_DEASS( 'DEVICE', .FALSE., STATUS )

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICIN_ERR',
     :     'PICIN: Unable to find the attributes of the interior '/
     :     /'picture.', STATUS )
      END IF

      END
