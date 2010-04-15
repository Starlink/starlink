      SUBROUTINE BEGPONGO( STATUS )
*+
*  Name:
*     BEGPONGO

*  Purpose:
*     Open a plotting device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Set up a device for subsequent PONGO plotting commands. This
*     application allows plotting onto an AGI picture created by a
*     different package (e.g. KAPPA), or the creation of a new base
*     picture.
*
*     If a picture created by a run of a previous application is made
*     (using either the "current" picture or selecting using a known
*     picture label see parameter ACTION) then it is possible to
*     overlay new graphics (such as annotations, lines etc.) using the
*     same coordinate system (see the OVERLAY parameter).

*  Usage:
*     begpongo [device] [action] [clear] [overlay]
*        { label=?
*        action

*  ADAM Parameters:
*     ACTION = _CHAR (Read and Write)
*        If 'B', the plotting device will be cleared and the whole of
*        its plotting surface used. If equal to 'C', the current picture
*        will be used and a PGPLOT viewport created inside it. If 'L'
*        then a previously labelled picture (set using KAPPA:PICLABEL)
*        can be selected. Once set, this parameter will retain its value
*        in subsequent invocations of BEGPONGO.
*        ['C']
*     CHEIGHT = _REAL (Write)
*        The character height scaling factor. A value of 1.0 implies a
*        nominal character height of 1/40th the viewport height. The
*        value is set by the application from the height of the chosen
*        picture (unless the picture is the base one). The result is
*        written to the global parameter PONGO_CHEIGHT.
*     CLEAR = _LOGICAL (Read)
*        If TRUE then the current picture will be cleared of any
*        existing graphics.
*        [TRUE]
*     DEVICE = DEVICE (Read and Write)
*        The name of the device to be used for plotting.  The names of
*        the currently available devices can be found using the INQUIRE
*        DEVICE command.
*
*        The value of the global parameter GRAPHICS_DEVICE is used
*        unless a value is specified on the command line. If
*        GRAPHICS_DEVICE is not defined and no value is specified on
*        the command line, the value will be prompted for.
*     LABEL = _CHAR (Read)
*        If ACTION=L is selected then the name of the AGI picture to be
*        selected is given by this parameter. AGI pictures can be labelled
*        using the KAPPA application PICLABEL.
*        ['']
*     OVERLAY = _LOGICAL (Read)
*        If TRUE, the PGPLOT viewport created will exactly overlay the
*        the last DATA picture. This is useful for drawing axis labels
*        using BOXFRAME on an image or contour map etc. that has been
*        displayed by another package (e.g. KAPPA:DISPLAY).
*        [FALSE]
*     XMIN = _REAL (Write)
*        The left hand edge of the world coordinate limits from the
*        selected picture. This defaults to 0.0 for a new (i.e.  not
*        overlayed) picture.  The result is written to the global
*        parameter PONGO_XMIN.
*     XMAX = _REAL (Write)
*        The right hand edge of the world coordinate limits from the
*        selected picture. This defaults to 1.0 for a new (i.e.  not
*        overlayed) picture.  The result is written to the global
*        parameter PONGO_XMAX.
*     YMIN = _REAL (Write)
*        The lower edge of the world coordinate limits from the
*        selected picture. This defaults to 0.0 for a new (i.e.  not
*        overlayed) picture.  The result is written to the global
*        parameter PONGO_YMIN.
*     YMAX = _REAL (Write)
*        The lower edge of the world coordinate limits from the
*        selected picture. This defaults to 1.0 for a new (i.e.  not
*        overlayed) picture.  The result is written to the global
*        parameter PONGO_YMAX.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     4-JUL-1990 (PAH):
*        Original version.
*     28-NOV-1990 (JBVAD::PAH):
*        The routine was updated to reflect the changes in AGI and to
*        add ability to either overlay plots on previous DATA pictures,
*        or to use the BASE picture as the plotting surface.
*     4-DEC-1991 (PCTR):
*        Tidy up AGI calls.
*     3-FEB-1992 (PAH):
*        Make the character height the same relative size for non-base
*        frame windows
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     10-AUG-1993 (PCTR):
*        Removed the BASE and OVERLAY parameters and replaced them with
*        the ACTION parameter.
*     2-JUN-1994 (PDRAPER):
*        Removed unused variable LSTAT.
*     17-JUN-1994 (PDRAPER):
*       Sorted out mess with help for ACTION parameter. OVERLAY is still
*       used, BASE is not.
*     17-JUN-1994 (PDRAPER):
*        Added help on CLEAR parameter.
*     20-MAR-1995 (PDRAPER):
*        Added traps for STATUS on some PGPLOT calls.
*     1-AUG-1996 (PDRAPER):
*        Added code to tidy up when exiting in error and device is
*        open. Now correctly searches for last DATA picture when OVERLAY
*        is set.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'AGI_PAR'          ! AGI_ global constants
      INCLUDE 'AGI_ERR'          ! AGI_ error codes

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      CHARACTER * ( 20 ) ACTION  ! Action prameter value
      CHARACTER * ( 80 ) COMMENT ! Picture comment
      CHARACTER * ( AGI__SZLAB ) LABEL ! Picture label
      CHARACTER * ( AGI__SZLAB ) PICLAB ! Picture label
      CHARACTER * ( AGI__SZNAM ) PICNAM ! Picture name

      LOGICAL CLEAR              ! Whether to clear the display
      LOGICAL OVER               ! Whether to overlay the picture

      INTEGER FPICID             ! Frame picture ID
      INTEGER LENACT             ! Length of the ACTION value
      INTEGER PICID              ! Picture ID
      INTEGER PICIDS             ! Picture ID
      INTEGER BASEID             ! Base picture ID
      INTEGER INPICID            ! Picture ID on start-up

      REAL NEWCHEIGHT            ! new character height for
                                 ! non-base frame pictures
      REAL XMINP                 ! Viewport bounds
      REAL XMAXP                 ! Viewport bounds
      REAL YMINP                 ! Viewport bounds
      REAL YMAXP                 ! Viewport bounds
      REAL WX1                   ! World coordinate bounds
      REAL WX2                   ! World coordinate bounds
      REAL WY1                   ! World coordinate bounds
      REAL WY2                   ! World coordinate bounds

*  Internal References:
      INTEGER CHR_LEN            ! Length of string

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check to see if PGPLOT is already open.
      IF ( PON_DEVOP( .FALSE., STATUS ) ) THEN

*  Report an error that PGPLOT is already active.
         STATUS = SAI__ERROR
         CALL ERR_REP( 'BEGPONGO_OPEN',
     :                 'PONGO already active - use ENDPLOT to ' //
     :                 'complete the previous plot.', STATUS )
      ELSE

*  Get the values of the OVERLAY and CLEAR parameters.
         CALL PAR_GET0L( 'OVERLAY', OVER, STATUS )
         CALL PAR_GET0L( 'CLEAR', CLEAR, STATUS )

*  Get the value of the ACTION parameter and act on its value.
         CALL PAR_GET0C( 'ACTION', ACTION, STATUS )
         LENACT = MAX( 1, CHR_LEN( ACTION ) )
         CALL CHR_UCASE( ACTION( : LENACT ) )

*  Check the inherited status and abort if there are any errors.
         IF ( STATUS .NE. SAI__OK ) GO TO 99

         IF ( ACTION .NE. 'B' .AND. ACTION .NE. 'C'
     :        .AND. ACTION .NE. 'L' ) THEN

*  The value of ACTION is not valid, report an error.
            CALL MSG_SETC( 'ACTION', ACTION )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'BEGPONGO_BADACT',
     :              'The value of ACTION (^ACTION) is not valid.',
     :              STATUS )
         ELSE

*  Get the current AGI picture ID, with the access mode appropriate to
*  CLEAR parameter value. Activate PGPLOT and start a BEGIN-END block.
            IF( CLEAR ) THEN
               CALL AGI_ASSOC( 'DEVICE', 'WRITE', INPICID, STATUS )
            ELSE
               CALL AGI_ASSOC( 'DEVICE', 'UPDATE', INPICID, STATUS )
            END IF
            CALL AGI_BEGIN
            CALL AGP_ACTIV( STATUS )

*  Select the required picture.
            IF ( ACTION .EQ. 'B' ) THEN

*  Get an identifier for the Base picture and select it as current.
               CALL AGI_IBASE( BASEID, STATUS )
               CALL AGI_SELP( BASEID, STATUS )

*  Assign the picture ID.
               PICID = BASEID
            ELSE IF ( ACTION .EQ. 'L' ) THEN

*  Find the picture with the given label.
               CALL PAR_GET0C( 'LABEL', LABEL, STATUS )

*  Get an identifier using the given AGI label. First get an identifier
*  for the Base picture and select it as current.
               CALL AGI_IBASE( BASEID, STATUS )
               CALL AGI_SELP( BASEID, STATUS )
               CALL AGI_ILAB( BASEID, PICLAB, STATUS )

*  Search forward through the database for a picture with this label.
               PICID = BASEID
               PICIDS = BASEID

*  Loop to perform the search.
 100           CONTINUE          ! Start of DO WHILE loop.
               IF ( PICLAB .NE. LABEL .AND. STATUS .EQ. SAI__OK ) THEN

*  Annul the last search start and select the base picture as current.
                  IF ( PICIDS .NE. BASEID ) CALL AGI_ANNUL( PICIDS,
     :                                                      STATUS )
                  CALL AGI_SELP( BASEID, STATUS )

*  Search for the next picture.
                  PICIDS = PICID
                  CALL AGI_RCS( ' ', PICIDS, PICID, STATUS )
                  CALL AGI_ILAB( PICID, PICLAB, STATUS )
                  GO TO 100
               END IF

*  Check the inherited status and report an error if the label has not
*  been found.
               IF ( STATUS .NE. SAI__OK ) THEN
                  CALL MSG_SETC( 'LABEL', LABEL )
                  CALL ERR_REP( 'BEGPLOT_NOLAB',
     :               'The picture label (^LABEL) was not found in ' //
     :                 'the AGI database.', STATUS )
               END IF
            ELSE

*  ACTION='C', use the current picture, unless overlaid, in which case
*  find the last DATA picture.
               PICID = INPICID
               IF ( OVER ) THEN
                  CALL AGI_RCL( 'DATA', INPICID, STATUS )
                  IF ( STATUS .NE. SAI__OK ) THEN

*  There is no DATA picture in the current frame so the application
*  will have to exit.
                     CALL ERR_REP( 'BEGPONGO_NDATAP',
     :'The current AGI database picture is not a DATA picture and it '//
     :'does not contain a DATA picture. This means it is not '//
     :'possible to overlay.', STATUS )
                     GO TO 99
                  END IF
               END IF
            END IF


*  Check the returned status from the initial AGI calls and abort if
*  necessary.
            IF ( STATUS .NE. SAI__OK ) GO TO 99

*  Report the picture name, comment and label.
            CALL AGI_INAME( PICNAM, STATUS )
            CALL AGI_ICOM( COMMENT, STATUS )
            CALL AGI_ILAB( PICID, PICLAB, STATUS)
            CALL MSG_SETC( 'PICNAM', PICNAM )
            CALL MSG_OUT( ' ', 'Current picture is a ^PICNAM picture.',
     :                    STATUS )
            CALL MSG_SETC( 'LAB', PICLAB )
            CALL MSG_OUT( ' ', '   Label  : ^LAB', STATUS )
            CALL MSG_SETC( 'PC', COMMENT )
            CALL MSG_OUT( ' ', '   Comment: ^PC', STATUS )

*  Inquire the world coordinates of the current picture.
            CALL AGI_IWOCO( WX1, WX2, WY1, WY2, STATUS )

*  Save a PONGO Frame picture in the database.
            CALL AGI_NUPIC( WX1, WX2, WY1, WY2, 'FRAME',
     :                      'PONGO: Frame picture.', 0.0, 1.0, 0.0, 1.0,
     :                      FPICID, STATUS )

*  Check if OVERLAY is required.
            IF ( OVER ) THEN

*  Create a viewport exactly matching the found DATA picture.
               CALL AGP_NVIEW( .FALSE., STATUS )
               CALL AGI_SELP( PICID, STATUS )

*  Set the world coordinates.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL PGWINDOW( WX1, WX2, WY1, WY2 )
               END IF
            ELSE

*  Create a new viewport so that axes, etc. lie within the
*  initial picture.
               CALL AGP_NVIEW( .TRUE., STATUS )

*  Set the default world coordinates.
               CALL PGWINDOW( 0.0, 1.0, 0.0, 1.0 )
            END IF

*  Set up the world coordinates as parameters.
            IF ( STATUS .EQ. SAI__OK ) THEN
               CALL PGQWIN( XMINP, XMAXP, YMINP, YMAXP )
            END IF
            CALL PAR_PUT0R( 'XMIN', XMINP, STATUS )
            CALL PAR_PUT0R( 'YMIN', YMINP, STATUS )
            CALL PAR_PUT0R( 'XMAX', XMAXP, STATUS )
            CALL PAR_PUT0R( 'YMAX', YMAXP, STATUS )

*  If not in base frame then set up the character sizes.
            IF ( ACTION .NE. 'B' ) THEN
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL PGQVP( 0, XMINP, XMAXP, YMINP, YMAXP )
               END IF
               NEWCHEIGHT = ( YMAXP - YMINP ) / 0.8
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL PGSCH( NEWCHEIGHT )
               END IF

*  Put the character height back into the global parameter.
               CALL PAR_PUT0R( 'CHEIGHT', NEWCHEIGHT, STATUS )
            END IF

*  Switch off PGPLOT new page prompting.
            CALL PGASK( .FALSE. )
         END IF

*  If this section creates an error then close down PGPLOT before
*  leaving (this makes sure that we do think the package is in
*  use in other programs).
 99      CONTINUE
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL AGI_END( -1, STATUS)
            CALL AGP_DEACT( STATUS )
            CALL AGI_CANCL( 'DEVICE', STATUS )
         END IF
      END IF

*  Check the returned status and report a contextual error message.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'BEGPONGO_END',
     :                              'BEGPONGO: Unable to open a ' //
     :                              'plotting device.', STATUS )

      END
* $Id$
