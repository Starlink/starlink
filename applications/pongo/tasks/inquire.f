      SUBROUTINE INQUIRE( STATUS )
*+
*  Name:
*     INQUIRE

*  Purpose:
*     Display PONGO status information.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Display information about the status of PONGO and the data which
*     have been read in. The options are:
*
*        - PGPLOT -- Display the current font, character height colour
*        etc.
*        - LIMITS -- Display the data limits and the PGPLOT world
*        coordinate limits.
*        - COLUMNS -- Display the column names from the data file, if
*        they have been set up appropriately.
*        - DEVICES -- Display the available graphics devices.
*        - DATA -- Show the data that has been read in.
*
*    More than one of these options can be specified on the command line
*    at any one time.
*
*    The DATA option uses additional parameters to allow scrolling.

*  Usage:
*     inquire

*  ADAM Parameters:
*     PGPLOT = _LOGICAL (Read)
*        Display the current PGPLOT plotting attributes.
*        [FALSE]
*     LIMITS = _LOGICAL (Read)
*        Display the data limits and the current PGPLOT viewport and
*        world coordinate limits.
*        [FALSE]
*     COLUMNS = _LOGICAL (Read)
*        Display the data file column headings (if available).
*        [FALSE]
*     DEVICES = _LOGICAL (Read)
*        Display the plotting devices available.
*        [FALSE]
*     DATA = _LOGICAL (Read)
*        Display the contents of all data areas in a formatted form.
*        [FALSE]
*     PAGE = _INTEGER (Read and Write)
*        The page length of the terminal (in the range 1 to 100). It is
*        used to stop information scrolling off the top of the screen.
*        The parameter will be prompted for at the end of each screen:
*        a null response to the prompt will terminate the listing.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 24.
*     FROM = _INTEGER (Read and Write)
*        The number of the first item to be displayed.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0
*        (implying the start of the list).
*     TO = _INTEGER (Read and Write)
*        The number of the last item to be displayed.
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 0
*        (implying the end of the list).

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Removed unused code and variables.
*     30-MAY-1996 (PDRAPER):
*        Extended fill styles to 4 and added hatch style output.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'GNS_PAR'          ! GNS_ public global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL GNS_FILTG
      LOGICAL GNS_FILTG          ! Filter function for GNS
      EXTERNAL PON_SHODATA       ! Deliver a line of data
      EXTERNAL PON_DEVOP         ! PGPLOT device is open
      LOGICAL PON_DEVOP

*  Local Variables:
      CHARACTER * ( 7 ) FONTS( 4 ) ! Font styles
      CHARACTER * ( 13 ) LINESTYLE( 5 ) ! Line styles
      CHARACTER * ( 80 ) OUTBUF  ! Output buffer

      LOGICAL DONE               ! Whether finished
      LOGICAL PGOPEN             ! Whether PGPLOT is open
      LOGICAL SHOW               ! Whether to display something

      INTEGER FROM               ! Last line to be shown
      INTEGER I                  ! Counter
      INTEGER ICIDX              ! PGPLOT colour index
      INTEGER IFONT              ! PGPLOT font
      INTEGER IFS                ! PGPLOT fill style
      INTEGER ILS                ! PGPLOT line style
      INTEGER ILW                ! PGPLOT line width
      INTEGER LENGTH             ! Length of GNS returned message
      INTEGER PAGE               ! Page length of terminal
      INTEGER TBCI               ! Colour index of text background
      INTEGER TO                 ! First line to be shown

      REAL CHARHT                ! PGPLOT character height
      REAL XWMAX                 ! Current world coordinate
      REAL XWMIN                 ! Current world coordinate
      REAL YWMAX                 ! Current world coordinate
      REAL YWMIN                 ! Current world coordinate
      REAL ANGLE                 ! Hatching angle
      REAL SEPN                  ! Hatching spacing
      REAL PHASE                 ! Hatching phase

*  Local Data:
      DATA FONTS / 'Normal', 'Roman', 'Italic', 'Script' /
      DATA LINESTYLE / 'Solid', 'Dashed', 'Dot-dash-dot-dash', 'Dotted',
     :                 'Dash-dot-dot-dot' /

*.

*  Check inherited global status.
      IF ( STATUS.NE.SAI__OK ) RETURN

*  Initialise DONE.
      DONE = .FALSE.
      PGOPEN = PON_DEVOP( .FALSE., STATUS )

*  Paging parameters (could get a PAGER function to allow large
*  quantities of data to be displayed).
      CALL PAR_GET0I( 'PAGE', PAGE, STATUS )
      CALL PAR_GET0I( 'FROM', FROM, STATUS )
      CALL PAR_GET0I( 'TO', TO, STATUS )

*  PGPLOT parameter.
      CALL PAR_GET0L( 'PGPLOT', SHOW, STATUS )

      IF ( SHOW ) THEN
         DONE = .TRUE.
         IF ( PGOPEN ) THEN
            CALL PGQINF( 'DEV/TYPE', OUTBUF, LENGTH ) ! Seem to need
                                ! DEV/TYPE other ways of getting device
                                ! name fail
            TO = INDEX( OUTBUF, '/')
            IF ( TO .NE. 0 ) LENGTH = TO - 1
            CALL MSG_SETC( 'DEV', OUTBUF( : LENGTH ) )
            CALL MSG_OUT( ' ', 'Current plotting device: ^DEV',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )
            CALL PGQCF( IFONT )
            CALL PGQCH( CHARHT )
            CALL PGQCI( ICIDX )
            CALL PGQFS( IFS )
            CALL PGQHS( ANGLE, SEPN, PHASE )
            CALL PGQLS( ILS )
            CALL PGQLW( ILW )
            CALL PGQTBG( TBCI )

            CALL MSG_OUT( ' ', 'Current PGPLOT plotting attributes:',
     :                    STATUS )
            CALL MSG_SETI( 'COLIDX', ICIDX )
            CALL MSG_OUT( ' ', '   Colour index: ^COLIDX', STATUS )
            CALL MSG_SETR( 'CHEIGHT', CHARHT )
            CALL MSG_OUT( ' ', '   Character height: ^CHEIGHT',
     :                    STATUS )
            CALL MSG_SETC( 'FONT', FONTS( IFONT ) )
            CALL MSG_OUT( ' ', '   Font: ^FONT', STATUS )

            IF ( IFS .EQ. 1 ) THEN
               CALL MSG_SETC( 'FILSTY', 'Solid' )
            ELSE IF ( IFS .EQ. 2 ) THEN
               CALL MSG_SETC( 'FILSTY', 'Hollow' )
            ELSE IF ( IFS .EQ. 3 ) THEN
               CALL MSG_SETC( 'FILSTY', 'Hatched' )
            ELSE IF ( IFS .EQ. 4 ) THEN
               CALL MSG_SETC( 'FILSTY', 'Cross-Hatched' )
            ELSE
               CALL MSG_SETC( 'FILSTY', 'invalid index (' )
               CALL MSG_SETI( 'FILSTY', IFS )
               CALL MSG_SETC( 'FILSTY', ')' )
            END IF
            CALL MSG_OUT( ' ', '   Fill style: ^FILSTY', STATUS )

            CALL MSG_SETR( 'ANGLE', ANGLE )
            CALL MSG_OUT( ' ', '   Hatch angle: ^ANGLE', STATUS )
            CALL MSG_SETR( 'SEPN', SEPN )
            CALL MSG_OUT( ' ', '   Hatch spacing: ^SEPN', STATUS )
            CALL MSG_SETR( 'PHASE', PHASE )
            CALL MSG_OUT( ' ', '   Hatch phase: ^PHASE', STATUS )

            CALL MSG_SETC( 'LINSTY', LINESTYLE( ILS ) )
            CALL MSG_OUT( ' ', '   Line style: ^LINSTY', STATUS )

            CALL MSG_SETI( 'LINWID', ILW )
            CALL MSG_OUT( ' ', '   Line width: ^LINWID', STATUS )

            IF ( TBCI .LT. 0 ) THEN
               CALL MSG_OUT( ' ', '   Text background: transparent',
     :                       STATUS )
            ELSE
               CALL MSG_SETI( 'TBCI', TBCI )
               CALL MSG_OUT( ' ', '   Text background: ^TBCI', STATUS )
            END IF
            CALL MSG_BLANK( STATUS )
         ELSE
            CALL MSG_OUT( ' ', 'PGPLOT is not active.', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF
      END IF

*  LIMITS parameter.
      CALL PAR_GET0L( 'LIMITS', SHOW, STATUS )

      IF ( SHOW ) THEN
         DONE = .TRUE.
         CALL MSG_OUT( ' ', 'Actual data limits:', STATUS )
         CALL MSG_SETR( 'XMIN', XMIN )
         CALL MSG_SETR( 'XMAX', XMAX )
         CALL MSG_OUT( ' ', '   X-axis data range: ^XMIN to ^XMAX',
     :                 STATUS )
         CALL MSG_SETR( 'YMIN', YMIN )
         CALL MSG_SETR( 'YMAX', YMAX )
         CALL MSG_OUT( ' ', '   Y-axis data range: ^YMIN to ^YMAX',
     :                 STATUS )

         IF ( PGOPEN ) THEN
            CALL MSG_BLANK( STATUS )
            CALL PGQWIN( XWMIN, XWMAX, YWMIN, YWMAX )
            CALL MSG_OUT( ' ', 'PGPLOT window limits:', STATUS )
            CALL MSG_SETR( 'XWMIN', XWMIN )
            CALL MSG_SETR( 'XWMAX', XWMAX )
            CALL MSG_OUT( ' ', '   X-axis limits: ^XWMIN to ^XWMAX',
     :                    STATUS )
            CALL MSG_SETR( 'YWMIN', YWMIN )
            CALL MSG_SETR( 'YWMAX', YWMAX )
            CALL MSG_OUT( ' ', '   Y-axis limits: ^YWMIN to ^YWMAX',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )
            CALL MSG_OUT( ' ', 'PGPLOT viewport limits:', STATUS )
            CALL PGQVP( 0, XWMIN, XWMAX, YWMIN, YWMAX )
            CALL MSG_SETR( 'XWMIN', XWMIN )
            CALL MSG_SETR( 'XWMAX', XWMAX )
            CALL MSG_OUT( ' ', '   X-axis limits: ^XWMIN to ^XWMAX',
     :                    STATUS )
            CALL MSG_SETR( 'YWMIN', YWMIN )
            CALL MSG_SETR( 'YWMAX', YWMAX )
            CALL MSG_OUT( ' ', '   Y-axis limits: ^YWMIN to ^YWMAX',
     :                    STATUS )
            CALL MSG_BLANK( STATUS )
         ELSE
            CALL MSG_OUT( ' ', 'PGPLOT is not active.', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF
      END IF

*  COLUMNS parameter.
      CALL PAR_GET0L( 'COLUMNS', SHOW, STATUS )

      IF ( SHOW ) THEN
         DONE = .TRUE.

         IF ( NCOLS .NE. 0 ) THEN

            CALL MSG_OUT( ' ', 'PONGO column labels:', STATUS )
            CALL MSG_BLANK( STATUS )

            DO 10 I = 1, NCOLS

*           Truncate the column labels if they are too long.
               CALL MSG_FMTI( 'COLUMN', 'I2', I )
               CALL MSG_SETC( 'LABEL', COLLAB( I ) )
               CALL MSG_OUT( ' ', '   ^COLUMN : ^LABEL', STATUS )
 10         CONTINUE
            CALL MSG_BLANK( STATUS )
         ELSE
            CALL MSG_OUT( ' ', 'No column labels exist.', STATUS )
            CALL MSG_BLANK( STATUS )
         END IF
      END IF

*  DEVICES parameter.
      CALL PAR_GET0L( 'DEVICES', SHOW, STATUS )

      IF ( SHOW ) THEN
         DONE = .TRUE.

*  Call the PGPLOT list devices routine.
         CALL PGLDEV
         CALL MSG_BLANK( STATUS )
      END IF

*  DATA parameter.
      CALL PAR_GET0L( 'DATA', SHOW, STATUS )

      IF ( SHOW ) THEN
         DONE = .TRUE.

*     Deliver the data to the user.
         CALL PON_SHOUSR( PON_SHODATA, 'PAGE', NDAT, FROM, TO, STATUS )
         CALL MSG_BLANK( STATUS )
      END IF

      IF ( ( .NOT. DONE )
     :     .AND. ( STATUS .EQ. SAI__OK ) ) CALL MSG_OUT( ' ',
     :                                        'No action has been ' //
     :                                        'taken.', STATUS )

*  Check the returned status and report a contextual error message
*  if necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'INQUIRE_END',
     :                              'INQUIRE: Unable to display ' //
     :                              'PONGO status information.',
     :                              STATUS )

      END
* $Id$
