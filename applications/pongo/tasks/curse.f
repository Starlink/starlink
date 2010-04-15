      SUBROUTINE CURSE( STATUS )
*+
*  Name:
*     CURSE

*  Purpose:
*     Use the cursor to get interactive input.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Display the cursor and perform various interactive tasks. These
*     tasks are as follows:
*
*        - Q -- QUIT
*           The application ends.
*
*        - D -- DRAW
*           A line is drawn from the PGPLOT "current position" to  the
*           cursor position. This position is also written to the
*           internal label list.
*
*        - E -- EXPAND
*           The plotting limits are expanded by a factor of 2
*           about the position of the cursor. No other action is taken.
*           This allows the screen to be cleared and the graph to be
*           re-plotted without having to set the limits explicitly.
*
*        - G -- GRADIENT
*           Draw a line between two consecutive cursor hits and report
*           the gradient of the line.
*
*        - L -- LABEL
*           Write a label directly onto the plot. The application
*           uses up to two points which define the angle at which the
*           label is to be drawn. Once "L" has been pressed, the user
*           is given the option to add another point in the standard
*           PGPLOT fashion (cf. the PGPLOT routine PGNCURSE); i.e.
*
*              - A -- add a point
*              - D -- delete a point
*              - X -- finish
*
*           On pressing the "X" key, a label is prompted for. If only
*           one point is supplied, the label is plotted horizontally.
*
*        - M -- MARK
*           Mark the current cursor position with the current symbol
*           type.
*
*        - P -- ANNOTATE
*           The label for the nearest data point is written with its
*           left hand end at the position defined by the cursor. If "O"
*           is pressed, the right hand end of the label is placed at
*           that position. The information to create this label is
*           stored in an internal table. This information can be
*           written out into file suitable for including in an ICL
*           command file by using the WRITEI application, e.g.
*
*              ICL> WRITEI LABLST
*
*        - S -- SHRINK
*           The plotting limits are set so as to zoom out by a factor
*           of 2 about the position of the cursor. No other action is
*           taken. This allows the screen to be cleared and the graph
*           to be re-plotted without having to set the limits
*           explicitly.
*
*        - V -- MOVE
*           The PGPLOT "current position" will be set to the cursor
*           position. This position is also written to the internal
*           label list.
*
*        - X -- POSITION
*           The current position of the cursor in world coordinates is
*           written to the terminal and the XCURSE and YCURSE
*           parameters. If a projection is in effect then approximate
*           sky coordinates are also reported in various forms.
*
*        - Z -- ZOOM
*           The limits for a zoomed version of the current plot are
*           set. The application will prompt for two points which
*           define the bottom left corner and the top right corner of
*           the new graph surface: i.e.
*
*              - A -- add a point
*              - D -- delete a point
*              - X -- finish

*  Usage:
*     curse [symbol] label=?

*  ADAM Parameters:
*     SYMBOL = _INTEGER (Read)
*        The symbol number used in the MARK option.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 1.
*     LABEL = _CHAR (Read)
*        The label to be written to the screen with the LABEL option.
*
*        [The value is prompted for.]
*     JUSTIFICATION = _REAL (Read and Write)
*        The justification about the point (in the range 0.0 to 1.0).
*        Here, 0.0 means left justify the text relative to the data
*        point, 1.0 means right justify the text relative to the data
*        point, 0.5 means centre the string on the data point, other
*        values will give intermediate justifications.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.5 (i.e.
*        centre the text).
*     XCURSE = _REAL (Write)
*        The X-axis position of the last graphics cursor hit when using
*        the "X" option.
*
*        The value is written to the PONGO_XCURSE global parameter.
*     YCURSE = _REAL (Write)
*        The Y-axis position of the last graphics cursor hit when using
*        the "X" option.
*
*        The value is written to the PONGO_YCURSE global parameter.
*     XMIN = _REAL (Write)
*        The left hand edge of the world coordinate limits. The value
*        is set by the application in the zooming options. It is not
*        intended that the value be specified on the command line.
*
*        The value is written to the global parameter PONGO_XMIN.
*     XMAX = _REAL (Write)
*        The right hand edge of the world coordinate limits. The value
*        is set by the application in the zooming options. It is not
*        intended that the value be specified on the command line.
*
*        The value is written to the global parameter PONGO_XMAX.
*     YMIN = _REAL (Write)
*        The lower edge of the world coordinate limits. The value is
*        set by the application in the zooming options. It is not
*        intended that the value be specified on the command line.
*
*        The value is written to the global parameter PONGO_YMIN.
*     YMAX = _REAL (Write)
*        The upper edge of the world coordinate limits. The value is
*        set by the application in the zooming options. It is not
*        intended that the value be specified on the command line.
*
*        The value is written to the global parameter PONGO_YMAX.
*     PROJECTION = _CHAR (Read)
*        The projection that has been used to plot the data.  This is
*        explained in more detail in the section on projections.
*        Allowed values: "NONE", "TAN", "SIN", "ARC", "GLS", "AITOFF",
*        "MERCATOR" and "STG".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_PROJECTN is used. If
*        PONGO_PROJECTN is not defined, the default value "NONE" is
*        used.
*     RACENTRE = _CHAR (Read)
*        The centre of the projection in RA (i.e. the angle must be
*        specified as hh:mm:ss.sss). This parameter is only required for
*        PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_RACENTRE is used. If
*        PONGO_RACENTRE is not defined, the default value "0" is used.
*     DECCENTRE = _CHAR (Read)
*        The centre of the projection in declination (i.e. the angle
*        must be specified as dd:mm:ss.sss). This parameter is only
*        required for PROJECTION values other than "NONE".
*
*        This parameter is not specified on the command line. The value
*        of the global parameter PONGO_DECCENTRE is used. If
*        PONGO_DECCENTRE is not defined, the default value "0" is used.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-MAR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Swapped behaviour of ZOOM and EXPAND options.
*        Added contextual error report on exit.
*     3-NOV-1992 (PCTR):
*        Changed expansion/shrinking factor from 10 to 2.
*     17-JUN-1994 (PDRAPER):
*        Added check for device status.
*     1-MAY-1997 (PDRAPER):
*        Extended cursor position read to also report sky coordinates.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PONGO_PAR'        ! PONGO global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      REAL BGREAL                ! Very large REAL
      PARAMETER ( BGREAL = 1.0E+10 )

*  External References:
      EXTERNAL PON_NEAR
      INTEGER PON_NEAR           ! Gets position nearest to cursor
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! Device is opened.

*  Local Variables:
      CHARACTER * ( 1 ) SIGN     ! Sign of sexagesimal
      CHARACTER * ( 1 ) CH       ! Graphics cursor choice

      INTEGER ID                 ! [local_variable_description]
      INTEGER IDAT               ! Counter
      INTEGER NPT
      INTEGER PROJECTION         ! Projection type
      INTEGER SYMBOL             ! PGPLOT symbol marker
      INTEGER IDMSF( 4 )         ! Integer decode for HH/DD:MM:SS.SS
      INTEGER LSTAT              ! Local status

      REAL GRAD
      REAL JUST                  ! Justification
      REAL X                     ! Cursor position
      REAL XDIST
      REAL XL( 2 )
      REAL XMAXP                 ! New window setting
      REAL XMINP                 ! New window setting
      REAL XMAXW                 ! Current window setting
      REAL XMINW                 ! Current window setting
      REAL XVPMAX                ! Current VIEWPORT setting
      REAL XVPMIN                ! Current VIEWPORT setting
      REAL Y                     ! Cursor position
      REAL YDIST                 ! [local_variable_description]
      REAL YL( 2 )
      REAL YMAXP                 ! New window setting
      REAL YMINP                 ! New window setting
      REAL YMAXW                 ! Current window setting
      REAL YMINW                 ! Current window setting
      REAL YVPMAX                ! Current VIEWPORT setting
      REAL YVPMIN                ! Current VIEWPORT setting
      REAL Z1
      REAL Z2                    ! [local_variable_description]

      DOUBLE PRECISION DEC0      ! Projection centre
      DOUBLE PRECISION L
      DOUBLE PRECISION M
      DOUBLE PRECISION RA0       ! Projection centre
      DOUBLE PRECISION PHI       ! Projected coordinate
      DOUBLE PRECISION THETA     ! Projected coordinate

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  See if the device is open.
      IF ( .NOT. PON_DEVOP ( .TRUE., STATUS ) ) GO TO 30

*  Get the projection status etc.
      CALL PON_GETPROJ( 'PROJECTION', 'RACENTRE', 'DECCENTRE',
     :                  PROJECTION, RA0, DEC0, STATUS )
      CALL PAR_GET0R( 'JUSTIFICATION', JUST, STATUS )
      CALL PAR_GET0I( 'SYMBOL', SYMBOL, STATUS )

*  Set up the data to include projections properly.
      DO 10 IDAT = 1,NDAT

         IF ( PROJECTION .NE. 1 ) THEN
            LSTAT = SAI__OK
            CALL PROJ_CONVPTLM( PROJECTION-1, RA0, DEC0,
     :                          XDATA( IDAT ), YDATA( IDAT ), L, M,
     :                          LSTAT )
            IF ( LSTAT .EQ. SAI__OK ) THEN
               WORK( IDAT ) = REAL( L )
               WORK( IDAT+NDAT ) = REAL( M )
            ELSE
               WORK( IDAT ) = 0.0
               WORK( IDAT+NDAT ) = 0.0
            END IF
         ELSE
            WORK( IDAT ) = REAL( XDATA( IDAT ) )
            WORK( IDAT+NDAT ) = REAL( YDATA( IDAT ) )
         END IF
 10   CONTINUE

*  Present a table of options.
      CALL MSG_OUT( ' ', 'CURSE cursor options:', STATUS )
      CALL MSG_OUT( ' ', '   Q - Quit.', STATUS )
      CALL MSG_OUT( ' ', '   D - Draw to the cursor position.', STATUS )
      CALL MSG_OUT( ' ', '   E - Expand the plotting limits.', STATUS )
      CALL MSG_OUT( ' ', '   G - Calculate the gradient.', STATUS )
      CALL MSG_OUT( ' ', '   L - Label the plot.', STATUS )
      CALL MSG_OUT( ' ', '   M - Mark a point.', STATUS )
      CALL MSG_OUT( ' ', '   O - Left justified annotation.', STATUS )
      CALL MSG_OUT( ' ', '   P - Right justified annotation.', STATUS )
      CALL MSG_OUT( ' ', '   S - Shrink the plotting limits.', STATUS )
      CALL MSG_OUT( ' ', '   V - Move to the cursor position.', STATUS )
      CALL MSG_OUT( ' ', '   X - Get the current position.', STATUS )
      CALL MSG_OUT( ' ', '   Z - Zoom the plotting limits.', STATUS )
      CALL MSG_BLANK( STATUS )

*  Loop to get the cursor hit choice and act.
      CH = 'Z'

*  DO WHILE loop.
 20   CONTINUE
      IF ( ( ILABPTR .LT. MAXLAB )
     :     .AND. ( STATUS .EQ. SAI__OK ) ) THEN
         CALL PGCURSE( X, Y, CH )
         CALL CHR_UCASE( CH )

         IF ( CH .EQ. 'Q' ) THEN

*        Quit the cursor session.
            GO TO 30
         ELSE IF ( CH .EQ. CHAR( 0 ) ) THEN
            STATUS = SAI__ERROR
            CALL ERR_REP( 'CURSE_ERROR',
     :                    'PGPLOT cursor error - exiting CURSE.',
     :                    STATUS )
         ELSE IF ( CH .EQ. 'D' ) THEN

*        Draw to the cursor position.
            CALL PGDRAW( X, Y )
            ILABPTR = ILABPTR + 1
            LABLST( ILABPTR )( 1 : 1 ) = 'D'
            XLABAN( ILABPTR ) = X
            YLABAN( ILABPTR ) = Y
         ELSE IF ( CH .EQ. 'E' ) THEN

*        Expand the plotting limits by a factor of two.
            CALL PGQWIN( XMINW, XMAXW, YMINW, YMAXW )
            Z1 = XMAXW - XMINW
            Z2 = YMAXW - YMINW
            XMINP = X - Z1
            XMAXP = X + Z1
            YMINP = Y - Z2
            YMAXP = Y + Z2
            CALL PGWINDOW( X-Z1, X+Z1, Y-Z2, Y+Z2 )

*        Write out the new limits to the parameter system.
            CALL PAR_PUT0R( 'XMIN', XMINP, STATUS )
            CALL PAR_PUT0R( 'XMAX', XMAXP, STATUS )
            CALL PAR_PUT0R( 'YMIN', YMINP, STATUS )
            CALL PAR_PUT0R( 'YMAX', YMAXP, STATUS )

*        Inform the user of the action.
            CALL MSG_OUT( ' ',
     :                    'World coordinate limits expanded by a ' //
     :                    'factor 2.', STATUS )
         ELSE IF ( CH .EQ. 'G' ) THEN

*        Draw a line using the cursor, calculate and report its
*        gradient.
            NPT = 1
            XL( 1 ) = X
            YL( 1 ) = Y
            CALL PGNCURSE( 2, NPT, XL, YL, 5 )

            IF ( NPT .EQ. 2 ) THEN

*           Check that the difference in X is not zero.
               IF ( XL( 1 ) .EQ. XL( 2 ) ) THEN
                  CALL MSG_OUT( ' ', 'The gradient is infinite.',
     :                          STATUS )
               ELSE
                  CALL PGLINE( 2, XL, YL )
                  GRAD = ( YL( 2 ) - YL( 1 ) ) / ( XL( 2 ) - XL( 1 ) )
                  CALL MSG_SETR( 'GRAD', GRAD )
                  CALL MSG_OUT( ' ', 'Gradient = ^GRAD', STATUS )
               END IF
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CURSE_G', 'The GRADIENT option ' //
     :                       'requires two input points to complete.',
     :                       STATUS )
               CALL ERR_FLUSH( STATUS )
            END IF
         ELSE IF ( CH .EQ. 'L' ) THEN

*        Draw a label at a given angle.
            CALL MSG_OUT( ' ', 'Use the cursor to define the ' //
     :                    'position of the label.', STATUS )
            NPT = 1
            XL( 1 ) = X
            YL( 1 ) = Y
            CALL PGNCURSE( 2, NPT, XL, YL, -1 )
            CALL PGQWIN( XMINW, XMAXW, YMINW, YMAXW )
            CALL PGQVP( 0, XVPMIN, XVPMAX, YVPMIN, YVPMAX )

            IF ( NPT .EQ. 2 ) THEN

*           Check if the gradient is infinite.
               IF ( XL( 1 ) .EQ. XL( 2 ) ) THEN
                  GRAD = BGREAL
               ELSE
                  GRAD = ( YL( 2 ) - YL( 1 ) ) / ( XL( 2 ) - XL( 1 ) ) /
     :                   ( YMAXW - YMINW ) * ( XMAXW - XMINW )
                  GRAD = GRAD * ( YVPMAX - YVPMIN ) /
     :                   ( XVPMAX - XVPMIN )
               END IF
            ELSE
               GRAD = 0.0
            END IF

            ILABPTR = ILABPTR + 1
            CALL PAR_CANCL( 'LABEL', STATUS )
            CALL PAR_GET0C( 'LABEL', LABLST( ILABPTR )( 2 : ), STATUS )

*        The L at the start is a hook to allow other functions to be
*        added.
            LABLST( ILABPTR )( 1 : 1 ) = 'L'
            XLABAN( ILABPTR ) = XL( 1 )
            YLABAN( ILABPTR ) = YL( 1 )
            LABANG( ILABPTR ) = ATAN( GRAD * REAL( DR2DEG ) )
            LABJUST( ILABPTR ) = JUST
            CALL PGPTEXT( XL( 1 ), YL( 1 ), LABANG( ILABPTR ),
     :                    LABJUST( ILABPTR ),
     :                    LABLST( ILABPTR )( 2 : ) )
         ELSE IF ( CH .EQ. 'M' ) THEN

*        Mark a symbol at the current cursor position.
            CALL PGPOINT( 1, X, Y, SYMBOL )
            ILABPTR = ILABPTR + 1
            LABLST( ILABPTR )( 1 : 1 ) = 'M'
            XLABAN( ILABPTR ) = X
            YLABAN( ILABPTR ) = Y
            LABJUST( ILABPTR ) = REAL( SYMBOL )
         ELSE IF( ( CH .EQ. 'P' ) .OR. ( CH .EQ. 'O' ) ) THEN

*        Annotate the nearest data point to the current cursor position.
            IF( LABCOL .NE. 0 ) THEN

*           If a label has been read in calculate the nearest datum.
               ID = PON_NEAR( WORK, NDAT, X, Y, XDIST, YDIST )

*           Write the label to the internal list along with the
*           position, justification and angle.
               ILABPTR = ILABPTR + 1
               LABJUST(ILABPTR) = 0
               IF ( CH .EQ. 'O' ) LABJUST( ILABPTR ) = 1
               LABLST( ILABPTR )( 1 : 1 ) = 'L'
               LABLST( ILABPTR )( 2 : ) = CLABELS( ID )( : LENLAB-1 )
               XLABAN( ILABPTR ) = X
               YLABAN( ILABPTR ) = Y
               LABANG( ILABPTR ) = 0.0

*           Draw the label.
               CALL PGPTEXT( X, Y, 0.0, LABJUST( ILABPTR ),
     :                       CLABELS( ID ) )
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CURSE_NOLAB',
     :                       'No labels have been read in.', STATUS )
               CALL ERR_FLUSH( STATUS )
            END IF
         ELSE IF ( CH .EQ. 'S' ) THEN

*        Shrink the plotting limits by a factor of 2.
            CALL PGQWIN( XMINW, XMAXW, YMINW, YMAXW )
            Z1 = ( XMAXW-XMINW ) / 4.0
            Z2 = ( YMAXW-YMINW ) / 4.0
            XMINP = X - Z1
            XMAXP = X + Z1
            YMINP = Y - Z2
            YMAXP = Y + Z2
            CALL PGWINDOW( X-Z1, X+Z1, Y-Z2, Y+Z2 )

*        Write out the new plotting limits to the parameter system.
            CALL PAR_PUT0R( 'XMIN', XMINP, STATUS )
            CALL PAR_PUT0R( 'XMAX', XMAXP, STATUS )
            CALL PAR_PUT0R( 'YMIN', YMINP, STATUS )
            CALL PAR_PUT0R( 'YMAX', YMAXP, STATUS )

*        Inform the user of the action.
            CALL MSG_OUT( ' ',
     :                    'World coordinate limits reduced by a ' //
     :                    'factor 2.', STATUS )
         ELSE IF ( CH .EQ. 'V' ) THEN

*        Move the PGPLOT "pen" to the current cursor position.
            CALL PGMOVE( X, Y )
            ILABPTR = ILABPTR + 1
            LABLST( ILABPTR )( 1 : 1 ) = 'V'
            XLABAN( ILABPTR ) = X
            YLABAN( ILABPTR ) = Y
         ELSE IF ( CH .EQ. 'X' ) THEN

*        Report the current cursor position. Convert this into an
*        value in radians and RA/DEC, if using a projection.
            CALL MSG_SETR( 'X', X )
            CALL MSG_SETR( 'Y', Y )
            CALL MSG_OUT( ' ', 'Cursor position: X = ^X, Y = ^Y',
     :                    STATUS )
            IF ( PROJECTION .NE. 1 ) THEN
               LSTAT = SAI__OK
               CALL PROJ_CONVLMPT( PROJECTION - 1 , RA0, DEC0,
     :                             DBLE( X ), DBLE( Y ), PHI, THETA,
     :                             LSTAT )
               IF ( LSTAT .EQ. SAI__OK ) THEN
                  CALL MSG_SETD( 'PHI', PHI )
                  CALL MSG_SETD( 'THETA', THETA )
                  CALL MSG_OUT( ' ', '                 '//
     :        'PHI = ^PHI, THETA = ^THETA (radians)', STATUS )
                  CALL MSG_SETD( 'PHI', PHI * DR2DEG )
                  CALL MSG_SETD( 'THETA', THETA * DR2DEG )
                  CALL MSG_OUT( ' ', '                 '//
     :        'PHI = ^PHI, THETA = ^THETA (degrees)', STATUS )

                  CALL SLA_DR2TF( 1, PHI, SIGN, IDMSF )
                  CALL MSG_SETC( 'S1', SIGN )
                  CALL MSG_SETI( 'H1', IDMSF( 1 ) )
                  CALL MSG_SETI( 'M1', IDMSF( 2 ) )
                  CALL MSG_SETI( 'SEC1', IDMSF( 3 ) )
                  CALL MSG_SETI( 'SS1', IDMSF( 4 ) )

                  CALL SLA_DR2AF( 1, THETA, SIGN, IDMSF )
                  CALL MSG_SETC( 'S2', SIGN )
                  CALL MSG_SETI( 'D1', IDMSF( 1 ) )
                  CALL MSG_SETI( 'M2', IDMSF( 2 ) )
                  CALL MSG_SETI( 'SEC2', IDMSF( 3 ) )
                  CALL MSG_SETI( 'SS2', IDMSF( 4 ) )
                  CALL MSG_OUT( ' ', '                 '//
     :'RA = ^S1^H1:^M1:^SEC1.^SS1, DEC = ^S2^D1:^M2:^SEC2.^SS2',
     :                      STATUS )
               ELSE

*  Probably off plot.
                  CALL MSG_OUT( ' ', 'Invalid sky position', STATUS )
               END IF
            END IF

*        Write out the current cursor position to the parameter system.
            CALL PAR_PUT0R( 'XCURSE', X, STATUS )
            CALL PAR_PUT0R( 'YCURSE', Y, STATUS )
         ELSE IF ( CH .EQ. 'Z' ) THEN

*        Zoom the plotting limits using the cursor.
            NPT = 1
            XL( 1 ) = X
            YL( 1 ) = Y
            CALL PGNCURSE( 2, NPT, XL, YL, 2 )

            IF ( NPT .EQ. 2 ) THEN
               XMINP = XL( 1 )
               YMINP = YL( 1 )
               XMAXP = XL( 2 )
               YMAXP = YL( 2 )
               CALL PGWINDOW( XMINP, XMAXP, YMINP, YMAXP )

*           Write out the current cursor position to the parameter
*           system.
               CALL PAR_PUT0R( 'XMIN', XMINP, STATUS )
               CALL PAR_PUT0R( 'XMAX', XMAXP, STATUS )
               CALL PAR_PUT0R( 'YMIN', YMINP, STATUS )
               CALL PAR_PUT0R( 'YMAX', YMAXP, STATUS )

*           Inform the user of the action.
               CALL MSG_SETR( 'XMIN', XMINP )
               CALL MSG_SETR( 'XMAX', XMAXP )
               CALL MSG_SETR( 'YMIN', YMINP )
               CALL MSG_SETR( 'YMAX', YMAXP )
               CALL MSG_OUT( ' ',
     :                       'World coordinate limits changed to ' //
     :                       'X = (^XMIN, ^XMAX), Y = (^YMIN, ^YMAX).',
     :                       STATUS )
            ELSE
               STATUS = SAI__ERROR
               CALL ERR_REP( 'CURSE_Z', 'The ZOOM option ' //
     :                       'requires two input points to complete.',
     :                       STATUS )
               CALL ERR_FLUSH( STATUS )
            END IF
         ELSE

*        Deliver an error message and abort.
            STATUS = SAI__ERROR
            CALL MSG_SETC( 'OPTION', CH )
            CALL ERR_REP( 'CURSE_INVOPT',
     :                    '^OPTION is not a valid option.', STATUS )
            CALL ERR_FLUSH( STATUS )
         END IF

         IF ( ILABPTR .EQ. MAXLAB ) THEN
            CALL MSG_OUT( ' ', 'Internal label buffer full - use ' //
     :                    'WRITEI LABLST followed by CLEAR LABLST.',
     :                    STATUS )
        END IF
      GO TO 20
      END IF

*  Cursor end.
 30   CONTINUE

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'CURSE_END',
     :                              'CURSE: Error occurred during ' //
     :                              'interactive input.', STATUS )

      END
* $Id$
