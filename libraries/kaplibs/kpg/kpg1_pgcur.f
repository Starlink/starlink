      SUBROUTINE KPG1_PGCUR( INFO, MESS, NACT, ACTDES, KEYS, X1, X2, Y1,
     :                       Y2, EXACT, X0, Y0, MAXPNT, RBMODE, LINE,
     :                       BOX, MARK, IPLOT, X, Y, ACT, NPNT, STATUS )
*+
*  Name:
*     KPG1_PGCUR

*  Purpose:
*     Uses the cursor to get a set of points.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGCUR( INFO, MESS, NACT, ACTDES, KEYS, X1, X2, Y1, Y2,
*                      EXACT, X0, Y0, MAXPNT, RBMODE, LINE, BOX, MARK,
*                      IPLOT, X, Y, ACT, NPNT, STATUS )

*  Description:
*     This routine uses the PGPLOT cursor to get a set of positions in the
*     world co-ordinate system of the current PGPLOT window. If a position
*     is given outside the box specified by X1, X2, Y1, Y2, then a
*     warning is issued and the position is ignored. The allowable box
*     can extend outside the PGPLOT viewport (extrapolated world
*     co-ordinates are returned for positions outside the viewport).
*
*     The routine returns when any one of the following occurs:
*
*     1) The maximum number of positions have been given (see MAXPNT).
*     2) The right mouse button, "X" or "." is pressed (but only if
*        KEYS contains "X" or "."). The cursor position is not returned.
*     3) The key/button specified by EXACT is pressed. The cursor position
*        IS returned.

*  Arguments:
*     INFO = LOGICAL (Given)
*        Display information describing the available actions before
*        getting the first position?
*     MESS = CHARACTER * ( * ) (Given)
*        The purpose for using the cursor. Eg "select 2 points". May be
*        blank.
*     NACT = INTEGER (Given)
*        The number of available actions. Ignored if INFO is .FALSE.
*     ACTDES( NACT ) = CHARACTER * ( * ) (Given)
*        Short descriptions of each action. Ignored if INFO is .FALSE.
*        Examples: "select a point", "exit", "mark a star". etc.
*     KEYS = CHARACTER * ( * ) (Given)
*        A string of NACT unique characters. These are the keyboard keys
*        which must be pressed to select the corresponding action. Note, case
*        is insignificant, but trailing spaces are significant. The left,
*        middle and right mouse buttons are represented by the upper case
*        characters A, D and X respectively (this is imposed by PGPLOT). A
*        dot (".") is considered equivalent to an X (i.e. the right mouse
*        button). In addition, due to problems in the GKS version of PGPLOT,
*        a space (" ") is considered equivalent to an A (i.e. left mouse
*        button).
*
*        The "X" and "." keys (or equivalently the right mouse button) are
*        special in that (if they are included in KEYS) they cause the routine
*        to exit without adding the cursor position to the list of returned
*        positions. If KEYS includes neither "X" nor ".", then presses of
*        "X", "." or the right mouse button are ignored.
*     X1 = REAL (Given)
*        World co-ord X at lower-left corner of region in which positons
*        may be entered.
*     X2 = REAL (Given)
*        World co-ord X at upper-right corner of region in which positons
*        may be entered. If X1 and X2 are equal then no restrictions are
*        placed on the region in which positions may be given.
*     Y1 = REAL (Given)
*        World co-ord Y at lower-left corner of region in which positons
*        may be entered.
*     Y2 = REAL (Given)
*        World co-ord Y at upper-right corner of region in which positons
*        may be entered. If Y1 and Y2 are equal then no restrictions are
*        placed on the region in which positions may be given.
*     EXACT = REAL (Given)
*        The index of an exit action. If the corresponding key/button press
*        is made, then the cursor position is added to the list of returned
*        positions and the routine then exits. Zero can be supplied if this
*        facility is not required. Note, if KEYS contains "X" or "." then
*        the routine also exits (WITHOUT adding the cursor position to the
*        returned list) if "X", "." or the right mouse button is pressed.
*     X0 = REAL (Given )
*        The X world co-ordinate of the initial cursor position.
*        Ignored if VAL__BADR.
*     Y0 = REAL (Given )
*        The Y world co-ordinate of the initial cursor position.
*        Ignored if VAL__BADR.
*     MAXPNT = INTEGER (Given)
*        The maximum number of positions which can be given by the user
*        before exiting.
*     RBMODE = INTEGER (Given)
*        The form of the rubber band which connects the cursor to the
*        previous position. Rubber bands are not available when using the
*        GKS version of PGPLOT:
*           0 - do not use a rubber band.
*           1 - use a straight-line rubber band.
*           2 - use a horizontal box rubber band.
*     LINE = INTEGER (Given)
*        Specifies lines to be drawn as follows:
*           -1: Join adjacent points and do not close the polygon.
*            0: Do not draw any lines.
*            1: Join adjacent points and close the polygon.
*            2: Draw a vertical line between y1 and y2 (or the height of
*               the window if y1=y2).
*            3: Draw a horizontal line between x1 and x2 (or the width of
*               the window if x1=x2).
*        The plotting attributes are specified by the CURVES(...) attributes
*        of the supplied Plot (see IPLOT).
*     BOX = INTEGER (Given)
*        If non-zero then a horizontal box is drawn between each position.
*        The plotting attributes are specified by the BORDER(...) attributes
*        of the supplied Plot (see IPLOT).
*     MARK = INTEGER (Given)
*        If -31 or larger, then a marker is drawn at each position. The
*        type of marker is given by the specific value (see PGPLOT routine
*        PGPT). The plotting attributes are specified by the MARKERS(...)
*        attributes of the supplied Plot (see IPLOT).
*     IPLOT = INTEGER (Given)
*        Defines the plotting styles for any graphics (see LINE, BOX and
*        MARK). If AST__NULL is supplied, ther current PGPLOT attributes
*        are used for all graphics.
*     X( MAXPNT ) = REAL (Returned)
*        Elements 1 to NPNT hold the selected X positions.
*     Y( MAXPNT ) = REAL (Returned)
*        Elements 1 to NPNT hold the selected X positions.
*     ACT( MAXPNT ) = INTEGER (Returned)
*        Elements 1 to NPNT hold the indices of the actions for each
*        selected point. In range 1 to NACT.
*     NPNT = INTEGER (Returned)
*        The number of positions given by the user (this includes the
*        position at which the action specified by EXACT was pressed -
*        if it was. It does not include the position at which "X", "." or
*        the right mouse button was pressed).
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998, 2000 Central Laboratory of the Research Councils.
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
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
*     02110-1301, USA.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-AUG-1998 (DSB):
*        Original version.
*     10-JAN-2000 (DSB):
*        Added argument IPLOT.
*     19-JAN-2000 (DSB):
*        Allow horizontal and vertical lines to be drawn.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL__ constants
      INCLUDE 'AST_PAR'          ! AST__ constants

*  Arguments Given:
      LOGICAL INFO
      CHARACTER MESS*(*)
      INTEGER NACT
      CHARACTER ACTDES( NACT )*(*)
      CHARACTER KEYS*(*)
      REAL X1
      REAL Y1
      REAL X2
      REAL Y2
      INTEGER EXACT
      REAL X0
      REAL Y0
      INTEGER MAXPNT
      INTEGER RBMODE
      INTEGER LINE
      INTEGER BOX
      INTEGER MARK
      INTEGER IPLOT

*  Arguments Returned:
      REAL X( MAXPNT )
      REAL Y( MAXPNT )
      INTEGER ACT( MAXPNT )
      INTEGER NPNT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      INTEGER PGBAND             ! Zero if an error occurred

*  Local Variables:
      CHARACTER CG*1             ! Terminator character
      CHARACTER KEY*1            ! Current key
      CHARACTER LKEYS*50         ! Lower case version of KEYS
      CHARACTER TEXT*128         ! Action desription
      CHARACTER VAL*1            ! PGPLOT information string
      DOUBLE PRECISION ATTR( 20 )! Saved graphics attribute values
      INTEGER I                  ! Loop counter
      INTEGER IACT               ! Index of terminator key in KEYS
      INTEGER IAT                ! No. of characters in string
      INTEGER LRBMOD             ! Rubber band mode
      INTEGER NPL                ! Number of graphical elements requested
      INTEGER OK                 ! Zero if no position was obtained
      INTEGER POSN               ! Zero if no acnhor position is available
      INTEGER VLEN               ! Length of PGPLOT information string
      LOGICAL LOOP               ! Get another position?
      LOGICAL RESTR              ! Restrict position to X1:X2,Y1:Y2 ?
      LOGICAL RESATT             ! Reset plotting attributes each time?
      REAL LX1                   ! X at left end of line to be drawn
      REAL LX2                   ! X at right end of line to be drawn
      REAL LY1                   ! Y at upper end of line to be drawn
      REAL LY2                   ! Y at lower end of line to be drawn
      REAL XG                    ! X at given position
      REAL XMAX                  ! Maximum acceptable X at given position
      REAL XMIN                  ! Minimum acceptable X at given position
      REAL YG                    ! Y at given position
      REAL YMAX                  ! Maximum acceptable Y at given position
      REAL YMIN                  ! Minimum acceptable Y at given position
*.

*  Check the inherited status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Report an error and abort if the length of the KEYS string is not the
*  same as the number of actions.
      IF( LEN( KEYS ) .NE. NACT ) THEN
         STATUS = SAI__ERROR
         CALL MSG_SETC( 'KEYS', KEYS )
         CALL MSG_SETI( 'NACT', NACT )
         CALL ERR_REP( 'KPG1_PGCUR_1', 'KPG1_PGCUT: Programming error'//
     :                 ' - KEYS argument (^KEYS) should have ^NACT '//
     :                 'characters.', STATUS )
         GO TO 999
      END IF

*  Report an error and abort if there is no cursor available on the
*  current device.
*  Is there a cursor?
      CALL PGQINF( 'CURSOR', VAL, VLEN )
      IF( VLEN .EQ. 0 .OR. VAL( 1 : 1 ) .NE. 'Y' ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'KPG1_PGCUR_1', 'Current workstation does '//
     :                 'not have a cursor.', STATUS )
         GO TO 999
      END IF

*  Convert KEYS to lower case.
      LKEYS = KEYS
      CALL CHR_LCASE( LKEYS )

*  Translate equivalent action characters into their PGPLOT equivalents.
      DO I = 1, NACT
         KEY = LKEYS( I : I )

         IF( KEY .EQ. ' ' ) THEN
            LKEYS( I : I ) = 'a'
         ELSE IF( KEY .EQ. '.' ) THEN
            LKEYS( I : I ) = 'x'
         END IF

      END DO

*  If required, display the action descriptions.
      IF( INFO ) THEN

*  Put out a blank line to ensure the commentary appears on the alpha
*  plane of the terminal.
         CALL MSG_BLANK( STATUS )

*  If supplied, display the purpose text.
         IF( MESS .NE. ' ' ) THEN
            CALL MSG_SETC( 'MES', MESS )
            CALL MSG_OUT( 'KPG1_PGCUR_2', '  Use the cursor to ^MES.',
     :                    STATUS )
         END IF

*  Loop round each action.
         DO I = 1, NACT
            TEXT = '    To '
            IAT = 7
            CALL CHR_APPND( ACTDES( I ), TEXT, IAT )

            KEY = LKEYS( I : I )

            IF( KEY .EQ. 'a' .OR. KEY .EQ. ' ' ) THEN
               CALL CHR_APPND( ' press the space bar or left mouse '//
     :                         'button', TEXT, IAT )

            ELSE IF( KEY .EQ. 'd' ) THEN
               CALL CHR_APPND( ' press "d" or the middle mouse button',
     :                         TEXT, IAT )

            ELSE IF( KEY .EQ. 'x' .OR. KEY .EQ. '.' ) THEN

               CALL CHR_APPND( ' press "." or the right mouse '//
     :                         'button', TEXT, IAT )

            ELSE
               CALL CHR_APPND( ' press "', TEXT, IAT )
               CALL CHR_APPND( KEY, TEXT, IAT )
               CALL CHR_APPND( '"', TEXT, IAT )
            END IF

            CALL MSG_OUT( 'KPG1_PGCUR_3', TEXT, STATUS )

         END DO

         CALL MSG_BLANK( STATUS )

      END IF

*  Set a flag if we have a good initial position.
      IF( X0 .NE. VAL__BADR .AND. Y0 .NE. VAL__BADR ) THEN
         XG = X0
         YG = Y0
         POSN = 1
      ELSE
         POSN = 0
      END IF

*  See if positions must be supplied in region X1:X2, Y1:Y2.
      IF( X1 .NE. X2 .AND. Y1 .NE. Y2 ) THEN
         RESTR = .TRUE.
         XMIN = MIN( X1, X2 )
         XMAX = MAX( X1, X2 )
         YMIN = MIN( Y1, Y2 )
         YMAX = MAX( Y1, Y2 )
      ELSE
         RESTR = .FALSE.
      END IF

*  If a Plot was supplied we need to establish the require PGPLOT plotting
*  attributes.
      IF( IPLOT .NE. AST__NULL ) THEN

*  First count the number  of plotting elements being used.
         NPL = 0
         IF( LINE .NE. 0 ) NPL = NPL + 1
         IF( BOX .NE. 0 ) NPL = NPL + 1
         IF( MARK .GT. -32 ) NPL = NPL + 1

*  If only a single plotting element is being used, we can establish the
*  plotting attributes now, and then leave them alone.
         IF( NPL .EQ. 1 ) THEN
            RESATT = .FALSE.

            IF( LINE .NE. 0  ) THEN
               CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR, STATUS )
            ELSE IF( BOX .NE. 0 ) THEN
               CALL KPG1_PGSTY( IPLOT, 'BORDER', .TRUE., ATTR, STATUS )
            ELSE
               CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR, STATUS )
            END IF

*  If there are two or more graphical elements to be drawn, we need to
*  reset the plotting attributes before drawing each one.
         ELSE IF( NPL .GT. 1 ) THEN
            RESATT = .TRUE.

*  If there are no graphical elements to be drawn, we do not need to
*  reset the plotting attributes.
         ELSE
            RESATT = .FALSE.
         END IF

*  If no Plot was supplied, we do not reset the plotting attributes
*  before drawing each graphical element.
      ELSE
         RESATT = .FALSE.
      END IF

*  Get the length of any required lines.
      IF( LINE .EQ. 2 ) THEN
         IF( Y1 .EQ. Y2 ) THEN
            CALL PGQWIN( LX2, LX2, LY1, LY2 )
         ELSE
            LY1 = Y1
            LY2 = Y2
         END IF
      ELSE IF( LINE .EQ. 3 ) THEN
         IF( X1 .EQ. X2 ) THEN
            CALL PGQWIN( LX2, LX2, LY1, LY2 )
         ELSE
            LX1 = X1
            LX2 = X2
         END IF
      END IF

*  Initialise the number of positions stored so far.
      NPNT = 0

*  Initialize the rubber band mode to "none" for the first point.
      LRBMOD = 0

*  Loop round getting positions.
      LOOP = .TRUE.
      DO WHILE( LOOP .AND. STATUS .EQ. SAI__OK )

*  Get a position using a straight line rubber band.
         IF( LRBMOD .EQ. 1 ) THEN
            IF( POSN .NE. 0 ) THEN
               OK = PGBAND( 1, POSN, XG, YG, XG, YG, CG )
            ELSE
               OK = PGBAND( 0, 0, 0.0, 0.0, XG, YG, CG )
            END IF

*  Get a position using a rectangular box rubber band.
         ELSE IF( LRBMOD .EQ. 2 ) THEN
            IF( POSN .NE. 0 ) THEN
               OK = PGBAND( 2, POSN, XG, YG, XG, YG, CG )
            ELSE
               OK = PGBAND( 0, 0, 0.0, 0.0, XG, YG, CG )
            END IF

*  Get a position using no rubber band.
         ELSE
            OK = PGBAND( 0, POSN, XG, YG, XG, YG, CG )
         END IF

*  From now on use the requested rubber band mode.
         LRBMOD = RBMODE

*  We now have an anchor point (the position just entered).
         POSN = 1

*  The GKS version of PGPLOT has a few oddities reqgarding mouse buttons.
*  Native PGPLOT returns A, D or X if the left, middle or right mouse
*  button is pressed, without error. But GKS PGPLOT returns, CHAR(32),
*  CHAR(13) and CHAR(0) instead. Further, an error is reported if the right
*  mouse button is pressed...

*  See if PGPLOT reported an error.
         CALL ERR_STAT( STATUS )

*  If an error was reported, and CHAR(0) as returned, annull the error
*  and assume the right mouse button was pressed.
         IF( STATUS .NE. SAI__OK .AND. ICHAR( CG ) .EQ. 0 ) THEN
            CALL ERR_ANNUL( STATUS )
            CG = 'X'

*  If a CHAR(13) was returned assume the middle mouse button was pressed.
         ELSE IF( ICHAR( CG ) .EQ. 13 ) THEN
            CG = 'D'

*  If a CHAR(32) was returned assume the left mouse button was pressed.
         ELSE IF( ICHAR( CG ) .EQ. 32 ) THEN
            CG = 'A'

*  In addition, if the Starlink standard "exit" key (".") was returned
*  assume the right mouse button was pressed.
         ELSE IF( CG .EQ. '.' ) THEN
            CG = 'X'

         END IF

*  Convert the choice character to lower case.
         CALL CHR_LCASE( CG )

*  If a right mouse button, an "." or an "x" was given, then the loop is
*  terminated if "x" or "." was supplied as an action key (the key/button
*  press is ignored otherwise).
         IF( CG .EQ. 'x' ) THEN
            OK = 0
            LOOP = ( INDEX( LKEYS, 'x' ) .EQ. 0 )
         END IF

*  If required, check that the position is within the acceptable region.
         IF( OK .NE. 0 .AND. RESTR ) THEN
            IF( XG .LT. XMIN .OR. XG .GT. XMAX .OR.
     :          YG .LT. YMIN .OR. YG .GT. YMAX ) THEN
               CALL MSG_OUT( 'KPG1_PGCUR_4', 'Point lies outside the '//
     :                       'allowed region.', STATUS )
               OK = 0
            END IF
         END IF

*  If the point was obtained succesfully...
         IF( OK .NE. 0 ) THEN

*  Check thst the (lower case) terminator was recognised.
            IACT = INDEX( LKEYS, CG )
            IF( IACT .NE. 0 ) THEN

*  Store the position in the returned arrays.
               NPNT = NPNT + 1
               X( NPNT ) = XG
               Y( NPNT ) = YG
               ACT( NPNT ) = IACT

*  If this is the terminator action, or if the maximum number of
*  positions have been supplied, leave the loop.
               IF( IACT .EQ. EXACT .OR. NPNT .EQ. MAXPNT ) THEN
                  LOOP = .FALSE.
               END IF

*  Draw a line through this position if required. Set up the plotting
*  attributes first if required.
               IF( LINE .NE. 0 ) THEN
                  IF( RESATT ) THEN
                     CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR,
     :                                STATUS )
                  END IF

*  If required, draw a line joining this point to the previous point, but
*  only if this is not the first point.
                  IF( ( LINE .EQ. -1 .OR. LINE .EQ. 1 ) .AND.
     :                NPNT .GT. 1 ) THEN
                     CALL PGMOVE( X( NPNT - 1 ), Y( NPNT - 1 ) )
                     CALL PGDRAW( XG, YG )

*  Otherwise, draw a vertical line if required.
                  ELSE IF( LINE .EQ. 2 ) THEN
                     CALL PGMOVE( XG, LY1 )
                     CALL PGDRAW( XG, LY2 )

*  Otherwise, draw a horizontal line if required.
                  ELSE IF( LINE .EQ. 3 ) THEN
                     CALL PGMOVE( LX1, YG )
                     CALL PGDRAW( LX2, YG )

                  END IF

*  R-instate the original drawing attributes if something else is to be
*  drawn.
                  IF( RESATT ) THEN
                     CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE.,
     :                                ATTR, STATUS )
                  END IF

               END IF

*  Draw a box from this position to the previous position if required.
*  Establish the plotting attributes first if required.
               IF( BOX .NE. 0 .AND. NPNT .GT. 1 ) THEN

                  IF( RESATT ) THEN
                     CALL KPG1_PGSTY( IPLOT, 'BORDER', .TRUE., ATTR,
     :                                STATUS )
                  END IF

                  CALL PGMOVE( XG, YG )
                  CALL PGDRAW( X( NPNT - 1 ), YG )
                  CALL PGDRAW( X( NPNT - 1 ), Y( NPNT - 1 ) )
                  CALL PGDRAW( XG, Y( NPNT - 1 ) )
                  CALL PGDRAW( XG, YG )

                  IF( RESATT ) THEN
                     CALL KPG1_PGSTY( IPLOT, 'BORDER', .FALSE., ATTR,
     :                                STATUS )
                  END IF

               END IF

*  Draw a mrker if required, establishing the plotting charactersistics
*  first if required.
               IF( MARK .GT. -32 ) THEN

                  IF( RESATT ) THEN
                     CALL KPG1_PGSTY( IPLOT, 'MARKERS', .TRUE., ATTR,
     :                                STATUS )
                  END IF

                  CALL PGPT( 1, XG, YG, MARK )

                  IF( RESATT ) THEN
                     CALL KPG1_PGSTY( IPLOT, 'MARKERS', .FALSE., ATTR,
     :                                STATUS )
                  END IF

               END IF

            END IF

         END IF

*  If we have finished, close the polygon if required.
         IF( .NOT. LOOP .AND. LINE .GT. 0 ) THEN
            IF( NPNT .GT. 2 ) THEN
               IF( RESATT ) THEN
                  CALL KPG1_PGSTY( IPLOT, 'CURVES', .TRUE., ATTR,
     :                             STATUS )
               END IF

               CALL PGMOVE( X( 1 ), Y( 1 ) )
               CALL PGDRAW( X( NPNT ), Y( NPNT ) )

               IF( RESATT ) THEN
                  CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR,
     :                             STATUS )
               END IF

            END IF
         END IF

      END DO

*  If a Plot was supplied we may need to re-establish the original PGPLOT
*  plotting attributes.
      IF( IPLOT .NE. AST__NULL .AND. .NOT. RESATT ) THEN
         IF( LINE .NE. 0 ) THEN
            CALL KPG1_PGSTY( IPLOT, 'CURVES', .FALSE., ATTR, STATUS )
         ELSE IF( BOX .NE. 0 ) THEN
            CALL KPG1_PGSTY( IPLOT, 'BORDER', .FALSE., ATTR, STATUS )
         ELSE IF( MARK .GT. -32 ) THEN
            CALL KPG1_PGSTY( IPLOT, 'MARKERS', .FALSE., ATTR, STATUS )
         END IF
      END IF

 999  CONTINUE

      END

