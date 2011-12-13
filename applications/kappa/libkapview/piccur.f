      SUBROUTINE PICCUR ( STATUS )
*+
*  Name:
*     PICCUR

*  Purpose:
*     Uses a graphics cursor to change the current picture.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Invocation:
*     CALL PICCUR( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     This application allows you to select a new current picture in
*     the graphics database using the cursor. Each time a position is
*     selected (usually by pressing a button on the mouse), details of
*     the topmost picture in the AGI database which encompasses that
*     position are displayed, together with the cursor position (in
*     millimetres from the bottom left corner of the graphics device).
*     On exit the last picture selected becomes the current picture.

*  Usage:
*     piccur [device] [name]

*  ADAM Parameters:
*     DEVICE = DEVICE (Read)
*        The graphics workstation. [The current graphics device]
*     NAME = LITERAL (Read)
*        Only pictures of this name are to be selected.  For instance, if
*        you want to select a DATA picture which is covered by a
*        transparent FRAME picture, then you could specify NAME=DATA.
*        A null (!) or blank string means that pictures of all names may
*        be selected. [!]
*     SINGLE = _LOGICAL (Read)
*        If TRUE then the user can supply only one position using the
*        cursor, where-upon the application immediately exits, leaving
*        the selected picture as the current picture. If FALSE is supplied,
*        then the user can supply multiple positions. Once all positions
*        have been supplied, a button is pressed to indicate that no more
*        positions are required. [FALSE]

*  Examples:
*     piccur
*        This selects a picture on the current graphics device by use of
*        the cursor.  The picture containing the last-selected point becomes
*        the current picture.
*     piccur name=data
*        This is like the previous example, but only DATA pictures can be
*        selected.

*  Notes:
*     -  Nothing is displayed on the screen when the message filter
*     environment variable MSG_FILTER is set to QUIET.

*  Related Applications:
*     KAPPA: CURSOR, PICBASE, PICDATA, PICEMPTY, PICENTIRE, PICFRAME,
*     PICLIST, PICSEL, PICVIS.

*  Copyright:
*     Copyright (C) 2001, 2004 Central Laboratory of the Research
*     Councils.
*     Copyright (C) 2009 Science and Technology Facilities Council.
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
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     27-SEP-2001 (DSB):
*        Re-write for AST/PGPLOT.
*     2004 September 3 (TIMJ):
*        Use CNF_PVAL.
*     2009 July 24 (MJC):
*        Remove QUIET parameter and use the current reporting level
*        instead (set by the global MSG_FILTER environment variable).
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! HDS constants
      INCLUDE 'AST_PAR'          ! AST constants
      INCLUDE 'PAR_ERR'          ! Parameter-system errors
      INCLUDE 'AGI_ERR'          ! AGI error constants
      INCLUDE 'MSG_PAR'          ! Message-system constants

*  Status:
      INTEGER STATUS             ! Global status

*  Local Constants:
      INTEGER SZNAM              ! Length of picture name
      PARAMETER ( SZNAM = 15 )

*  Local Variables:
      CHARACTER AMES( 3 )*40     ! Informational messages about use of cursor
      CHARACTER COMENT*256       ! Comment for the latest picture
      CHARACTER KEYS*3           ! Keys which activate each cursor action
      CHARACTER LABEL*( SZNAM )  ! Picture label
      CHARACTER LINE*256         ! Text buffer for screen
      CHARACTER NAME*( DAT__SZNAM ) ! Name of pictures which can be selected
      CHARACTER PNAME*( DAT__SZNAM )! Name for the latest picture
      CHARACTER PURP*80          ! Purpose for using cursor
      DOUBLE PRECISION XB        ! Cursor X position in BASE world co-ords
      DOUBLE PRECISION YB        ! Cursor Y position in BASE world co-ords
      INTEGER ACT                ! Cursor choice
      INTEGER BMAP               ! GRAPHICS to BASE world co-ords Mapping
      INTEGER EXACT              ! Index of action to leave immediately
      INTEGER IAT                ! No. of characters in the string
      INTEGER IPIC               ! AGI id for current picture
      INTEGER IPIC0              ! Current (input) picture identifier
      INTEGER IPIC2              ! AGI id for new picture
      INTEGER IPICB              ! BASE picture identifier
      INTEGER IPLOTB             ! Plot for BASE picture
      INTEGER NACT               ! No. of cursor actions
      INTEGER NPNT               ! No. of cursor positions supplied
      LOGICAL INFO               ! Display mouse instructions?
      LOGICAL LOOP               ! Continue to get a new cursor position?
      LOGICAL QUIET              ! Run quietly?
      LOGICAL SINGLE             ! Process a single cursor selecton?
      REAL X1                    ! PGPLOT X world coord at bottom left
      REAL X2                    ! PGPLOT X world coord at top right
      REAL XC                    ! PGPLOT X world coord at current cursor posn
      REAL Y1                    ! PGPLOT Y world coord at bottom left
      REAL Y2                    ! PGPLOT Y world coord at top right
      REAL YC                    ! PGPLOT Y world coord at current cursor posn

*.

*  Check the inherited global status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Start a new AST context.
      CALL AST_BEGIN( STATUS )

*  See if only one position is to be supplied.
      CALL PAR_GET0L( 'SINGLE', SINGLE, STATUS )

*  See if we are to run quietly, i.e not at NORMAL or lower priority.
      QUIET = .NOT. MSG_FLEVOK( MSG__NORM, STATUS )

*  Get the NAME parameter.  A null value is made equivalent to a blank
*  string, i.e. all pictures of any name may be selected.
      CALL PAR_GET0C( 'NAME', NAME, STATUS )
      IF( STATUS .EQ. PAR__NULL ) THEN
         CALL ERR_ANNUL( STATUS )
         NAME = ' '
      END IF

*  Open the graphics device for plotting with PGPLOT, obtaining an
*  identifier for the current AGI picture.
      CALL KPG1_PGOPN( 'DEVICE', 'UPDATE', IPIC0, STATUS )

*  Get the the AGI identifier and AST Plot associated with the BASE picture.
*  The Base Frame in this Plot is GRAPHICS co-ordinates (millimetres from
*  the bottom left corner of the view surface), and the Current Frame is
*  AGI world co-ordinates in the BASE picture. The PGPLOT viewport is set
*  to match the BASE picture and PGPLOT world co-ordinates within the
*  viewport are GRAPHICS co-ordinates. The BASE picture becomes the
*  current picture in KPG1_GDGET.
      CALL AGI_IBASE( IPICB, STATUS )
      CALL KPG1_GDGET( IPICB, AST__NULL, .FALSE., IPLOTB, STATUS )

*  Get the Mapping from GRAPHICS co-ordinates to AGI world co-ordinates in
*  the BASE picture.
      BMAP = AST_GETMAPPING( IPLOTB, AST__BASE, AST__CURRENT, STATUS )

*  The cursor will be positioned initially at the centre of the screen.
*  Get the bounds the PGPLOT window, and store the mid position.
      CALL PGQWIN( X1, X2, Y1, Y2 )
      XC = 0.5*( X1 + X2 )
      YC = 0.5*( Y1 + Y2 )

*  Store the instructions for the allowed actions. Also store the keys
*  which must be pressed to activate the corresponding action.
      IF( SINGLE ) THEN
         NACT = 1
         KEYS = ' '
         AMES( 1 ) = 'select a picture,'
         EXACT = 1
      ELSE
         NACT = 3
         KEYS = ' D.'
         AMES( 1 ) = 'select a picture,'
         AMES( 2 ) = 'quit re-instating original picture,'
         AMES( 3 ) = 'quit retaining the new picture,'
         EXACT = 2
      END IF

*  Store the purpose of using the cursor.
      PURP = 'select a new picture'

*  Loop until no more positions are required, or an error occurs.
      LOOP = .TRUE.
      INFO = ( .NOT. QUIET )
      DO WHILE( LOOP .AND. STATUS .EQ. SAI__OK )

*  Get a position using the cursor, in PGPLOT world co-ordinates. This
*  corresponds to the GRAPHICS Frame of the current Plot (i.e. millimetres
*  from the bottom left corner of the view surface). The positions which
*  may be selected are unrestricted and may be given anywhere on the
*  view surface.
         CALL KPG1_PGCUR( INFO, PURP, NACT, AMES, KEYS( : NACT ), 0.0,
     :                    0.0, 0.0, 0.0, EXACT, XC, YC, 1, 0, 0,
     :                    0, -32, AST__NULL, XC, YC, ACT, NPNT, STATUS )

*  Indicate that instructions on the use of the mouse should not be
*  displayed again.
         INFO = .FALSE.

*  If the third action ("quit retaining new picture") was performed,
*  just leave the loop.
         IF( NPNT .EQ. 0 ) THEN
            LOOP = .FALSE.

*  If the first action ("select a picture") was performed, find the
*  required picture.
         ELSE IF( ACT .EQ. 1 ) THEN

*  There is now a change from the graphics cursor operation to report
*  values on the text screen (assuming the device is a terminal).  In
*  order for the message to appear in the correct plane, there must be
*  a delay, so that the graphics system can complete its work before
*  the (faster and independent) message system reports the cursor
*  position.  The following calls achieves this synchronisation.
            CALL MSG_SYNC( STATUS )

*  Display the graphics coordinates at the cursor.
            IF( .NOT. QUIET ) THEN
               CALL MSG_BLANK( STATUS )
               LINE = ' '
               IAT = 0
               CALL CHR_APPND( 'Position: X =', LINE, IAT )
               IAT = IAT + 1
               CALL CHR_PUTI( NINT( XC ), LINE, IAT )
               CALL CHR_APPND( 'mm  Y =', LINE, IAT )
               IAT = IAT + 1
               CALL CHR_PUTI( NINT( YC ), LINE, IAT )
               CALL CHR_APPND( 'mm', LINE, IAT )
               CALL MSG_OUT( ' ', LINE( : IAT ), STATUS )
            END IF

*  Transform the cursor position from GRAPHICS co-ordinates into the AGI
*  world co-ordinate system of the BASE picture.
            CALL AST_TRAN2( BMAP, 1, DBLE( XC ), DBLE( YC ), .TRUE.,
     :                      XB, YB, STATUS )

*  Ensure the Base picture is the current picture.
            CALL AGI_SELP( IPICB, STATUS )

*  Get the last picture of the chosen name which encompasses the cursor
*  position. If found it becomes the current AGI picture.
            CALL AGI_RCLP( NAME, REAL( XB ), REAL( YB ), IPIC2, STATUS )

*  Watch for the case when there is no picture of that name at the
*  selected position. Annul the error and issue a warning, select the
*  original current picture.
            IF( STATUS .EQ. AGI__NONAM ) THEN
               CALL ERR_ANNUL( STATUS )
               CALL MSG_SETC( 'NAME', NAME )
               CALL MSG_OUT( ' ', 'There is no ^NAME picture at '//
     :                       'the given position.', STATUS )
               CALL AGI_SELP( IPIC0, STATUS )

*  Report details of the picture, unless we are running quiet.
            ELSE IF( .NOT. QUIET ) THEN

*  Obtain details of the new picture.
               CALL AGI_ICOM( COMENT, STATUS )
               CALL AGI_INAME( PNAME, STATUS )
               CALL AGI_ILAB( IPIC, LABEL, STATUS )

* Construct a message giving these details.
               LINE = ' '
               IAT = 0

               CALL CHR_APPND( 'Picture comment:', LINE, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( COMENT, LINE, IAT )

               CALL CHR_APPND( ', name:', LINE, IAT )
               IAT = IAT + 1
               CALL CHR_APPND( PNAME, LINE, IAT )

               IF( LABEL( 1 : 1 ) .NE. ' ' ) THEN
                  CALL CHR_APPND( ', label:', LINE, IAT )
                  IAT = IAT + 1
                  CALL CHR_APPND( LABEL, LINE, IAT )
               END IF

*  Display this line.
               CALL MSG_OUT( ' ', LINE( : IAT ), STATUS )

            END IF

*  If we are in singkle mode, leave the loop.
            IF( SINGLE ) LOOP = .FALSE.

*  If the second action ("quit retaining original picture") was performed,
*  re-instate the original picture.
         ELSE IF( ACT .EQ. 2 ) THEN
            CALL AGI_SELP( IPIC0, STATUS )
            LOOP = .FALSE.

         END IF

      END DO

      IF( .NOT. QUIET ) CALL MSG_BLANK( STATUS )

*  Shutdown PGPLOT and the graphics database, retaining the current picture.
      CALL KPG1_PGCLS( 'DEVICE', .TRUE., STATUS )

*  End the AST context.
      CALL AST_END( STATUS )

*  Give a contextual error message if anything went wrong.
      IF( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'PICCUR_ERR', 'PICCUR: Failed to select '//
     :                 'a picture using a graphics cursor.', STATUS )
      END IF

      END
