      SUBROUTINE CHANGE( STATUS )
*+
*  Name:
*     CHANGE

*  Purpose:
*     Change plotting attributes.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     Change the PGPLOT plotting attributes: e.g. line style, pen
*     colour etc. Several of the attributes can be changed at the same
*     time. Each of the parameters is remembered from the last
*     invocation of CHANGE: after BEGPLOT has been run, a single
*     invocation of CHANGE can be used to reset the plotting attributes
*     to their values the last time PONGO was used.

*  Usage:
*     change

*  ADAM Parameters:
*     ANGLE = _REAL (Read)
*       The angle hatch lines make with horizontal, in degrees.
*       Only used when FILLSTY is 3.
*       [45.0]
*     COLOUR = _INTEGER (Read and Write)
*        The pen number (colour index) PGPLOT uses for plotting. The
*        value should be between 0 and 255. Usually the first 16 pens
*        are predefined to have the following colours:
*
*           - 0 -- background,
*           - 1 -- foreground (default),
*           - 2 -- red,
*           - 3 -- green,
*           - 4 -- blue,
*           - 5 -- cyan,
*           - 6 -- magenta,
*           - 7 -- yellow,
*           - 8 -- red + yellow (orange),
*           - 9 -- green + yellow,
*           - 10 -- green + cyan,
*           - 11 -- blue + cyan,
*           - 12 -- blue + magenta,
*           - 13 -- red + magenta,
*           - 14 -- dark grey,
*           - 15 -- light grey.
*
*        It is possible to change the colour representation of any of
*        the pen colour indices using the PALETTE application.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is intially set to 1 (i.e.
*        foreground).
*        [1]
*     CHEIGHT = _REAL (Read and Write)
*        The character height scaling. This parameter scales the
*        default character height and also alters the size of the tick
*        marks and symbols that PGPLOT plots. The default character
*        height in PGPLOT is about 1/40 of the viewport height.
*
*        [The value of the global parameter PONGO_CHEIGHT is used. If
*        PONGO_CHEIGHT is not defined, the default value 1.0 is used.]
*     FONT = _INTEGER (Read)
*        The font used by PGPLOT. The styles for each font are as
*        follows:
*
*           - 1 -- single-stroke font (default),
*           - 2 -- roman font,
*           - 3 -- italic font,
*           - 4 -- script font.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 1 (i.e.
*        single-stroke font).
*        [1]
*     FILLSTY = _INTEGER (Read)
*        The fill style used by PGPLOT. The fill styles are as follows:
*
*           - 1 -- solid fill,
*           - 2 -- hollow fill,
*           - 3 -- hatched,
*           - 4 -- cross-hatched
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 2 (i.e.
*        hollow fill).
*        [2]
*     LINESTY = _INTEGER (Read)
*        The line style used by PGPLOT.  The line style may be one of
*        the following:
*
*           - 1 -- full line (default),
*           - 2 -- dashed,
*           - 3 -- dot-dash-dot-dash,
*           - 4 -- dotted,
*           - 5 -- dash-dot-dot-dot.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 1 (i.e.
*        full line).
*        [1]
*     LINEWID = _INTEGER (Read)
*        The line width scaling. This parameter scales the default line
*        width.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 1.
*        [1]
*     PHASE = _REAL (Read)
*        The fraction of SEPARATION that hatched lines are displaced.
*        Modifying this between regions makes their separation more
*        obvious. This is only used when FILLSTY is 3.
*        [0]
*     SEPARATION = _REAL (Read)
*        The separation of hatched lines. The unit spacing is 1 percent
*        of the smaller of the height or width of the view surface.
*        Only used when FILLSTY is 3.
*        [1]
*     TEXTBACK = _INTEGER (Read)
*        The pen number (colour index) of the background used when
*        drawing text. If less than zero then a transparent
*        background is used. Zero erases the plot under the text
*        region.
*        [-1]

*  Examples:
*     ICL> CHANGE RESET
*
*        will reset the plotting attributes to their default values.

*  Authors:
*     PAH: P.A. Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: P.W. Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     5-JUL-1990 (PAH):
*        Original version.
*     26-NOV-1991 (PCTR):
*        Removed pen colour modification code: now a new application -
*        PALETTE. This is to avoid having to assume and set default
*        background and foreground colours every time CHANGE is
*        executed.  This overcomes problems with Xwindows and
*        Postscript.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     2-JUN-1994 (PDRAPER):
*        Removed unused variables IDX, OUTBUF, FONTS and LINESTYLE.
*     21-JUN-1994 (PDRAPER):
*        Added check for device opened.
*     31-MAY-1996 (PDRAPER):
*        Added parameters for controlling hatch size.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PAR_ERR'
      INCLUDE 'PONGO_PAR'        ! PONGO global constants
      INCLUDE 'GNS_PAR'          ! GNS public global constants

*  Global Variables:
      INCLUDE 'PONGO_CMN'        ! PONGO global variables

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      INTEGER IFONT
      INTEGER ICIDX
      INTEGER IFS
      INTEGER ILS
      INTEGER ILW
      INTEGER TBCI

      REAL CHARHT
      REAL ANGLE
      REAL SEPN
      REAL PHASE

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that a PGPLOT device is open for plotting.
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Set the colour index (pen).
      CALL PAR_GET0I( 'COLOUR', ICIDX, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL ERR_REP( 'CHANGE_COLOUR',
     :                    'CHANGE: Unable to change the colour index.',
     :                    STATUS )
            GO TO 99
         END IF
      ELSE
         CALL PGSCI( ICIDX )
      END IF

*  Set the character height (the expansion factor).
      CALL PAR_GET0R( 'CHEIGHT', CHARHT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL ERR_REP( 'CHANGE_CHEIGHT',
     :           'CHANGE: Unable to change the character height.',
     :           STATUS )
            GO TO 99
         END IF
      ELSE
         CALL PGSCH( CHARHT )
      END IF

*  Set the font.
      CALL PAR_GET0I( 'FONT', IFONT, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL ERR_REP( 'CHANGE_FONT', 'CHANGE: Unable to ' //
     :                    'change the character font.', STATUS )
            GO TO 99
         END IF
      ELSE
         CALL PGSCF( IFONT )
      END IF

*  Set the point fill style.
      CALL PAR_GET0I( 'FILLSTY', IFS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL ERR_REP( 'CHANGE_FILSTY', 'CHANGE: Unable to change ' /
     :                  /'the point fill style.', STATUS )
            GO TO 99
         END IF
      ELSE
         CALL PGSFS( IFS )
      END IF

*  If fill style is 3 then set the other related attributes.
      IF ( IFS .EQ. 3 ) THEN
         CALL PAR_GET0R( 'ANGLE', ANGLE, STATUS )
         CALL PAR_GET0R( 'SEPARATION', SEPN, STATUS )
         CALL PAR_GET0R( 'PHASE', PHASE, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( STATUS .EQ. PAR__NULL ) THEN
               CALL ERR_ANNUL( STATUS )
            ELSE
               CALL ERR_REP( 'CHANGE_HATCH', 'CHANGE: Unable to ' //
     :                     'change the hatch fill attributes.', STATUS )
               GO TO 99
            END IF
         ELSE
            CALL PGSHS( ANGLE, SEPN, PHASE )
         END IF
      END IF

*  Set the line style.
      CALL PAR_GET0I( 'LINESTY', ILS, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL ERR_REP( 'CHANGE_LINSTY',  'CHANGE: Unable to change '
     :                  //'the line style.', STATUS )
            GO TO 99
         END IF
      ELSE
         CALL PGSLS( ILS )
      END IF

*  Set the line width.
      CALL PAR_GET0I( 'LINEWID', ILW, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL ERR_REP( 'CHANGE_LINWID', 'CHANGE: Unable to change ' /
     :                  /'the line width.', STATUS )
            GO TO 99
         END IF
      ELSE
         CALL PGSLW( ILW )
      END IF

*  Set the text background colour index.
      CALL PAR_GET0I( 'TEXTBACK', TBCI, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_ANNUL( STATUS )
         ELSE
            CALL ERR_REP( 'CHANGE_TEXTBACK', 'CHANGE: Unable to change '
     :                  //'the text background colour index.', STATUS )
            GO TO 99
         END IF
      ELSE
         CALL PGSTBG( TBCI )
      END IF

*  Exit in error label.
 99   CONTINUE

*  If an error occurred, then report a contextual message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'CHANGE_ERR',
     :     'CHANGE: Failed to change plotting attribute(s).',
     :     STATUS )
      END IF
      END
* $Id$




