      SUBROUTINE BOXFRAME( STATUS )
*+
*  Name:
*     BOXFRAME

*  Purpose:
*     Draw a frame and axes on the plot.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw a frame, axes and tick-marks on a plot. The application has
*     great flexibility in the type of axis labelling that can be
*     produced -- it is essentially an interface to the PGPLOT routines
*     PGBOX and PGTBOX.

*  Usage:
*     boxframe [xopt] [yopt] [xtic] [ytic] [nxsub] [nysub]

*  ADAM Parameters:
*     XOPT = _CHAR (Read and Write)
*        A string that controls the style of the X-axis labelling and
*        tick-marks. It consists of a series of letters, which have the
*        meanings shown below (reproduced from the PGPLOT manual):
*
*           - "A" -- Draw an axis (the X axis is the horizontal line
*           Y=0, the Y axis is the vertical line X=0).
*           - "B" -- Draw the bottom (X) or left (Y) edge of the frame.
*           - "C" -- Draw the top (X) or right (Y) edge of the frame.
*           - "G" -- Draw a grid of vertical (X) or horizontal (Y)
*           lines.
*           - "I" -- Invert the tick-marks (i.e. draw them outside the
*           viewport instead of inside).
*           - "L" -- Label the axis logarithmically (see below).
*           - "N" -- Write numeric labels in the conventional location
*           below the viewport (X) or to the left of the viewport (Y).
*           - "P" -- Extend (project) major tick-marks outside the box
*           (ignored if option I is specified).
*           - "M" -- Write numeric labels in the unconventional location
*           above the viewport (X) or to the right of the viewport (Y).
*           - "T" -- Draw major tick-marks at the major coordinate
*           interval.
*           - "S" -- Draw minor tick-marks (sub-ticks).
*           - "V" -- Orient numeric labels vertically (this is only
*           applicable to Y -- the default is to write Y-axis labels
*           parallel to the axis).
*           - "1" -- Force decimal labelling.
*           - "2" -- Force exponential labelling.
*
*        A set of special letters control the plotting of RA and DEC
*        axes.
*
*           - "Z" for (DD) HH MM SS.S time labelling
*           - "H" means superscript numbers with d, h, m, & s  symbols
*           - "D" means superscript numbers with    o, ', & '' symbols
*           - "F" causes the first label (left- or bottom-most) to
*                 be omitted. Useful for sub-panels that abut each other.
*                 Care is needed because first label carries sign as well.
*           - "O" means omit leading zeros in numbers < 10
*                 E.g.  3h 3m 1.2s rather than 03h 03m 01.2s  Useful
*                 to help save space on X-axes. The day field does not
*                 use this facility.
*
*        Note that to use these features your data values should be in
*        radians (PONGO automatically converts HH:MM:SS.SSS strings to
*        radians, and the DEGTOR command from degrees to radians).
*
*        [The global parameter PONGO_XOPT is used. If PONGO_XOPT is not
*        defined, the default value "BCNST" is used.]
*     YOPT = _CHAR (Read and Write)
*        A string that controls the style of the Y-axis labelling and
*        tick-marks. It consists of a series of letters, as given for
*        the parameter XOPT.
*
*        [The global parameter PONGO_YOPT is used.  If PONGO_YOPT is not
*        defined, the default value "BCNST" is used.]
*     XTIC = _REAL (Read and Write)
*        The major tick-mark interval on the X-axis. If XTIC is set to
*        0.0, PGPLOT makes a sensible choice.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     YTIC = _REAL (Read and Write)
*        The major tick-mark interval on the Y-axis. If YTIC is set to
*        0.0, PGPLOT makes a sensible choice.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     NXSUB = _INTEGER (Read and Write)
*        The number of minor tick-marks between each major tick-mark on
*        the X-axis. If NXSUB is set to 0, PGPLOT makes a sensible
*        choice.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.
*     NYSUB = _INTEGER (Read and Write)
*        The number of minor tick-marks between each major tick-mark on
*        the Y-axis. If NYSUB is set to 0, PGPLOT makes a sensible
*        choice.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.

*  Notes:
*     - Right Ascension and Declination axes.
*
*        It is possible to draw RA and DEC axes using this routine. To
*        do this read in your X and Y data in radians (this is the
*        default if your data are stored in the HH:MM:SS.SSS,
*        DD:MM:SS.SSS formats) and then set the "Z" character in the
*        XOPT and YOPT parameters. A good combination of options are:
*
*           XOPT='BCNSTZHG' YOPT='BCNSTZDG'
*
*        For more about this (in particular how the interval options are
*        set) see the PGPLOT documentation about PGTBOX. Note that the
*        "Y" option is always set and that the Y axis values are
*        converted into pseudo arc seconds.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     21-JUN-1994 (PDRAPER):
*        Added check for device open.
*     30-MAY-1996 (PDRAPER):
*        Added options to plot RA and DEC. Added "1" & "2" to XOPT
*        and YOPT descriptions.
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

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN           ! Used length of character string

*  Local Variables:
      CHARACTER * ( 20 ) XOPT
      CHARACTER * ( 20 ) YOPT

      REAL XTIC
      REAL YTIC
      REAL RALM1
      REAL RALM2
      REAL DECLM1
      REAL DECLM2

      INTEGER NXSUB
      INTEGER NYSUB
      INTEGER IAT

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN

*  Get the axes control parameters.
         CALL PAR_GET0C( 'XOPT', XOPT, STATUS )
         CALL CHR_UCASE( XOPT )
         CALL PAR_GET0C( 'YOPT', YOPT, STATUS )
         CALL CHR_UCASE( YOPT )
         CALL PAR_GET0R( 'XTIC', XTIC, STATUS )
         CALL PAR_GET0R( 'YTIC', YTIC, STATUS )
         CALL PAR_GET0I( 'NXSUB', NXSUB, STATUS )
         CALL PAR_GET0I( 'NYSUB', NYSUB, STATUS )

*  Check the returned status and draw the box if possible.
         IF ( STATUS .EQ. SAI__OK ) THEN

*  If RA and DEC are to be used the adjust the window to seconds of time
*  and check that the Y axis doesn't wrap in days.
            IF ( INDEX( XOPT, 'Z' ) .NE. 0 ) THEN
               IF ( INDEX( YOPT, 'Y' ) .EQ. 0 ) THEN
                  IAT = CHR_LEN( YOPT )
                  CALL CHR_APPND( 'Y', YOPT, IAT)
               END IF
               IF ( INDEX( XOPT, 'Y' ) .EQ. 0 ) THEN
                  IAT = CHR_LEN( XOPT )
                  CALL CHR_APPND( 'Y', XOPT, IAT)
               END IF

*  Convert current world coordinates to seconds of time (assumes the
*  limits are in radians, factor 15 is to make DEC look like minutes of
*  time).
               RALM1 =  24.0 * 3600.0 * XMIN / S2PI
               RALM2 =  24.0 * 3600.0 * XMAX / S2PI
               DECLM1 = 24.0 * 3600.0 * 15.0 * YMIN / S2PI
               DECLM2 = 24.0 * 3600.0 * 15.0 * YMAX / S2PI
               CALL PGSWIN( RALM1, RALM2, DECLM1, DECLM2 )
               CALL PGTBOX( XOPT, XTIC, NXSUB, YOPT, YTIC, NYSUB )

*  Restore old limits (radians).
               CALL PGSWIN( XMIN, XMAX, YMIN, YMAX )
            ELSE
               CALL PGTBOX( XOPT, XTIC, NXSUB, YOPT, YTIC, NYSUB )
            END IF
         END IF
      END IF
      IF ( STATUS .NE. SAI__OK ) THEN

*     Report a contextual error message.
         CALL ERR_REP( 'BOXFRAME_END',
     :                 'BOXFRAME: Cannot draw axes on the plot.',
     :                 STATUS )
      END IF

      END
* $Id$
