      SUBROUTINE LIMITS( STATUS )
*+
*  Name:
*     LIMITS

*  Purpose:
*     Set the world coordinate limits.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     The world coordinate limits are set from the parameters given
*     on the command line.

*  Usage:
*     limits [xmin] [xmax] [ymin] [ymax]

*  ADAM Parameters:
*     XMIN = _REAL (Read and Write)
*        The world coordinate of the left-hand edge of the plot.
*     XMAX = _REAL (Read and Write)
*        The world coordinate of the right-hand edge of the plot.
*     YMIN = _REAL (Read and Write)
*        The world coordinate of the lower edge of the plot.
*     YMAX = _REAL (Read and Write)
*        The world coordinate of the upper edge of the plot.

*  Authors:
*     PDRAPER: Peter W. Draper (Starlink, Durham University)
*     {enter_new_authors_here}

*  History:
*     29-APR-1997 (PDRAPER):
*        Original version.
*     {enter_changes_here}

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
      EXTERNAL CHR_LEN
      INTEGER CHR_LEN            ! Used legth of string
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT is open

*  Local Variables:
      REAL XMINP                 ! World coodinate bounds
      REAL XMAXP                 ! World coodinate bounds
      REAL YMINP                 ! World coodinate bounds
      REAL YMAXP                 ! World coodinate bounds

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that PGPLOT is open
      IF ( .NOT. PON_DEVOP( .TRUE., STATUS ) ) GO TO 99

*  Set the current values as the dynamic defaults and get the actual
*  values.
      CALL PAR_DEF0R( 'XMIN', XMIN, STATUS )
      CALL PAR_DEF0R( 'XMAX', XMAX, STATUS )
      CALL PAR_DEF0R( 'YMIN', YMIN, STATUS )
      CALL PAR_DEF0R( 'YMAX', YMAX, STATUS )
      CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
      CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )
      CALL PAR_GET0R( 'YMIN', YMINP, STATUS )
      CALL PAR_GET0R( 'YMAX', YMAXP, STATUS )

*  Now write the values of these parameters back.
      CALL PAR_PUT0R( 'XMIN', XMINP, STATUS )
      CALL PAR_PUT0R( 'XMAX', XMAXP, STATUS )
      CALL PAR_PUT0R( 'YMIN', YMINP, STATUS )
      CALL PAR_PUT0R( 'YMAX', YMAXP, STATUS )

*  Set up the window.
      CALL PGWINDOW( XMINP, XMAXP, YMINP, YMAXP )

*  And report the bounds of selected.
      CALL MSG_SETR( 'XMIN', XMINP )
      CALL MSG_SETR( 'XMAX', XMAXP )
      CALL MSG_SETR( 'YMIN', YMINP )
      CALL MSG_SETR( 'YMAX', YMAXP )
      CALL MSG_OUT( ' ',
     :     'World limits set to ^XMIN:^XMAX, ^YMIN:^YMAX', STATUS )

*  And set the common block bounds.
      XMIN = XMINP
      YMIN = YMINP
      XMAX = XMAXP
      YMAX = YMAXP

*  Check the returned status and report a contextual error message if
*  necessary.
 99   CONTINUE
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'LIMITS_END',
     :                              'LIMITS: World coordinates for ' //
     :                              'the plot could not be set.',
     :                              STATUS )

      END
* $Id$
