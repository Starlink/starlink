      SUBROUTINE VIEWPORT( STATUS )
*+
*  Name:
*     VIEWPORT

*  Purpose:
*     Set the viewport for the current plotting device.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Control the PGPLOT viewport. The viewport is the region of the
*     plotting surface through which the graph is seen.

*  Usage:
*     viewport action
*        {[xvpmin] [xvpmax] [yvpmin] [yvpmax]
*        action

*  ADAM Parameters:
*     ACTION = _CHAR (Read)
*        The method used to set the viewport. It may be one of the
*        following:
*
*           - "STANDARD" -- The viewport is set to the standard PGPLOT
*           viewport.
*           - "ADJUST" -- The viewport is adjusted so that the scales
*           along the X and Y axes are the same number of world
*           coordinate units per unit length. The newly created
*           viewport fits within the old viewport.
*           - "NDC" -- The viewport is set by specifying its extent in
*           the X and Y directions in terms of normalised device
*           coordinates (i.e. coordinates that run from 0 to 1 along
*           the horizontal and vertical directions).
*           - "INCHES" -- The viewport is set by specifying its extent
*           in the X and Y directions in terms of inches.
*
*        [The value is prompted for.]
*     XVPMIN = _REAL (Read and Write)
*        The left hand side of the viewport.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     XVPMAX = _REAL (Read and Write)
*        The right hand side of the viewport.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 1.0.
*     YVPMIN = _REAL (Read and Write)
*        The lower side of the viewport.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 0.0.
*     YVPMAX = _REAL (Read and Write)
*        The upper side of the viewport.
*
*        If the value is not specified on the command line, the current
*        value is used. The current value is initially set to 1.0.
*     XMIN = _REAL (Read)
*        The left hand edge of the world coordinate system.
*
*        [The value of the global parameter PONGO_XMIN is used. If
*        PONGO_XMIN is not defined, the default value 0.0 is used.]
*     XMAX = _REAL (Read)
*        The right hand edge of the world coordinate system.
*
*        [The value of the global parameter PONGO_XMAX is used. If
*        PONGO_XMAX is not defined, the default value 1.0 is used.]
*     YMIN = _REAL (Read)
*        The lower edge of the world coordinate system.
*
*        [The value of the global parameter PONGO_YMIN is used. If
*        PONGO_YMIN is not defined, the default value 0.0 is used.]
*     YMAX = _REAL (Read)
*        The upper edge of the world coordinate system.
*
*        [The value of the global parameter PONGO_YMAX is used. If
*        PONGO_YMIN is not defined, the default value 1.0 is used.]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     24-JUN-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     22-JUN-1994 (PDRAPER):
*        Added check for device open.
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
      INCLUDE 'PONGO_CMN'        ! PONGO global varaibles

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      CHARACTER * ( 20 ) ACTION  ! Action to be taken

      INTEGER LENACT             ! Length of ACTION string

      REAL XMINP                 ! Lower X world coordinate
      REAL XMAXP                 ! Upper X world coordinate
      REAL YMINP                 ! Lower Y world coordinate
      REAL YMAXP                 ! Upper Y world coordinate

*  Internal References:
      INTEGER CHR_LEN

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If the plotting device is open.
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN

*  Get ACTION.
         CALL PAR_GET0C( 'ACTION', ACTION, STATUS )

         LENACT = MAX( 1, CHR_LEN( ACTION ) )
         CALL CHR_UCASE( ACTION( : LENACT ) )

         IF ( ACTION .EQ. 'STANDARD' ) THEN
            CALL PGVSTAND
         ELSE IF ( ACTION .EQ. 'ADJUST' ) THEN
            CALL PAR_GET0R( 'XMIN', XMINP, STATUS )
            CALL PAR_GET0R( 'XMAX', XMAXP, STATUS )
            CALL PAR_GET0R( 'YMIN', YMINP, STATUS )
            CALL PAR_GET0R( 'YMAX', YMAXP, STATUS )
            CALL PGWNAD( XMINP, XMAXP, YMINP, YMAXP )
         ELSE IF ( ( ACTION .EQ. 'NDC' ) .OR. ( ACTION .EQ. 'INCHES' ) )
     :   THEN
            CALL PAR_GET0R( 'XVPMIN', XMINP, STATUS )
            CALL PAR_GET0R( 'XVPMAX', XMAXP, STATUS )
            CALL PAR_GET0R( 'YVPMIN', YMINP, STATUS )
            CALL PAR_GET0R( 'YVPMAX', YMAXP, STATUS )

            IF ( ACTION .EQ. 'NDC' ) THEN
               CALL PGVPORT( XMINP, XMAXP, YMINP, YMAXP )
            ELSE
               CALL PGVSIZE( XMINP, XMAXP, YMINP, YMAXP )
            END IF
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'VIEWPORT_END',
     :                              'VIEWPORT: Unable to set the ' //
     :                              'viewport limits.', STATUS )

      END
* $Id$
