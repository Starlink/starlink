      SUBROUTINE ELLIPSES( STATUS )
*+
*  Name:
*     ELLIPSES

*  Purpose:
*     Draw error ellipses.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     Draw error ellipses at each of the data points using values in
*     the EXCOL and EYCOL error data areas, and the ZCOL data values as
*     the normalized covariance. Depending upon the value of the AXES
*     parameter, the major and minor axes of the ellipses will also be
*     drawn.
*
*     The size of the ellipses depends upon the parameter ERSCALE which
*     should be set before the data are read in so that the WORLD
*     application can calculate the viewing area properly. This allows
*     the ellipse size to be varied so that it can be drawn for
*     different confidence levels.
*
*        ERSCALE   Confidence
*                    level
*         1.00        46%
*         2.30        68.3%
*         4.61        90%
*         9.21        99%

*  Usage:
*     ellipses [axes] [erscale]

*  ADAM Parameters:
*     AXES = _LOGICAL (Read and Write)
*        If TRUE, the axes of the ellipses will be drawn.
*
*        If the value is not specified on the command line, the current
*        value is used.  The current value is initially set to FALSE.
*     ERSCALE = _REAL (Read and Write)
*        Scale the error ellipses as described above. If the command
*        WORLD DATA (which automatically sets the graph limits) is to
*        work properly, the global parameter PONGO_ERSCALE should have
*        been set by READF. However, if this facility is not required,
*        it is perfectly acceptable to set ERSCALE when invoking
*        ELLIPSES.
*
*        [The value of the global parameter PONGO_ERSCALE is used. If
*        PONGO_ERSCALE is not defined, the default value 1.0 is used.]

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-APR-1990 (JBVAD::PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     20-JUN-1994 (PDRAPER):
*        Added device is open checks.
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

*  Local Variables:
      LOGICAL DRAXES             ! Switch for drawing axes

      REAL SCALE                 ! Scale factor for ellipses

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that the device is open.
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN
         CALL PAR_GET0L( 'AXES', DRAXES, STATUS )
         CALL PAR_GET0R( 'ERSCALE', SCALE, STATUS )
         CALL PON_ELLIPSES( DRAXES, XDATA, YDATA, ZDATA, ERRX, ERRY,
     :                      NDAT, SCALE, STATUS )
      END IF

*  Check the returned status and report a contextula error report if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'ELLIPSES_END',
     :                              'ELLIPSES: Cannot draw error ' //
     :                              'ellipses.', STATUS )

      END
* $Id$
