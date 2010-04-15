      SUBROUTINE PAPER( STATUS )
*+
*  Name:
*     PAPER

*  Purpose:
*     Change the size and aspect ratio of the plotting surface.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     The width of the plotting surface and its aspect ratio (i.e.
*     height/width) are modified.

*  Usage:
*     paper width aspect

*  ADAM Parameters:
*     WIDTH = _REAL (Read and Write)
*        The width of the plotting surface in inches. If the specified
*        width is 0.0, the maximum possible width for the device is
*        used.
*
*        [The value is prompted for.]
*     ASPECT = _REAL (Read and Write)
*        The aspect ratio of the plotting surface: i.e. height/width.
*
*        [The value is prompted for.]

*  Authors:
*     JBVAD::PAH: Paul Harrison (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     13-MAR-1992 (PAH):
*        Original version.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     21-JUN-1994 (PDRAPER):
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

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      REAL WIDTH                 ! plot width in inches
      REAL ASPECT                ! aspect ratio

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN
      IF ( PON_DEVOP( .TRUE., STATUS ) ) THEN
         CALL PAR_GET0R( 'WIDTH', WIDTH, STATUS )
         CALL PAR_GET0R( 'ASPECT', ASPECT, STATUS )
         IF ( ASPECT .GE. 0.0 ) THEN
            CALL PGPAPER( WIDTH, ASPECT )
         ELSE
            CALL MSG_SETR( 'ASPECT', ASPECT )
            STATUS = SAI__ERROR
            CALL ERR_REP( 'PAPER_BADASP',
     :           'The aspect ratio ^ASPECT is not valid.', STATUS )
         END IF
      END IF

*  Check the returned status and report a contextual error message if
*  necessary.
      IF ( STATUS .NE. SAI__OK ) CALL ERR_REP( 'PAPER_END',
     :     'PAPER: Unable to resize the plotting surface.', STATUS )
      END
* $Id$
