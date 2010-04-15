      SUBROUTINE PALETTE( STATUS )
*+
*  Name:
*     PALETTE

*  Purpose:
*     Change the plotting pen colours.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     ADAM A-task

*  Description:
*     The colour representation for a given colour index is updated.

*  Usage:
*     palette [colour] [red] [green] [blue]

*  ADAM Parameters:
*     COLOUR = _INTEGER (Read and Write)
*        The colour index, i.e. pen, to be updated (in the range 0 to
*        255).
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 1.
*     RED = _REAL (Read and Write)
*        The red intensity (in the range 0.0 to 1.0).
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 1.0.
*     GREEN = _REAL (Read and Write)
*        The green intensity (in the range 0.0 to 1.0).
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 1.0.
*     BLUE = _REAL (Read and Write)
*        The blue intensity (in the range 0.0 to 1.0).
*
*        If no value is specified on the command line, the current
*        value is used. The current value is initially set to 1.0.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     PCTR: P.C.T. Rees (STARLINK)
*     PDRAPER: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     26-NOV-1991 (PCTR):
*        Original version - based upon code from CHANGE.
*     19-OCT-1992 (PCTR):
*        Added contextual error report on exit.
*     20-JUN-1994 (PDRAPER):
*        Addec check for device open.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL PON_DEVOP
      LOGICAL PON_DEVOP          ! PGPLOT device is open

*  Local Variables:
      INTEGER ICIDX              ! Colour index

      REAL BLUE                  ! Blue relative intensity
      REAL GREEN                 ! Green relative intensity
      REAL RED                   ! Red relative intensity

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK .OR.
     :     .NOT. PON_DEVOP( .TRUE., STATUS ) ) RETURN

*  Set the colour representation of a particular colour (pen)
*  using RGB colour intensities.
      CALL PAR_GET0I( 'COLOUR', ICIDX, STATUS )
      CALL PAR_GET0R( 'RED', RED, STATUS )
      CALL PAR_GET0R( 'GREEN', GREEN, STATUS )
      CALL PAR_GET0R( 'BLUE', BLUE, STATUS )

*  Check the returned status and act.
      IF ( STATUS .EQ. SAI__OK ) THEN
         CALL PGSCR( ICIDX, RED, GREEN, BLUE )
      ELSE
         CALL ERR_REP( 'PALETTE_END',
     :                 'PALETTE: Unable to change the colour ' //
     :                 'representation.', STATUS )
      END IF

      END
* $Id$
