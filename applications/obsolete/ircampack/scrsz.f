      SUBROUTINE SCRSZ( WIDTH, HEIGHT, STATUS )
*+
*  Name:
*     SCRSZ

*  Purpose:
*     Interrogates the system to find the width and height of the screen
*     on which it is running.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SCRSZ( WIDTH, HEIGHT, STATUS )

*  Description:
*     This routine interrogates the system to find the width and height
*     of the screen on which it is running.  Should an error occur or
*     the width is not positive, set to the default of 80 characters by
*     0 lines (thus suppresses paging).

*  Arguments:
*     WIDTH = INTEGER (Returned)
*        The width of the screen in characters.
*     HEIGHT = INTEGER (Returned)
*        The height of the screen in lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This is the UNIX version.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13-OCT-1993 (DSB):
*        Original version, copied from IRAS90 routine IRM1_SCRSZ
*     03-OCT-1996 (DSB):
*        Changed to use kappa function KPG1_TRMSZ instead of similar
*        private function.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Returned:
      INTEGER WIDTH
      INTEGER HEIGHT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL KPG1_TRMSZ
      INTEGER KPG1_TRMSZ         ! C function for actually obtaining
                                 ! the height and width

*  Local Variables:
      INTEGER ISTAT              ! Local status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inquire the terminal size.
      ISTAT = KPG1_TRMSZ ( WIDTH, HEIGHT )

*  If this failed to get a good value, set default which causes no
*  paging.
      IF ( ( ISTAT .NE. 1 ) .OR. ( WIDTH .LE. 0 ) ) THEN
         WIDTH = 80
         HEIGHT = 0
      END IF

      END
