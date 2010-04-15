      SUBROUTINE KPG1_SCRSZ( WIDTH, HEIGHT, STATUS )
*+
*  Name:
*     KPG1_SCRSZ

*  Purpose:
*     Interrogates the system to find the width and height of the screen
*     on which it is running.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_SCRSZ( WIDTH, HEIGHT, STATUS )

*  Description:
*     This routine interrogates the system to find the width and height of the screen
*     on which it is running.  Should an error occur or the width is
*     not positive, set to the default of 80 characters by 24 lines.


*  Arguments:
*     WIDTH = INTEGER (Returned)
*        The width of the screen in characters.
*     HEIGHT = INTEGER (Returned)
*        The height of the screen in lines.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This is the UNIX version.

*  [optional_subroutine_items]...
*  Authors:
*     AJC: Alan Chipperfield (STARLINK)
*     MJC: Malcolm J. Currie (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1992 June 17 (AJC):
*        Original version.
*     1992 June 22 (MJC):
*        Renamed and modified for KAPPA use.  Converted to SST prologue.
*     1993 March 14 (PDRAPER):
*        Removed kpg1_trmsz call and replaced with subpar_trmsz.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'
      INCLUDE 'PAR_PAR'

*  Arguments Returned:
      INTEGER WIDTH
      INTEGER HEIGHT

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SUBPAR_TRMSZ
      INTEGER SUBPAR_TRMSZ         ! C function for actually obtaining
                                 ! the height and width

*  Local Variables:
      INTEGER ISTAT              ! Local status

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Inquire the terminal size.
      ISTAT = SUBPAR_TRMSZ( WIDTH, HEIGHT )

*  If this failed to get a good value, set default which causes no
*  paging.
      IF ( ( ISTAT .NE. 1 ) .OR. ( WIDTH .LE. 0 ) ) THEN
         WIDTH = 80
         HEIGHT = 0
      END IF

      END
* @(#)kpg1_scrsz.f	1.1     10/7/93     1
