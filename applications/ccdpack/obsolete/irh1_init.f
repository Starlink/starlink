      SUBROUTINE IRH1_INIT( STATUS )
*+
*  Name:
*     IRH1_INIT

*  Purpose:
*     Initialise the IRH_ system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRH1_INIT( STATUS )

*  Description:
*     A new temporary HDS object is created to hold an array of GROUP
*     structures. Each such structure holds the information associated
*     with each group. All IRH identifiers are set invalid.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-MAY-1991 (DSB):
*        Original version.
*     26-FEB-1992 (PDRAPER):
*        Added DAT_PAR.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants.
      INCLUDE 'DAT_PAR'          ! HDS DAT constants
      INCLUDE 'IRH_PAR'          ! IRH constants.

*  Global Variables:
      INCLUDE 'IRH_COM'          ! IRH common blocks.
*        HCM_LOC = CHARACTER (Write)
*           An HDS locator to the array of GROUP structures.
*        HCM_STATE = CHARACTER (Write)
*           If HCM_STATE = IRH__GOING then IRH has been initialised.
*        HCM_VALID( IRH__MAXG ) = LOGICAL (Write)
*           True if the corresponding group identifier is valid (i.e. in
*           use).

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IDH                ! An IDH identifier.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create a temporary HDS object to hold the group structures. This is
*  a one dimensional array of GROUP structures with initial size given 
*  by symbolic constant IRH__INITG. Further elements are added as 
*  needed. The locator to the whole array is stored in common.
      CALL AIF_TEMP( 'GROUP', 1, IRH__INITG, HCM_LOC, STATUS )

*  Initialise all groups to "not in use".
      DO IDH = 1, IRH__MAXG
         HCM_VALID( IDH ) = .FALSE.
      END DO

*  If all has gone OK, set the common variable HCM_STATE to indicate
*  that the IRH_ system is initialised.
      IF( STATUS .EQ. SAI__OK ) HCM_STATE = IRH__GOING

      END
* $Id$
