      SUBROUTINE IRC_INIT( STATUS )
*+
*  Name:
*     IRC_INIT

*  Purpose:
*     Initialise the IRC system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_INIT( STATUS )

*  Description:
*     This routine should be called before calling any other IRC
*     routines.  If IRC has not previously been initialised, or has
*     been closed down (by calling IRC_CLOSE), then all IRC identifiers
*     are released but no check is made to see if they are valid (since
*     such checks could give spurious results).  If IRC has previously
*     been initialised but has not yet been closed down then all data
*     structures associated with currently valid IRC identifiers are
*     annuled and the identifiers are released.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     31-JAN-1991 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'DAT_PAR'          ! DAT__ constants
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Read and Write)
*           Each element is true if the other common arrays have valid
*           CRDD information stored in the corresponding element.
*        CCM_IRASL( IRC__MAX ) = CHARACTER (Write)
*           HDS locators to the IRAS NDF extension.
*        CCM_CRDDL( IRC__MAX ) = CHARACTER (Write)
*           HDS locators to the CRDD_INFO object.
*        CCM_STATE = CHARACTER (Read and Write)
*           Equal to the symbolic constant IRC__GOING if the IRC system
*           has been initialised but not closed down.

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! IRC identifier loop count.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If IRC has not previously been initialised, just ensure all the IRC
*  identifiers are free, and then set the STATE to GOING.
      IF( CCM_STATE .NE. IRC__GOING ) THEN
         DO I = 1, IRC__MAX
            CCM_VALID( I ) = .FALSE.
         END DO

         CCM_STATE = IRC__GOING

*  If IRC is already going, then annul all the currently valid entries.
*  This involves annuling the locators to the IRAS NDF extension, the
*  CRDD_INFO object, clearing out pointing information, and releasing
*  the IRC identifier.
      ELSE

         DO I = 1, IRC__MAX

            IF( CCM_VALID( I ) ) THEN
               CALL DAT_ANNUL( CCM_IRASL( I ), STATUS )
               CALL DAT_ANNUL( CCM_CRDDL( I ), STATUS )
               CALL IRC1_CLPNT( I, STATUS )
               CCM_VALID( I ) = .FALSE.
            END IF

         END DO

      END IF

      END
