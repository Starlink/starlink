      SUBROUTINE IRC_CLOSE( STATUS )
*+
*  Name:
*     IRC_CLOSE

*  Purpose:
*     Close down the IRC CRDD handling package.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_CLOSE( STATUS )

*  Description:
*     This routine should be called when all other IRC routines have
*     been finished with. It annuls any remaining valid IRC identifiers.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it

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
      INCLUDE 'DAT_ERR'          ! DAT__ error constants
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

*  Return immediately if IRC has not been initialised.
      IF( CCM_STATE .NE. IRC__GOING ) RETURN

*  Begin a new error reporting environment.
      CALL ERR_BEGIN( STATUS )

*  Annul all the currently valid entries. This involves annuling the
*  locators to the IRAS NDF extension, the CRDD_INFO object, clearing
*  out pointing information, and releasing the IRC identifier.
      DO I = 1, IRC__MAX

         IF( CCM_VALID( I ) ) THEN
            CALL DAT_ANNUL( CCM_IRASL( I ), STATUS )
            CALL DAT_ANNUL( CCM_CRDDL( I ), STATUS )

*  If either of the locators were invalid (as caused for instance by
*  the NDF container file previously having been closed), annul the
*  error.
            IF( STATUS .EQ. DAT__LOCIN ) CALL ERR_ANNUL( STATUS )

            CALL IRC1_CLPNT( I, STATUS )
            CCM_VALID( I ) = .FALSE.
         END IF

      END DO

*  Indicate that IRC has been closed down.
      CCM_STATE = IRC__STOPT

*  End the error reporting environment.
      CALL ERR_END( STATUS )

      END
