      SUBROUTINE IRC_ANNUL( IDC, STATUS )
*+
*  Name:
*     IRC_ANNUL

*  Purpose:
*     Release a CRDD file from the IRC_ system.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC_ANNUL( IDC, STATUS )

*  Description:
*     The IRC identifier is annuled and all resources reserved by the
*     IRC_ system for this CRDD file are released. NB, this routine does
*     not effect the associated NDF identifier.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier of the CRDD file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Notes:
*     -  This routine attempts to execute even if STATUS is set on
*     entry, although no further error report will be made if it
*     subsequently fails under these circumstances. In particular, it
*     will fail if the identifier supplied is not initially valid, but
*     this will only be reported if STATUS is set to SAI__OK on entry.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JAN-1991 (DSB):
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
      INCLUDE 'NDF_PAR'          ! NDF constants.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC errors.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_CRDDL( IRC__MAX ) = CHARACTER (Read and Write)
*           HDS locators for the CRDD_INFO components.
*        CCM_IRASL( IRC__MAX ) = CHARACTER (Read and Write)
*           HDS locators for the IRAS NDF extensions.
*        CCM_VALID( IRC__MAX ) = LOGICAL (Write)
*           True if the IRC indentifier for a CRDD file is valid.

*  Arguments Given:
      INTEGER IDC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER TSTAT              ! Value of STATUS on entry.
*.

*  Save the STATUS value and mark the error stack.
      TSTAT = STATUS
      CALL ERR_MARK

*  Set the local STATUS value to the OK value.
      STATUS = SAI__OK

*  If the given IRC identifier is valid...
      IF( IDC .GT. 0 .AND. IDC .LE. IRC__MAX ) THEN
         IF( CCM_VALID( IDC ) ) THEN

*  Release the identifier for future use.
            CCM_VALID( IDC ) = .FALSE.

*  Release resources used to store pointing information.
            CALL IRC1_CLPNT( IDC, STATUS )

*  Annul the HDS locators to the CRDD_INFO structure and the IRAS NDF
*  extension.
            CALL DAT_ANNUL( CCM_CRDDL( IDC ), STATUS )
            CALL DAT_ANNUL( CCM_IRASL( IDC ), STATUS )

*  If the supplied identifier is invalid, give an error report.
         ELSE
            STATUS = IRC__INVID
            CALL ERR_REP( 'IRC_ANNUL_ERR1',
     :                    'IRC_ANNUL: Invalid IRC identifier supplied',
     :                    STATUS )
         END IF
      ELSE
         STATUS = IRC__INVID
         CALL ERR_REP( 'IRC_ANNUL_ERR2',
     :                 'IRC_ANNUL: Invalid IRC identifier supplied',
     :                 STATUS )
      END IF

*  Annul any error if STATUS was previously bad, otherwise let the new
*  error report stand. Release the error stack.
      IF ( STATUS .NE. SAI__OK ) THEN
         IF ( TSTAT .NE. SAI__OK ) THEN
            CALL ERR_ANNUL( STATUS )
            STATUS = TSTAT
         ELSE
            CALL ERR_REP( 'IRC_ANNUL_ERR3',
     :                  'IRC_ANNUL: Unable to annul an IRC identifier',
     :                    STATUS )
         END IF
      ELSE
         STATUS = TSTAT
      END IF

*  Relese the error stack.
      CALL ERR_RLSE

      END
