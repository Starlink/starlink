      SUBROUTINE IRC1_CLPNT( IDC, STATUS )

*+
*  Name:
*     IRC1_CLPNT

*  Purpose:
*     Annuls pointing information.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_CLPNT( IDC, STATUS )

*  Description:
*     Each type of CRDD may require different tiding up operations to be
*     performed when the associated pointing information is no longer
*     required. This routine has a section for each recognised CRDD
*     type.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier (i.e. the index into the IRC common arrays
*        associated with the CRDD file). No check is made on the
*        validity of this identifier.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

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
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.
      INCLUDE 'IRC_ERR'          ! IRC error values.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_POINT( IRC__MAX ) = LOGICAL (Write and Read)
*           True if pointing information has previously been set up.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Read)
*           The HDS type of the DETAILS component within the CRDD_INFO
*           structure.

*  Arguments Given:
      INTEGER IDC

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  If pointing information has been set up...
      IF( CCM_POINT( IDC ) ) THEN

*  Check for Survey Boresight data.
         IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN

*  No tidying up required, just reset the CCM_POINT value.
            CCM_POINT( IDC ) = .FALSE.

*  If the data type is unrecognised, give an error report.
         ELSE
            STATUS = IRC__BADTY
            CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
            CALL ERR_REP( 'IRC1_CLPNT_ERR1',
     :                    'IRC1_CLPNT does not yet support ^T data',
     :                    STATUS )
         END IF

      END IF


      END
