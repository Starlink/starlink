      SUBROUTINE IRC1_TRACI( IDC, ROUTNE, STATUS )
*+
*  Name:
*     IRC1_TRACI

*  Purpose:
*     Display information from a "DETAILS" structure.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL IRC1_TRACI( IDC, ROUTNE, STATUS )

*  Description:
*     This routine displays information about the DETAILS structure
*     associated with the supplied IRC identifier.

*  Arguments:
*     IDC = INTEGER (Given)
*        The IRC identifier for the astrometry structure.
*     ROUTNE = EXTERNAL (Given)
*        A routine to which is passed each line of text for display.
*        It should have the same argument list as MSG__OUTIF (see
*        SUN/104), and should be declared EXTERNAL in the calling
*        routine.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     19-OCT-1992 (DSB):
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
      INCLUDE 'IRC_ERR'          ! IRC error constants.

*  Global Variables:
      INCLUDE 'IRC_COM'          ! IRC common blocks.
*        CCM_TYPE( IRC__MAX ) = CHARACTER (Read)
*           HDS type of the DETAILS component.

*  Arguments Given:
      INTEGER IDC
      EXTERNAL ROUTNE

*  Status:
      INTEGER STATUS             ! Global status
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Call an appropriate routine for each CRDD type.
      IF( CCM_TYPE( IDC ) .EQ. 'SURVEY_BSIGHT' ) THEN
         CALL IRC1_TRASB( IDC, ROUTNE, STATUS )

*  If the CRDD type is unrecognised, give an error report.
      ELSE
         STATUS = IRC__BADTY
         CALL MSG_SETC( 'T', CCM_TYPE( IDC ) )
         CALL ERR_REP( 'IRC1_TRACI_ERR1',
     :                 'IRC1_TRACI does not yet support ^T data',
     :                 STATUS )
      END IF

      END
