      SUBROUTINE KPG1_IWCG( WKCHAR, VALUE, STATUS )
*+
*  Name:
*     KPG1_IWCG

*  Purpose:
*     Inquires a characteristic of a GKS device.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_IWCG( WKCHAR, VALUE, STATUS )

*  Description:
*     This inquiries a characteristic of the current GKS graphics
*     workstation by accessing GNS.

*  Arguments:
*     WKCHAR = CHARACTER * ( * ) (Given)
*        The name of the characteristic.  See SUN/57 for a full list.
*     VALUE = CHARACTER * ( GNS__SZKEY ) (Returned)
*        The value of the specified characteristic for the current
*        workstation.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  [optional_subroutine_items]...
*  Authors:
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1996 October 1 (MJC):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) WKCHAR

*  Arguments Returned:
      CHARACTER * ( * ) VALUE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IWKID              ! GKS workstation identifier

*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Start the GNS system for GKS.
      CALL GNS_START( 'GKS', STATUS )

*  Inquire the workstation identifier for GKS inquiries.
      CALL SGS_ICURW( IWKID )

*  Find the characteristic of the workstation.
      CALL GNS_IWCG( IWKID, WKCHAR, VALUE, STATUS )

*  Stop the GNS system for GKS.
      CALL GNS_STOP( 'GKS', STATUS )

      END
