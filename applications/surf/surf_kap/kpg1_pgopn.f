      SUBROUTINE KPG1_PGOPN( PNAME, MODE, IPIC, STATUS )
*+
*  Name:
*     KPG1_PGOPN

*  Purpose:
*     Open the AGI daqtabase anc activate a PGPLOT workstation.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPG1_PGOPN( PNAME, MODE, IPIC, STATUS )

*  Description:
*     This routine opens the graphics data abse and actiavtes a PGPLOT
*     workstation selected using trhe specified parameter. Ther user's
*     pallette is then re-instated, over-riding the pallette established
*     by PGPLOT. 
*
*     The device should normally be shut down using KPG1_PGCLS.

*  Arguments:
*     PNAME = CHARACTER * ( * ) (Given)
*        The nameof the parameter to use.
*     MODE = CHARACTER * ( * ) (Given)
*        The AGI access mode; "WRITE" or "UPDATE". Write causes the 
*        current picture to be cleared (the contents of the database are
*        unaffected).
*     IPIC = INTEGER (Returned)
*        AN AGI identifier for the current picture.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David S. Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1999 (DSB):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-
      
*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER PNAME*(*)
      CHARACTER MODE*(*)

*  Arguments Returned:
      INTEGER IPIC

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER IPICB              ! Base picture identifier
*.

*  Check the inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Associate the parameter with a workstation and current picture.
      CALL AGI_ASSOC( PNAME, MODE, IPIC, STATUS )

*  Start a new AGI context.
      CALL AGI_BEGIN

*  Activate PGPLOT and create a viewport.
      CALL AGP_ACTIV( STATUS )

*  Create a viewport for the current picture.
      CALL AGP_NVIEW( .FALSE., STATUS )

*  PGPLOT resets the colour table each time it is opened. So re-instate
*  the user's pallette by loading it from a previously saved file. (see
*  KPG1_PLSAV).
      CALL KPG1_PLLOD( STATUS )

*  Attempt to close down the device if an error has occurred.
      IF( STATUS .NE. SAI__OK ) CALL KPG1_PGCLS( PNAME, .FALSE., 
     :                                           STATUS )

      END
