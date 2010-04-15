      SUBROUTINE SPD_PAAB( PARAM, STATUS )
*+
*  Name:
*     SPD_PAAB

*  Purpose:
*     Open interactive display via an ADAM parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PAAB( PARAM, STATUS )

*  Description:
*     This routine opens the interactive display whose name is retrieved
*     from the specified ADAM parameter. Opening the display comprises
*      - beginning an AGI scope,
*      - opening the device with AGI_ASSOC such that the device will be
*        cleared,
*      - selecting the AGI base picture,
*      - deleting all other AGI pictures for the device,
*      - activating PGPLOT and opening a viewport in the base picture.
*
*     A call to this routine must be matched with a call to SPD_PAAC to
*     close the device.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter with the device to be opened.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     27 Apr 1994 (hme):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Global Variables:
      INCLUDE 'SPD_PCOM'         ! Specdre SPLOOP common block

*  Arguments Given:
      CHARACTER * ( * ) PARAM

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      EXTERNAL SPD_PBLK          ! Block data routine

*  Local Variables:
      INTEGER PICID              ! AGI picture ID

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Check that no device open yet.
      IF ( DEVOPN ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_PAAB_E01', 'SPD_PAAB: Error opening ' //
     :      'interactive graphics device. A device is already open.',
     :      STATUS )
         GO TO 500
      END IF

*  Begin AGI scope.
      CALL AGI_BEGIN

*  Open the device, select its base picture.
      CALL AGI_ASSOC( PARAM, 'WRITE', PICID, STATUS )
      CALL AGI_IBASE( PICID, STATUS )
      CALL AGI_SELP(  PICID, STATUS )

*  Delete all other AGI pictures for this device from the AGI data base.
      CALL AGI_PDEL( STATUS )

*  Activate PGPLOT and open a viewport on the base picture.
      CALL AGP_ACTIV( STATUS )
      CALL AGP_NVIEW( .FALSE., STATUS )

*  Set flag.
      DEVOPN = ( STATUS .EQ. SAI__OK )

*  Return.
 500  CONTINUE
      END
