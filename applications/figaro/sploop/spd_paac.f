      SUBROUTINE SPD_PAAC( STATUS )
*+
*  Name:
*     SPD_PAAC

*  Purpose:
*     Close interactive display.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_PAAC( STATUS )

*  Description:
*     This routine closes the interactive display opened with SPD_PAAA
*     or SPD_PAAB. Closing the display comprises
*      - deactivating PGPLOT,
*      - ending the AGI scope.

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine tries to do its job even if the
*        given status is bad.

*  Implementation Deficiencies:
*     This routine should also save the visible PGPLOT viewports as AGI
*     pictures.

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

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PICID              ! AGI picture ID

*.

*  Begin error context.
      CALL ERR_BEGIN( STATUS )

*  Check that a device open.
      IF ( .NOT. DEVOPN ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'SPD_PAAC_E01', 'SPD_PAAC: Error closing ' //
     :      'interactive graphics device. No device is open.' //
     :      'Probable programming error.',
     :      STATUS )
         GO TO 500
      END IF

*  Deactivate PGPLOT.
      CALL AGP_DEACT( STATUS )

*  Select the base picture, in order to end the scope.
      CALL AGI_IBASE( PICID, STATUS )
      CALL AGI_SELP(  PICID, STATUS )
      CALL AGI_END( PICID, STATUS )

*  Set flag.
      DEVOPN = .FALSE.

*  End the error context.
 500  CONTINUE
      CALL ERR_END( STATUS )

*  Return.
      END
