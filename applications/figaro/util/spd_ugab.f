      SUBROUTINE SPD_UGAB( PARAM, PARCAN, STATUS )
*+
*  Name:
*     SPD_UGAB

*  Purpose:
*     Dissociate a device from AGI and PGPLOT in an SGS zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SPD_UGAB( PARAM, PARCAN, STATUS )

*  Description:
*     The routine quadruplet SPD_UGA{ABCD} is used by
*     Specdre instead of AGP_ASSOC, AGP_DEASS, PGPAGE, AGP_SVIEW to
*     overcome the problem that normally the view surface is the base
*     picture and PGPAGE will clear more than the AGI picture to be
*     used.
*
*     This routine closes PGPLOT and disassociates SGS and AGI.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The name of the ADAM parameter for associated with the device.
*        This is passed to AGS_DEASS.
*     PARCAN = LOGICAL (Given)
*        If true the parameter given by PARAM is cancelled, otherwise it
*        is annulled. This is passed to AGS_DEASS.
*     STATUS = INTEGER (Given and Returned)
*        The global status. This routine is executed regardless of the
*        given status.

*  Authors:
*     hme: Horst Meyerdierks (UoE, Starlink)
*     {enter_new_authors_here}

*  History:
*     24 Jun 1993 (hme):
*        Original version.
*     19 May 1994 (hme):
*        Renamed from SPAED.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      LOGICAL PARCAN

*  Status:
      INTEGER STATUS             ! Global status

*.

*  Close PGPLOT
      CALL PGEND

*  Dissociate the device from AGI and SGS.
      CALL AGS_DEASS( PARAM, PARCAN, STATUS )

*  Return.
      END
