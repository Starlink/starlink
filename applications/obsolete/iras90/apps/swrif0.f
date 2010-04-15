      SUBROUTINE SWRIF0( FRCT, GZONE, STATUS )
*+
*  Name:
*     SWRIF0

*  Purpose:
*     Creat a zone FRCT% larger than the current zone.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SWRIF0( FRCT, GZONE, STATUS )

*  Description:
*     This subroutine create a SGS zone larger than the current zone by
*     a given precentage on the currently used graphic display surface
*     The zone created is clipped by the base zone which is the largest
*     zone can be created on the device. On exit the created zone is
*     selected as current.

*  Arguments:
*     FRCT = REAL (Given)
*       The precentage by which the created zone is larger than the
*       current zone on entry.
*     GZONE = INTEGER (Returned)
*       The ID of the newly created zone.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     11-FEB-1993 (WG):
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
      REAL FRCT

*  Arguments Returned:
      INTEGER GZONE

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER PICIDB             ! AGI ID of the base picture
      REAL XB1, XB2, YB1, YB2, XMB, YMB
                                 ! Extension and size of the base zone
      REAL XP1, XP2, YP1, YP2, XMP, YMP
                                 ! Extension and size of the current
                                 ! zone on entry
      REAL XPB1, XPB2, YPB1, YPB2  ! Extension of the current zone on
                                   ! entry in the coordinate of the base
                                   ! zone
      REAL XW1, XW2, YW1, YW2    ! Extension of created larger zone
      REAL XWP1, XWP2, YWP1, YWP2  ! Extension of the created zone in
                                   ! the coordinate of the zone on entry
      INTEGER ZONEB              ! SGS ID of the base zone
      INTEGER ZONEP              ! SGS ID of the zone on entry
      INTEGER ZONEW              ! SGS ID of the new zone

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get an ID of the current zone and find its size.
      CALL SGS_ICURZ( ZONEP )
      CALL SGS_IZONE( XP1, XP2, YP1, YP2, XMP, YMP )

*  Select the base picture as current and get a SGS zone ID for it
*  first.
      CALL AGI_IBASE( PICIDB, STATUS )
      CALL AGI_SELP( PICIDB, STATUS )
      CALL AGS_NZONE( ZONEB, STATUS )

*  Find the extension of the basezone.
      CALL SGS_IZONE( XB1, XB2, YB1, YB2, XMB, YMB )

*  Find the extension of the zone on entry in the base zone.
      CALL SGS_TPZ( ZONEP, XP1, YP1, ZONEB, XPB1, YPB1, STATUS )
      CALL SGS_TPZ( ZONEP, XP2, YP2, ZONEB, XPB2, YPB2, STATUS )

*  Find the bounds of a zone which is the required percentage larger
*  than the zone on entry.
      XW1 = MAX( XB1, XPB1 - FRCT*0.01*( XPB2 - XPB1 ) )
      XW2 = MIN( XB2, XPB2 + FRCT*0.01*( XPB2 - XPB1 ) )
      YW1 = MAX( YB1, YPB1 - FRCT*0.01*( YPB2 - YPB1 ) )
      YW2 = MIN( YB2, YPB2 + FRCT*0.01*( YPB2 - YPB1 ) )

*  Create a zone with these bounds. And set the its coordinate system
*  as that of base zone.
      CALL SGS_ZONE( XW1, XW2, YW1, YW2, ZONEW, STATUS )
      CALL SGS_SW( XW1, XW2, YW1, YW2, STATUS )

*  Find its extension under the coordinate of the zone on entry.
      CALL SGS_TPZ( ZONEW, XW1, YW1, ZONEP, XWP1, YWP1, STATUS )
      CALL SGS_TPZ( ZONEW, XW2, YW2, ZONEP, XWP2, YWP2, STATUS )

*  Set the coordinate of the new zone as that of the zone on entry.
      CALL SGS_SW( XWP1, XWP2, YWP1, YWP2, STATUS )

*  Release the base zone.
      CALL SGS_RELZ( ZONEB )

      END
