      SUBROUTINE SLINF1( IRA, SCS, LBND, UBND, NPARL,
     :                   PLON, PLAT, PSCT, STATUS )
*+
*  Name:
*     SLINF1

*  Purpose:
*     Draw parallel sections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINF1( IRA, SCS, LBND, UBND, NPARL,
*                  PLON, PLAT, PSCT, STATUS )

*  Description:
*     This subroutine is used to draw parallel sections specified by
*     their start positions and their lengths. If the given length of a
*     section is 0, A longest parallel section will be drawn over the
*     image.

*  Arguments:
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The bounds of the current SGS zone in pixels.
*     NPARL = INTEGER (Given)
*        Number of meridian section to be drawn.
*     PLON( NPARL ) = DOUBLE PRECISION (Givne)
*        Longitude of begin position of each parallel section.
*     PLAT( NPARL ) = DOUBLE PRECISION (Givne)
*        Latitude of begin position of each parallel section.
*     PSCT( NPARL ) = DOUBLE PRECISION (Givne)
*        Length of each parallel section.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     {enter_new_authors_here}

*  History:
*     7-FEB-1993 (WG):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT_ constants
      INCLUDE 'IRA_PAR'          ! IRA_ constants

*  Arguments Given:
      INTEGER IRA
      CHARACTER*( * ) SCS
      REAL LBND( 2 ), UBND( 2 )
      INTEGER NPARL
      DOUBLE PRECISION PLON( NPARL ), PLAT( NPARL ), PSCT( NPARL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      DOUBLE PRECISION LONTMP    ! A temporary longitude value
      DOUBLE PRECISION SCTTMP    ! A temporary sction length

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Draw the sections one by one.
      DO I = 1, NPARL

*  If the section length is not zero, draw the section as
*  specified.
         IF ( PSCT( I ) .NE. 0.0D0 ) THEN
               CALL IRA_DRPAR( IRA, PLON( I ), PLAT( I ), PSCT( I ),
     :                         SCS, LBND, UBND, STATUS )

*  Otherwise, draw a longest parallel section passing the start point.
         ELSE
               CALL IRA_DRPAR( IRA, PLON( I ), PLAT( I ), IRA__TWOPI,
     :                         SCS, LBND, UBND, STATUS )
         END IF
      END DO

      END
