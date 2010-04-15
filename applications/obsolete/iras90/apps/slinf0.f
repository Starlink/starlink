      SUBROUTINE SLINF0( IRA, SCS, LBND, UBND, NMERD,
     :                   MLON, MLAT, MSCT, STATUS )
*+
*  Name:
*     SLINF0

*  Purpose:
*     Draw meridian sections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINF0( IRA, SCS, LBND, UBND, NMERD,
*                  MLON, MLAT, MSCT, STATUS )

*  Description:
*     This subroutine is used to draw meridian sections specified by
*     their start positions and their lengths. If the given length of
*     a meridian section is 0, a longest meridian section will be drawn
*     over the image.

*  Arguments:
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The bounds of the current SGS zone in pixels.
*     NMERD = INTEGER (Given)
*        Number of meridian section to be drawn.
*     MLON( NMERD ) = DOUBLE PRECISION (Givne)
*        Longitude of begin position of each meridian section.
*     MLAT( NMERD ) = DOUBLE PRECISION (Givne)
*        Latitude of begin position of each meridian section.
*     MSCT( NMERD ) = DOUBLE PRECISION (Givne)
*        Length of each meridian section.
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
      INTEGER NMERD
      DOUBLE PRECISION MLON( NMERD ), MLAT( NMERD ), MSCT( NMERD )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index
      DOUBLE PRECISION LATTMP    ! A temporary latitude value
      DOUBLE PRECISION SCTTMP    ! A temporary sction length

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Draw the sections one by one.
      DO I = 1, NMERD

*  If the section length is not zero, draw the section as
*  specified.
         IF ( MSCT( I ) .NE. 0.0D0 ) THEN
            CALL IRA_DRMER( IRA, MLON( I ), MLAT( I ), MSCT( I ), SCS,
     :                      LBND, UBND, STATUS )

*  Otherwise, draw a longest meridian section passing the start point.
         ELSE
            CALL IRA_DRMER( IRA, MLON( I ), MLAT( I ), IRA__TWOPI, SCS,
     :                      LBND, UBND, STATUS )
         END IF
      END DO

      END
