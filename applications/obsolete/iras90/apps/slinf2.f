      SUBROUTINE SLINF2( IRA, SCS, LBND, UBND, NGCRL, GLON, GLAT, GANG,
     :                   GSCT, STATUS )
*+
*  Name:
*     SLINF2

*  Purpose:
*     Draw great circle sections.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SLINF2( IRA, SCS, LBND, UBND, NGCRL, GLON, GLAT, GANG,
*                  GSCT, STATUS )

*  Description:
*     This subroutine is used to draw great circle sections specified by
*     their start positions, their position angle and their lengths. If
*     the given length of a great circle section is 0, a longest great
*     circle section will be drawn over the image.

*  Arguments:
*     IRA = INTEGER (Given)
*        The ID of the IRA system.
*     SCS = CHARACTER*( * ) (Given)
*        Name of sky coordinate system used.
*     LBND( 2 ), UBND( 2 ) = REAL (Given)
*        The bounds of the current SGS zone in pixels.
*     NGCRL = INTEGER (Given)
*        Number of great circle sections to be drawn.
*     GLON( NGCRL ) = DOUBLE PRECISION (Givne)
*        Longitude of begin position of each great circle section.
*     GLAT( NGCRL ) = DOUBLE PRECISION (Givne)
*        Latitude of begin position of each great circle section.
*     GANG( NGCRL ) = DOUBLE PRECISION (Given)
*        Position angle of each great circle section.
*     GSCT( NGCRL ) = DOUBLE PRECISION (Givne)
*        Length of each great circle section.
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
      INTEGER NGCRL
      DOUBLE PRECISION GLON( NGCRL ), GLAT( NGCRL ), GANG( NGCRL ),
     :                 GSCT( NGCRL )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I                  ! Do loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Draw the sections one by one.
      DO I = 1, NGCRL

*  If the section length is not zero, draw the section as
*  specified.
         IF ( GSCT( I ) .NE. 0.0D0 ) THEN
            CALL IRA_DRGTC( IRA, GLON( I ), GLAT( I ), GANG( I ),
     :                      GSCT( I ), SCS, LBND, UBND, STATUS )

*  Otherwise, draw a longest great circle section passing the start
*  point.
         ELSE
            CALL IRA_DRGTC( IRA, GLON( I ), GLAT( I ), GANG( I ),
     :                      IRA__TWOPI, SCS, LBND, UBND, STATUS )

         END IF
      END DO

      END
