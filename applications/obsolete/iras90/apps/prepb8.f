      SUBROUTINE PREPB8( NCARD, FITS, LOC, STATUS )
*+
*  Name:
*     PREPB8

*  Purpose:
*     Create and write extra components to IMAGE_INFO of a YORIC NDF

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB8( NCARD, FITS, LOC, STATUS )

*  Description:
*     This routine creates following component in theIMAGE_INFO
*     structure of an IRAS Point Observation image NDF.
*                    YORTYPE <_CHAR*3>
*     Where
*           YORTYPE - Type of YORIX image.
*
*     These components will be assigned values according to the
*     information obtained from the FITS extenion of the NDF.


*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the FITS extension of the NDF.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     LOC = CHARACTER * ( * ) (Given)
*        The locator of the IMAGE_INFO structure of the NDF file.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-SEP-1991 (WG):
*        Original version.
*     3-DEC-1992 (DSB):
*        Name changed from YOINFO to PREPB8, etc.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)
      CHARACTER LOC*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER SUBTYP*3         ! Type of YORIC image.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create required components within IMAGE_INFO structure.
      CALL DAT_NEW0C( LOC, 'YORTYPE', 3, STATUS )

*  See what type of YORIX image it is.
      CALL PREPC6( NCARD, FITS, SUBTYP, STATUS )

*  Write it into the YORTYPE component of the IMAGE_INFO structure.
      CALL CMP_PUT0C( LOC, 'YORTYPE', SUBTYP, STATUS )

      END

