      SUBROUTINE PREPB2( NCARD, FITS, LOC, STATUS )
*+
*  Name:
*     PREPB2

*  Purpose:
*     Write extra components into the IMAGE_INFO structure of a CPC NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPB2( NCARD, FITS, LOC, STATUS )

*  Description:
*     This subroutine creates the following components in the IMAGE_INFO
*     structure, and assigns values to these components according to
*     the information obtained from the FITS extension of the NDF file,
*
*        CPCRAW <_LOGICAL>
*
*     Where
*        CPCRAW - specifies whether the image is CLEAN or RAW. It is
*                 true if the image is a RAW CPC, otherwise it is flase.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of card images in the FITS extension of the NDF.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     LOC = CHARACTER * ( * ) (Given)
*        Locator for the IMAGE_INFO structure.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     WG: Wei Gong (IPMAF)
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     6-SEP-1991 (WG):
*        Original version.
*     3-DEC-1992 (DSB):
*        Name changed from CPINFO to PREPB2, etc.
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
      CHARACTER CMNT*72          ! A comment string from FITS head
      CHARACTER OBJECT*20        ! Value of keyword OBJECT

      INTEGER CARD               ! Card number of a FITS keyword
      INTEGER STCARD             ! Start card number when search FITS

      LOGICAL CLNFLG             ! Flag showing the CPC is a clean one
      LOGICAL THERE              ! FITS keyword flag
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Create required components within IMAGE_INFO structure.
      CALL DAT_NEW0L( LOC, 'CPCRAW', STATUS )

*  Find the FITS keyword OBJECT.
      CALL IRM_GKEYC( NCARD, FITS, 1, 'OBJECT', THERE, OBJECT, STCARD,
     :             STATUS )

*  Get the comment string after keyword OBJECT.
      CALL IRM_COMNT( NCARD, FITS, STCARD, THERE, CMNT, CARD, STATUS )

*  The image is a clean CPC image if the comment string does not
*  contain the string 'RAW'.
      CLNFLG = INDEX( CMNT , 'RAW' ) .EQ. 0
      CALL CMP_PUT0L( LOC, 'CPCRAW', .NOT.CLNFLG, STATUS )

      END
