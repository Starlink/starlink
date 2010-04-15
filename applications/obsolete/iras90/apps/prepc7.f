      SUBROUTINE PREPC7( NCARD, FITS, FLUX, NOISE, STATUS )
*+
*  Name:
*     PREPC7

*  Purpose:
*     See what type of PO image is being handled.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPC7( NCARD, FITS, FLUX, NOISE, STATUS )

*  Description:
*     This routine determines if the image being handled is a PO FLUX or
*     INTENSITY grid, and also if the map is a data or noise map. The
*     grid type is found by looking at the FITS keyword BUNIT, the map
*     type is found by looking at the comment following the BUNIT
*     keyword.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     FLUX = LOGICAL (Returned)
*        True if the image is a PO flux grid, false if it is an
*        intensity grid.
*     NOISE = LOGICAL (Returned)
*        True if the map is a noise map, false if it is a data map.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     14-DEC-1992 (DSB):
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
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)

*  Arguments Returned:
      LOGICAL FLUX
      LOGICAL NOISE

*  Status:
      INTEGER STATUS             ! Global status

*  External References:
      LOGICAL CHR_SIMLR          ! True if two strings are the same
                                 ! apart from case.

*  Local Variables:
      CHARACTER TEXT*80          ! A text string.

      INTEGER CARD               ! FITS header card number.

      LOGICAL THERE              ! True if a keyword or comment was
                                 ! found.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get the comment string of keyword BUINT, and remove leading blanks.
      CALL IRM_GKCMT( NCARD, FITS, 1, 'BUNIT', '/', 0, THERE, TEXT,
     :                CARD, STATUS )
      CALL CHR_LDBLK( TEXT )

*  Set the flag if this is a noise grid.
      IF( CHR_SIMLR( TEXT( : 4 ), 'NOIS' ) ) THEN
         NOISE = .TRUE.
      ELSE
         NOISE = .FALSE.
      END IF

*  Get value of the FITS keyword BUNIT, and remove all blanks.
      CALL IRM_GKEYC( NCARD, FITS, CARD, 'BUNIT', THERE, TEXT, CARD,
     :                STATUS )
      CALL CHR_RMBLK( TEXT )

*  If the units are 'JY', the image is a PO FLUX.
      IF ( CHR_SIMLR( TEXT, 'JY' ) ) THEN
         FLUX = .TRUE.

*  Or if the units are 'JY/SR', image is a PO INTENSITY.
      ELSE IF ( CHR_SIMLR( TEXT, 'JY/SR' ) ) THEN
         FLUX = .FALSE.
      END IF

      END
