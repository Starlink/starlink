      SUBROUTINE PREPA8( NCARD, FITS, BAND, STATUS )
*+
*  Name:
*     PREPA8

*  Purpose:
*     Get a survey waveband index from the input FITS header.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PREPA8( NCARD, FITS, BAND, STATUS )

*  Description:
*     This routine examines the value of the FITS keyword CRVAL3 to get
*     the IRAS wave band, and returns the index (1 to 4) of that band.

*  Arguments:
*     NCARD = INTEGER (Given)
*        The number of FITS header cards.
*     FITS( NCARD ) = CHARACTER * ( * ) (Given)
*        The FITS header cards.
*     BAND = INTEGER (Returned)
*        The waveband index.
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
      INCLUDE 'I90_DAT'          ! IRAS90 data.

*  Arguments Given:
      INTEGER NCARD
      CHARACTER FITS( NCARD )*(*)

*  Arguments Returned:
      INTEGER BAND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER CARD               ! FITS header card index.
      INTEGER I                  ! Loop count.

      LOGICAL THERE              ! True if keyword was found.

      REAL WAVELN                ! Value of CRVAL3 keyword.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the returned band to indicate that no valid waveband index
*  has yet been found.
      BAND = 0

*  Get the value of the FITS keyword CRVAL3.
      CALL IRM_GKEYR( NCARD, FITS, 1, 'CRVAL3', THERE, WAVELN, CARD,
     :             STATUS )

*  Convert the wavelength from metres to microns.
      WAVELN = WAVELN * 1.0E6

*  Set the wave band number according to the wave length.
      IF( THERE ) THEN
         DO I = 1, I90__BANDS
            IF ( NINT( WAVELN ) .EQ. I90__WAVEL( I ) ) BAND = I
         END DO
      END IF

*  Report an error if the band was not found.
      IF( BAND .EQ. 0 .AND. STATUS .EQ. SAI__OK ) THEN
         STATUS = SAI__ERROR

         IF( THERE ) THEN
            CALL MSG_SETR( 'W', WAVELN )
            CALL ERR_REP( 'PREPA8_ERR1',
     :             'PREPA8: Unknown IRAS survey wave band found: ^W um',
     :                 STATUS )

         ELSE
            CALL ERR_REP( 'PREPA8_ERR2',
     :                    'PREPA8: No IRAS survey wave band found',
     :                    STATUS )
         END IF

      END IF

      END
