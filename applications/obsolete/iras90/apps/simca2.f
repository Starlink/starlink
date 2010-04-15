      SUBROUTINE SIMCA2( PHIST, INDF4, INDF3, INDF1, IGRPC, DGOOD,
     :                   DUSED, STATUS )
*+
*  Name:
*     SIMCA2

*  Purpose:
*     Add history to a simulated CRDD file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SIMCA2( PHIST, INDF4, INDF3, INDF1, IGRPC, DGOOD, DUSED,
*                  STATUS )

*  Description:
*     The output CRDD file, input sky image and CRDD file, and all the
*     PSF images are recorded in the HISTORY component of the output
*     CRDD file (so long as the user doesn't suppress this using the
*     parameter given by PHIST).

*  Arguments:
*     PHIST = CHARACTER * ( * ) (Given)
*        The name of the parameter used to supress history.
*     INDF4 = INTEGER (Given)
*        An NDF identifier for the output simulated CRDD file.
*     INDF3 = INTEGER (Given)
*        An NDF identifier for the input real CRDD file on which the
*        output simulated CRDD file is modelled.
*     INDF1 = INTEGER (Given)
*        An NDF identifier for the sky image.
*     IGRPC = INTEGER (Given)
*        The GRP identifier for the group holding the PSFs.
*     DGOOD = INTEGER ( Given)
*        The number of detectors for which simulated data was
*        successfully generated.
*     DUSED( DGOOD ) = INTEGER ( Given)
*        The detector numbers for which simulated data was successfully
*        generated.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     8-JAN-1993 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'I90_DAT'          ! IRAS90 data
      INCLUDE 'GRP_PAR'          ! GRP_ constants

*  Arguments Given:
      CHARACTER PHIST*(*)
      INTEGER INDF4
      INTEGER INDF3
      INTEGER INDF1
      INTEGER IGRPC
      INTEGER DGOOD
      INTEGER DUSED( DGOOD )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TEXT( 4 + I90__MAXDT )*80 ! History text.
      CHARACTER NDFNAM*(GRP__SZNAM)! Name of PSF NDF.

      INTEGER DET                ! Index into the DUSED array.
      INTEGER DETNO              ! Detector number.
      INTEGER I                  ! Current no. of lines of text.
      INTEGER LEN                ! Used length of text.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Construct the text of the history record.
      CALL NDF_MSG( 'NDF', INDF4 )
      CALL MSG_LOAD( ' ', 'Simulated CRDD file ^NDF created:',
     :               TEXT( 1 ), LEN, STATUS )

      CALL NDF_MSG( 'NDF', INDF3 )
      CALL MSG_LOAD( ' ', '  Sample positions defined by CRDD file '//
     :               '^NDF', TEXT( 2 ), LEN, STATUS )

      CALL NDF_MSG( 'NDF', INDF1 )
      CALL MSG_LOAD( ' ', '  Sky brightness defined by image '//
     :               '^NDF', TEXT( 3 ), LEN, STATUS )

      TEXT( 4 ) = '  Point Spread Functions used:'
      I = 4

      DO DET = 1, DGOOD
         DETNO = DUSED( DET )
         I = I + 1
         CALL GRP_GET( IGRPC, DETNO, 1, NDFNAM, STATUS )
         CALL MSG_SETC( 'NDF', NDFNAM )
         CALL MSG_SETI( 'DET', DETNO )
         CALL MSG_LOAD( ' ', '    #^DET : ^NDF', TEXT( I ), LEN,
     :                  STATUS )
      END DO

*  Store the lines of history text.
      CALL IRM_HIST( PHIST, INDF4, 'IRAS90:SIMCRDD', I, TEXT, STATUS )

      END
