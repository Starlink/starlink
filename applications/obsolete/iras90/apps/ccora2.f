      SUBROUTINE CCORA2( PARAM, INDF1, INDF2, INDF3, BAND, STATUS )
*+
*  Name:
*     CCORA2

*  Purpose:
*     Add history to COLCORR output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCORA2( PARAM, INDF1, INDF2, INDF3, BAND, STATUS )

*  Description:
*     The name of the input and output NDFs and waveband are stored in
*     the history.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        A parameter to use to control the storage of HISTORY in the
*        output NDFs. See routine IRM_HIST.
*     INDF1 = INTEGER (Given)
*        An identifier for the temperature input NDF.
*     INDF2 = INTEGER (Given)
*        An identifier for the optical depth input NDF.
*     INDF3 = INTEGER (Given)
*        An identifier for the surface brightness output NDF.
*     BAND = INTEGER (Given)
*        The output waveband index.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-MAY-1993 (DSB):
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
      CHARACTER PARAM*(*)
      INTEGER INDF1
      INTEGER INDF2
      INTEGER INDF3
      INTEGER BAND

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TEXT( 3 )*80     ! History text.
      INTEGER LENGTH             ! Used length of text.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First set up the text of the history record for the output
*  temperature map.
      CALL NDF_MSG( 'OUT', INDF3 )
      CALL MSG_SETI( 'W', I90__WAVEL( BAND ) )
      CALL MSG_LOAD( ' ',
     :' ^W micron colour corrected image ^OUT created from:', TEXT( 1 ),
     : LENGTH, STATUS )

      CALL NDF_MSG( 'IN1', INDF1 )
      CALL MSG_LOAD( ' ', '   Colour temperature map ^IN1',
     :               TEXT( 2 ), LENGTH, STATUS )

      CALL NDF_MSG( 'IN2', INDF2 )
      CALL MSG_LOAD( ' ', '   Optical depth map ^IN2',
     :               TEXT( 3 ), LENGTH, STATUS )

*  Put the text into a history record.
      CALL IRM_HIST( PARAM, INDF3, 'IRAS90:COLTEMP', 3, TEXT, STATUS )

      END
