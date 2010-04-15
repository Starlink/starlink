      SUBROUTINE CTEMZ3( PARAM, INDF1, INDF2, INDF3, INDF4, BAND1,
     :                   BAND2, BETA, LERR, RERR, WAVEL, STATUS )
*+
*  Name:
*     CTEMZ3

*  Purpose:
*     Add history to both COLTEMP output NDFs.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CTEMZ3( PARAM, INDF1, INDF2, INDF3, INDF4, BAND1, BAND2,
*                  BETA, LERR, RERR, WAVEL, STATUS )

*  Description:
*     The name of the input and output NDFs together with the values of
*     important parameters are stored in the history.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        A parameter to use to control the storage of HISTORY in the
*        output NDFs. See routine IRM_HIST.
*     INDF1 = INTEGER (Given)
*        An identifier for the shorter waveband input NDF.
*     INDF2 = INTEGER (Given)
*        An identifier for the longer waveband input NDF.
*     INDF3 = INTEGER (Given)
*        An identifier for the output NDF holding temperatures.
*     INDF4 = INTEGER (Given)
*        An identifier for the output NDF holding optical depths. Not
*        used if WAVEL is less than or equal to zero.
*     BAND1 = INTEGER (Given)
*        The lower waveband index.
*     BAND2 = INTEGER (Given)
*        The higher waveband index.
*     BETA = REAL (Given)
*        The value of parameter BETA.
*     LERR = REAL (Given)
*        The value of parameter LERR.
*     RERR = REAL (Given)
*        The value of parameter RERR.
*     WAVEL = REAL( Given)
*        The value of parameter WAVEL. Zero or negative if no optical
*        depth image was created.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     21-MAY-1993 (DSB):
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
      INTEGER INDF4
      INTEGER BAND1
      INTEGER BAND2
      REAL BETA
      REAL LERR
      REAL RERR
      REAL WAVEL

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER TEXT( 9 )*80     ! History text.
      INTEGER LENGTH             ! Used length of text.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  First set up the text of the history record for the output
*  temperature map.
      CALL NDF_MSG( 'OUT', INDF3 )
      CALL MSG_LOAD( ' ', ' Colour temperature map ^OUT created from:',
     :               TEXT( 1 ), LENGTH, STATUS )

      CALL NDF_MSG( 'IN1', INDF1 )
      CALL MSG_SETI( 'W', I90__WAVEL( BAND1 ) )
      CALL MSG_LOAD( ' ', '   ^W um surface brightness map ^IN1',
     :               TEXT( 2 ), LENGTH, STATUS )

      CALL NDF_MSG( 'IN2', INDF2 )
      CALL MSG_SETI( 'W', I90__WAVEL( BAND2 ) )
      CALL MSG_LOAD( ' ', '   ^W um surface brightness map ^IN2',
     :               TEXT( 3 ), LENGTH, STATUS )

      TEXT( 4 ) = ' '
      TEXT( 5 ) = 'Parameter values:'

      CALL MSG_SETR( 'B', BETA )
      CALL MSG_LOAD( ' ', '   BETA : ^B', TEXT( 6 ), LENGTH, STATUS )

      CALL MSG_SETR( 'L', LERR )
      CALL MSG_LOAD( ' ', '   LERR : ^L', TEXT( 7 ), LENGTH, STATUS )

      CALL MSG_SETR( 'R', RERR )
      CALL MSG_LOAD( ' ', '   RERR : ^R', TEXT( 8 ), LENGTH, STATUS )

*  If no optical depth output image was created...
      IF( WAVEL .LE. 0.0D0 ) THEN
         TEXT( 9 ) = '   WAVEL: (no optical depth image produced )'

*  If an optical depth image was created...
      ELSE
         CALL MSG_SETR( 'W', WAVEL)
         CALL MSG_LOAD( ' ', '   WAVEL: ^W', TEXT( 9 ), LENGTH, STATUS )
      END IF

*  Put the text into a history record.
      CALL IRM_HIST( PARAM, INDF3, 'IRAS90:COLTEMP', 9, TEXT, STATUS )

*  Modify the text for the optical depth map (if one was created).
      IF( WAVEL .GT. 0.0 ) THEN
         CALL NDF_MSG( 'OUT', INDF4 )
         CALL MSG_LOAD( ' ', ' Optical depth map ^OUT created from:',
     :               TEXT( 1 ), LENGTH, STATUS )

*  Put the text into a history record.
         CALL IRM_HIST( PARAM, INDF4, 'IRAS90:COLTEMP', 9, TEXT,
     :                  STATUS )
      END IF

      END
