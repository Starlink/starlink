      SUBROUTINE BACKA2( INDF1, INDF2, OUTTYP, TYPE, REMOVE, GRAD,
     :                   OFFSET, SLOW, DNLOW, STATUS )
*+
*  Name:
*     BACKA2

*  Purpose:
*     Add history to a BACKCRDD output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL BACKA2( INDF1, INDF2, OUTTYP, TYPE, REMOVE, GRAD, OFFSET,
*                  SLOW, DNLOW, STATUS )

*  Description:
*     The HISTORY added to the output NDF consists of a record of the
*     input and output CRDD files, the values of the essential
*     BACKCRDD parameters, and the estimated background.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The NDF identifier for the input CRDD file.
*     INDF2 = INTEGER (Given)
*        The NDF identifier for the output CRDD file.
*     OUTTYP = CHARACTER * ( * ) (Given)
*        The type of output NDF; DATA or BACKGROUND.
*     TYPE = CHARACTER * ( * ) (Given)
*        The type of background fit; UNIFORM or LINEAR.
*     REMOVE = REAL (Given)
*        If TYPE is UNIFORM, this gives the estimated uniform
*        background, in Mega-Janskys per steradian.
*     GRAD = REAL (Given)
*        If TYPE is LINEAR, this gives the gradient of the linear
*        background, in Mega-Janskys per steradian, per radian.
*     OFFSET = REAL (Given)
*        If TYPE is LINEAR, this gives the value of the background (in
*        Mega-Jansky per steradian) at the in-scan position of sample
*        SLOW from the detector with index DNLOW.
*     SLOW = INTEGER (Given)
*        The sample number to which OFFSET refers.
*     DNLOW = INTEGER (Given)
*        The detector number to which OFFSET refers.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-OCT-1992 (DSB):
*        Original version.
*     10-MAY-1994 (DCP):
*        Release 1.0 bugfix 12 - Implement DSB changes to the history ie
*        change TYPE .EQ. 'DATA' to OUTTYP .EQ. 'DATA' in IF statement
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'DAT_PAR'          ! DAT constants
      INCLUDE 'I90_DAT'          ! IRAS90 constants.
      INCLUDE 'IRA_PAR'          ! IRA constants.
      INCLUDE 'IRC_PAR'          ! IRC constants.

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      CHARACTER OUTTYP*(*)
      CHARACTER TYPE*(*)
      REAL REMOVE
      REAL GRAD
      REAL OFFSET
      INTEGER SLOW
      INTEGER DNLOW

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CVAL*30          ! Character parameter value.
      INTEGER L                  ! Index of next line of text.
      INTEGER LENGTH             ! Length of text string.
      REAL RVAL                  ! Real parameter value.
      CHARACTER TEXT( 13 )*80    ! Lines of history text.

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the index of the current line of history text.
      L = 1

*  The first task is to set up the text of the history record. First
*  list the input and output CRDD files...
      CALL NDF_MSG( 'OUT', INDF2 )
      CALL MSG_LOAD( ' ', ' ^OUT created by BACKCRDD',
     :               TEXT( L ), LENGTH, STATUS )
      L = L + 1

      CALL NDF_MSG( 'IN', INDF1 )
      CALL MSG_LOAD( ' ', ' Input NDF: ^IN',
     :               TEXT( L ), LENGTH, STATUS )
      L = L + 1

      IF( OUTTYP .EQ. 'DATA' ) THEN
         CALL MSG_LOAD( ' ', ' Contents: Background subtracted CRDD',
     :                  TEXT( L ), LENGTH, STATUS )
      ELSE
         CALL MSG_LOAD( ' ', ' Contents: Estimated background',
     :                  TEXT( L ), LENGTH, STATUS )
      END IF
      L = L + 1

      TEXT( L ) = ' '
      L = L + 1

*  Now store selected parameter values...
      TEXT( L ) = 'BACKCRDD parameter values:'
      L = L + 1

*  CLIP...
      CALL PAR_GET0R( 'CLIP', RVAL, STATUS )
      CALL MSG_SETR( 'R', RVAL )
      CALL MSG_LOAD( ' ', '   CLIP       = ^R', TEXT( L ),  LENGTH,
     :               STATUS )
      L = L + 1

*  OUTBAC...
      CALL PAR_GET0R( 'OUTBACK', RVAL, STATUS )
      CALL MSG_SETR( 'R', RVAL )
      CALL MSG_LOAD( ' ', '   OUTBACK    = ^R', TEXT( L ),  LENGTH,
     :               STATUS )
      L = L + 1

*  QEXP...
      CALL PAR_GET0C( 'QEXP', CVAL, STATUS )
      CALL MSG_SETC( 'C', CVAL )
      CALL MSG_LOAD( ' ', '   QEXP       = ^C', TEXT( L ), LENGTH,
     :               STATUS )
      L = L + 1

*  QNAME...
      CALL PAR_GET0C( 'QNAME', CVAL, STATUS )
      IF( CVAL .NE. ' ' ) THEN
         CALL MSG_SETC( 'C', CVAL )
      ELSE
         CALL MSG_SETC( 'C', '<parameter not used>' )
      END IF
      CALL MSG_LOAD( ' ', '   QNAME      = ^C', TEXT( L ), LENGTH,
     :               STATUS )
      L = L + 1

      TEXT( L ) = ' '
      L = L + 1

*  Now describe the estimated background.
      IF( TYPE .EQ. 'UNIFORM' ) THEN
         CALL MSG_SETR( 'R', REMOVE )
         CALL MSG_SETC( 'U', IRC__MJPS )
         CALL MSG_LOAD( ' ', '   A uniform background of ^R ^U was '//
     :                  'subtracted', TEXT( L ), LENGTH, STATUS )
         L = L + 1

      ELSE IF( TYPE .EQ. 'LINEAR' ) THEN
         CALL MSG_LOAD( ' ', '   A linear background was '//
     :                  'subtracted...', TEXT( L ), LENGTH, STATUS )
         L = L + 1

         CALL MSG_SETR( 'G', REAL( GRAD*IRA__AM2R ) )
         CALL MSG_SETC( 'U', IRC__MJPS )
         CALL MSG_LOAD( ' ', '     Gradient: ^G ^U per arc-minute',
     :                  TEXT( L ), LENGTH, STATUS )
         L = L + 1

         CALL MSG_SETI( 'DN', DNLOW )
         CALL MSG_SETR( 'O', OFFSET )
         CALL MSG_SETC( 'U', IRC__MJPS )
         CALL MSG_SETI( 'S', SLOW )

         CALL MSG_LOAD( ' ', '     Offset: ^O ^U at sample ^S in '//
     :                  'detector #DN', TEXT( L ), LENGTH, STATUS )
         L = L + 1

      END IF

*  Now put the text into a history record.
      CALL IRM_HIST( 'HISTORY', INDF2, 'IRAS90:BACKCRDD', L - 1, TEXT,
     :                STATUS )

      END
