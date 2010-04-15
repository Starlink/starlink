      SUBROUTINE DESTA1( INDF1, INDF2, SHIGH, SLOW, DETNO, DETOFF,
     :                   UNITS, STATUS )
*+
*  Name:
*     DESTA1

*  Purpose:
*     Add history to a destriped output NDF.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DESTA1( INDF1, INDF2, SHIGH, SLOW, DETNO, DETOFF, UNITS,
*                  STATUS )

*  Description:
*     The HISTORY added to the output NDF consists of a record of the
*     input and output CRDD files, the values of the essential
*     DESTCRDD parameters, and the offsets subtracted from each
*     detector.

*  Arguments:
*     INDF1 = INTEGER (Given)
*        The NDF identifier for the input CRDD file.
*     INDF2 = INTEGER (Given)
*        The NDF identifier for the output destriped CRDD file.
*     SHIGH = INTEGER (Given)
*        The upper bound of the DETOFF and DETNO arrays.
*     SLOW = INTEGER (Given)
*        The lower bound of the DETOFF and DETNO arrays.
*     DETNO( SLOW:SHIGH ) = INTEGER (Given)
*        The detector numbers.
*     DETOFF( SLOW:SHIGH ) = REAL (Given)
*        The detector offsets.
*     UNITS = CHARACTER * ( * ) (Given)
*        The units of the detector offsets.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DSB: David Berry (STARLINK)
*     {enter_new_authors_here}

*  History:
*     1-OCT-1992 (DSB):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! VAL_ constants.
      INCLUDE 'I90_DAT'          ! IRAS90 constants.

*  Arguments Given:
      INTEGER INDF1
      INTEGER INDF2
      INTEGER SHIGH
      INTEGER SLOW
      INTEGER DETNO( SLOW:SHIGH )
      REAL DETOFF( SLOW:SHIGH )
      CHARACTER UNITS*(*)

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER CVAL*30          ! Character parameter value.
      INTEGER I                  ! Loop count.
      INTEGER IVAL               ! Integer parameter value.
      INTEGER L                  ! Index of next line of text.
      INTEGER LENGTH             ! Length of text string.
      REAL RVAL                  ! Real parameter value.
      CHARACTER TEXT( 10 + I90__MAXDT )*80! Lines of history text.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialise the index of the current line of history text.
      L = 1

*  The first task is to set up the text of the history record. First
*  list the input and output CRDD files...
      CALL NDF_MSG( 'OUT', INDF2 )
      CALL MSG_LOAD( ' ', ' ^OUT created by DESTCRDD', TEXT( L ),
     :               LENGTH, STATUS )
      L = L + 1

      CALL NDF_MSG( 'IN', INDF1 )
      CALL MSG_LOAD( ' ', ' Input NDF: ^IN', TEXT( L ), LENGTH, STATUS )
      L = L + 1

      TEXT( L ) = ' '
      L = L + 1

*  Now store selected parameter values...
      TEXT( L ) = 'DESTCRDD parameter values:'
      L = L + 1

*  BOX...
      CALL PAR_GET0I( 'BOX', IVAL, STATUS )
      CALL MSG_SETI( 'I', IVAL )
      CALL MSG_LOAD( ' ', '   BOX        = ^I', TEXT( L ), LENGTH,
     :               STATUS )
      L = L + 1

*  CLIP...
      CALL PAR_GET0R( 'CLIP', RVAL, STATUS )
      CALL MSG_SETR( 'R', RVAL )
      CALL MSG_LOAD( ' ', '   CLIP       = ^R', TEXT( L ),  LENGTH,
     :               STATUS )
      L = L + 1

*  NITER...
      CALL PAR_GET0I( 'NITER', IVAL, STATUS )
      CALL MSG_SETI( 'I', IVAL )
      CALL MSG_LOAD( ' ', '   NITER      = ^I', TEXT( L ), LENGTH,
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

*  Now include the detector offsets.
      DO I = SLOW, SHIGH
         CALL MSG_SETI( 'D', DETNO( I ) )

         IF( DETOFF( I ) .NE. VAL__BADR ) THEN
            CALL MSG_SETR( 'O', DETOFF( I ) )
            CALL MSG_SETC( 'U', UNITS )
            CALL MSG_LOAD( ' ', '   ^O ^U subtracted from detector #^D',
     :                     TEXT( L ), LENGTH, STATUS )
         ELSE
            CALL MSG_LOAD( ' ', '   Detector #^D contains no good data',
     :                     TEXT( L ), LENGTH, STATUS )
         END IF

         L = L + 1

      END DO

*  Now put the text into a history record.
      CALL IRM_HIST( 'HISTORY', INDF2, 'IRAS90:DESTCRDD', L - 1, TEXT,
     :               STATUS )

      END
