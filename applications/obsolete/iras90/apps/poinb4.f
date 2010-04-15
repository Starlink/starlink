      SUBROUTINE POINB4( MAXDET, DETNUM, IDET, LOGFID, LOGREQ, STATUS )
*+
*  Name:
*     POINB4

*  Purpose:
*     To report error found in current detector

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL POINB4( MAXDET, DETNUM, IDET, LOGFID, LOGREQ, STATUS )

*  Description:
*     To report error found in current detector
*
*  Arguments:
*     MAXDET = INTEGER (Given)
*        Maximum number of detectors
*     DETNUM( MAXDET )  = INTEGER (Given)
*        Detector number for given detector index
*     IDET = INTEGER (Given)
*        Detector index for the current detector
*     LOGFID = INTEGER (Returned)
*        When logging is required, it gives the ID of the logfile.
*     LOGREQ = LOGICAL (Returned)
*        TRUE when logging results to the logfile is required.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     DCP: Diana Parsons (FIIS\RAL)
*     {enter_new_authors_here}

*  History:
*     17-OCT-1994 (DCP):  (My son's 18th birthday)
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'MSG_PAR'          ! MSG system constants

*  Arguments Given:
      INTEGER MAXDET
      INTEGER DETNUM( MAXDET )
      INTEGER IDET
      INTEGER LOGFID
      LOGICAL LOGREQ

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      CHARACTER*( 5 )DETST       ! Character string of detector number
      INTEGER DETLN	         ! Length of the DETST
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Translate the detector number associated with the detector index number
*  to a character string
         CALL CHR_ITOC( DETNUM( IDET ), DETST, DETLN )

*  If no source has been detected, report.
         CALL MSG_BLANK( STATUS )
         CALL MSG_OUT( 'POINB4_MSG2', 'Insufficient valid data '/
     :              /'in the trace of detector '//DETST(  : DETLN ),
     :              STATUS )

*  Write to the logging file as well if requested.
         IF ( LOGREQ ) THEN
            CALL FIO_WRITE( LOGFID, ' ', STATUS )
            CALL FIO_WRITE( LOGFID, 'Insufficient valid data in the '/
     :                       /'trace of detector '//DETST( : DETLN ),
     :                       STATUS )
         END IF
      END
