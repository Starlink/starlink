      SUBROUTINE RFRIP( UFILE, STATUS )
*+
*  Name:
*     SUBROUTINE RFRIP

*  Purpose:
*     Read ripple calibration from a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RFRIP( UFILE, STATUS )

*  Arguments:
*     UFILE = BYTE( * ) (Given)
*        Name of the specific file to be read.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     The default file is $IUEDR_DATA/<camera><geom>.rip,
*     which can be over-ridden if UFILE is provided.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     25-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMRIP'

*  Local Constants:
      INTEGER FILENAMESIZE       ! Maximum file name string length.
      INTEGER HIRES              ! HIRES index.
      PARAMETER ( FILENAMESIZE = 81, HIRES = 2 )

*  Arguments Given:
      BYTE UFILE( FILENAMESIZE )  ! User supplied filename.

*  Status:
      INTEGER STATUS             ! Global status.

*  External References:
      INTEGER STR_LEN            ! String length.

*  Local Variables:
      BYTE FILE( FILENAMESIZE )  ! File for default inputs.

      INTEGER FD                 ! File descriptor.
      INTEGER IRES               ! Resolution index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get resolution index.
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         RETURN
      END IF

*   Ripple Calibration.
      IF ( NORIP .AND. IRES.EQ.HIRES .AND. PHOT ) THEN
         IF ( STR_LEN( UFILE ) .GT. 0 ) THEN
            CALL STR_MOVE( UFILE, FILENAMESIZE, FILE )

         ELSE
            CALL STR_MOVE( 'IUEDR_DATA:\\', FILENAMESIZE, FILE )
            CALL STR_APPND( CAMERA, FILENAMESIZE, FILE )
         END IF

         CALL RDFILE( FILE, '.rip\\', FD, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL LINE_WCONT( '%p No Ripple Calibration.\\' )
            STATUS = SAI__OK
            CALL PRTBUF( STATUS )

         ELSE
            CALL RDRIP( FD, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: in ripple calibration\\', STATUS )
            END IF
            CALL CLFILE( FILE, FD, STATUS )
         END IF
      END IF

      END
