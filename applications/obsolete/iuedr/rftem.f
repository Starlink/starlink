      SUBROUTINE RFTEM( UFILE, STATUS )
*+
*  Name:
*    SUBROUTINE RFTEM

*  Purpose:
*     Read spectrum template data from a file.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RFTEM( UFILE, STATUS )

*  Arguments:
*     UFILE = BYTE( * ) (Given)
*        Name of the file to read.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Description:
*     For LORES, the default file is
*
*        $IUEDR_DATA/<camera><resolution>.tem,
*
*     which can be replaced by a user supplied file.
*     For HIRES, the default file is
*
*        $IUEDR_DATA/<camera><aperture><resolution>.tem,
*
*     since only a single aperture is stored in each file.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*   History:
*     01-MAY-82 (JRG):
*       AT4 version
*     06-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     18-OCT-94 (MJC):
*       IUEDR Vn. 3.1-8
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
      INCLUDE 'CMTEM'

*  Local Constants:
      INTEGER FILENAMESIZE       ! Maximum file name length.
      INTEGER HIRES              ! HIRES index.
      INTEGER LORES              ! LORES index.
      PARAMETER ( FILENAMESIZE = 81, HIRES = 2, LORES = 1 )

*  Arguments Given:
      BYTE UFILE( FILENAMESIZE ) ! User supplied filename.

*  Status:
      INTEGER STATUS             ! Global status.

*  External References:
      INTEGER STR_LEN            ! String length.

*  Local Variables:
      BYTE FILE( FILENAMESIZE )  ! File for default inputs.

      INTEGER ACTVAL        ! Parameter value count.
      INTEGER FD            ! File descriptor.
      INTEGER IRES          ! Resolution index.
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get resolution index.
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         GO TO 999
      END IF

*   Template data.
      IF ( NOTEM ) THEN
         IF ( STR_LEN( UFILE ) .GT. 0 ) THEN
            CALL STR_MOVE( UFILE, FILENAMESIZE, FILE )

         ELSE
            CALL STR_MOVE( 'IUEDR_DATA:\\', FILENAMESIZE, FILE )
            CALL STR_APPND( CAMERA, FILENAMESIZE, FILE )

            IF ( IRES .EQ. HIRES ) THEN
               CALL STR_APPND( 'HI\\', FILENAMESIZE, FILE )
               CALL STR_APPND( APER, FILENAMESIZE, FILE )

            ELSE IF ( IRES .EQ. LORES ) THEN
               CALL STR_APPND( 'LO\\', FILENAMESIZE, FILE )
            END IF
         END IF

         CALL RDFILE( FILE, '.tem\\', FD, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL LINE_WCONT( '%p No Spectrum Template Data.\\' )
            STATUS = SAI__OK
            CALL PRTBUF( STATUS )

         ELSE
            CALL RDTEM( FD, IRES, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: in template data\\', STATUS )
            END IF

            CALL CLFILE( FILE, FD, STATUS )
         END IF
      END IF

 999  CONTINUE

      END
