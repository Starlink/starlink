      SUBROUTINE RFFID( UFILE, STATUS )
*+
*   Name:
*      SUBROUTINE RFFID
*
*   Description:
*      The fiducial positions are read from a file.
*      The default file is $IUEDR_DATA/<camera><geom>.fid,
*      which can be replaced by a user supplied file.
*
*   History:
*      Jack Giddings      01-MAY-82     AT4 version
*      Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     25-NOV-94     IUEDR Vn. 3.2
*
*   Method:
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Global varibales:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFIDS'
      INCLUDE 'CMFIDT'

*   Constants:
      INTEGER FILENAMESIZE       ! maximum string length for file name
      PARAMETER (FILENAMESIZE = 81)

*   Import:
      BYTE UFILE(FILENAMESIZE)   ! user supplied file

*   Export:
      INTEGER STATUS             ! status return

*   External references:
      INTEGER STR_LEN            ! string length

*   Local variables:
      BYTE FILE(FILENAMESIZE)   ! file for default inputs

      INTEGER FD                ! file descriptor
*.

*   Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*   FIDUCIALS
      IF ( NOFIDS ) THEN
         IF ( STR_LEN( UFILE ) .GT. 0 ) THEN
            CALL STR_MOVE( UFILE, FILENAMESIZE, FILE )

         ELSE
            CALL STR_MOVE( 'IUEDR_DATA:\\', FILENAMESIZE, FILE )
            CALL STR_APPND( CAMERA, FILENAMESIZE, FILE )

            IF ( GEOM ) THEN
               CALL STR_APPND( 'G\\', FILENAMESIZE, FILE )

            ELSE
               CALL STR_APPND( 'R\\', FILENAMESIZE, FILE )
            END IF
         END IF

         CALL RDFILE( FILE, '.fid\\', FD, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: no fiducial defaults\\', STATUS )

         ELSE
            CALL RDFIDS( FD, GEOM, STATUS )
            CALL CLFILE( FILE, FD, STATUS )
         END IF
      END IF

      END
