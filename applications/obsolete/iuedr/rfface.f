      SUBROUTINE RFFACE( STATUS )
*+
*   Name:
*      SUBROUTINE RFFACE
*
*   Description:
*      The face-plate data are read from a file.
*      The default file is $IUEDR_DATA/<camera><geom>.fpt,
*      which can be changed if the FACEFILE parameter is defined.
*
*   History:
*      Jack Giddings      01-MAY-82     AT4 version
*      Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*      Martin Clayton     25-NOV-94     IUEDR Vn. 3.2
*
*   Method:
*
*   Deficiencies:
*      The FACEFILE parameter is not read.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMFACE'

*   Constants:
      INTEGER ERR               ! error status
      INTEGER FILENAMESIZE      ! maximum length of filename
      INTEGER HIRES             ! HIRES index
      INTEGER LORES             ! LORES index
      PARAMETER (ERR = -3, FILENAMESIZE = 81, HIRES = 2, LORES = 1)

*   Export:
      INTEGER STATUS            ! status return

*   Local variables:
      BYTE FILE(FILENAMESIZE)   ! file for default inputs

      INTEGER FD                ! file descriptor
      INTEGER IRES              ! resolution index

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Get resolution index
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS.NE.SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         RETURN
      END IF

*   FACEPLATE
      IF ( NOFACE ) THEN
         CALL STR_MOVE( 'IUEDR_DATA:\\', FILENAMESIZE, FILE )
         CALL STR_APPND( CAMERA, FILENAMESIZE, FILE )

         IF ( GEOM ) THEN
            CALL STR_APPND( 'G\\', FILENAMESIZE, FILE )

         ELSE
            CALL STR_APPND( 'R\\', FILENAMESIZE, FILE )
         END IF

         IF ( IRES .EQ. LORES ) THEN
            CALL STR_APPND( 'LO\\', FILENAMESIZE, FILE )

         ELSE IF ( IRES .EQ. HIRES ) THEN
            CALL STR_APPND( 'HI\\', FILENAMESIZE, FILE )
         END IF

         CALL RDFILE( FILE, '.fpt\\', FD, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: no face-plate defaults\\', STATUS )

         ELSE
            CALL RDFACE( FD, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL ERROUT( 'Error: reading face-plate defaults\\',
     :                      STATUS )
            END IF
            CALL CLFILE( FILE, FD, STATUS )
         END IF
      END IF
      END
