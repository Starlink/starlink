      SUBROUTINE RFDISP( UFILE, STATUS )
*+
*  Name:
*     SUBROUTINE RFDISP

*  Description:
*     The dispersion constants are read from a file.
*     The default file is $IUEDR_DATA/<camera><geom><resolution>.dsp,
*     which can be replaced by a user supplied file.

*  History:
*     Jack Giddings      01-MAY-82     AT4 version
*     Paul Rees          06-OCT-88     IUEDR Vn. 2.0
*     Martin Clayton     25-NOV-94     IUEDR Vn. 3.2

*-

*  Implicit:
      IMPLICIT NONE

*  Starlink includes:
      INCLUDE 'SAE_PAR'

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISP'

*  Constants:
      INTEGER FILENAMESIZE       ! maximum file name string length
      INTEGER HIRES              ! HIRES index
      INTEGER LORES              ! LORES index
      PARAMETER (FILENAMESIZE = 81, HIRES = 2, LORES = 1)

*  Import:
      BYTE UFILE(FILENAMESIZE)   ! user supplied file

*  Export:
      INTEGER STATUS             ! status return

*  External references:
      INTEGER STR_LEN            ! string length

*  Local variables:
      BYTE FILE(FILENAMESIZE)    ! file for default inputs

      INTEGER FD                 ! file descriptor
      INTEGER IRES               ! resolution index

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get resolution index
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         RETURN
      END IF

*  Dispersion constants
      IF ( NODISP ) THEN
         IF ( STR_LEN( UFILE ) .GT. 0 ) THEN
            CALL STR_MOVE( UFILE, FILENAMESIZE, FILE )

         ELSE
            CALL STR_MOVE( 'IUEDR_DATA:\\', FILENAMESIZE, FILE )
            CALL STR_APPND( CAMERA, FILENAMESIZE, FILE )

            IF ( IRES .EQ. LORES ) THEN
               CALL STR_APPND( 'LO\\', FILENAMESIZE, FILE )

            ELSE IF ( IRES .EQ. HIRES ) THEN
               CALL STR_APPND( 'HI\\', FILENAMESIZE, FILE )
            END IF
         END IF

         CALL RDFILE( FILE, '.dsp\\', FD, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: no dispersion defaults\\', STATUS )

         ELSE
            CALL RDDISP( FD, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
                 CALL ERROUT( 'Error: reading dispersion defaults\\',
     :                        STATUS )
            END IF
            CALL CLFILE( FILE, FD, STATUS )
         END IF
      END IF
      END
