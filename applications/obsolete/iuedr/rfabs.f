      SUBROUTINE RFABS( UFILE, STATUS )
*+
*  Name:
*     SUBROUTINE RFABS
*
*  Description:
*     The absolute calibration is read from a file.
*     The default file is $IUEDR_DATA/<camera><geom><resolution><itf>.abs,
*     which can be over-ridden by a supplied file name UFILE.
*
*  History:
*     Jack Giddings      01-MAY-82     AT4 version
*     Paul Rees          08-DEC-87     IUEDR Vn. 1.4
*        Changed to include Aperture, Trailed and Date
*        sensitivities for IUEDR Version 1.4.
*     Paul Rees          16-JAN-88     IUEDR Vn. 2.0
*     Martin Clayton     03-AUG-94     IUEDR Vn. 3.1-2
*        Fixed ITF number range check.
*-

*  Implicit:
      IMPLICIT NONE

*  Starlink includes:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Constants:
      INTEGER FILENAMESIZE       ! maximum length of file name string
      INTEGER HIRES              ! HIRES index
      INTEGER LORES              ! LORES index
      PARAMETER (FILENAMESIZE = 81, HIRES = 2, LORES = 1)

*  Import:
      BYTE UFILE(FILENAMESIZE)   ! user supplied file

*  Export:
      INTEGER STATUS             ! status return

*  External references:
      LOGICAL STR_SIMLR          ! string case independent equality

      INTEGER STR_LEN            ! string length

*  Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMABS'

*  Local variables:
      BYTE FILE(FILENAMESIZE)    ! file for default inputs

      INTEGER ACTVAL             ! parameter value count
      INTEGER FD                 ! file descriptor
      INTEGER IRES               ! resolution index
      INTEGER ISTAT              ! status

*  Check inherited global status
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get resolution index
      CALL IUE_RESN( RESOL, IRES, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: resolution invalid\\', STATUS )
         GO TO 999
      END IF

*  Absolute Calibration
      IF ( NOABS .AND. PHOT ) THEN
         IF ( STR_LEN( UFILE ) .GT. 0 ) THEN
            CALL STR_MOVE( UFILE, FILENAMESIZE, FILE )

         ELSE IF ( IRES .EQ. LORES ) THEN
            CALL STR_MOVE( 'IUEDR_DATA:\\', FILENAMESIZE, FILE )
            CALL STR_APPND( CAMERA, FILENAMESIZE, FILE )
            CALL STR_APPND( 'LO\\', FILENAMESIZE, FILE )

*        Get ITF parameter
            CALL RDPARI( 'ITF\\', .FALSE., 1, ITF, ACTVAL, STATUS )
            CALL CNPAR( 'ITF\\', ISTAT )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'ITF\\', STATUS )
               GO TO 999

            ELSE IF ( ISTAT .NE. SAI__OK ) THEN
               CALL PCANER( 'ITF\\', STATUS )
               GO TO 999

            ELSE IF ( ITF .EQ. 0 ) THEN
               CALL STR_APPND( 'P\\', FILENAMESIZE, FILE )
               CALL LINE_WCONT(
     :               '%p ITFP Photometric Calibration Assumed for \\' )

            ELSE IF ( ITF .EQ. 1 ) THEN
               CALL STR_APPND( '1\\', FILENAMESIZE, FILE )
               CALL LINE_WCONT(
     :               '%p ITF1 Photometric Calibration Assumed for \\' )

            ELSE IF ( ITF .EQ. 2 ) THEN
               CALL STR_APPND( '2\\', FILENAMESIZE, FILE )
               CALL LINE_WCONT(
     :               '%p ITF2 Photometric Calibration Assumed for \\' )

            ELSE
               CALL STR_TERM( 0, FILENAMESIZE, FILE )
            END IF

            IF ( ITF.GE.0 .AND. ITF.LE.2 ) THEN
               IF ( STR_SIMLR( CAMERA, 'SWP\\' ) ) THEN
                  CALL LINE_WCONT( 'SWP Camera.\\' )

               ELSE IF ( STR_SIMLR( CAMERA, 'LWR\\' ) ) THEN
                  CALL LINE_WCONT( 'LWR Camera.\\' )

               ELSE IF ( STR_SIMLR( CAMERA, 'LWP\\' ) ) THEN
                  CALL LINE_WCONT( 'LWP Camera.\\' )
               END IF
            END IF

            CALL PRTBUF( STATUS )
         END IF

         IF ( STR_LEN( FILE ) .GT. 0 ) THEN
            CALL RDFILE( FILE, '.abs\\', FD, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL LINE_WCONT( '%p No Absolute Calibration.\\' )
               STATUS = SAI__OK
               CALL PRTBUF( STATUS )

            ELSE
               CALL RDABS( FD, STATUS )
               IF ( STATUS .NE. SAI__OK ) THEN
                    CALL ERROUT( 'Error: in absolute calibration\\',
     :                           STATUS )
               END IF

               CALL CLFILE( FILE, FD, STATUS )
            END IF
            CALL STR_MOVE( UFILE, FILENAMESIZE, FILE )

         ELSE
            CALL LINE_WCONT( '%p No Absolute Calibration.\\' )
            CALL PRTBUF( STATUS )
         END IF

      END IF

 999  CONTINUE

      END
