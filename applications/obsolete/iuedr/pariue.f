      SUBROUTINE PARIUE( NEWNAME, STATUS )
*+
*  Name:
*     SUBROUTINE PARIUE

*  Purpose:
*     Provide dynamic defaults for missing parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL PARIUE( NEWNAME, STATUS )

*  Arguments:
*     NEWNAME = BYTE( MAXFILENAME ) (Given)
*        Name of the DATASET.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*    The parameters describing the image and its calibration are read.
*    Calibration datasets are also read, based on CAMERA, APERTURE,
*    RESOLUTION.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       AT4 version
*     03-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     10-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Variables:
      INCLUDE 'SAE_PAR'

*  External References:
      INTEGER STR_LEN      ! string length

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMTRUN'
      INCLUDE 'CMISAF'

*  Local Constants:
      INTEGER BAP          ! BAP index
      INTEGER LAP          ! LAP index
      INTEGER SAP          ! SAP index
      INTEGER HIRES        ! HIRES index
      INTEGER LORES        ! LORES index
      INTEGER MAXNAME      ! maximum length of name
      INTEGER MAXFILENAME  ! maximum length of filename
      PARAMETER ( BAP = 3, LAP = 2, SAP = 1, HIRES = 2,
     :            LORES = 1, MAXNAME = 16, MAXFILENAME = 81 )

*  Arguments Given:
      BYTE NEWNAME( MAXFILENAME )

*  Status:
      INTEGER STATUS       ! Global status.

*  Local Variables:
      INTEGER ACTVAL       ! Parameter value count.
      INTEGER DAYS( 12 )   ! Number of days in each month.
      INTEGER I            ! Temporary size.
      INTEGER IAPER        ! Aperture index.
      INTEGER ICAM         ! Camera index.
      INTEGER IRES         ! Resolution index.
      INTEGER ISTAT        ! Status.
      INTEGER IDUM

*   Initialisations:
      DATA DAYS / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
*.

*   Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   CAMERA - a check.
      DO WHILE ( .TRUE. )
         CAMERA( 1 ) = ISACAMERA( 1 )
         CAMERA( 2 ) = ISACAMERA( 2 )
         CAMERA( 3 ) = ISACAMERA( 3 )

         CALL RDPARC( 'CAMERA\\', .TRUE., MAXNAME, CAMERA, ACTVAL,
     :                STATUS )
         CALL CNPAR( 'CAMERA\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'CAMERA\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'CAMERA\\', STATUS )
            GO TO 999
         END IF

         CALL IUE_CAMN( CAMERA, ICAM, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 100

         ELSE
            CALL ERRPAR( 'CAMERA\\')
            CALL ERROUT( ': invalid\\', STATUS )
         END IF
      END DO
 100  CONTINUE

*   IMAGE - a check.
      IMAGE = NEWNAME( 8 ) - 48
      IDUM = NEWNAME( 7 )
      IMAGE = IMAGE + ( IDUM - 48 ) * 10
      IDUM = NEWNAME( 6 )
      IMAGE = IMAGE + ( IDUM - 48 ) * 100
      IDUM = NEWNAME( 5 )
      IMAGE = IMAGE + ( IDUM - 48 ) * 1000
      IDUM = NEWNAME( 4 )
      IMAGE = IMAGE + ( IDUM - 48 ) * 10000

      CALL RDPARI( 'IMAGE\\', .TRUE., 1, IMAGE, ACTVAL, STATUS )
      CALL CNPAR( 'IMAGE\\', ISTAT )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'IMAGE\\', STATUS )
         GO TO 999

      ELSE IF ( ISTAT .NE. SAI__OK ) THEN
         CALL PCANER( 'IMAGE\\', STATUS )
         GO TO 999
      END IF

*   RESOLUTION - check and assign index.
      DO WHILE ( .TRUE. )
         RESOL( 1 ) = ISARES( 1 )
         RESOL( 2 ) = ISARES( 2 )
         RESOL( 3 ) = ISARES( 3 )
         RESOL( 4 ) = ISARES( 4 )
         RESOL( 5 ) = ISARES( 5 )

         CALL RDPARC( 'RESOLUTION\\', .TRUE., MAXNAME, RESOL, ACTVAL,
     :                STATUS )
         CALL CNPAR( 'RESOLUTION\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RESOLUTION\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'RESOLUTION\\', STATUS )
            GO TO 999
         END IF

         CALL IUE_RESN( RESOL, IRES, STATUS )
         IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 200

         ELSE
            CALL ERRPAR( 'RESOLUTION\\')
            CALL ERROUT( ': invalid\\', STATUS )
         END IF
      END DO
 200  CONTINUE

*   APERTURES.
      DO WHILE ( .TRUE. )
         CALL RDPARC( 'APERTURES\\', .FALSE., MAXNAME, APER, ACTVAL,
     :                STATUS )
         CALL CNPAR( 'APERTURES\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'APERTURES\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'APERTURES\\', STATUS )
            GO TO 999
         END IF

         CALL IUE_APRN( APER, IAPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERRPAR( 'APERTURES\\')
            CALL ERROUT( ': invalid\\', STATUS )

         ELSE IF ( IRES .EQ. LORES ) THEN
            NAPER = 0

            IF ( IAPER .EQ. SAP ) THEN
               CALL NEWAPR( 'SAP\\', STATUS )

            ELSE IF ( IAPER .EQ. LAP ) THEN
               CALL NEWAPR( 'LAP\\', STATUS )

            ELSE IF ( IAPER .EQ. BAP ) THEN
               CALL NEWAPR( 'SAP\\', STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  CALL NEWAPR( 'LAP\\', STATUS )
               END IF
            END IF

            IF ( STATUS .EQ. SAI__OK ) THEN
               GO TO 300

            ELSE
               CALL ERROUT( 'Error: creating LORES aperture(s)\\',
     :                      STATUS )
            END IF

         ELSE IF ( IRES .EQ. HIRES ) THEN
            NAPER = 0

            IF ( IAPER.EQ.SAP .OR. IAPER.EQ.LAP ) THEN
               CALL NEWAPR( APER, STATUS )
               IF ( STATUS .EQ. SAI__OK ) THEN
                  GO TO 300

               ELSE
                  CALL ERROUT( 'Error: creating HIRES aperture\\',
     :                         STATUS )
               END IF

            ELSE
               CALL ERROUT( 'Error: ambiguous HIRES aperture\\',
     :                      STATUS )
            END IF

         ELSE
            CALL ERROUT( 'Error: weird resolution!\\', STATUS )
         END IF
      END DO
 300  CONTINUE

*   EXPOSURES - exposure time for each aperture.
      IF ( NAPER .GT. 0 ) THEN

         TSECS( 1 ) = FLOAT( ISATIME )
         DO WHILE ( .TRUE. )
            CALL RDPARF( 'EXPOSURES\\', .TRUE., NAPER, TSECS, ACTVAL,
     :                   STATUS )
            CALL CNPAR( 'EXPOSURES\\', ISTAT )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL PARFER( 'EXPOSURES\\', STATUS )
               GO TO 999

            ELSE IF ( ISTAT .NE. SAI__OK ) THEN
               CALL PCANER( 'EXPOSURES\\', STATUS )
               GO TO 999

            ELSE IF ( ACTVAL .LT. NAPER ) THEN
               CALL ERROUT( 'Error: too few values given\\',
     :                      STATUS )
            ELSE
               DO I = 1, NAPER
                  IF ( TSECS( I ) .LE. 0.0 ) TSECS(I) = 1.0
               END DO
               GO TO 400
            END IF
         END DO
      END IF
 400  CONTINUE

*   YEAR - a check.
      DO WHILE ( .TRUE. )
         YEAR = ISAYEAR
         CALL RDPARI( 'YEAR\\', .TRUE., 1, YEAR, ACTVAL, STATUS )
         CALL CNPAR( 'YEAR\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'YEAR\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'YEAR\\', STATUS )
            GO TO 999

         ELSE IF ( YEAR.LT.1978 .OR. YEAR.GT.2000 ) THEN
            CALL ERRPAR( 'YEAR\\')
            CALL ERROUT( ': invalid\\', STATUS )

         ELSE
            GO TO 500
         END IF
      END DO
 500  CONTINUE

*   MONTH - a check.
      DO WHILE ( .TRUE. )
         MONTH = ISAMONTH
         CALL RDPARI( 'MONTH\\', .TRUE., 1, MONTH, ACTVAL, STATUS )
         CALL CNPAR( 'MONTH\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'MONTH\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'MONTH\\', STATUS )
            GO TO 999

         ELSE IF ( MONTH.LT.1 .OR. MONTH.GT.12 ) THEN
            CALL ERRPAR( 'MONTH\\')
            CALL ERROUT( ': invalid\\', STATUS )

         ELSE
            GO TO 600
         END IF
      END DO
 600  CONTINUE

*   DAY - a check.
      DO WHILE ( .TRUE. )
         DAY = ISADAY
         CALL RDPARI( 'DAY\\', .TRUE., 1, DAY, ACTVAL, STATUS )
         CALL CNPAR( 'DAY\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'DAY\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'DAY\\', STATUS )
            GO TO 999

         ELSE IF ( DAY.LT.1 .OR. DAY.GT.DAYS( MONTH ) ) THEN
            CALL ERRPAR( 'DAY\\')
            CALL ERROUT( ': invalid\\', STATUS )

         ELSE
            GO TO 700
         END IF
      END DO
 700  CONTINUE

*   Calculate date.
      CALL MSC_DAYN( DAY, MONTH, YEAR, DATE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not construct IUE DATE\\',
     :                STATUS )
         GO TO 999
      END IF

*   OBJECT - a check.
      DO WHILE ( .TRUE. )
         DO I = 1, 40
            TITLE( I ) = ISANAME( I )
         END DO
         CALL RDPARC( 'OBJECT\\', .TRUE., 40, TITLE, ACTVAL, STATUS )
         CALL CNPAR( 'OBJECT\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'OBJECT\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'OBJECT\\', STATUS )
            GO TO 999

         ELSE IF ( STR_LEN( TITLE ) .EQ. 0 ) THEN
            CALL ERRPAR( 'OBJECT\\')
            CALL ERROUT( ': too brief\\', STATUS )

         ELSE
            GO TO 800
         END IF
      END DO
 800  CONTINUE

*   THDA - a check.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'THDA\\', .FALSE., 1, THDA, ACTVAL, STATUS )
         CALL CNPAR( 'THDA\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'THDA\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'TDHA\\', STATUS )
            GO TO 999

         ELSE IF ( THDA.LT.0.0 .OR. THDA.GT.20.0 ) THEN
            CALL ERRPAR( 'THDA\\')
            CALL ERROUT( ': invalid\\', STATUS )

         ELSE
            GO TO 900
         END IF
      END DO
 900  CONTINUE

*   Fiducials.
      CALL RFFID( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading fiducial data\\', STATUS )
         GO TO 999
      END IF

*   FACEPLATE.
      CALL RFFACE( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading faceplate data\\', STATUS )
         GO TO 999
      END IF

*   Dispersion constants.
      CALL RFDISP( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading dispersion constants\\',
     :                STATUS )
         GO TO 999
      END IF

*   GEOMETRY.
      CALL RFGEOM( STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading geometry parameters\\',
     :                STATUS )
         GO TO 999
      END IF

*   Absolute calibration.
      CALL RFABS( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading absolute calibration\\',
     :                STATUS )
         GO TO 999
      END IF

*   Ripple Calibration.
      CALL RFRIP( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading ripple calibration\\',
     :                STATUS )
         GO TO 999
      END IF

*   Wavelength Cutoff Data.
      CALL RFCUT( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading wavelength range data\\',
     :                STATUS )
         GO TO 999
      END IF

*   Spectrum template data.
      CALL RFTEM( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading spectrum template data\\',
     :                STATUS )
         GO TO 999
      END IF

 999  CONTINUE

      END
