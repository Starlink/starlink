      SUBROUTINE PARSPC( NEWNAME, STATUS )
*+
*  Name:
*     SUBROUTINE PARSPC
*
*  Description:
*     Provide dynamic defaults for missing spectrum parameters.
*
*  History:
*     Jack Giddings      01-MAY-82     AT4 version
*     Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*     Martin Clayton     09-DEC-94     IUEDR Vn. 3.2
*
*  Method:
*     The parameters describing the spectrum and its calibration are read.
*     Calibration datasets are also read, based on CAMERA, APERTURE,
*     RESOLUTION.
*-

*  Implicit:
      IMPLICIT NONE

*  Starlink includes:
      INCLUDE 'SAE_PAR'

*  Export:
      INTEGER STATUS       ! status return

*  External references:
      INTEGER STR_LEN      ! string length

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMISAF'
      INCLUDE 'CMSPC'

*  Local Constants:
      INTEGER MAXFILENAME
      PARAMETER ( MAXFILENAME=81 )

*  Local variables:
      INTEGER ACTVAL       ! parameter value count
      INTEGER DAYS( 12 )   ! number of days in each month
      INTEGER I            ! temporary size
      INTEGER IAPER        ! aperture index
      INTEGER ICAM         ! camera index
      INTEGER ISTAT        ! status
      INTEGER NTEMP        ! temporary aperture count
      INTEGER IDUM

      BYTE    NEWNAME(MAXFILENAME)

*.

*  Initialisations:
      DATA DAYS / 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /

*  CAMERA - a check
      CALL IUE_CAMN( CAMERA, ICAM, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         DO WHILE ( .TRUE. )
            CAMERA( 1 ) = ISACAMERA( 1 )
            CAMERA( 2 ) = ISACAMERA( 2 )
            CAMERA( 3 ) = ISACAMERA( 3 )
            CALL RDPARC( 'CAMERA\\', .TRUE., 16, CAMERA, ACTVAL,
     :                   STATUS )
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
            END IF
            CALL ERRPAR( 'CAMERA\\' )
            CALL ERROUT( ': invalid\\', STATUS )
         END DO
      END IF
 100  CONTINUE

*  IMAGE - a check
      IF ( IMAGE .LE. 0 ) THEN
         IMAGE = NEWNAME( 8 ) - 48
         IDUM = NEWNAME( 7 )
         IMAGE = IMAGE + ( IDUM - 48 )  * 10
         IDUM = NEWNAME( 6 )
         IMAGE = IMAGE + ( IDUM - 48 )  * 100
         IDUM = NEWNAME( 5 )
         IMAGE = IMAGE + ( IDUM - 48 )  * 1000
         IDUM = NEWNAME( 4 )
         IMAGE = IMAGE + ( IDUM - 48 )  * 10000

         CALL RDPARI( 'IMAGE\\', .TRUE., 1, IMAGE, ACTVAL, STATUS )
         CALL CNPAR( 'IMAGE\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'IMAGE\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'IMAGE\\', STATUS )
            GO TO 999
         END IF
      END IF

*  APERTURES
      NTEMP = 0
      DO WHILE ( .TRUE. )
         CALL IUE_APRN( APER, IAPER, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            IF ( NTEMP .GT. 0 ) THEN
               CALL ERRPAR( 'APERTURES\\' )
               CALL ERROUT( ': invalid\\', STATUS )
            END IF

         ELSE IF ( IAPER.NE.1 .AND. IAPER.NE.2 ) THEN
            CALL ERRPAR( APER )
            CALL ERROUT( ': invalid\\', STATUS )

         ELSE
            NAPER = 0
            CALL NEWAPR( APER, STATUS )
            IF ( STATUS .EQ. SAI__OK ) THEN
               GO TO 200
            END IF
            CALL ERROUT( 'Error: creating aperture\\', STATUS )
         END IF

         NTEMP = NTEMP + 1
         CALL RDPARC( 'APERTURES\\', .FALSE., 16, APER, ACTVAL, STATUS )
         CALL CNPAR( 'APERTURES\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'APERTURES\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'APERTURES\\', STATUS )
            GO TO 999
         END IF
      END DO
 200  CONTINUE

*  EXPOSURES - exposure time for each aperture
      DO WHILE ( .TRUE. )
         TSECS( 1 ) = FLOAT( ISATIME )
         CALL RDPARF( 'EXPOSURES\\', .TRUE., NAPER, TSECS, ACTVAL,
     :                STATUS )
         CALL CNPAR( 'EXPOSURES\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'EXPOSURES\\', STATUS )
            GO TO 999

         ELSE IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'EXPOSURES\\', STATUS )
            GO TO 999

         ELSE IF ( ACTVAL .LT. NAPER ) THEN
            CALL ERROUT( 'Error: too few values given\\', STATUS )

         ELSE
            DO I = 1, NAPER
               IF ( TSECS( I ) .LE. 0.0 ) THEN
                  TSECS(I) = 1.0
               END IF
            END DO
            GO TO 300
         END IF
      END DO
 300  CONTINUE

*  YEAR - a check
      IF ( YEAR .LE. 0 ) THEN
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
               CALL ERRPAR( 'YEAR\\' )
               CALL ERROUT( ': invalid\\', STATUS )

            ELSE
               GO TO 400
            END IF
         END DO
      END IF
 400  CONTINUE

*  MONTH - a check
      IF ( MONTH .LE. 0 ) THEN
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
               CALL ERRPAR( 'MONTH\\' )
               CALL ERROUT( ': invalid\\', STATUS )

            ELSE
               GO TO 500
            END IF
         END DO
      END IF
 500  CONTINUE

*  DAY - a check
      IF ( DAY .LE. 0 ) THEN
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

            ELSE IF ( DAY.LT.1 .OR. DAY.GT.DAYS(MONTH) ) THEN
               CALL ERRPAR( 'DAY\\' )
               CALL ERROUT( ': invalid\\', STATUS )

            ELSE
               GO TO 600
            END IF
         END DO
      END IF
 600  CONTINUE

*  Calculate date
      CALL MSC_DAYN( DAY, MONTH, YEAR, DATE, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not construct IUE DATE\\', STATUS )
         GO TO 999
      END IF

*  OBJECT - a check
      DO WHILE ( .TRUE. )
         DO I = 1, 16
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
            CALL ERROUT( 'OBJECT\\' )
            CALL ERROUT( ': too brief\\', STATUS )

         ELSE
            GO TO 700
         END IF
      END DO
 700  CONTINUE

*  THDA - a check
      IF ( THDA .LE. 0.0 ) THEN
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
               CALL ERRPAR( 'THDA\\' )
               CALL ERROUT( ': invalid\\', STATUS )

            ELSE
               GO TO 800
            END IF
         END DO
      END IF
 800  CONTINUE

*  Dispersion constants
      CALL RFDISP( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading dispersion constants\\', STATUS )
         GO TO 999
      END IF

*  Absolute calibration
      CALL RFABS( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading absolute calibration\\', STATUS )
         GO TO 999
      END IF

*  Ripple Calibration
      CALL RFRIP( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading ripple calibration\\', STATUS )
         GO TO 999
      END IF

*  Wavelength Cutoff Data
      CALL RFCUT( '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: reading wavelength range data\\', STATUS )
         GO TO 999
      END IF

 999  CONTINUE
      END
