      SUBROUTINE USR_SETA( STATUS )
*+
*  Name:
*     SUBROUTINE USR_SETA

*  Purpose:
*     Set calibration values that are specific to a perticular aperture.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SETA( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Read parameters, providing dataset values as dynamic defaults.
*     Since the resolution path is (CURRENT,DYNAMIC), any values
*     set by the used will provide "different" values from the
*     dynamic defaults.
*     The old and new parameters are compared to see if they have
*     really changed, and whether the dataset calibration needs
*     update in any way.
*     Retain swicthes for file update, and current spectrum calibration
*     update.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     02-OCT-88 (PCTR):
*       IUEDR Vn. 2.0
*     14-SEP-94 (MJC):
*       IUEDR Vn. 3.1-3
*     18-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Status:
      INTEGER STATUS      ! Global status.

*  External References:
      LOGICAL STR_SIMLR   ! Caseless string equality.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMDISP'
      INCLUDE 'CMWCOR'
      INCLUDE 'CMECOR'
      INCLUDE 'CMVEL'

*  Local Variables:
      REAL*8 FVAL( 32 )   ! Float temporary.

      LOGICAL NEWCAL      ! Whether current spectrum calibration changes.
      LOGICAL NEWFIL      ! Whether calibration file requires update.

      INTEGER ACTVAL      ! Parameter value count.
      INTEGER I           ! Loop index.
      INTEGER IAPER       ! Aperture index.
      INTEGER ISTAT       ! Local status.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration.
      CALL DASSOC( '\\', '\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE IF ( NAPER .LT. 1 ) THEN
         CALL ERROUT( 'Error: no apertures defined\\', STATUS )
         GO TO 999

      ELSE
         NEWFIL = .FALSE.
         NEWCAL = .FALSE.
      END IF

*  Aperture/resolution.
      CALL DEFAPR( 0, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: aperture/resolution invalid\\',
     :                STATUS )
         GO TO 999
      END IF

*  EXPOSURE - exposure times for each aperture.
      DO WHILE ( .TRUE. )
         FVAL( 1 ) = TSECS( IAPER )
         CALL RDPARF( 'EXPOSURE\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'EXPOSURE\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .NE. TSECS( IAPER ) ) THEN
            TSECS( IAPER ) = FVAL( 1 )
            NEWFIL = .TRUE.
            NEWCAL = .TRUE.
         END IF

         CALL CNPAR( 'EXPOSURE\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'EXPOSURE\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 100
         END IF
      END DO
 100  CONTINUE

*  FSCALE - arbitrary flux scale factor.
      DO WHILE ( .TRUE. )
         FVAL( 1 ) = FSCALE( IAPER )
         CALL RDPARF( 'FSCALE\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'FSCALE\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .NE. FSCALE( IAPER ) ) THEN
            FSCALE( IAPER ) = FVAL( 1 )
            NEWFIL = .TRUE.
            NEWCAL = .TRUE.
         END IF

         CALL CNPAR( 'FSCALE\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'FSCALE\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 200
         END IF
      END DO
 200  CONTINUE

*  WSHIFT - wavelength shift for each aperture (LORES only).
      DO WHILE ( STR_SIMLR( 'LORES\\', RESOL ) )
         IF ( NOWCOR ) THEN
            DO I = 1, NAPER
               WCOR( I ) = 0.0
            END DO
            NOWCOR = .FALSE.
         END IF

         FVAL( 1 ) = WCOR( IAPER )
         CALL RDPARF( 'WSHIFT\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'WSHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .NE. WCOR( IAPER ) ) THEN
            WCOR( IAPER ) = FVAL( 1 )
            NEWFIL = .TRUE.
            NEWCAL = .TRUE.
         END IF

         CALL CNPAR( 'WSHIFT\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'WSHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 300
         END IF
      END DO
 300  CONTINUE

*  VSHIFT - velocity shift for each aperture.
      DO WHILE ( .TRUE. )
         IF ( NOVEL ) THEN
            DO I = 1, NAPER
               VEL( I ) = 0.0
            END DO
            NOVEL = .FALSE.
         END IF

         FVAL( 1 ) = VEL( IAPER )
         CALL RDPARF( 'VSHIFT\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'VSHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .NE. VEL( IAPER ) ) THEN
            VEL( IAPER ) = FVAL( 1 )
            NEWFIL = .TRUE.
            NEWCAL = .TRUE.
         END IF

         CALL CNPAR( 'VSHIFT\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'VSHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 400
         END IF
      END DO
 400  CONTINUE

*  ESHIFT - echelle wavelength shift for each aperture.
      DO WHILE ( STR_SIMLR( 'HIRES\\', RESOL ) )
         IF ( NOECOR ) THEN
            DO I = 1, NAPER
               ECOR( I ) = 0.0
            END DO
            NOECOR = .FALSE.
         END IF

         FVAL( 1 ) = ECOR( IAPER )
         CALL RDPARF( 'ESHIFT\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'ESHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .NE. ECOR( IAPER ) ) THEN
            ECOR( IAPER ) = FVAL( 1 )
            NEWFIL = .TRUE.
            NEWCAL = .TRUE.
         END IF

         CALL CNPAR( 'ESHIFT\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'ESHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 600
         END IF
      END DO
 600  CONTINUE

*  GSHIFT.
      DO WHILE ( .NOT. NODISP )
         FVAL( 1 ) = DISPSG( IAPER )
         FVAL( 2 ) = DISPLG( IAPER )

         CALL RDPARF( 'GSHIFT\\', .TRUE., 2, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'GSHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( ACTVAL .LT. 2 ) THEN
            CALL ERRPAR( 'GSHIFT\\' )
            CALL ERROUT( ': too few values\\', STATUS )

         ELSE
            IF ( DISPSG( IAPER ) .NE. FVAL( 1 ) ) THEN
               DISPSG( IAPER ) = FVAL( 1 )
               NEWFIL = .TRUE.
            END IF
            IF ( DISPLG( IAPER ) .NE. FVAL( 2 ) ) THEN
               DISPLG( IAPER ) = FVAL( 2 )
               NEWFIL = .TRUE.
            END IF
         END IF

         CALL CNPAR( 'GSHIFT\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'GSHIFT\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 700
         END IF
      END DO
 700  CONTINUE

*  Only calibrate if new.
      IF ( NEWCAL ) THEN
         CALL MODCAL
         CALL RECAL( STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL ERROUT( 'Error: recalibrating spectrum\\', STATUS )
            GO TO 999
         END IF

      ELSE IF ( NEWFIL ) THEN
         CALL MODCAL
      END IF

 999  CONTINUE

      END
