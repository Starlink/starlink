      SUBROUTINE USR_SETM( STATUS )
*+
*  Name:
*     SUBROUTINE USR_SETM

*  Purpose:
*     Set values that are specific to a specific echelle order.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL USR_SETM( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Method:
*     Read parameters, providing dataset values as dynamic defaults.
*     Since the resolution path is (CURRENT, DYNAMIC), any values
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
*     MJC: Martin Clayton (Starlink)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     08-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     27-JUL-94 (MJC):
*       IUEDR Vn. 3.1-2
*     20-JAN-95 (MJC):
*       IUEDR Vn. 3.2
*       Clarified invalid resolution error message.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Status:
      INTEGER STATUS     ! Global status.

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMRIP'
      INCLUDE 'CMCUT'

*  Local Variables:
      REAL*8 FVAL( 32 )  ! Float temporary.
      REAL*8 CS( 6 )     ! Coefficients.
      REAL*8 WLIM( 2 )   ! Wavelength limits.
      REAL*8 RIPA        ! A-value.
      REAL*8 RIPK        ! K-value.

      INTEGER ACTVAL     ! Parameter value count.
      INTEGER I          ! Loop index.
      INTEGER IAPER      ! Aperture index.
      INTEGER ISTAT      ! Status.
      INTEGER IORDC      ! Order index.
      INTEGER IORDR      ! Ripple order index.
      INTEGER IORDW      ! Cutoff index.
      INTEGER NCS        ! Number of coefficients.
      INTEGER ORD1       ! Actual order.

      LOGICAL DEF        ! Whether default parameter values supplied.
      LOGICAL NEW        ! Whether a new value has been acquired.
      LOGICAL NEWCAL     ! Whether current spectrum calibration changes.
      LOGICAL NEWFIL     ! Whether calibration file requires update.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Get Calibration and Spectrum (raw).
      CALL DASSOC( 'S\\', 'F\\', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: could not access dataset\\', STATUS )
         GO TO 999

      ELSE
         NEWFIL = .FALSE.
         NEWCAL = .FALSE.
      END IF

*  Aperture/resolution.
      CALL DEFAPR( 2, IAPER, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERROUT( 'Error: can only SETM on HIRES images\\', STATUS )
         GO TO 999
      END IF

*  Default global ripple data.
      CALL DEFRIP

*  ORDER.
      DO WHILE ( .TRUE. )
         CALL RDPARI( 'ORDER\\', .FALSE., 1, ORD1, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'ORDER\\', STATUS )
            GO TO 999

         ELSE IF ( ORD1.LT.65 .OR. ORD1.GT.125 ) THEN
            CALL ERRPAR( 'ORDER\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            CALL FNORD( ORD1, IORDC )
            IF ( IORDC .GT. 0 ) THEN
               GO TO 100
            END IF
            CALL ERRPAR( 'ORDER\\' )
            CALL ERROUT( ': unknown\\', STATUS )
         END IF

         CALL CNPAR( 'ORDER\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'ORDER\\', STATUS )
            GO TO 999
         END IF
      END DO
 100  CONTINUE

*  RIPK - echelle ripple constant for order.
      DO WHILE ( .TRUE. )
         CALL FNRIP( ORD1, IORDR )
         IF ( IORDR .GT. 0 ) THEN
            FVAL( 1 ) = RIPKS( IORDR )
            DEF = .TRUE.

         ELSE
            CALL GETRIP( ORD1, RIPK, RIPA, 6, CS, NCS )
            FVAL( 1 ) = RIPK
            DEF = .TRUE.
         END IF

         CALL RDPARF( 'RIPK\\', DEF, 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            IF ( IORDR .GT. 0 ) THEN
               CALL DLRIP( ORD1 )
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RIPK\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .LT. 1.0 ) THEN
            CALL ERRPAR( 'RIPK\\' )
            CALL ERROUT( ': has invalid value\\', STATUS )

         ELSE
            NEW = ( FVAL( 1 ) .NE. RIPK )
            IF ( NEW ) THEN
               NEWCAL = .TRUE.
               NEWFIL = .TRUE.
               IF ( IORDR .LE. 0 ) THEN
                  CALL ALRIP( ORD1, IORDR )
               END IF
               IF ( IORDR .GT. 0 ) THEN
                  RIPKS( IORDR ) = FVAL( 1 )
               END IF
            END IF
         END IF

         CALL CNPAR( 'RIPK\\', ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL PCANER( 'RIPK\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 200
         END IF
      END DO
 200  CONTINUE

*  RIPA - echelle ripple scale factor.
      DO WHILE ( .TRUE. )
         CALL FNRIP( ORD1, IORDR )
         IF ( IORDR .GT. 0 ) THEN
            FVAL( 1 ) = RIPAS( IORDR )
            DEF = .TRUE.

         ELSE
            CALL GETRIP( ORD1, RIPK, RIPA, 6, CS, NCS )
            FVAL( 1 ) = RIPA
            DEF = .TRUE.
         END IF

         CALL RDPARF( 'RIPA\\', DEF, 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            IF ( IORDR .GT. 0 ) THEN
               CALL DLRIP( ORD1 )
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RIPA\\', STATUS )
            GO TO 999

         ELSE IF ( FVAL( 1 ) .LE. 0.0 ) THEN
            CALL ERRPAR( 'RIPA\\' )
            CALL ERROUT( ': has invalid value\\', STATUS )

         ELSE
            NEW = ( FVAL( 1 ) .NE. RIPA )
            IF ( NEW ) THEN
               NEWCAL = .TRUE.
               NEWFIL = .TRUE.
               IF ( IORDR .LE. 0 ) THEN
                  CALL ALRIP( ORD1, IORDR )
               END IF
               IF ( IORDR .GT. 0 ) THEN
                  RIPAS( IORDR ) = FVAL( 1 )
               END IF
            END IF
         END IF

         CALL CNPAR( 'RIPA\\', ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL PCANER( 'RIPA\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 300
         END IF
      END DO
 300  CONTINUE

*  RIPC - polynomial (in X) to modify ripple for order.
      DO WHILE ( .TRUE. )
         CALL FNRIP( ORD1, IORDR )
         DO I = 1, 6
            FVAL( I ) = 0.0
         END DO
         IF ( IORDR .GT. 0 ) THEN
            DO I = 1, NRIPCS( IORDR )
               FVAL( I ) = RIPCS( I, IORDR )
            END DO
            DEF = .TRUE.

         ELSE
            CALL GETRIP( ORD1, RIPK, RIPA, 6, CS, NCS )
            DO I = 1, NCS
               FVAL( I ) = CS( I )
            END DO
            DEF = .TRUE.
         END IF

         CALL RDPARF( 'RIPC\\', DEF, 6, FVAL, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            IF ( IORDR .GT. 0 ) THEN
               CALL DLRIP( ORD1 )
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RIPC\\', STATUS )
            GO TO 999

         ELSE
            DO WHILE ( ACTVAL .GT. 0 )
               IF ( FVAL( ACTVAL ) .NE. 0.0 ) THEN
                  GO TO 400
               END IF
               ACTVAL = ACTVAL - 1
            END DO
 400        CONTINUE

            NEW = .FALSE.
            IF ( ACTVAL .NE. NCS ) THEN
               NEW = .TRUE.

            ELSE IF ( ACTVAL .GT. 0 ) THEN
               DO I = 1, ACTVAL
                  IF ( FVAL( I ) .NE. CS( I ) ) THEN
                     NEW = .TRUE.
                     GO TO 500
                  END IF
               END DO
            END IF
 500        CONTINUE

            IF ( NEW ) THEN
               NEWCAL = .TRUE.
               NEWFIL = .TRUE.
               IF ( IORDR .LE. 0 ) THEN
                  CALL ALRIP( ORD1, IORDR )
               END IF
               IF ( IORDR .GT. 0 ) THEN
                  NRIPCS( IORDR ) = ACTVAL
                  DO I = 1, ACTVAL
                     RIPCS( I, IORDR ) = FVAL( I )
                  END DO
               END IF
            END IF
         END IF

         CALL CNPAR( 'RIPC\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'RIPC\\', STATUS )
            GO TO 999

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 600
         END IF
      END DO
 600  CONTINUE

*  Initialise CUT data.
      IF ( NOCUT ) THEN
         NCUT = 0
         NOCUT = .FALSE.
      END IF

*  WCUT - echelle ripple scale factor.
      DO WHILE ( .TRUE. )
         CALL FNCUT( ORD1, IORDW )
         IF ( IORDW .GT. 0 ) THEN
            WLIM( 1 ) = CUTW1( IORDW )
            WLIM( 2 ) = CUTW2( IORDW )
            DEF = .TRUE.
            DO I = 1, 2
               FVAL( I ) = WLIM( I )
            END DO
         END IF

         CALL RDPARF( 'WCUT\\', DEF, 2, FVAL, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            IF ( IORDW .GT. 0 ) THEN
               CALL DLCUT( ORD1 )
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'WCUT\\', STATUS )
            GO TO 999

         ELSE IF ( ACTVAL .NE. 2 ) THEN
            CALL ERRPAR( 'WCUT\\' )
            CALL ERROUT( ': too few values\\', STATUS )

         ELSE IF ( FVAL( 1 ) .GE. FVAL( 2 ) ) THEN
            CALL ERRPAR( 'WCUT\\' )
            CALL ERROUT( ': values illegal\\', STATUS )

         ELSE IF ( IORDW .GT. 0 ) THEN
            DO I = 1, 2
               IF ( WLIM( I ) .NE. FVAL( I ) ) THEN
                  NEWFIL = .TRUE.
                  NEWCAL = .TRUE.
               END IF
            END DO
            CUTW1( IORDW ) = FVAL( 1 )
            CUTW2( IORDW ) = FVAL( 2 )

         ELSE
            CALL ALCUT( ORD1, IORDW )
            IF ( IORDW .GT. 0 ) THEN
               CUTW1( IORDW ) = FVAL( 1 )
               CUTW2( IORDW ) = FVAL( 2 )
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF
         END IF

         CALL CNPAR( 'WCUT\\', ISTAT )
         IF ( ISTAT .NE. 0 ) THEN
            CALL PCANER( 'WCUT\\', STATUS )
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
