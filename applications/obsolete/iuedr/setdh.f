      SUBROUTINE SETDH( NEWFIL, NEWCAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE SETDH
*
*   Description:
*      Set dataset halation parameters.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*      Martin Clayton     14-SEP-94     IUEDR Vn. 3.1-3
*
*   Method:
*      Set values that are global the the dataset calibration:
*      this routines deals with the echelle order overlap parameters.
*
*-

*   Implicit:
      IMPLICIT NONE

*   Starlink includes:
      INCLUDE 'SAE_PAR'

*   Import-Export:
      LOGICAL NEWCAL        ! whether current spectrum calibration changes
      LOGICAL NEWFIL        ! whether calibration file requires update

*   Export:
      INTEGER STATUS        ! status return

*   External references:
      LOGICAL STR_SIMLR     ! caseless string equality

*   Global variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMHAL'

*   Local variables:
      REAL*8 FVAL(32)         ! float temporary

      BYTE SVAL(400)        ! string temporary

      INTEGER ISTAT         ! status
      INTEGER ACTVAL

*   Provide a start, if none exists
      IF ( NOHAL ) THEN
         CALL STR_MOVE( 'POWER\\', 16, HALTP )
         HALC = 0.0
         IF ( STR_SIMLR('SWP\\', CAMERA) .OR.
     :        STR_SIMLR('SWR\\', CAMERA) ) THEN
            HALWC = 1200.0
            HALW0 = 1400.0

         ELSE IF ( STR_SIMLR('LWP\\', CAMERA) .OR.
     :             STR_SIMLR('LWR\\', CAMERA) ) THEN
            HALWC = 1800.0
            HALW0 = 2400.0

         ELSE
            HALWC = 1000.0
            HALW0 = 10000.0
         END IF

         HALAV = 30.0
         NOHAL = .FALSE.
      END IF

*   HALTYPE - halation correction type
 100  CONTINUE
      CALL STR_MOVE( HALTP, 16, SVAL )
      CALL RDPARC( 'HALTYPE\\', .TRUE., 16, SVAL, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'HALTYPE\\', STATUS )
         RETURN
      ELSE IF ( .NOT. STR_SIMLR('POWER\\', SVAL) ) THEN
         CALL ERRPAR( 'HALTYPE\\' )
         CALL ERROUT( ': has illegal value\\', STATUS )
      ELSE IF ( .NOT. STR_SIMLR(SVAL, HALTP) ) THEN
         CALL STR_MOVE( SVAL, 16, HALTP )
         NEWFIL = .TRUE.
         NEWCAL = .TRUE.
      END IF

      CALL CNPAR( 'HALTYPE\\', ISTAT )
      IF ( ISTAT .NE. SAI__OK ) THEN
         CALL PCANER( 'HALTYPE\\', STATUS )
         RETURN
      ELSE IF ( STATUS .EQ. SAI__OK ) THEN
         GO TO 200
      END IF

      GO TO 100
 200  CONTINUE

*   HALC - echelle halation scale factor
      IF ( STR_SIMLR('POWER\\', HALTP) ) THEN
 250     CONTINUE
         FVAL(1) = HALC
         CALL RDPARF( 'HALC\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'HALC\\', STATUS )
            RETURN
         ELSE IF ( FVAL(1) .LT. 0.0) THEN
            CALL ERRPAR( 'HALC\\' )
            CALL ERROUT( ': has invalid value\\', STATUS )
         ELSE
            IF ( FVAL(1) .NE. HALC ) THEN
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

            HALC = FVAL(1)
         END IF

         CALL CNPAR( 'HALC\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'HALC\\', STATUS )
            RETURN
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 300
         END IF

         GO TO 250
      END IF
 300  CONTINUE

*   HALWC - wavelength for echelle halation scale factor
      IF ( STR_SIMLR('POWER\\', HALTP) ) THEN
 350     CONTINUE
         FVAL(1) = HALWC
         CALL RDPARF( 'HALWC\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'HALWC\\', STATUS )
            RETURN
         ELSE
            IF ( FVAL(1) .NE. HALWC ) THEN
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

            HALWC = FVAL(1)
         END IF

         CALL CNPAR( 'HALWC\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'HALWC\\', STATUS )
            RETURN
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 400
         END IF

         GO TO 350
      END IF
 400  CONTINUE

*   HALW0 - wavelength for zero echelle halation scale factor
      IF ( STR_SIMLR('POWER\\', HALTP) ) THEN
 450     CONTINUE
         FVAL(1) = HALW0
         CALL RDPARF( 'HALW0\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'HALW0\\', STATUS )
            RETURN
         ELSE
            IF ( FVAL(1) .NE. HALW0 ) THEN
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

            HALW0 = FVAL(1)
         END IF

         CALL CNPAR( 'HALW0\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'HALW0\\', STATUS )
            RETURN
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 500
         END IF

         GO TO 450
      END IF
 500  CONTINUE

*   HALAV - averaging range for halation correction (g-pixels)
      IF ( STR_SIMLR('POWER\\', HALTP) ) THEN
 550     CONTINUE
         FVAL(1) = HALAV
         CALL RDPARF( 'HALAV\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'HALAV\\', STATUS )
            RETURN
         ELSE IF ( HALAV .LT. 0.0 ) THEN
            CALL ERRPAR('HALAV\\')
            CALL ERROUT(': has invalid value\\', STATUS)
         ELSE
            IF ( FVAL(1) .NE. HALAV ) THEN
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF

            HALAV = FVAL(1)
         END IF

         CALL CNPAR( 'HALAV\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'HALAV\\', STATUS )
            RETURN
         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 600
         END IF

         GO TO 550
      END IF
 600  CONTINUE
      END
