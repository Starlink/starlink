      SUBROUTINE SETDR( NEWFIL, NEWCAL, STATUS )

*+
*
*   Name:
*      SUBROUTINE SETDR
*
*   Description:
*      Set dataset ripple parameters.
*
*   History:
*      Jack Giddings      01-MAY-82     IUEDR Vn. 1.0
*      Paul Rees          07-NOV-88     IUEDR Vn. 2.0
*
*   Method:
*      Set values that are global the the dataset calibration:
*      this routine deals with the echelle ripple parameters.
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
      INCLUDE 'CMRIP'

*   Local variables:
      REAL*8 FVAL(32)         ! float temporary

      INTEGER ACTVAL        ! parameter value count
      INTEGER I             ! loop index
      INTEGER ISTAT         ! status

      LOGICAL EXIST

*.

*   Set up basics if not available
      IF ( NORIP .OR. NRIPM.LT.1 ) THEN
         CALL STR_MOVE( 'IUE_RIP\\', 16, RIPTP )
         CALL STR_MOVE( 'Internal default.\\', 40, RIPID )

         IF ( STR_SIMLR( 'SWP\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 137725.0

         ELSE IF ( STR_SIMLR( 'LWR\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 231180.0

         ELSE IF ( STR_SIMLR( 'LWP\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 231150.0

         ELSE IF ( STR_SIMLR( 'SWR\\', CAMERA ) ) THEN
            NRIPM = 1
            RIPM(1) = 137725.0

         ELSE
            NRIPM = 0
         END IF

         XRLIM( 1 ) = -3.0
         XRLIM( 2 ) = +3.0
         RIPALF = 1.0
         NRIPO = 0
         NORIP = .FALSE.
      END IF

*   RIPK - echelle ripple constant (poly in M)
 100  CONTINUE
      IF ( .NOT.(.NOT.(.TRUE.)) ) THEN
         EXIST = (NRIPM.GT.0)
         IF ( EXIST ) THEN
            DO I = 1, 6
               FVAL( I ) = 0.0
            END DO
            DO I = 1, NRIPM
               FVAL( I ) = RIPM( I )
            END DO
         END IF

         CALL RDPARF( 'RIPK\\', EXIST, 6, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RIPK\\', STATUS )
            RETURN

         ELSE
 160        CONTINUE
            IF ( ACTVAL .GT. 0 ) THEN
               IF ( FVAL( ACTVAL ) .NE. 0 ) THEN
                  GO TO 180
               END IF
               ACTVAL = ACTVAL - 1
               GO TO 160
            END IF

 180        CONTINUE
            IF ( ACTVAL .EQ. 0 ) THEN
               CALL ERRPAR( 'RIPK\\' )
               CALL ERROUT( ': too few values\\', STATUS )

            ELSE IF ( ACTVAL .NE. NRIPM ) THEN
               NRIPM = ACTVAL
               DO I = 1, NRIPM
                  RIPM( I ) = FVAL( I )
               END DO

               NEWFIL = .TRUE.
               NEWCAL = .TRUE.

            ELSE
               DO I = 1, NRIPM
                  IF ( FVAL( I ) .NE. RIPM( I ) ) THEN
                     NEWFIL = .TRUE.
                     NEWCAL = .TRUE.
                  END IF
                  RIPM( I ) = FVAL( I )
               END DO
            END IF
         END IF

         CALL CNPAR( 'RIPK\\', ISTAT )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'RIPK\\', STATUS )
            RETURN

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 300
         END IF
         GO TO 100
      END IF
 300  CONTINUE

*   RIPA - echelle ripple scale factor
      IF ( .NOT.(.NOT.(.TRUE.)) ) THEN
         FVAL(1) = RIPALF
         CALL RDPARF( 'RIPA\\', .TRUE., 1, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RIPA\\', STATUS )
            RETURN

         ELSE
            IF ( FVAL( 1 ) .NE. RIPALF ) THEN
               NEWFIL = .TRUE.
               NEWCAL = .TRUE.
            END IF
            RIPALF = FVAL(1)
         END IF

         CALL CNPAR( 'RIPA\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'RIPA\\', STATUS )
            RETURN

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 400
         END IF
         GO TO 300
      END IF

 400  CONTINUE

*   XCUT - echelle ripple scale factor
      IF ( .NOT.(.NOT.(.TRUE.)) ) THEN
         DO I = 1, 2
            FVAL( I ) = XRLIM( I )
         END DO

         CALL RDPARF( 'XCUT\\', .TRUE., 2, FVAL, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'XCUT\\', STATUS )
            RETURN

         ELSE IF ( ACTVAL .NE. 2 ) THEN
            CALL ERRPAR( 'XCUT\\' )
            CALL ERROUT( ': too few values\\', STATUS )

         ELSE
            DO I = 1, 2
               IF ( XRLIM( I ) .NE. FVAL( I ) ) THEN
                  NEWFIL = .TRUE.
                  NEWCAL = .TRUE.
               END IF
               XRLIM( I ) = FVAL( I )
            END DO
         END IF

         CALL CNPAR( 'XCUT\\', ISTAT )
         IF ( ISTAT .NE. SAI__OK ) THEN
            CALL PCANER( 'XCUT\\', STATUS )
            RETURN

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN
            GO TO 500
         END IF
         GO TO 400
      END IF

 500  CONTINUE
      END
