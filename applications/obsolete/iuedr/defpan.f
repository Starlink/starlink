      SUBROUTINE DEFPAN( STATUS )
*+
*  Name:
*     SUBROUTINE DEFPAN

*  Purpose:
*     Read the general LBLS creation parameters.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL DEFPAN( STATUS )

*  Arguments:
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Authors:
*     JRG: Jack Giddings (UCL)
*     PCTR: Paul Rees (UCL)
*     MJC: Martin Clayton (UCL)
*     {enter_new_authors_here}

*  History:
*     01-MAY-82 (JRG):
*       IUEDR Vn. 1.0
*     04-NOV-88 (PCTR):
*       IUEDR Vn. 2.0
*     15-FEB-95 (MJC):
*       IUEDR Vn. 3.2
*       Upped the Pseudo-order limit from 55 to 120.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PAR_ERR'

*  Global Variables:
      INCLUDE 'CMHEAD'
      INCLUDE 'CMEXTP'

*  Local Constants:
      INTEGER MAXPO         ! Maximum number of pseudo-orders.
      PARAMETER ( MAXPO = 120 )

*  Status:
      INTEGER STATUS        ! Global status.

*  External refernces:
      LOGICAL STR_SIMLR     ! Caseless string equality.

*  Local variables:
      INTEGER ACTVAL        ! parameter value count.
      INTEGER N             ! RL range test.
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  GSAMP.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'GSAMP\\', .FALSE., 1, GSAMP, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'GSAMP\\', STATUS )
            GO TO 999

         ELSE IF ( GSAMP .LE. 0.5 ) THEN
            CALL ERRPAR( 'GSAMP\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 100
         END IF

         CALL CNPAR( 'GSAMP\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'GSAMP\\', STATUS )
            GO TO 999
         END IF
      END DO
 100  CONTINUE

*  CUTWV - whether cutoff wavelengths used to define extraction grid.
      IF ( STR_SIMLR( RESOL, 'HIRES\\' ) ) THEN
         CALL RDPARL( 'CUTWV\\', .FALSE., 1, CUTWV, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'CUTWV\\', STATUS )
            GO TO 999
         END IF

      ELSE
         CUTWV = .FALSE.
      END IF

*  CENTM - whether pre-existing centroid template is used.
      CALL RDPARL( 'CENTM\\', .FALSE., 1, CENTM, ACTVAL, STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL PARFER( 'CENTM\\', STATUS )
         GO TO 999
      END IF

*  RSAMP.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'RSAMP\\', .FALSE., 1, RSAMP, ACTVAL, STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RSAMP\\', STATUS )
            GO TO 999

         ELSE IF ( RSAMP .LT. 0.25 ) THEN
            CALL ERRPAR( 'RSAMP\\' )
            CALL ERROUT( ': out of range\\', STATUS )

         ELSE
            GO TO 200
         END IF

         CALL CNPAR( 'RSAMP\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'RSAMP\\', STATUS )
            GO TO 999
         END IF
      END DO
 200  CONTINUE

*  RL - radial grid limits.
      DO WHILE ( .TRUE. )
         CALL RDPARF( 'RL\\', .FALSE., 2, RL, ACTVAL, STATUS )
         IF ( STATUS .EQ. PAR__NULL ) THEN
            CALL ERR_FLUSH( STATUS )
            CALL LINE_WRITS( '%p!  RL will take the value [0,0]\\' )
            CALL PRTBUF( STATUS )
            RL( 1 ) = 0.0
            RL( 2 ) = 0.0
            AUSLIT = .TRUE.
            GO TO 999

         ELSE IF ( STATUS .NE. SAI__OK ) THEN
            CALL PARFER( 'RL\\', STATUS )
            GO TO 999

         ELSE IF ( ACTVAL.EQ.1 .AND. RL( 1 ).EQ.0.0 ) THEN
            RL( 2 ) = RL( 1 )
            AUSLIT = .TRUE.
            GO TO 999

         ELSE IF ( ACTVAL .EQ. 1 ) THEN
            RL( 2 ) = NINT( REAL( ABS( RL( 1 ) ) / RSAMP ) ) * RSAMP
            RL( 1 ) = -RL( 2 )
            N = NINT( REAL( ( RL( 2 ) - RL( 1 ) ) / RSAMP ) ) + 1
            IF ( N.LE.1 .OR. N.GT.MAXPO ) THEN
               CALL ERRPAR( 'RL\\' )
               CALL ERROUT( ': out of range\\', STATUS )

            ELSE
               AUSLIT = .FALSE.
               GO TO 999
            END IF

         ELSE IF ( RL( 2 ) .EQ. RL( 1 ) ) THEN
            RL( 1 ) = 0.0
            RL( 2 ) = 0.0
            AUSLIT = .TRUE.
            GO TO 999

         ELSE
            N = NINT( REAL( ( RL( 2 ) - RL( 1 ) ) / RSAMP ) ) + 1
            IF ( N.LE.1 .OR. N.GT.MAXPO ) THEN
               CALL ERRPAR( 'RL\\' )
               CALL ERROUT( ': out of range\\', STATUS )

            ELSE
               AUSLIT = .FALSE.
               GO TO 999
            END IF
         END IF

         CALL CNPAR( 'RL\\', STATUS )
         IF ( STATUS .NE. SAI__OK ) THEN
            CALL PCANER( 'RL\\', STATUS )
            GO TO 999
         END IF
      END DO

 999  CONTINUE

      END
