      SUBROUTINE SLAVETEST( STATUS )

      IMPLICIT NONE              ! No implicit typing
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

      INTEGER KAPPAPID
      INTEGER NDFPID
      INTEGER SLV_LOADW
      INTEGER STATUS
      INTEGER TIMEOUT
      INTEGER VIEWID
      LOGICAL DETACH

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      DETACH = .FALSE.
      TIMEOUT = 15

*  Load tasks.
      NDFPID = SLV_LOADW( 'NDFPACK', 'ndfpack_mon', DETACH, TIMEOUT,
     :                    STATUS )
      KAPPAPID = SLV_LOADW( 'KAPPA', 'kappa_mon', DETACH, TIMEOUT,
     :                      STATUS )
      VIEWID = SLV_LOADW( 'KAPVIEW', 'kapview_mon', DETACH, TIMEOUT,
     :                    STATUS )
      
*  Run the applications.
      CALL SLV_OBEYW( 'NDFPACK', 'ndftrace', 'prompt', 'NDF<NDF',
     :                STATUS )
      CALL SLV_OBEYW( 'KAPPA', 'stats', 'prompt', 'NDF<NDF',
     :                STATUS )
      CALL SLV_OBEYW( 'KAPVIEW', 'display', 'prompt',
     :                'IN<NDF,MODE<MODE,PERCENTILES<PERCENTILES',
     :                STATUS )

*  You could kill the tasks here (like this), but they die anyway when
*  the main application task terminates.
c      CALL SLV_KILL( NDFPID, STATUS )
c      CALL SLV_WAITK( NDFPID, 10, STATUS )

*  Report a contextual error message.
      IF ( STATUS .NE. SAI__OK ) THEN
         CALL ERR_REP( 'SLAVETEST_ERR',
     :                 'SLAVETEST: Error occurred in test program.',
     :                 STATUS )
      END IF
      END
