*+  CRED4_INIT_NBFLAGS - Initialise the noticeboard flags
      SUBROUTINE CRED4_INIT_NBFLAGS( STATUS )
*    Description :
*     This routine initialises the flags contained in the noticeboard.
*    Invocation :
*     CALL CRED4_INIT_NBFLAGS( STATUS )
*    Authors :
*     S M Beard  (UK.AC.ROE.STAR::SMB)
*     P N Daly (JACH::PND)
*    History :
*     21-Jun-1990: Original version, as part of the phase 2 major changes (SMB)
*     30-Aug-1994: Ported to Unix (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'
*    Status :
      INTEGER STATUS             ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'     ! CRED4 common block
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Initialise the flags.
      CALL NBS_PUT_VALUE( STOP_REDUCTION_ID, 0, VAL__NBI, .FALSE., STATUS )
      CALL NBS_PUT_VALUE( PAUSE_REDUCTION_ID, 0, VAL__NBI, .FALSE., STATUS )
      CALL NBS_PUT_VALUE( ABORT_REDUCTION_ID, 0, VAL__NBI, .FALSE., STATUS )
      CALL NBS_PUT_VALUE( REDUCING_ID, 0, VAL__NBI, .FALSE., STATUS )
      CALL NBS_PUT_VALUE( CRED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
      CALL NBS_PUT_VALUE( RED4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )
      CALL NBS_PUT_VALUE( P4_BUSY_ID, 0, VAL__NBI, .FALSE., STATUS )

      END
