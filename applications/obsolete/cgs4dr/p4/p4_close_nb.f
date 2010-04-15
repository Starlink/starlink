*+  P4_CLOSE_NB - Close the P4 noticeboard
      SUBROUTINE P4_CLOSE_NB( STATUS )
*    Description :
*     This routine closes the P4 noticeboard
*    Invocation :
*     CALL P4_CLOSE_NB( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      3-Aug-1994: Original Unix version.                           (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Global variables :
      INCLUDE 'P4COM.INC'              ! P4 common block
*    Status :
      INTEGER STATUS                   ! Global status
*    Local variables :
      INTEGER ERR_STAT                 ! An error status
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Unmap (lose) the noticeboard
      CALL NBS_LOSE_NOTICEBOARD( NB_TOPID, 'FORCE', STATUS )
      IF ( STATUS .NE. SAI__OK ) THEN
        ERR_STAT = STATUS
        STATUS = SAI__ERROR
        CALL MSG_SETI( 'ES', ERR_STAT )
        CALL ERR_REP( ' ', 'P4_CLOSE_NB: '/
     :    /'Failed to lose noticeboard global section, '/
     :    /'Status = ^ES', STATUS )
      ENDIF

      END
