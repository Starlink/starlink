*+  CRED4_LIST_NB - Lists the CRED4 noticeboard
      SUBROUTINE CRED4_LIST_NB( STATUS )
*    Description :
*     This routine creates a memory-resident noticeboard
*    Invocation :
*     CALL CRED4_LIST_NB( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*     30-Aug-1994: Original Unix version.                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                        ! Global status
*    Global variables :
      INCLUDE 'CRED4COM.INC'                ! CRED4 common block
*-

*   Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*   Read the item identifiers
      CALL CRED4_READ_NB( STATUS )

*   List the parameters
      CALL CRED4_LIST_PARAMETERS( STATUS )

      END
