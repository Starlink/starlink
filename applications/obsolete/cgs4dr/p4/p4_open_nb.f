*+  P4_OPEN_NB - Open the P4 noticeboard
      SUBROUTINE P4_OPEN_NB( STATUS )
*    Description :
*     This routine creates a memory-resident noticeboard.
*    Invocation :
*     CALL P4_OPEN_NB( STATUS )
*    Authors :
*     P N Daly   (JACH.HAWAII.EDU::PND)
*    History :
*      4-Aug-1994: Original version.                            (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS                        ! Global status
*    Global variables :
      INCLUDE 'P4COM.INC'                   ! P4 common block
*-

*    Check for error on entry.
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Define a noticeboard
      CALL P4_DEFINE_NB( STATUS )

*    Define a noticeboard
      CALL P4_FIND_NB( STATUS )

*    Initialise the common block to pre-defined values
      CALL P4_INIT_CB( -1, STATUS )
      CALL P4_INIT_SYS( STATUS )

*    Populate the noticeboard with common block values
      CALL P4_WRITE_NB( -1, STATUS )

      END
