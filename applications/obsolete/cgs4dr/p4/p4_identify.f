*+  P4_IDENTIFY - Identify a graph by adding the username, date and time.
      SUBROUTINE P4_IDENTIFY( STATUS )
*    Invocation :
*     CALL P4_IDENTIFY( STATUS )
*    Authors :
*     P. N. Daly (JACH::PND)
*    History :
*     12-Aug-1994: Original Unix version.                       (PND)
*    endhistory
*    Type Definitions :
      IMPLICIT NONE
*    Global constants :
      INCLUDE 'SAE_PAR'
*    Status :
      INTEGER STATUS
*    Local variables :
      CHARACTER*20 PG_STATE               ! Return from PGQINF call
      INTEGER IGNORE                      ! Length of returned string
*-

*    Check for error in entry
      IF ( STATUS .NE. SAI__OK ) RETURN

*    Identify the plot on the open device
      CALL PGQINF( 'STATE', PG_STATE, IGNORE )
      IF ( PG_STATE .EQ. 'OPEN' ) CALL PGIDEN

      END
