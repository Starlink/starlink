************************************************************************

      SUBROUTINE AGI_ICURP ( PICID, STATUS )

*+
*  Name :
*     AGI_ICURP
*
*  Purpose :
*     Inquire the current picture
*
*  Invocation :
*     CALL AGI_ICURP( PICID, STATUS )
*
*  Description :
*     The picture identifier of the current picture is returned.
*
*  Arguments :
*     PICID = INTEGER (Returned)
*        Picture identifier
*     STATUS = INTEGER (Given and Returned)
*        The global status
*
*  Algorithm :
*     Check status on entry.
*     If the current picture identifier is valid then return it.
*
*  Authors :
*     NE: Nick Eaton (Durham University)
*
*  History :
*     July 1990 (NE):
*        Original version
*-
*  Type Definitions :
      IMPLICIT NONE

*  Global constants :
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'AGI_PAR'
      INCLUDE 'AGI_ERR'

*  Global variables :
      INCLUDE 'agi_pfree'

*  Arguments Returned :
      INTEGER PICID

*  Status :
      INTEGER STATUS
*.

*   Check status on entry
      IF ( STATUS .EQ. SAI__OK ) THEN

*   If the current picture identifier is valid then return it
         IF ( CURPID .GT. 0 ) THEN
            PICID = CURPID

*   Otherwise flag an error
         ELSE
            STATUS = AGI__NOCUP
            CALL ERR_REP( 'AGI_ICURP_NOCUP', 'No current picture',
     :                    STATUS )
         ENDIF

      ENDIF

      END

