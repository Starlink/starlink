      SUBROUTINE SUBPAR_STATE ( NAMECODE, STATE, STATUS )
*+
*  Name:
*     SUBPAR_STATE

*  Purpose:
*     return the state of a parameter.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_STATE ( NAMECODE, STATE, STATUS )

*  Description:
*     Return the current state of the indicated parameter.

*  Arguments:
*     NAMECODE=INTEGER (given)
*        pointer to parameter
*     STATE=INTEGER (returned)
*        current state of parameter, one of
*        SUBPAR__GROUND
*        SUBPAR__ACTIVE
*        SUBPAR__CANCEL
*        SUBPAR__NULL
*        SUBPAR__EOL
*        SUBPAR__RESET
*        SUBPAR__ACCEPT
*        SUBPAR__RESACC
*        SUBPAR__FPROMPT
*        SUBPAR__RESPROM
*        SUBPAR__ACCPR
*        SUBPAR__RESACCPR
*     STATUS=INTEGER
*        Global status

*  Algorithm:
*     Return the parameter state from the SUBPAR common blocks.

*  Authors:
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipoperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     30-JUL-1987 (BDK):
*        Original
*      1-MAR-1993 (AJC):
*        Add INCLUDE DAT_PAR
*     16-MAR-1993 (AJC):
*        Add new states
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'

*  Arguments Given:
      INTEGER NAMECODE     ! pointer to parameter

*  Arguments Returned:
      INTEGER STATE        ! current state of parameter

*  Status:
      INTEGER STATUS

*  Global Variables:
      INCLUDE 'SUBPAR_CMN'

*.
      IF ( STATUS .NE. SAI__OK ) RETURN

      STATE = PARSTATE(NAMECODE)

      END
