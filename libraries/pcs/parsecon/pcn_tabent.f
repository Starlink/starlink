      SUBROUTINE PARSECON_TABENT ( STATE, TOKTYPE, ACTCODE,
     :  NEWSTATE, STATUS )
*+
*  Name:
*     PARSECON_TABENT

*  Purpose:
*     Look-up parsing state-table.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_TABENT ( STATE, TOKTYPE, ACTCODE,
*    :   NEWSTATE, STATUS )

*  Description:
*     Look-up parsing state-table

*  Arguments:
*     STATE=INTEGER (given)
*        current parsing state
*     TOKTYPE=INTEGER (given)
*        type of current token
*     ACTCODE=INTEGER (returned)
*        action code
*     NEWSTATE=INTEGER (returned)
*        new parsing state
*     STATUS=INTEGER

*  Authors:
*     {original_author_entry}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER STATE           ! current parsing state

      INTEGER TOKTYPE         ! type of current token


*  Arguments Returned:
      INTEGER ACTCODE         ! action code (0 = none)

      INTEGER NEWSTATE        ! new parsing state


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON_CMN'


*.


      IF ( STATUS .NE. SAI__OK ) RETURN
*
*   Look-up the two values corresponding to the given combination of
*   parse-state and token-type. An invalid combination will result in
*          ACTCODE = ERROR
*          NEWSTATE = FACEGOT
*
      ACTCODE = ACTTAB(STATE,TOKTYPE)
      NEWSTATE = STATETAB(STATE,TOKTYPE)

      END
