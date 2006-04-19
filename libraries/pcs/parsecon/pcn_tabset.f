      SUBROUTINE PARSECON_TABSET ( STATE, TOKTYPE, ACTCODE, NEWSTATE )
*+
*  Name:
*     PARSECON_TABSET

*  Purpose:
*     puts values into state-table for interface parsing.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_TABSET ( STATE, TOKTYPE, ACTCODE, NEWSTATE )

*  Arguments:
*     STATE=INTEGER (given)
*           current parse-state
*     TOKTYPE=INTEGER (given)
*           type of token read from interface file
*     ACTCODE=INTEGER (given)
*           code for action to be taken
*     NEWSTATE=INTEGER (given)
*           new parse state

*  Description:
*     The given action-code and new parse state are loaded into the
*     state table at the location corresponding to the given state and
*     token-type.

*  Algorithm:
*     There are a pair of 2-d byte arrays held in a common block.
*     One holds action codes (ACTTAB) the other parse states
*     (STATETAB). This routine simply sets the given action code and
*     state code into the (STATE,TOKTYPE) elements of each of the two
*     arrays. The arrays have previously been filled with values
*     corresponding to error conditions, and so PARSECON_TABSET is only
*     called to fill-in valid conditions.

*  Authors:
*     B.D.Kelly (REVAD::BDK)
*     {enter_new_authors_here}

*  History:
*     17.09.1984:  Original (REVAD::BDK)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER STATE          ! current parse-state

      INTEGER TOKTYPE        ! token-type

      INTEGER ACTCODE        ! code for action to be taken

      INTEGER NEWSTATE       ! new parse-state


*  Global Variables:
      INCLUDE 'PARSECON_CMN'


*.


      ACTTAB(STATE,TOKTYPE) = ACTCODE
      STATETAB(STATE,TOKTYPE) = NEWSTATE

      END
