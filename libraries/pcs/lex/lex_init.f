      SUBROUTINE LEX_INIT(NSTATE,TABLE)
*+
*  Name:
*     name

*  Purpose:
*     LEX_INIT

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE
*     Initialize a state table for the LEX parser

*  Invocation:
*     CALL LEX_INIT(NSTATE,TABLE)

*  Arguments:
*     NSTATE = INTEGER (given)
*           The number of states in the state table
*     TABLE(4,0:127,NSTATE) = BYTE (returned)
*           The state table

*  Algorithm:
*     The state table is filled with entries which cause the
*     parser to signal an error. Valid state transitions will
*     be subsequently overwritten by calls to the LEX_SET
*     routine.

*  Authors:
*     Jeremy Bailey (AAOEPP::JAB) 8 Jan 1987
*     {enter_new_authors_here}

*  History:
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
*     <any INCLUDE files containing global constant definitions>
*  Arguments Given:
      INTEGER NSTATE
*  Arguments Returned:
      BYTE TABLE(4,0:127,NSTATE)
*  Local Variables:
      INTEGER I,C
*.

      DO I=1,NSTATE
         DO C=0,127
            TABLE(1,C,I)=0
            TABLE(2,C,I)=-1
            TABLE(3,C,I)=0
            TABLE(4,C,I)=0
         ENDDO
      ENDDO

      END
