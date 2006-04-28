      SUBROUTINE LEX_SET(NSTATE,TABLE,STATE,C1,C2,NEWSTATE,ACTION,
     :       BACK,COPY,WRITE,STACK,CH,STATUS)
*+
*  Name:
*     name

*  Purpose:
*     LEX_SET

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL LEX_SET(NSTATE,TABLE,STATE,C1,C2,NEWSTATE,ACTION,
*            BACK,COPY,WRITE,STACK,CH)

*  Description:
*     Set one or more entries in the state table for the LEX parser

*  Arguments:
*     NSTATE = INTEGER (given)
*           The number of states in the state table
*     TABLE(4,0:127,NSTATE) = BYTE (given)
*           The state table
*     STATE = INTEGER (given)
*           The state for which the entry is being defined
*     C1 = CHARACTER*1 (given)
*           The first character in the range to be defined
*     C2 = CHARACTER*1 (given)
*           The last character in the range to be defined
*     NEWSTATE = INTEGER (given)
*           The new state resulting from the state transition
*           A value of zero causes the new state to be taken
*           from the stack.
*     ACTION = INTEGER (given)
*           An action code associated with the transition
*     BACK = LOGICAL (given)
*           If true the parser remains on the current character
*           rather than advancing to the next.
*     COPY = LOGICAL (given)
*           If true the current character is copied to the
*           token string
*     WRITE = LOGICAL (given)
*           If true the character specified by parameter CH is
*           written to the token string
*     STACK = LOGICAL (given)
*           If true the state is saved on the stack before changing
*           to the new state
*     CH = CHARACTER*1 (given)
*           The character to be written to the token string if
*           WRITE is specified
*     STATUS = INTEGER 

*  Algorithm:
*     The specified entry is written into the state table for
*     the state and range of characters specified by STATE
*     and C1 - C2.

*  Authors:
*     Jeremy Bailey  (AAOEPP::JAB) 8 Jan 1987
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
      INCLUDE 'LEX_ERR'
*  Arguments Given:
      INTEGER NSTATE
      BYTE TABLE(4,0:127,NSTATE)
      INTEGER STATE
      CHARACTER*1 C1,C2
      INTEGER NEWSTATE
      INTEGER ACTION
      LOGICAL BACK,COPY,WRITE,STACK
      CHARACTER*1 CH
*  Status:
      INTEGER STATUS
*  Local Variables:
      INTEGER I,I1,I2,IC
*.

      IF (STATUS .EQ. SAI__OK) THEN
         IF (STATE .GT. NSTATE .OR. STATE .LT. 1) THEN
            STATUS = LEX__NOSTATE
         ELSE

*  Convert character range to integers

            I1 = ICHAR(C1)
            I2 = ICHAR(C2)

*  Calculate code entry (one bit for each flag)

            IC = 0
            IF (BACK) IC = IC+1
            IF (COPY) IC = IC+2
            IF (WRITE) IC = IC+4
            IF (STACK) IC = IC+8

*  Loop over range of characters setting up table

            DO I=I1,I2
               TABLE(1,I,STATE) = NEWSTATE
               TABLE(2,I,STATE) = IC
               IF (WRITE) THEN
                  TABLE(3,I,STATE) = ICHAR(CH)
               ENDIF
               TABLE(4,I,STATE) = ACTION
            ENDDO
         ENDIF
      ENDIF
      END
