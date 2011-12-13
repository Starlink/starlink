      SUBROUTINE LEX_PARSE(INIT,STRING,NSTATE,TABLE,ACTION,TOKEN,
     :      TLEN,POSN,STATUS)
*+
*  Name:
*     name

*  Purpose:
*     LEX_PARSE

*  Language:
*     Starlink Fortran 77

*  Type Of Module:
*     SUBROUTINE

*  Invocation:
*     CALL LEX_PARSE(INIT,STRING,NSTATE,TABLE,ACTION,TOKEN,
*     :      TLEN,POSN,STATUS)

*  Description:
*     LEX parser - main routine

*  Arguments:
*     INIT = LOGICAL (given)
*           If TRUE a parse of a new string is initiated, otherwise
*           a continuation of a current parse
*     STRING = CHARACTER*(*) (given)
*           The string to be parsed
*     NSTATE = INTEGER (given)
*           The number of states in the state table
*     TABLE(4,0:127,NSTATE) = BYTE (given)
*           The state table
*     ACTION = INTEGER (returned)
*           Action code of action to be performed
*     TOKEN = CHARACTER*(*) (returned)
*           Token string
*     TLEN = INTEGER (returned)
*           Length of token string
*     POSN = INTEGER (returned)
*           Current position in parse
*     STATUS = INTEGER

*  Algorithm:
*     On the initital call (INIT = TRUE) the character position is
*     set to 1, and the state is set to 1. Then for each character
*     the entry in the state table is found corresponding to the
*     current character and state. This entry specifies the new state,
*     a code which can be some combination of BACK, COPY, WRITE and
*     STACK, and optionally an ACTION. If an action is specified
*     the routine returns to its caller with the action number and
*     current token. Otherwise it advances to the next character
*     (unless BACK is specified). It also returns to the caller in the
*     event of an invalid character for the state, or on reaching the
*     end of the string.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council.
*     Copyright (C) 1996, 2003 Central Laboratory of the Research
*     Councils. All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     Jeremy Bailey (AAOEPP::JAB)
*     Alan Chipperfield (RLVAD::AJC)
*     Tim Jenness (JACH::TIMJ)
*     {enter_new_authors_here}

*  History:
*     23-SEP-1991 (RLVAD::AJC):
*        Report errors correctly
*     06-MAR-1996 (RLVAD::AJC):
*        Correctly save OSTATE not STATE
*     28-MAR-2003: Initialise TOKEN on Linux to prevent uninitialised
*                 warnings from memory checkers (JACH::TIMJ)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE
*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'LEX_ERR'
*  Arguments Given:
      LOGICAL INIT
      CHARACTER*(*) STRING
      INTEGER NSTATE
      BYTE TABLE(4,0:127,NSTATE)
*  Arguments Returned:
      INTEGER ACTION
      CHARACTER*(*) TOKEN
      INTEGER TLEN
      INTEGER POSN
*  Status:
      INTEGER STATUS
*    External references :
*     <declarations for external function references>
*  Local Constants:
      INTEGER STACKSIZE
      PARAMETER (STACKSIZE = 20)     ! Size of state stack
*  Local Variables:
      INTEGER STATE, OSTATE          ! Current state, old state
      INTEGER NC                     ! Current character position
      INTEGER NL                     ! Length of string
      INTEGER NI                     ! Character posn in token
      INTEGER CODE                   ! internal action code
      INTEGER NCHAR	             ! Current character
      INTEGER STACK(STACKSIZE)       ! State stack
      INTEGER SP                     ! Stack pointer
      LOGICAL FINISHED               ! completion flag

      SAVE OSTATE,NL,NC,STACK,SP
*.

      IF (STATUS .EQ. SAI__OK) THEN
          IF (INIT) THEN

*  Initialize variables for first time through
             TOKEN = ' '

*  Old state is PAR
             OSTATE = 1
             NL = LEN(STRING)
             NC = 1
             SP = 1
          ENDIF
          NI = 1
          FINISHED = .FALSE.

          DO WHILE (.NOT. FINISHED)

*  Get next character in string

             NCHAR = ICHAR(STRING(NC:NC))

*  Look up in state table the new state, code and action

             STATE = TABLE(1,NCHAR,OSTATE)
             IF (STATE .EQ. 0) THEN
                SP = SP-1
                STATE = STACK(SP)
             ENDIF
             CODE = TABLE(2,NCHAR,OSTATE)
             ACTION = TABLE(4,NCHAR,OSTATE)

*  Look for a code which means something to be done

             IF (CODE .GE. 1) THEN

*  Perform the required action (use computed goto for efficiency)

                GOTO (1,2,3,4,5,4,5,8,9,10,11,12,13,12,13), CODE

 1              NC = NC-1                                  ! BACK
                GOTO 16

 2              TOKEN(NI:NI) = STRING(NC:NC)               ! COPY
                NI = NI+1
                GOTO 16

 3              TOKEN(NI:NI) = STRING(NC:NC)               ! BACK, COPY
                NI = NI+1
                NC = NC-1
                GOTO 16

 4              TOKEN(NI:NI) = CHAR(TABLE(3,NCHAR,OSTATE))  ! WRITE
                NI = NI+1
                GOTO 16

 5              TOKEN(NI:NI) = CHAR(TABLE(3,NCHAR,OSTATE))  ! BACK, WRITE
                NI = NI+1
                NC = NC-1
                GOTO 16

*  CODE 6 (COPY, WRITE) is interpreted as WRITE
*  CODE 7 (COPY, BACK, WRITE) is interpreted as BACK, WRITE

 8              STACK(SP) = OSTATE                         ! STACK
                SP = SP+1
                GOTO 16

 9              STACK(SP) = OSTATE                         ! STACK, BACK
                SP = SP+1
                NC = NC-1
                GOTO 16

 10             STACK(SP) = OSTATE                         ! STACK, COPY
                SP = SP+1
                TOKEN(NI:NI) = STRING(NC:NC)
                NI = NI+1
                GOTO 16

 11             STACK(SP) = OSTATE                     ! STACK, BACK, COPY
                SP = SP+1
                TOKEN(NI:NI) = STRING(NC:NC)
                NI = NI+1
                NC = NC-1
                GOTO 16

 12             STACK(SP) = OSTATE                         ! STACK, WRITE
                SP = SP+1
                TOKEN(NI:NI) = CHAR(TABLE(3,NCHAR,OSTATE))
		NI = NI+1
		GOTO 16

 13             STACK(SP) = OSTATE                    ! STACK, BACK, WRITE
		SP = SP+1
                TOKEN(NI:NI) = CHAR(TABLE(3,NCHAR,OSTATE))
                NI = NI+1
                NC = NC-1
                GOTO 16

*  CODE 14 (STACK, COPY, WRITE) is interpreted as STACK, WRITE
*  CODE 15 (STACK, COPY, BACK, WRITE) is interpreted as STACK, BACK, WRITE

 16             CONTINUE

             ELSE IF (CODE .LE. -1) THEN
		STATUS = LEX__INVCHAR
                CALL EMS_REP('LEX_PARSE1',
     :          'LEX: Invalid character for parser state', STATUS )
		FINISHED = .TRUE.
	     ENDIF

	     NC = NC+1
	     IF (NC .GT. NL) THEN
                STATUS = LEX__ENDPARSE
                CALL EMS_REP ( 'LEX_PARSE2',
     :          'LEX: Hit end of string', STATUS )
		FINISHED = .TRUE.
   	     ENDIF
             OSTATE = STATE
             IF (ACTION .NE. 0) THEN
                FINISHED = .TRUE.
             ENDIF
          ENDDO
	  TLEN = NI-1
	  POSN = NC-1
       ENDIF
       END
