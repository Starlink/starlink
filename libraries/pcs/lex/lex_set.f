      SUBROUTINE LEX_SET(NSTATE,TABLE,STATE,C1,C2,NEWSTATE,ACTION,
     :       BACK,COPY,WRITE,STACK,CH,STATUS)
*+
*  Name:
*     LEX_SET

*  Purpose:
*     Set one or more entires in the state table for the LEX parser.

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

*  Copyright:
*     Copyright (C) 1987 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     Jeremy Bailey  (AAOEPP::JAB)
*     {enter_new_authors_here}

*  History:
*     08-JAN-1987 (JAB):
*        Original version.
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
