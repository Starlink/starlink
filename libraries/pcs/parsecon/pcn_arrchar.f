      SUBROUTINE PARSECON_ARRCHAR ( STRING, MXVALS, COUNT, CARRAY,
     :  CLENS, STATUS )
*+
*  Name:
*     PARSECON_ARRCHAR

*  Purpose:
*     Split character string into a set of values.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_ARRCHAR ( STRING, MXVALS, COUNT, CARRAY,
*    :   CLENS, STATUS )

*  Description:
*     Split a character string up into a set of values. Values are
*     separated by spaces or commas not inside quoted strings.

*  Arguments:
*     STRING=CHARACTER*(*) (given)
*        string to be parsed
*     MXVALS=INTEGER (given)
*        maximum possible number of values
*     COUNT=INTEGER (returned)
*        number of values found
*     CARRAY(MXVALS)=CHARACTER*(*) (returned)
*        array of 'values' - ie substrings
*     CLENS(MXVALS)=INTEGER (returned)
*        array of lengths of substrings
*     INTEGER STATUS

*  Algorithm:
*     For the purposes of this routine, a token is a sequence of characters
*     which are either all alphanumeric (with .+-()_[]"<>/~ being honorary
*     alphanumerics) or are all non-alphanumeric (ie are anything other
*     than alphanumeric and "white"). For the purposes of this routine, a
*     white character is a space, tab, newline or comma.

*     Tokens are thus terminated by either a character of the opposite class
*     or by a "white" character. No characters need separate tokens.
*     Apart from their role as delimiters, white characters are never
*     significant.

*     Two exceptions to this rule are -

*      1) A token may consist of a set of characters enclosed in single
*         quotes. The first quote must be the first character of the token
*         and the token is terminated by the next isolated (ie.not '') quote
*         or end of buffer (whichever comes first).
*         Note that double quotes within a quoted token are not collapsed.

*      2) Terminators occurring within brackets within an alphanumeric
*         token will be ignored ( as in STRUCTURE(1,2).NUMBER ).

*     Hexadecimal constants are recognised and converted to decimal
*     strings.

*     All characters on a line that follow a token starting with # token are
*     ignored, AS IS THE # .

*     The tokens within STRING are recognised, and transferred to
*     CARRAY, one token per array element.
*     Brackets identified as being part of a list of array elements are
*     returned as individual tokens. To achieve this it is necessary to
*     keep track of the nesting of brackets, and to switch between '('
*     and ')' being handled as special characters and being handled as
*     alphanumeric. The full array list of whatever dimensionality has
*     to be contained within the one text line.

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1990, 1991, 1992 Science & Engineering Research Council.
*     Copyright (C) 1996 Central Laboratory of the Research Councils.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     W.F.Lupton (RGO)
*     {enter_new_authors_here}

*  History:
*     18.09.1984:  VAX version (REVAD::BDK)
*     27.09.1984:  ARRCHAR variant of GETTOK (REVAD::BDK)
*     21.11.1984:  Make # sign recognised if it is just the first
*        character of a token - ie. it need not be followed by
*        a delimiter. (REVAD::BDK)
*     27.02.1985:  handle brackets in array list (REVAD::BDK)
*     07.05.1987:  stop ! being a comment character (REVAD::BDK)
*     07.05.1987:  ignore delimiters inside brackets embedded within a
*        token - eg JUNK(3,4) (REVAD::BDK)
*     21.10.1987:  accept ? and @ as alphanumeric (REVAD::BDK)
*     28.11.1990:  correct test for termination of brackets within
*        token and don't terminate token at termination of
*        brackets.
*        Improve comments (RLVAD::AJC)
*     28.11.1990:  rename from STRING_ARRCHAR
*        use CHR_SKCHR (RLVAD::AJC)
*     01,10,1991:  revised spec for CHR_SKCHR (RLVAD::AJC)
*     10.10.1991:  correct Z in list of letters (RLVAD::AJC)
*     04.11.1991:  fix for CHR_FIWS bug if length = 1 (RLVAD::AJC)
*     20.11.1991:  stop ' being honorary alphanumeric (RLVAD::AJC)
*     24.02.1992:  report errors
*        make ~ and / honorary aplhanumeric
*        (for Unix names) (RLVAD::AJC)
*     26.02.1992:  Don't convert to upper case (RLVAD::AJC)
*     05.08.1996:  Insert closing quote if missing (AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PARSECON_ERR'
      INCLUDE 'CHR_ERR'


*  Arguments Given:
      CHARACTER*(*) STRING           ! string to be parsed
      INTEGER MXVALS                 ! maximum possible number of values


*  Arguments Returned:
      INTEGER COUNT                  ! number of values found

      CHARACTER*(*) CARRAY(MXVALS)   ! array of 'values' - ie substrings

      INTEGER CLENS(MXVALS)          ! array of lengths of substrings


*  Status:
      INTEGER STATUS


*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )


*  Local Variables:
      INTEGER LBRACK             ! count of bracket nesting

      INTEGER TOKLEN             ! no of chars in token,
                                 ! zero => error,
                                 ! -1 => end-of-recod
                                 ! -2 => EOF

      INTEGER HLEN               ! length of integer from hex string

      INTEGER PTR                ! pointer to input string

      INTEGER ISTAT              ! system service status

      INTEGER J                  ! temporary pointer to input string

      LOGICAL FINISHED           ! loop controller for copying string
                                 ! constants

      LOGICAL HEX                           ! .TRUE. => syntax of token
                                            ! is like HEX constant

      INTEGER ITEMP                         ! temporary store for
                                            ! integer converted from HEX

      INTEGER LENGTH                        ! length of STRING

      INTEGER PT2                           ! temporary pointer into
                                            ! BUFFER

      INTEGER START                         ! pointer into tokens of
                                            ! type JUNK(3,4)

      INTEGER TBRACK                        ! counter of bracket nesting
                                            ! within token of type
                                            ! JUNK(3,4)
      INTEGER LALN                          ! used length of ALN

      CHARACTER*80 ALN                      ! The current set of alphanumerics

      CHARACTER*(*) ALN1                    ! Alphanumerics including ()
      PARAMETER (ALN1='ABCDEFGHIJKLMNOPQRSTUVWXYZ'//
     :'abcdefghijklmnopqrstuvwxyxz'//
     :'0123456789'//
     :'$:;.+-_[]"<>/~()' )
      INTEGER LALN1                         ! Used length of LALN1
      PARAMETER (LALN1=78)

      CHARACTER*(*) ALN2                    ! Alphanumerics excluding ()
      PARAMETER (ALN2='ABCDEFGHIJKLMNOPQRSTUVWXYX'//
     :'abcdefghijklmnopqrstuvwxyx'//
     :'0123456789'//
     :'$:;.+-_[]"<>/~' )
      INTEGER LALN2                         ! Used length of LALN2
      PARAMETER (LALN2=76)

      CHARACTER*(*) OTHER
      PARAMETER ( OTHER = '!#%&*=\\^`{|}' )


*  Local Data:

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*
*   Find the start and end of the given string.
*
      PTR = 1
      LENGTH = CHR_LEN( STRING )
      IF ( LENGTH .GT. 1 ) THEN
*     Skip spaces, tabs and commas to find the start
         CALL CHR_FIWS( STRING(1:LENGTH), PTR, STATUS )
*     Status returned is bound to be OK
      ENDIF

*
*   Loop picking up tokens and loading them into CARRAY
*
      COUNT = 0
      LBRACK = 0

*   Start with () being alphanumeric
      ALN = ALN1
      LALN = LALN1
      DO WHILE ( ( PTR .LE. LENGTH ) .AND. ( COUNT .LT. MXVALS ) .AND.
     :  ( STATUS .EQ. SAI__OK ) )

         COUNT = COUNT + 1
*
*      check first char in token, which determines token type, so
*      end of token is first char not of this type
*
         HEX = .FALSE.

         IF ( INDEX( ALN(1:LALN), STRING(PTR:PTR) ) .NE. 0 ) THEN

*        First character is alphanumeric but not 'quote'
            IF ( STRING(PTR:PTR) .NE. '(' ) THEN

               TOKLEN = 1
               CALL CHR_SKCHR( ALN(1:LALN), STRING(PTR:LENGTH),
     :          .TRUE., TOKLEN )
               TOKLEN = TOKLEN - 1

               IF ( TOKLEN .LE. 0 ) TOKLEN = LENGTH - PTR + 1
*
*            Check for delimiter within brackets embedded in token
*
               IF ( PTR+TOKLEN .LE. LENGTH ) THEN
                  START = INDEX ( STRING(PTR:PTR+TOKLEN), '(' )
               ELSE
                  START = INDEX ( STRING(PTR:LENGTH), '(' )
               ENDIF
               IF ( START .GT. 0 ) THEN
                  TBRACK = 1
                  START = START + PTR - 1
                  DO WHILE ( ( TBRACK .GT. 0 ) .AND.
     :              ( START .LT. LENGTH ) )
                     DOWHILE ( START .LT. PTR + TOKLEN - 1 )
                        START = START + 1
                        IF ( STRING(START:START) .EQ. '(' ) THEN
                           TBRACK = TBRACK + 1
                        ELSE IF ( STRING(START:START) .EQ. ')' ) THEN
                           TBRACK = TBRACK - 1
                        ENDIF
                     ENDDO

*                 Now if level is GT 0, the token ended with brackets open
*                 set START and TOKLEN to end of next token and continue
*                 counting levels
                     IF ( TBRACK .GT. 0 ) THEN
*                    Brackets open.
*                    Skip to start of next token
                        PT2 = 1
                        CALL CHR_FIWS( STRING(PTR+TOKLEN:LENGTH), PT2,
     :                                 ISTAT )
                        START = START + PT2
*                    then find end of next token
                        CALL CHR_SKCHR
     :                    ( ALN(1:LALN), STRING(START:LENGTH),
     :                      .TRUE., TOKLEN )
                        TOKLEN = TOKLEN - 1

                     ENDIF

*                    Now continue counting levels
                  ENDDO

                  TOKLEN = START - PTR + 1
               ENDIF

            ELSE
*
*            start of an array of values.
*            change character type of '(' and ')'
*            start counting nesting
*            the '(' will be returned as a token.
*
               ALN = ALN2
               LALN = LALN2
               LBRACK = 1
               TOKLEN = 1

            ENDIF

         ELSE IF ( STRING(PTR:PTR) .EQ. QUOTE ) THEN
*
*         Search for terminating quote, checking for 'escaped' quote
*         or terminating 'X
*
            FINISHED = .FALSE.
            J = PTR + 1

            DO WHILE ( ( J .LE. LENGTH-1 ) .AND. ( .NOT. FINISHED ) )

               IF ( STRING(J:J+1) .EQ. QUOTE//QUOTE ) THEN
                  J = J + 2
               ELSE IF ( ( STRING(J:J+1) .EQ. QUOTE//'X' ) .OR.
     :           ( STRING(J:J+1) .EQ. QUOTE//'x' ) ) THEN
                  J = J + 2
                  HEX = .TRUE.
                  FINISHED = .TRUE.
               ELSE IF ( STRING(J:J) .EQ. QUOTE ) THEN
                  J = J + 1
                  FINISHED = .TRUE.
               ELSE
                  J = J + 1
               ENDIF

            ENDDO

            IF ( FINISHED ) THEN
               TOKLEN = J - PTR
            ELSE
               TOKLEN = J - PTR + 1
            ENDIF

         ELSE IF ( STRING(PTR:PTR) .EQ. ')' ) THEN
*
*         right bracket within a string
*         reduce the nesting count, and if zero switch '(' and ')' back
*         to being ALPHANUM
*         return the ')' as a token.
*
             IF ( LBRACK .EQ. 1 ) THEN
                LBRACK = 0
                ALN = ALN1
                LALN = LALN1
                TOKLEN = 1
             ELSE IF ( LBRACK .GT. 1 ) THEN
                LBRACK = LBRACK - 1
                TOKLEN = 1
             ELSE
                LBRACK = 0
                ALN = ALN1
                LALN = LALN1
                TOKLEN = 1
                STATUS = PARSE__IVSYN
                CALL EMS_REP ( 'PCN_ARRCHAR1',
     :          'PARSECON: Unmatched '')''', STATUS )
             ENDIF

         ELSE IF ( STRING(PTR:PTR) .EQ. '(' ) THEN
*
*         increase depth of bracket nesting
*         return '(' as token
*
            LBRACK = LBRACK + 1
            TOKLEN = 1

         ELSE
*
*         something else
*

            TOKLEN = 1
            CALL CHR_SKCHR(
     :                OTHER, STRING(PTR:LENGTH), .TRUE., TOKLEN )
            TOKLEN = TOKLEN - 1

         ENDIF
*
*      and write token, converting to decimal string if HEX, adding end
*      quote if missing
*
         IF ( HEX ) THEN

*        Convert HEX to integer
            ISTAT = SAI__OK
            CALL CHR_HTOI( STRING(PTR+1:PTR+TOKLEN-3),
     :                         ITEMP, ISTAT )
            IF( ISTAT .EQ. SAI__OK ) THEN
*           Convert INTEGER to CHARACTER
               CALL CHR_ITOC( ITEMP, CARRAY(COUNT), HLEN )
            ELSE
               CARRAY(COUNT) = ' '
               STATUS = PARSE__IVSYN
               CALL EMS_REP ( 'PCN_ARRCHAR2',
     :         'PARSECON: Failed to convert HEX number', STATUS )
            ENDIF

         ELSE IF ( STRING(PTR:PTR) .EQ. QUOTE ) THEN

*      Quoted string - ensure it is end quoted
            IF ( STRING( PTR+TOKLEN-1:PTR+TOKLEN-1 ) .NE. QUOTE ) THEN
*            It must be unquoted at end of string
               CARRAY(COUNT) = STRING(PTR:PTR+TOKLEN-1) // QUOTE
               TOKLEN = TOKLEN+1

            ELSE
               CARRAY(COUNT) = STRING(PTR:PTR+TOKLEN-1)

            ENDIF

         ELSE

             CARRAY(COUNT) = STRING(PTR:PTR+TOKLEN-1)

         ENDIF
*
*      now set ptr ready for next token (skip any WHITE space)
*      If token implies a comment, ignore rest of STRING.
*
         IF ( ( CARRAY(COUNT)(1:1) .EQ. '#' ) .OR.
     :     ( PTR .GT. LENGTH ) ) THEN
            PTR = LENGTH + 1
         ELSE
            PTR = PTR + TOKLEN

*      Skip white space to start of next token
            PT2 = 1
            ISTAT = SAI__OK
            CALL CHR_FIWS( STRING(PTR:LENGTH), PT2, ISTAT )

*        If not found, force new line
            IF ( ISTAT .EQ. CHR__WRDNOTFND ) THEN
               PTR = LENGTH + 1
*        Else increment pointer to start of new token
            ELSE
               PTR = PTR + PT2 - 1
            ENDIF

            IF ( HEX ) TOKLEN = HLEN

         ENDIF

         CLENS(COUNT) = TOKLEN

      ENDDO
*
*   Check for the splitting being terminated by a comment or by running
*   out of array elements.
*
      IF ( CARRAY(COUNT)(1:1) .EQ. '#' )
     :  COUNT = COUNT - 1

      IF ( PTR .LT. LENGTH ) THEN

         STATUS = PARSE__BIGVEC
         CALL EMS_SETI ( 'MXVALS', MXVALS )
         CALL EMS_REP ( 'PCN_ARRCHAR3',
     :   'PARSECON: Too many tokens on a line (max is ^MXVALS)',
     :    STATUS )

      ENDIF

      END

