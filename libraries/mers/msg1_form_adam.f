      SUBROUTINE MSG1_FORM( PARAM, TEXT, CLEAN, MSGSTR, MSGLEN, STATUS )
*+
*  Name:
*     MSG1_FORM

*  Purpose:
*     Form a message from its text and components.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL MSG1_FORM( PARAM, TEXT, CLEAN, MSGSTR, MSGLEN, STATUS )

*  Description:
*     Construct the final text, MSGSTR( 1 : MSGLEN ), of a message
*     using the text in TEXT, and available message tokens, keywords and 
*     parameter-object associations. 
*     Clean any non-printing characters from the final string if CLEAN
*     is TRUE.

*  Arguments:
*     PARAM = CHARACTER * ( * ) (Given)
*        The message parameter name.
*     TEXT = CHARACTER * ( * ) (Given)
*        The input message text, with any tokens.
*     CLEAN = LOGICAL (Given)
*        If the string is to be 'cleaned'
*     MSGSTR = CHARACTER * ( * ) (Returned)
*        Resultant message text, with parsed tokens.
*     MSGLEN = INTEGER (Returned)
*        The filled length of MSGSTR.
*     STATUS = INTEGER (Given)
*        The global status.

*  Implementation Notes:
*     -  This subroutine is the ADAM version of MSG1_FORM.

*  Algorithm:
*     -  Attempt to get a message text from the parameter system,
*     otherwise use TEXT. 
*     -  Parse the message text and copy it into MSGSTR, making 
*     translations for parameter, status and token escapes.
*     -  The message length is returned in MSGLEN.

*  Copyright:
*     Copyright (C) 1982, 1984, 1985, 1989, 1990, 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1999, 2001 Central Laboratory of the Research Councils.
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
*     Foundation, Inc., 59 Temple Place,Suite 330, Boston, MA
*     02111-1307, USA

*  Authors:
*     JRG: Jack Giddings (UCL)
*     BDK: Dennis Kelly (ROE)
*     AJC: Alan Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     BKM: B.K.McIlwrath (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1982 (JRG):
*        Original version.
*     13-NOV-1984 (BDK):
*        ADAM version.
*     11-JUN-1985 (BDK):
*        Discard leading % from error message.
*     5-JUN-1989 (AJC):
*        Check whole string for equality with escape.
*     13-SEP_1989 (PCTR):
*        Converted to new prologue and layout.
*     13-MAR-1990 (PCTR):
*        Converted to use EMS_ calls and changed subroutine name.
*     9-APR-1990 (PCTR):
*        Replaced DO WHILE construct with ANSI Fortran 77 equivalent.
*     9-OCT-1991 (PCTR):
*        New token parsing algorithm.
*     24-OCT-1991 (PCTR):
*        Removed bugs from new token parsing algorithm.
*     12-AUG-1994 (BKM):
*        Change reserved token STATUS to call EMS_FACER (from EMS_SYSER)
*     15-SEP-1999 (AJC):
*        Add argument CLEAN
*     21-FEB-2001 (AJC):
*        Remove use of internal EMS routines
*        Use EMS_EXPND instead of EMS1_GTOK
*        EMS1_PUTC is now MSG1_PUTC
*        Use MSG1_KTOK not EMS1_KTOK
*     19-OCT-2001 (AJC):
*        Correct finding ^STATUS if previous token in message
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE                     ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'                 ! Standard SAE constants
      INCLUDE 'EMS_PAR'                 ! EMS_ public constants
      INCLUDE 'MSG_PAR'                 ! MSG_ public constants
      INCLUDE 'MSG_SYS'                 ! MSG_ private constants

*  Arguments Given:
      CHARACTER * ( * ) PARAM
      CHARACTER * ( * ) TEXT
      LOGICAL CLEAN

*  Arguments Returned:
      CHARACTER * ( * ) MSGSTR

      INTEGER MSGLEN

*  Status:
      INTEGER STATUS

*  External References:
      LOGICAL MSG1_GENV                 ! Get the message text
      LOGICAL MSG1_GKEY                 ! Get the parameter keyword
      LOGICAL MSG1_GREF                 ! Get the parameter reference

      LOGICAL CHR_SIMLR                 ! Caseless string compariso
      INTEGER CHR_LEN                   ! Character string length

*  Local Variables:
      LOGICAL FOUND                     ! ^STATUS found
      LOGICAL DEFINE                    ! Whether a token is defined
      LOGICAL DOUBLE                    ! Whether a double escape
      LOGICAL LITERL                    ! Whether a literal token escape

      INTEGER ITOK                      ! Position of STATUS token
      INTEGER STMLEN                    ! Length of STATUS message
      INTEGER CURPOS                    ! Current character position
      INTEGER LSTAT                     ! Local status
      INTEGER LSTPOS                    ! Previous value of CURPOS
      INTEGER NAMLEN                    ! Name string length
      INTEGER PSTAT                     ! Local status
      INTEGER TEXLEN                    ! Message text length
      INTEGER TKVLEN                    ! Token value length

      CHARACTER * 1 ESCAPE              ! Current escape character
      CHARACTER * ( 2 ) ESCSTR          ! Escape string
      CHARACTER * ( MSG__SZMSG ) NAMSTR ! Token name string
      CHARACTER * ( MSG__SZTOK ) STMSG  ! STATUS message
      CHARACTER * 1 PREVEC              ! Previous escape character
      CHARACTER * ( MSG__SZMSG ) TEXST1 ! Message text string
      CHARACTER * ( MSG__SZMSG ) TEXST2 ! Part expanded text string
      CHARACTER * ( MSG__SZTOK ) TOKVAL ! Token value string

*.

*  Operate regardless of STATUS - use local status
      LSTAT = SAI__OK

*  Select the message text from the interface module or program.
      IF ( .NOT. MSG1_GENV( PARAM, TEXST1, TEXLEN ) ) THEN
         TEXST1 = TEXT
         TEXLEN = CHR_LEN( TEXST1 )
      END IF

*  Initialise the returned message.

      MSGSTR = ' '
      MSGLEN = 1

*  Check for an empty string.
      IF ( TEXLEN .GT. 0 ) THEN

*     Check for the STATUS token in text
*     If it's there, substitute it here and now.
*     We must do this to avoid concatenation if ^STATUS is used after RENEW.
*     However, it means that ^STATUS is now truly reserved and cannot be
*     preset for concatenation.
*     We assume there is not more than one occurrence of ^STATUS within TEXT
         CURPOS = 1
         FOUND = .FALSE.

*     Find a token
         ITOK = INDEX( TEXST1(1:TEXLEN), '^' )
*        DOWHILE loop
 10      CONTINUE

         IF ( .NOT. FOUND
     :        .AND. ( ITOK .GT. 0 )
     :        .AND. ( ITOK .LT. TEXLEN ) ) THEN

*     Is it STATUS
            NAMSTR = TEXST1(ITOK+1:ITOK+6)            
            IF( CHR_SIMLR( NAMSTR, 'STATUS' ) ) THEN
*       It is STATUS
*       Find the associated message
               CALL EMS_MARK
               CALL EMS_FACER( 'STATUS', STATUS )
               CALL EMS_EXPND( '^STATUS', STMSG, TKVLEN, LSTAT )

*       Save the remainder of TEXST1
               IF ( TEXLEN .GT. ITOK + 7 ) 
     :           TEXST2 = TEXST1(ITOK+7:TEXLEN)

*       Copy the message to the ^STATUS position
               CURPOS = ITOK - 1
               CALL MSG1_PUTC( STMSG(1:TKVLEN), TEXST1, CURPOS, LSTAT )
*       and add any remaining part of message
               IF ( TEXLEN .GT. ITOK + 7 ) 
     :           CALL MSG1_PUTC(
     :             TEXST2(1:TEXLEN-(ITOK+7)+1), TEXST1, CURPOS, LSTAT )
               TEXLEN = CURPOS
               CALL EMS_RLSE
*     We assume there's only one ^STATUS
               FOUND = .TRUE.

            END IF

            LSTPOS = ITOK
            ITOK = INDEX( TEXST1(LSTPOS+1:TEXLEN), '^' )
            IF ( ITOK .GT. 0 ) ITOK = LSTPOS + ITOK

*     Continue loop
            GO TO 10
         END IF

*     Make a first pass to expand plain ^ tokens
*     This will also kill tokens
         CALL EMS_EXPND(
     :     TEXST1(1:TEXLEN), TEXST2, TEXLEN, LSTAT )

*     Initialise the returned message length.
         MSGLEN = 0

*     Initialise token escape state flag.
         LITERL = .FALSE.

*     Initialise the text pointers and local status.
         CURPOS = 0
         LSTPOS = 0
         LSTAT = SAI__OK
         PSTAT = SAI__OK

*     Initialise the escape string and previous escape character.
         ESCSTR = MSG__KEYEC // MSG__REFEC
         PREVEC = ' '
         ESCAPE = ' '

*     Parse and translate the returned message text.
*     DO WHILE loop.
 20      CONTINUE
         IF ( PSTAT .EQ. SAI__OK .AND. CURPOS .LT. TEXLEN ) THEN 

*        Check if a double or paired escape character sequence has 
*        occurred.
            IF ( ESCAPE .EQ. PREVEC ) THEN
               DOUBLE = .TRUE.
            ELSE
               DOUBLE = .FALSE.
            END IF

         ELSE

*        No paired token escapes, so annul the DOUBLE flag
            DOUBLE = .FALSE.

         END IF

*     Find the next occurrence of an escape character.
         CALL MSG1_GESC( ESCSTR, TEXST2( 1 : TEXLEN ), CURPOS )

*     Append any text prior to the escape character to the returned 
*     string.
         IF ( CURPOS .EQ. 0 ) THEN

*        No more escape characters have been found, so append all 
*        the text that remains to the returned message text and exit 
*        the loop.
            CALL MSG1_PUTC( TEXST2( LSTPOS+1 : TEXLEN ), MSGSTR, 
     :                      MSGLEN, PSTAT )
            GO TO 30

         ELSE
*        A token escape has been found, so get which escape character 
*        has occurred.
            ESCAPE = TEXST2( CURPOS : CURPOS )

*        Check if it is a double token escape.
            IF ( LITERL .AND. CURPOS .EQ. LSTPOS+1 ) THEN

*           Check if a double escape character sequence has occurred.
               IF ( ESCAPE .EQ. PREVEC ) THEN
                  DOUBLE = .TRUE.
               ELSE
                  DOUBLE = .FALSE.
               END IF
            ELSE

*           No paired token escapes, so annul the DOUBLE flag
               DOUBLE = .FALSE.
            END IF

*           Act.
            IF ( DOUBLE ) THEN

*           A double token escape, so do nothing except update
*              CURPOS and reset the literal token escape flag.
               LITERL = .FALSE.

*           Assign a "null" ESCAPE.
               ESCAPE = ' '

            ELSE
*           Reset the literal token escape flag.
               LITERL = .FALSE.

*           Append any text prior to the escape character.
               IF ( CURPOS .GT. 1 ) CALL MSG1_PUTC( 
     :                                 TEXST2( LSTPOS+1 : CURPOS-1 ), 
     :                                 MSGSTR, MSGLEN, PSTAT )

*           Find the token name.
               CALL MSG1_GNAM( TEXST2( 1 : TEXLEN ), CURPOS, NAMSTR,
     :                            NAMLEN, LSTAT )

*           Check that a token name exists.
               IF ( LSTAT .NE. SAI__OK ) THEN

*              The name string has been over-run, so indicate this 
*              in the message text.
                  CALL MSG1_PUTC( ESCAPE, MSGSTR, MSGLEN, PSTAT )
                  CALL MSG1_PUTC( '<', MSGSTR, MSGLEN, PSTAT )
                  CALL MSG1_PUTC( NAMSTR( 1 : NAMLEN ), MSGSTR, 
     :                               MSGLEN, PSTAT )
                  CALL MSG1_PUTC( '>', MSGSTR, MSGLEN, PSTAT )

*              Reset the local status.
                  LSTAT = SAI__OK

               ELSE IF ( NAMLEN .GT. 0 ) THEN

                  IF ( ESCAPE .EQ. MSG__REFEC ) THEN

*                 Get a value for the object name token.
                     DEFINE = MSG1_GREF( NAMSTR( 1 : NAMLEN ), 
     :                                      TOKVAL, TKVLEN )

                  ELSE IF ( ESCAPE .EQ. MSG__KEYEC ) THEN

*                 Get a value for the keyword token.
                     DEFINE = MSG1_GKEY( NAMSTR( 1 : NAMLEN ), 
     :                                      TOKVAL, TKVLEN )
                  END IF

*              Check if the token value was defined.
                  IF ( DEFINE ) THEN

*                 Append the token value to the returned string.
                     CALL MSG1_PUTC( TOKVAL( 1 : TKVLEN ), MSGSTR, 
     :                                  MSGLEN, PSTAT )
                  ELSE

*                 A value has not been found, so append the 
*                 "undefined" token string.
                     CALL MSG1_PUTC( ESCAPE, MSGSTR, MSGLEN, PSTAT )
                     CALL MSG1_PUTC( '<', MSGSTR, MSGLEN, PSTAT )
                     CALL MSG1_PUTC( NAMSTR( 1 : NAMLEN ), MSGSTR, 
     :                                  MSGLEN, PSTAT )
                     CALL MSG1_PUTC( '>', MSGSTR, MSGLEN, PSTAT )
                  END IF

*              Assign a "null" ESCAPE.
                  ESCAPE = ' '

               ELSE
*              There is an isolated token escape character in the 
*              string, so set the literal token escape flag.
                  LITERL = .TRUE.
                  CALL MSG1_PUTC( ESCAPE, MSGSTR, MSGLEN, PSTAT )

               END IF
            END IF

*        Update LSTPOS and PREVEC.
            LSTPOS = CURPOS
            PREVEC = ESCAPE
            GO TO 20
         END IF

 30   CONTINUE
      END IF

*  Clean the returned message string unless in STREAM mode.
      IF ( CLEAN ) CALL CHR_CLEAN( MSGSTR( 1 : MSGLEN ) )

*  Clear the message token table.
!      CALL MSG1_KTOK
 
      END
