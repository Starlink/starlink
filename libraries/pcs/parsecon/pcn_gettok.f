      SUBROUTINE PARSECON_GETTOK ( LU, TOKEN, TOKLEN, STATUS )
*+
*  Name:
*     PARSECON_GETTOK

*  Purpose:
*     Return next token from interface file.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_GETTOK ( LU, TOKEN, TOKLEN, STATUS )

*  Description:
*     Return next token from connection file on given logical unit.

*  Arguments:
*     LU=INTEGER (given)
*        logical unit of connection file
*     TOKEN=CHARACTER*(*) (returned)
*        token
*     TOKLEN=INTEGER (returned)
*        no of chars in token, zero on error, -1 on EOL, -2 on EOF
*     INTEGER STATUS

*  Algorithm:
*     Tokens are returned one at a time from an internal array. When all
*     the tokens in the array have been returned, then a new record is
*     read from the interface file and it is split into tokens and
*     loaded into the storage array, and a condition (TOKLEN=-1) is
*     returned informing the calling routine that a record has been
*     read. On end-of-file TOKLEN=-2 is returned and status set to
*     PARSE__READERR as it shouldn't happen.

*     A token is a sequence of characters which are either all alphanumeric
*     (with {.+-()_[]"'<>/~} being honorary alphanumerics) or are all
*     non-alphanumeric (ie are anything other than alphanumeric and "white").
*     Tokens are thus terminated by either a character of the opposite class
*     or by a "white" character. For the purposes of this routine, a white
*     character is a space, tab, newline, comma or non-printable. Apart from
*     their role as delimiters, white characters are never significant. The
*     exception to this rule is that, as a special case, a token may consist
*     of a set of characters enclosed in single quotes. The first quote must
*     be the first character of the token and the token is terminated by the
*     next isolated (ie.not '') quote or end of buffer (whichever comes
*     first).
*     Hexadecimal constants are recognised and converted to decimal strings.
*     All alphabetic characters other than those in quotes are converted to
*     upper case. All characters on a line that follow a # token are ignored,
*     AS IS THE #.

*  Copyright:
*     Copyright (C) 1984, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     18.09.1984:  VAX version (REVAD::BDK)
*     11.10.1984:  rewritten around STRING_ARRCHAR (REVAD::BDK)
*     04.07.1990:  remove BUFFER, BUFFLEN to common (RLVAD::AJC)
*     29.08.1990:  include SUBPAR_CMN for NAMELEN (RLVAD::AJC)
*     19.09.1990:  set status on End_of_File (RLVAD::AJC)
*     25.09.1990:  set count = done on bad line to force another
*        read if re-entered (RLVAD::AJC)
*     25.06.1991:  STRING_ARRCHAR changed to PARSECON_* (RLVAD::AJC)
*     24.02.1991:  Report errors
*        comment that / now alpha (RLVAD::AJC)
*     26.02.1992:  comment that ~ now alpha (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'DAT_PAR'
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON_ERR'
      INCLUDE 'PARSECON3_CMN'


*  Arguments Given:
      INTEGER LU                 ! logical unit of connection file


*  Arguments Returned:
      CHARACTER*(*) TOKEN        ! token

      INTEGER TOKLEN             ! no of chars in token,
                                 ! zero => error,
                                 ! -1 => end-of-recod
                                 ! -2 => EOF


*  Status:
      INTEGER STATUS


*  External References:
      INTEGER CHR_LEN                       ! used length of string
      EXTERNAL CHR_LEN


*  Local Constants:
      INTEGER MAXVALS
      PARAMETER ( MAXVALS = 32 )


*  Local Variables:
      CHARACTER*(PARSE__BUFSIZ) CARRAY(MAXVALS)  ! array holding tokens

      INTEGER CLENGTHS(MAXVALS)             ! lengths of tokens

      INTEGER COUNT                         ! number of tokens

      INTEGER DONE                          ! number of tokens returned
                                            ! to calling routine so far

      INTEGER INSTAT                        ! status from i/o

      SAVE CARRAY, CLENGTHS, COUNT, DONE


*  Local Data:
      DATA COUNT / 0 /
      DATA DONE / 0 /

*.


      IF ( STATUS .NE. SAI__OK ) RETURN


      IF ( DONE .LT. COUNT ) THEN

*      Have tokens waiting to be returned
         DONE = DONE + 1
         TOKEN = CARRAY(DONE)
         TOKLEN = CLENGTHS(DONE)

      ELSE
*      No tokens waiting to be returned, read in a new buffer and split
*      it into tokens.
         INSTAT = 0
         READ ( UNIT=LU, FMT='(A)', IOSTAT=INSTAT ) BUFFER

         IF ( INSTAT .LT. 0 ) THEN
*          Flag End-of-file
            TOKLEN = -2
            STATUS = PARSE__READERR
            CALL EMS_FIOER ( 'IOSTAT', INSTAT )
            CALL EMS_REP ( 'PCN_GETTOK1',
     :      'PARSECON: Premature end of file', STATUS )

         ELSE IF ( INSTAT .GT. 0 ) THEN
*         Read error
            TOKLEN = 0
            STATUS = PARSE__READERR
            CALL EMS_FIOER ( 'IOSTAT', INSTAT )
            CALL EMS_REP ( 'PCN_GETTOK2',
     :      'PARSECON: ^IOSTAT', STATUS )

         ELSE
*         Trim trailing blanks
            BUFFLEN = CHR_LEN( BUFFER )

            IF ( BUFFLEN .GT. 0 ) THEN
*            Split the string into an array of tokens
               CALL PARSECON_ARRCHAR ( BUFFER(1:BUFFLEN), MAXVALS,
     :           COUNT, CARRAY, CLENGTHS, STATUS )

*            If tokens have been found OK, reset the DONE counter,
*            otherwise force a new record to be read if the routine
*            is re-enterd.
               IF ( STATUS .EQ. SAI__OK ) THEN
                  DONE = 0
               ELSE
                  DONE = COUNT
               ENDIF

            ENDIF

*         Flag end-of-record
            TOKLEN = -1

         ENDIF

      ENDIF

      END
