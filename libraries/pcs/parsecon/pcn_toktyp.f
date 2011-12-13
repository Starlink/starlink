      SUBROUTINE PARSECON_TOKTYP ( TOKEN, UTOKN, TOKTYP, STATUS )
*+
*  Name:
*     PARSECON_TOKTYP

*  Purpose:
*     Determine type of a token from an interface file.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_TOKTYP ( TOKEN, UTOKN, TOKTYP, STATUS )

*  Description:
*     Determine the type of a token, for use in parsing an interface
*     file phrase.

*  Arguments:
*     TOKEN=CHARACTER*(*) (given)
*        token, guaranteed to have no trailing spaces.
*        All alpha chars except in char constants are in upper case
*     UTOKN=CHARACTER*(*) (returned)
*        the token converted to upper case except in the case of
*        quoted strings, when a copy of the given token is returned
*     TOKTYP=INTEGER (returned)
*        token type.
*     STATUS=INTEGER

*  Algorithm:
*     The token-type may be one of a set of reserved words, or a
*     character or numeric constant, or a name. Anything not fitting
*     into one of these categories is classified as OTHER.
*     The token is checked for the following conditions in turn
*     and TOKTYP set accordingly to one of a set of symbolic constants
*     defined in PARSECON_CMN.
*      1. The token is one of the set of reserved words.
*         (TOKTYP set to IFACE, EFACE etc.)
*      2. The first character of the token is a quote.
*         (TOKTYP set to CONST)
*      3. The token is "!"
*         (TOKTYP set to CONST)
*      4. The token can be interpreted (by an internal READ) as a
*         number (TOKTYP set to CONST)
*      5. Anything else (TOKTYP set to NAME)

*  Copyright:
*     Copyright (C) 1984, 1985, 1986, 1987, 1990, 1992 Science & Engineering Research Council.
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
*     21.09.1984:  VAX version (REVAD::BDK)
*     08.10.1984:  Optimised character checking (REVAD::BDK)
*     23.08.1985:  add MONOLITH, ENDMONOLITH (REVAD::BDK)
*     13.05.1986:  add MENU, MENUCOORDS (REVAD::BDK)
*     05.05.1987:  allow ! to be const (REVAD::BDK)
*     05.05.1987:  add PPATH (REVAD::BDK)
*     15.05.1990:  add HELPKEY (RLVAD::AJC)
*     04.07.1990:  add HELPLIB (RLVAD::AJC)
*     16.10.1990:  tidy unused declarations
*        change internal READ to CHR_CTOD (RLVAD::AJC)
*        improve comments (RLVAD::AJC)
*     26.02.1992:  expect uncapitalised token - return capitalised version
*        except for quoted string (RLVAD::AJC)
*     11.09.1992:  Trap D and E else they may be typed as numbers (RLVAD::AJC)
*     11.12.1992:  Trap anything starting with a letter as NAME - this
*        cures problem on Sun (RLVAD::AJC)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-


*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      CHARACTER*(*) TOKEN             ! token, guaranteed to have no
                                      ! trailing spaces.


*  Arguments Returned:
      CHARACTER*(*) UTOKN             ! The given token converted to upper
                                      ! case except for character constants

      INTEGER TOKTYP                  ! token type.


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'PARSECON_CMN'


*  External References:
      LOGICAL CHR_ISALF
      EXTERNAL CHR_ISALF


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

      INTEGER RESNUM
      PARAMETER ( RESNUM = 35 )


*  Local Variables:
      INTEGER RESMAP(RESNUM)
      INTEGER RESMIN(RESNUM)
      INTEGER TOKLEN
      INTEGER ISTAT
      INTEGER I
      DOUBLE PRECISION D
      CHARACTER*12 RESERVED(RESNUM)


*  Local Data:
      DATA RESERVED / 'INTERFACE', 'ENDINTERFACE', 'PARAMETER',
     :  'ENDPARAMETER', 'ACTION', 'ENDACTION', 'OBEY', 'ENDOBEY',
     :  'CANCEL', 'ENDCANCEL', 'RANGE', 'IN', 'DEFAULT', 'TYPE',
     :  'NEEDS', 'KEYWORD', 'POSITION', 'ACCESS', 'VPATH', 'HELP',
     :  'PTYPE', 'ASSOCIATION', 'PROGRAM', 'EPATH', 'MESSAGE', 'TEXT',
     :  'ENDMESSAGE', 'PROMPT', 'MONOLITH', 'ENDMONOLITH', 'MENU',
     :  'MENUCOORDS', 'PPATH', 'HELPKEY', 'HELPLIB' /

      DATA RESMAP / IFACE, EFACE, PARAM, EPARAM, ACTION,
     :  ENDACT, OBEY, ENDOBEY, CANCEL, ENDCANC,
     :  RANGE, IN, DEFAULT, TYPE, NEEDS,
     :  KEYWORD, POSITION,
     :  ACCESS, VPATH, HELP, PTYPE, ASSOC,
     :  PROGRAM, EPATH, MESSAGE, TEXT, EMESS, PROMPT, MONOL, EMONOL,
     :  MENU, COORDS, PPATH, HELPKEY, HELPLIB /

      DATA RESMIN / 9, 12, 9, 12, 6, 9, 4, 7, 6, 9, 5, 2, 7, 4, 5, 7, 8,
     :  6, 5, 4, 5, 11, 7, 5, 7, 4, 10, 6, 8, 11, 4, 10, 5, 7, 7 /


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Assume the used length is passed
      TOKLEN = LEN (TOKEN)

*   Take a copy of the given token.
      UTOKN = TOKEN

*   If in quotes (trailing one is optional if was last thing on record)
*   CONST.
      IF ( TOKEN(1:1) .EQ. QUOTE ) THEN

         TOKTYP = CONST

*   Treat a single ! as a constant
      ELSE IF ( TOKEN .EQ. '!' ) THEN

         TOKTYP = CONST

*   Check for reserved words or numeric constant - name by default
      ELSE
         CALL CHR_UCASE ( UTOKN )
         DO I = 1, RESNUM
            IF ( TOKLEN .EQ. RESMIN(I) ) THEN
               IF ( UTOKN .EQ. RESERVED(I) ) THEN
                  TOKTYP = RESMAP(I)
                  GO TO 10
               ENDIF
            ENDIF
         ENDDO

*     It's not a reserved word

*      Check for first character is a letter
         IF ( CHR_ISALF( TOKEN(1:1) ) ) THEN

            TOKTYP = NAME

*     otherwise if can convert to DOUBLE it's a CONST, else NAME
         ELSE
            ISTAT = SAI__OK
            CALL CHR_CTOD( TOKEN, D, ISTAT )
            IF ( ISTAT .EQ. SAI__OK ) THEN
               TOKTYP = CONST
            ELSE
               TOKTYP = NAME
            ENDIF

         ENDIF

      ENDIF

10    CONTINUE

      END
