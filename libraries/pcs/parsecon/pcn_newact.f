      SUBROUTINE PARSECON_NEWACT ( NAME, STATUS )
*+
*  Name:
*     PARSECON_NEWACT

*  Purpose:
*     Create new action-list entry.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_NEWACT ( NAME, STATUS )

*  Description:
*     Adds a new name to the action-list.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*        action-name to be added to list
*     STATUS=INTEGER

*  Algorithm:
*     The action-list is searched to ensure that the name hasn't been
*     declared as an action already. Then the pointer to the action-list
*     is incremented, and the name is copied into the new location,
*     along with the length of the name. Also initialize the keyword for
*     the action to be the same as the name.
*     If the action is actually a program within a monolith, set up the
*     pointers to the parameter store for the program.
*     Save the action name in the common block for error reporting

*  Copyright:
*     Copyright (C) 1984, 1985, 1987, 1990, 1991, 1992, 1993 Science & Engineering Research Council.
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
*     B.D.Kelly (REVAD::BDK)
*     A J Chipperfield (STARLINK)
*     {enter_new_authors_here}

*  History:
*     13.09.1984:  Original (REVAD::BDK)
*     23.08.1985:  handle monoliths (REVAD::BDK)
*     26.05.1987:  initialize keyword (REVAD::BDK)
*     18.06.1987:  check for overlong names (REVAD::BDK)
*     10.11.1987:  accept character constants (REVAD::BDK)
*     16.08.1990:  save name for possible error reporting (RLVAD::AJC)
*     16.10.1990:  use CHR for conversion
*        define QUOTE portably (RLVAD::AJC)
*     24.02.1991:  Report errors (RLVAD::AJC)
*     26.02.1992:  Assume NAME already ucase unless quoted string (RLVAD::AJC)
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
      INCLUDE 'PARSECON_ERR'


*  Arguments Given:
      CHARACTER*(*) NAME             ! name of action to be
                                     ! added to lists.


*  Status:
      INTEGER STATUS


*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'
      INCLUDE 'PARSECON3_CMN'


*  Local Constants:
      CHARACTER QUOTE
      PARAMETER ( QUOTE = '''' )


*  Local Variables:
      INTEGER NAMCOD
      CHARACTER*30 STRING          ! processed name
      INTEGER NAMLEN              ! length of name

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check that the action-list is not full
      IF ( ACTPTR .LT. SUBPAR__MAXACT ) THEN

*      Remove quotes if necessary
         IF ( NAME(1:1) .EQ. QUOTE ) THEN
            CALL STRING_STRIPQUOT ( NAME, STRING, STATUS )
            CALL CHR_UCASE ( STRING )
         ELSE
            STRING = NAME
         ENDIF

*      Trap overlength names
         NAMLEN = CHR_LEN( STRING )
         IF ( NAMLEN .GT. SUBPAR__NAMELEN ) THEN
            STATUS = PARSE__ACTLEN
            CALL EMS_SETI ( 'MXLN', SUBPAR__NAMELEN )
            CALL EMS_REP ( 'PCN_NEWACT1',
     :      'PARSECON: Action names must be '//
     :      'less than ^MXLN characters', STATUS )

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*        search action-list for name - error if found
*        Probably want to annul errors from PARSECON_FINDACT
            CALL EMS_MARK
            CALL PARSECON_FINDACT ( STRING(1:NAMLEN), NAMCOD, STATUS )

            IF ( STATUS .EQ. SAI__OK ) THEN
*           Action already there - error
                STATUS = PARSE__OLDACT
                CALL EMS_REP ( 'PCN_NEWACT2',
     :          'PARSECON: Action name already defined', STATUS )

            ELSE
*           Not there, so add it to the list.
               CALL EMS_ANNUL ( STATUS )

               ACTPTR = ACTPTR + 1
               ACTNAMES(ACTPTR) = STRING(1:NAMLEN)
               ACTLEN(ACTPTR) = NAMLEN
               ACTKEY(ACTPTR) = STRING(1:NAMLEN)
               IF ( MONOLITH ) THEN
                  PROGADD(1,ACTPTR) = PARPTR + 1
               ELSE
                  PROGADD(1,ACTPTR) = 1
               ENDIF

*            Save name in error report common block
               ACNAME = STRING(1:NAMLEN)

            ENDIF

*         Release the error context
            CALL EMS_RLSE

         ENDIF

      ELSE

         STATUS = PARSE__NOMEM
         CALL EMS_SETI ( 'MAXACT', SUBPAR__MAXACT )
         CALL EMS_REP ( 'PCN_NEWACT3',
     :   'PARSECON: Too many actions defined (max is ^MAXACT)',
     :    STATUS )

      ENDIF

      END
