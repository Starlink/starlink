      SUBROUTINE PARSECON_NEWPAR ( NAME, STATUS )
*+
*  Name:
*     PARSECON_NEWPAR

*  Purpose:
*     Create new parameter-list entry.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_NEWPAR ( NAME, STATUS )

*  Description:
*     Adds a new name to the parameter-list.

*  Arguments:
*     NAME=CHARACTER*(*) (given)
*        parameter-name to be added to list
*     STATUS=INTEGER

*  Algorithm:
*     The parameter-list is searched to ensure that the name hasn't been
*     declared as a parameter already. Then the pointer to the parameter-list
*     is incremented, and the name is copied into the new location,
*     along with the length of the name. The parameter is then marked
*     for write access, and its prompt-string and keyword blanked.
*     It is also marked as not a LITERAL parameter.
*     Save the name in the common block for possible error reporting

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
*     11.11.1985:  set the element of PARLIT to .FALSE. (REVAD::BDK)
*     18.06.1987:  check for overlength names (REVAD::BDK)
*     10.11.1987:  accept character constants (REVAD::BDK)
*     04.07.1990:  add PARHKEY (RLVAD::AJC)
*     09.07.1990:  initialize PARDYN (RLVAD::AJC)
*     09.08.1990:  initialise PARTYPE to NOTYPE (RLVAD::AJC)
*     16.08.1990:  save name for possible error reporting (RLVAD::AJC)
*     16.10.1990:  define QUOTE portably
*        use CHR not VAX specifics (RLVAD::AJC)
*     24.02.1991:  Report errors (RLVAD::AJC)
*     26.02.1992:  Assume NAME already ucase unless quoted string (RLVAD::AJC)
*     19.08.1992:  Initialize PARDYN(1,-) AND PARDYN(3,-) (RLVAD::AJC)
*     19.11.1992:  Initialize PARMIN/MAX (RLVAD::AJC)
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
      INCLUDE 'SUBPAR_PAR'


*  Arguments Given:
      CHARACTER*(*) NAME             ! name of action to be
                                     ! added to list.


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
      CHARACTER*30 STRING            ! processed name
      INTEGER NAMLEN                 ! length of name

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   Check that the parameter-list is not full
      IF ( PARPTR .LT. SUBPAR__MAXPAR ) THEN

*      Remove quotes if necessary
         IF ( NAME(1:1) .EQ. QUOTE ) THEN
            CALL STRING_STRIPQUOT ( NAME, STRING, STATUS )
            CALL CHR_UCASE ( STRING )
         ELSE
            STRING = NAME
         ENDIF

*      Trap overlength name.
         NAMLEN = CHR_LEN( STRING )
         IF ( NAMLEN .GT. SUBPAR__NAMELEN) THEN
            STATUS = PARSE__PARLEN
            CALL EMS_SETI ( 'MXLN', SUBPAR__NAMELEN )
            CALL EMS_REP ( 'PCN_NEWPAR1',
     :      'PARSECON: Parameter names must be '//
     :      'less than ^MXLN characters', STATUS )

         ELSE IF ( STATUS .EQ. SAI__OK ) THEN

*        search parameter-list for name - error if found
*        Probably want to annul errors from PARSECON_FINDACT
            CALL EMS_MARK
            CALL PARSECON_FINDPAR ( STRING(1:NAMLEN), NAMCOD, STATUS )

            IF ( NAMCOD .NE. 0 ) THEN

                STATUS = PARSE__OLDPAR
                CALL EMS_SETC( 'PAR', STRING(1:NAMLEN) )
                CALL EMS_REP ( 'PCN_NEWPAR2',
     :             'PARSECON: Parameter name "^PAR" already defined',
     :             STATUS )

            ELSE
*
*            Not there, so add it to the list.
               CALL EMS_ANNUL ( STATUS )
*
*            Store the name
*
               PARPTR = PARPTR + 1
               PARNAMES(PARPTR) = STRING(1:NAMLEN)
               PARLEN(PARPTR) = NAMLEN
*
*            Mark the parameter for write-access ( the default )
*
               PARWRITE(PARPTR) = .TRUE.
*
*            Set the default keyword
*
               PARKEY(PARPTR) = STRING(1:NAMLEN)
*
*            Set the default type
*
               PARTYPE(PARPTR) = SUBPAR__NOTYPE
*
*            Blank character strings
*
               PARPROM(PARPTR) = ' '
               PARPTY(PARPTR) = ' '
               PARHELP(PARPTR) = ' '
               PARHKEY(PARPTR) = ' '
*
*            Initialise the flag for LITERAL parameters to false.
*
               PARLIT(PARPTR) = .FALSE.
*
*            Initialize the dynamic default pointer array to indicate
*            no dynamic default space is allocated.
*
               PARDYN(1,PARPTR) = 0
               PARDYN(3,PARPTR) = -1
*
*            Initialize the PARMIN?MAX pointer arrays to indicate
*            no MIN/MAX values set
*
               PARMIN(1,PARPTR) = 0
               PARMIN(2,PARPTR) = -1
               PARMAX(1,PARPTR) = 0
               PARMAX(2,PARPTR) = -1
*
*            Update pointer to end of parameter store for
*            current program.
*
               IF ( MONOLITH ) THEN
                  PROGADD(2,ACTPTR) = PARPTR
               ELSE
                  PROGADD(2,1) = PARPTR
               ENDIF
*
*            Save the name in the error reporting common block
               PRNAME = STRING(1:NAMLEN)

            ENDIF

*         Release the error context
            CALL EMS_RLSE

         ENDIF

      ELSE

         STATUS = PARSE__NOMEM
         CALL EMS_SETI ( 'MAXPAR', SUBPAR__MAXPAR )
         CALL EMS_REP ( 'PCN_NEWPAR3',
     :   'PARSECON: Too many parameters defined (max is ^MAXPAR)',
     :    STATUS )


      ENDIF

      END
