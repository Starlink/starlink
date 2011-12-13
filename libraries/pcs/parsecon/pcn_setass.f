      SUBROUTINE PARSECON_SETASS ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETASS

*  Purpose:
*     Sets-up an association for a parameter.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETASS ( ENTRY, STATUS )

*  Description:
*     Interprets whether the association is to be read, write, or
*     update, and loads the corresponding indicator into the store for
*     the most recently declared program parameter, along with the name
*     of the global association.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        string specifying a global association
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, and the
*     result is tested for access-type and an indicator stored. The name
*     of the associated global is is concatenated with the logical name
*     for the subdirectory containing the global container file, and put
*     into general string storage. A pointer to it is stored with the
*     parameter.

*  Copyright:
*     Copyright (C) 1984, 1985, 1990, 1992, 1993 Science & Engineering Research Council.
*     Copyright (C) 2004 Central Laboratory of the Research Councils.
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
*     19.09.1984:  Original (REVAD::BDK)
*     13.05.1985:  force string to uppercase (REVAD::BDK)
*     16.10.1990:  use CHR for upper case converson
*        define QUOTE as character constant (RLVAD::AJC)
*     25.02.1992:  Report errors (RLVAD::AJC)
*     27.02.1992:  Assume entry is ucase unless quoted string (RLVAD::AJC)
*     24.03.1993:  Add DAT_PAR for SUBPAR_CMN
*     01.02.2004:  Checked into CVS repository cvs.starlink.ac.uk.  For any
*        further changes, see the repository.
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
      CHARACTER*(*) ENTRY             ! the keyword string


*  Status:
      INTEGER STATUS


*  External References:
      INTEGER STRING_INANYL
      EXTERNAL STRING_INANYL


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )


*  Local Variables:
      CHARACTER*132 VALUE
      INTEGER NAMESTART


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   If ENTRY is a quoted string, process the quotes
*   and convert to upper cae
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

      ELSE
         VALUE = ENTRY

      ENDIF

*   Find the access specifier
      NAMESTART = STRING_INANYL ( VALUE, '<->' )

      IF ( NAMESTART .GT. 2 ) THEN

*     Interpret the access-mode
         IF ( VALUE(1:NAMESTART-1) .EQ. '<->' ) THEN
            PARASSOC(2,PARPTR) = SUBPAR__UPDATE
         ELSE IF ( VALUE(1:NAMESTART-1) .EQ. '->' ) THEN
            PARASSOC(2,PARPTR) = SUBPAR__WRITE
         ELSE IF ( VALUE(1:NAMESTART-1) .EQ. '<-' ) THEN
            PARASSOC(2,PARPTR) = SUBPAR__READ
         ELSE
            STATUS = PARSE__GLOBACC
            CALL EMS_REP ( 'PCN_SETASS1',
     :      'PARSECON: Incorrect "ASSOCIATION" access mode (<->)',
     :       STATUS )
         ENDIF

*   Store the name prefixed by the logical name for the directory
*   containing the global structure, and put a pointer to it into the
*   parameters' list.
         IF ( STATUS .EQ. SAI__OK ) THEN

            IF ( CHARPTR .LT. SUBPAR__MAXLIMS ) THEN

               CHARPTR = CHARPTR + 1
               CHARLIST(CHARPTR) = 'ADAM_USER:' // VALUE(NAMESTART:)
               PARASSOC(1,PARPTR) = CHARPTR

            ELSE

               STATUS = PARSE__NOMEM
               CALL EMS_REP ( 'PCN_SETASS2',
     :         'PARSECON: Exceeded storage for global associations',
     :          STATUS )

            ENDIF

         ENDIF

      ELSE

         STATUS = PARSE__GLOBACC
         CALL EMS_REP ( 'PCN_SETASS1',
     :   'PARSECON: Incorrect "ASSOCIATION" access mode (<->)',
     :    STATUS )

      ENDIF

      END
