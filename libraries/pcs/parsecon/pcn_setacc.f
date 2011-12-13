      SUBROUTINE PARSECON_SETACC ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETACC

*  Purpose:
*     Sets-up parameter access mode.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETACC ( ENTRY, STATUS )

*  Description:
*     Interprets the provided keyword as READ WRITE or UPDATE regardless
*     of case and puts a logical value into the store indicating read or
*     write access for the most recently declared program parameter.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        access type
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, and the
*     result is interpreted and stored.

*  Copyright:
*     Copyright (C) 1984, 1990, 1992, 1993 Science & Engineering Research Council.
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
*     16.10.1990:  Force all string to upper case
*        Use CHR for upper case conversion
*        don't assume READ if not WRITE or UPDATE (RLVAD::AJC)
*     25.02.1992:  Report errors (RLVAD::AJC)
*     27.02.1992:  Assume ENTRY is ucase unless a quoted string (RLVAD::AJC)
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
      CHARACTER*(*) ENTRY             ! the keyword string


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )


*  Local Variables:
      CHARACTER*6 VALUE

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   If ENTRY is a quoted string, process the quotes
*   and convert to upper case
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN
         CALL STRING_STRIPQUOT ( ENTRY, VALUE, STATUS )
         CALL CHR_UCASE( VALUE )

      ELSE
         VALUE = ENTRY
      ENDIF

*   Interpret the string
      IF ( ( VALUE .EQ. 'WRITE' ) .OR. ( VALUE .EQ. 'UPDATE' ) ) THEN
         PARWRITE(PARPTR) = .TRUE.
      ELSEIF ( VALUE .EQ. 'READ' ) THEN
         PARWRITE(PARPTR) = .FALSE.
      ELSE
*     Illegal value
         STATUS = PARSE__IVSYN
         CALL EMS_REP ( 'PCN_SETACC1',
     :   'PARSECON: "ACCESS" value must be '//
     :   '''READ'', ''WRITE'' or ''UPDATE''', STATUS )

      ENDIF

      END
