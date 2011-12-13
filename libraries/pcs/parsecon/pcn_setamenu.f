      SUBROUTINE PARSECON_SETAMENU ( ENTRY, STATUS )
*+
*  Name:
*     PARSECON_SETAMENU

*  Purpose:
*     Sets-up action menu name.

*  Language:
*     VAX Fortran

*  Invocation:
*     CALL PARSECON_SETAMENU ( ENTRY, STATUS )

*  Description:
*     Loads the provided name into the menu store for the most
*     recently declared program action.

*  Arguments:
*     ENTRY=CHARACTER*(*) (given)
*        action menu name
*     STATUS=INTEGER

*  Algorithm:
*     Superfluous quotes are removed from the given string, and the
*     result is put into the array holding menu names.

*  Copyright:
*     Copyright (C) 1986, 1990, 1992, 1993 Science & Engineering Research Council.
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
*     13.05.1986:  Original (REVAD::BDK)
*     16.10.1990:  Convert all strings to upper case
*        Use CHR for conversion  (RLVAD::AJC)
*     27.02.1992:  Assume ENTRY is ucase unless quoted string (RLVAD::AJC)
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


*  Arguments Given:
      CHARACTER*(*) ENTRY             ! the action menu name


*  Status:
      INTEGER STATUS


*  Global Variables:
      INCLUDE 'SUBPAR_CMN'


*  External References:
*     None


*  Local Constants:
      CHARACTER*(*) QUOTE
      PARAMETER ( QUOTE = '''' )

*.


      IF ( STATUS .NE. SAI__OK ) RETURN

*   If the name is a quoted string, process the quotes
*   and convert to upper case
      IF ( ENTRY(1:1) .EQ. QUOTE ) THEN

         CALL STRING_STRIPQUOT ( ENTRY, ACTMENU(ACTPTR), STATUS )
         CALL CHR_UCASE( ACTMENU(ACTPTR) )

      ELSE

         ACTMENU(ACTPTR) = ENTRY

      ENDIF

      END
