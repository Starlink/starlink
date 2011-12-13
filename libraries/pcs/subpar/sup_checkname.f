      SUBROUTINE SUBPAR_CHECKNAME ( STRING, STRUCTURE, STATUS )
*+
*  Name:
*     SUBPAR_CHECKNAME

*  Purpose:
*     check a string has the syntax of an HDS name.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_CHECKNAME ( STRING, STRUCTURE, STATUS )

*  Description:
*     Checks that the syntax of the given string is consistent with it
*     being the name of an HDS structure. If the first character is @,
*     the @ is stripped from the string. If the given value of STRUCTURE
*     is FALSE, surrounding quotes are stripped.

*  Arguments:
*     STRING=CHARACTER*(*) (given/returned)
*        The string to be checked. Any leading @ will be removed.
*     STRUCTURE=LOGICAL (given/returned)
*        Given .TRUE. => Don't strip quoted strings
*        Returned .TRUE. => syntax is ok.
*     STATUS=INTEGER (returned)

*  Algorithm:
*     Check for a legal format.
*     Actually anything is permitted but an initial @ will be
*     stripped on the assumption that it is the forced name indicator.
*     and quoted strings will be stripped.

*  Implementation Deficiencies:
*     Probably ought to do fuller check, eg. using SUBPAR_SPLIT

*  Copyright:
*     Copyright (C) 1984, 1987, 1988, 1992 Science & Engineering Research Council.
*     Copyright (C) 1997, 1998 Central Laboratory of the Research Councils.
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
*     BDK: B D Kelly (ROE)
*     AJC: A J Chipperfield (Starlink)
*     {enter_new_authors_here}

*  History:
*     02-OCT-1984 (BDK):
*        Original
*     21-NOV-1984 (BDK):
*        Check for logicals
*     12-MAY-1987 (BDK):
*        relax syntax
*     03-DEC-1987 (AJC):
*        Use lex_cmdline for syntax check
*        This will cope with @name form
*     25-AUG-1988 (AJC):
*        Allow almost anything but strip @ if
*        it is the first character
*     19-JUN-1992 (AJC):
*        Avoid overlapping substrings
*        and remove quotes (allows quoted strings to be interpreted as names)
*     28-OCT-1997 (AJC):
*        Use SUBPAR_UNQUOTE instead of STRING_STRIPQUOT to remove either
*        single or double quotes.
*      6-FEB-1998 (AJC):
*        Make STRUCTURE given also. To indicate no quote strip to be done.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'

*  Arguments Given and Returned:
      CHARACTER*(*) STRING      ! The string to be interpreted
                                ! @ is stripped from forced names
      LOGICAL STRUCTURE         ! Given .TRUE. => Don't strip quotes
                                ! Returned .TRUE. => syntax like HDS name

*  Status:
      INTEGER STATUS

*  Local Variables:
      CHARACTER*132 CTEMP       ! Temporary string

*.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF (STRING(1:1) .EQ. '@') THEN
*       Forced name - Return the parsed string @ removed
         CTEMP = STRING(2:)
         STRING = CTEMP
      ELSE IF ( STRUCTURE ) THEN
         CONTINUE
      ELSE IF ( ( STRING(1:1) .EQ. '''' ) .OR.
     :          ( STRING(1:1) .EQ. '"' ) ) THEN
         CALL SUBPAR_UNQUOTE( STRING, CTEMP, STATUS )
         STRING = CTEMP
      ENDIF

      STRUCTURE = .TRUE.

      END
