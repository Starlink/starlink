      SUBROUTINE GAI1_NXTAB( STR, ISTART, IFOUND, STATUS )
*+
*  Name:
*     GAI1_NXTAB

*  Purpose:
*     Locate the next occurence of the TAB character.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GAI1_NXTAB( STR, ISTART, IFOUND, STATUS )

*  Description:
*     This routine locates the next occurence of a <TAB> character in
*     the given string, starting from a given position. If found the
*     IFOUND argument will be set to the position of the <TAB> character
*     within the string. If a <TAB> character is not found, but the
*     string contains non-blank characters after that point then IFOUND
*     will be set to the last non-blank character plus 1. If all remaining
*     characters are blank then IFOUND will be set to 0.

*  Arguments:
*     STR = CHARACTER * ( * ) (Given)
*        The string to be searched.
*     ISTART = INTEGER (Given)
*        The starting point from which elements of STR are searched.
*     IFOUND = CHARACTER * ( * ) (Given and Returned)
*        The position of the <TAB> character, or the end of the
*        string + 1 if no <TAB>'s are located and the remaining string isn't
*        blank. Otherwise it will be set to 0.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1998 Central Laboratory of the Research Councils
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of the
*     License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied warranty
*     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
*     GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA


*  Authors:
*     PWD: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     29-SEP-1998 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      CHARACTER * ( * ) STR
      INTEGER ISTART

*  Arguments Returned:
      INTEGER IFOUND

*  Status:
      INTEGER STATUS            ! Global status

*  External References:
      INTEGER CHR_LEN
      EXTERNAL CHR_LEN          ! Used length of string

*  Local Variables:
      CHARACTER * ( 1 ) TAB     ! <TAB> character.
      INTEGER IAT               ! Position within string
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Initialization.
      TAB = CHAR( 9 )

*  Scan for the next <TAB> character.
      IAT = INDEX( STR( ISTART : ), TAB )
      IF ( IAT .EQ. 0 ) THEN

*  See if line is blank.
         IAT = CHR_LEN( STR( ISTART : ) )
         IF ( IAT .EQ. 0 ) THEN

*  Yes.
            IFOUND = 0
         ELSE

*  Return absolute position of end of string.
            IFOUND = ISTART + IAT
         END IF
      ELSE

*  Set absolute position of <TAB> character.
         IFOUND = ISTART + IAT - 1
      END IF
      END
