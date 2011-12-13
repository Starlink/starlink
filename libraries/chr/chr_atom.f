      SUBROUTINE CHR_ATOM( STR1, STR2 )
*+
*  Name:
*     CHR_ATOM

*  Purpose:
*     Translate a string from ASCII to the machine's character set.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_ATOM( STR1, STR2 )

*  Description:
*     The string STR1, which has been written on a machine which uses
*     the ASCII character set and subsequently read on another
*     machine is returned in STR2 translated into the correct
*     character set for that machine.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The character string written on a machine with an ASCII
*        character set and read on a machine which may not use
*        ASCII.
*     STR2 = CHARACTER * ( * ) (Returned)
*        The character string translated into the machine's character
*        set. If STR2 is shorter than STR1, the translated string will
*        be truncated; if STR2 is longer than STR1, STR2 will be padded
*        with blanks beyond the translated string.

*  Copyright:
*     Copyright (C) 1991, 1994 Science & Engineering Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     25-FEB-1991 (PCTR):
*        Original version.
*     10-MAR-1994 (ACC for PCTR):
*        Modifications to prologue.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STR1 * ( * )

*  Arguments Returned:
      CHARACTER STR2 * ( * )

*.

*  Version for machines which use the ASCII character set.
      STR2 = STR1

*  Template for a portable version.
*  External References:
*     CHARACTER * 1 MCH_ATOM     ! Machine-specific character conversion

*  Local Variables:
*     INTEGER ICHR               ! Character loop index
*     INTEGER LENGTH             ! Maximum loop index
*     INTEGER LEN1               ! Declared length of STR1
*     INTEGER LEN2               ! Declared length of STR2

*.

*  Initialise the returned string.
      STR2 = ' '

*  Get the declared length of the two given strings.
*     LEN1 = LEN( STR1 )
*     LEN2 = LEN( STR2 )

*  Get the maximum loop index.
*     LENGTH = MIN( LEN1, LEN2 )

*  Trap a zero length strings.
*     IF ( LENGTH .GT. 0 ) THEN

*     Loop to apply character conversion.
*        DO 10 ICHR = 1, LENGTH
*           STR2( ICHR : ICHR ) = MCH_ATOM( STR1( ICHR : ICHR ) )
*10      CONTINUE
*     END IF

      END
