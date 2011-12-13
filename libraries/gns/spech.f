      SUBROUTINE GNS_1SPECH ( SPECH, REPCH, INSTR, THERE )

*+
*  Name:
*     GNS_1SPECH

*  Purpose:
*     Locate a special character in a string

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL GNS_1SPECH( SPECH, REPCH, INSTR, THERE )

*  Description:
*     Locate the first occurence of a special character in the input
*     string and replace it with the replacement character. Return a
*     logical argument indicating if the special character was present
*     in the string or not.

*  Arguments:
*     SPECH = CHARACTER (Given)
*        Special character.
*     REPCH = CHARACTER (Given)
*        Replacement character.
*     INSTR = CHARACTER*(*) (Given and Returned)
*        Input string.
*     THERE = LOGICAL (Returned)
*        True if special character was in input string,
*        otherwise false.

*  Copyright:
*     Copyright (C) 1992 Science & Engineering Research Council.
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
*     NE: Nick Eaton (Durham University)
*     {enter_new_authors_here}

*  History:
*     13-AUG-1992 (NE):
*        Original version.
*     {enter_changes_here}
*-

*  Type Definitions:
      IMPLICIT NONE

*  Arguments Given:
      CHARACTER SPECH
      CHARACTER REPCH

*  Arguments Given and Returned:
      CHARACTER * ( * ) INSTR

*  Arguments Returned:
      LOGICAL THERE

*  Local Variables:
      INTEGER ISPECH
*.

*   Locate the first occurence of the special character in the
*   input string
      ISPECH = INDEX( INSTR, SPECH )

*   If the special character was found then replace it with the
*   replacement string
      IF ( ISPECH .GT. 0 ) THEN
         INSTR( ISPECH:ISPECH ) = REPCH
         THERE = .TRUE.

*   Otherwise indicate that the special character was not found
      ELSE
         THERE = .FALSE.
      ENDIF

      END

