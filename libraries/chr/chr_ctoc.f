      SUBROUTINE CHR_CTOC( STR1, STR2, NCHAR )
*+
*  Name:
*     CHR_CTOC

*  Purpose:
*     Write a CHARACTER string into another string.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_CTOC( STR1, STR2, NCHAR )

*  Description:
*     Write the given character string into the returned character string.
*     If the given string is longer than the returned string, the given
*     string is truncated. If the returned string is longer than the
*     given character variable, the remainder of the returned string
*     is padded with blanks.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The value to be written.
*     STR2 = CHARACTER * ( * ) (Returned)
*        The character string into which the value is to be written.
*     NCHAR = INTEGER (Returned)
*        The resulting length of the character string, ignoring
*        trailing blanks.

*  Algorithm:
*     The copy is done using an assignment.
*     The string length is determined, ignoring trailing blanks.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1991, 1994 Science & Engineering Research Council.
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
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
*     JRG: Jack Giddings (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     PCTR: P.C.T. Rees (STARLINK)
*     ACC: A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     16-NOV-1984 (ACD):
*        Documentation improved.
*     24-OCT-1988 (AJC):
*        Documentation improved.
*     13-FEB-1991 (PCTR):
*        Changed order of length check and assignment so that NCHAR
*        always returns the correct length.
*     10-MAR-1994 (ACC):
*        Modified prologue.
*     24-JAN-1997 (AJC):
*        Use the length of STR1 as this may well be shorter.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STR1 * ( * )

*  Arguments Returned:
      CHARACTER STR2 * ( * )

      INTEGER NCHAR

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*.

      STR2 = STR1
      NCHAR = CHR_LEN( STR1 )

      END
