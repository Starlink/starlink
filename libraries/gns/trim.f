      SUBROUTINE gns_1TRIM (IN,OUT,L)
*+
*  Name:
*     gns_1TRIM

*  Purpose:
*     Trims leading and trailing blanks from a string.

*  Language:
*     Starlink Fortran 77

*  Description:
*     Leading and trailing blanks are removed from the input string and
*     the length of the resulting string returned. If the input string
*     is all blanks the length is set to zero.

*  Arguments:
*     IN = CHAR (Given)
*         Input string
*     OUT = CHAR (Returned)
*         Output string
*     L = INTEGER (Returned)
*         Length of output string

*  Copyright:
*     Copyright (C) 1988 Science & Engineering Research Council. All
*     Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful, but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 51 Franklin Street,Fifth Floor, Boston, MA
*     02110-1301, USA

*  Authors:
*     DLT: D L Terrett (Starlink)
*     {enter_new_authors_here}

*  History:
*     3-JUN-1988 (DLT):
*        Modified.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*  External References:
*     none

*  Implicit Inputs:
*     none

*  Implicit Outputs:
*     none

*-
      IMPLICIT NONE

      CHARACTER*(*) IN, OUT
      INTEGER L, I

*  Remove leading blanks
      DO 10 I = 1,LEN(IN)
         IF (IN(I:I).NE.' ') GO TO 20
   10 CONTINUE

*  If we get here then the string is all blanks.
      L = 0
      GO TO 40

   20 CONTINUE
      OUT = IN(I:)

*  Find position of last non blank character
      DO 30 L = LEN(OUT),1,-1
         IF (OUT(L:L).NE.' ') GO TO 40
   30 CONTINUE

   40 CONTINUE
      END
