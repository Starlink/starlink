      SUBROUTINE CHR_MOVE( STR1, STR2 )
*+
*  Name:
*     CHR_MOVE

*  Purpose:
*     Move one string into another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_MOVE( STR1, STR2 )

*  Description:
*     The string STR1, or as much of it as there is room for, is
*     copied into STR2 beginning at position 1.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The given string.
*     STR2 = CHARACTER * ( * ) (Returned)
*        The returned string.

*  Notes:
*     This routine is OBSOLETE.  It exists for historical reasons.
*     Its function is identical to a Fortran assignment statement.
*     It is recommended that an assignment statement be used instead
*     of CHR_MOVE.

*  Algorithm:
*     Copy as much as possible of the given string into the
*     returned string.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1991, 1994 Science & Engineering Research Council.
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
*     SLW: Sid Wright (UCL)
*     ACD: A.C. Davenhall (ROE)
*     AJC: A.J. Chipperfield (STARLINK)
*     ACC:  A.C. Charles (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-MAR-1983 (SLW):
*        Original version.
*     11-NOV-1984 (ACD):
*        Documentation improved.
*     13-SEP-1988 (AJC):
*        Use LEN instead of CHR_SIZE.
*        Improve documentation.
*     19-FEB-1991 (PCTR):
*        Code replaced with a character assignment - it does exactly
*        the same thing, only faster.
*     30-MAR-1994 (ACC):
*        Modifications to prologue.
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

*.

*  Perform string assignment.
      STR2 = STR1

      END
