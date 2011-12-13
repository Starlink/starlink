      SUBROUTINE CHR_APPND( STR1, STR2, IPOSN )
*+
*  Name:
*     CHR_APPND

*  Purpose:
*     Copy one string into another, ignoring trailing blanks.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_APPND( STR1, STR2, IPOSN )

*  Description:
*     The string STR1 (or as much of it as there is room for) is
*     copied into the part of STR2 beginning at position IPOSN+1.
*     IPOSN is updated to indicate the final length of STR2 after
*     this operation. Trailing blanks in STR1 are ignored.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The string to be copied.
*     STR2 = CHARACTER * ( * ) (Given and Returned)
*        The string to be updated.
*     IPOSN = INTEGER (Given and Returned)
*        The position in STR2 at which STR1 is to be appended. This
*        value is returned updated to be the position of the last
*        non-blank character in STR2 after the copy.

*  Algorithm:
*     -  Compute the maximum size of the output string.
*     -  Compute the length of the input string that can be copied;
*     either all of it or as much as there is space for in the
*     output string.
*     -  Copy the input string into the output string.
*     -  Update the length (last non-blank character) of the output
*     string.

*  Notes:
*     If the output string is too small to completely append the input
*     string truncation will occur and IPOSN will be set to the end of
*     STR2.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1991 Science & Engineering Research Council.
*     Copyright (C) 1997 Central Laboratory of the Research Councils.
*     Copyright (C) 2006 Particle Physics & Astronomy Research Council.
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
*     PCTR: P.C.T. Rees (STARLINK)
*     TIMJ: Tim Jenness (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     7-MAR-1983 (SLW):
*        Original version.
*     6-NOV-1984 (ACD):
*        Documentation improved.
*     13-SEP-1988 (AJC):
*        Use LEN instead of CHR_SIZE. Improve documentation.
*     12-FEB-1991 (PCTR):
*        Some optimisation.
*     22-JAN-1997 (AJC):
*        Calculate new length from used length of STR1 which
*        is probably shorter than STR2.
*     20-JUL-2006 (TIMJ):
*        If STR1 + STR2 exceeds the allocated size of STR2, make sure that
*        IPOSN reflects the truncation.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Arguments Given:
      CHARACTER STR1 * ( * )

*  Arguments Given and Returned:
      CHARACTER STR2 * ( * )

      INTEGER IPOSN

*  External References:
      INTEGER CHR_LEN            ! String length (ignoring trailing blanks)

*  Local Variables:
      INTEGER STR1LEN            ! Used length of STR1
      INTEGER STR2SZ             ! Allocated size of STR2

*.

      STR1LEN = CHR_LEN( STR1 )
      STR2SZ = LEN( STR2 )
      IF ( STR2SZ .GT. IPOSN ) THEN
         STR2( IPOSN+1 : ) = STR1
         IPOSN = IPOSN + STR1LEN
         IF ( IPOSN .GT. STR2SZ ) IPOSN = STR2SZ
      END IF

      END
