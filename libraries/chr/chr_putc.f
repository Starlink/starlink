      SUBROUTINE CHR_PUTC( STR1, STR2, IPOSN )
*+
*  Name:
*     CHR_PUTC

*  Purpose:
*     Put a CHARACTER string into another at a given position.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CHR_PUTC( STR1, STR2, IPOSN )

*  Description:
*     The string STR1 (or as much of it as there is room for) is
*     copied into the part of STR2 beginning at position IPOSN+1.
*     IPOSN is updated to indicate the end position of the copy of
*     STR1 within STR2 after this operation. If no copying is
*     done, IPOSN is returned unchanged. The sizes of STR1 and
*     STR2 are based on the declared Fortran 77 size given by the
*     intrinsic function LEN.

*  Arguments:
*     STR1 = CHARACTER * ( * ) (Given)
*        The string to be copied.
*     STR2 = CHARACTER * ( * ) (Given and Returned)
*        The string into which STR1 is to be copied.
*     IPOSN = INTEGER (Given and Returned)
*        The position pointer within STR2.

*  Copyright:
*     Copyright (C) 1983, 1984, 1988, 1989 Science & Engineering Research Council.
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
*     {enter_new_authors_here}

*  History:
*     3-JAN-1983 (JRG):
*        Original version.
*     2-OCT-1984 (ACD):
*        Documenation improved.
*     13-SEP-1988 (AJC):
*        Documentation improved.
*     24-FEB-1989 (AJC):
*        Check on string sizes.
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

*  Local Variables:
      INTEGER SIZE1              ! Size of STR1
      INTEGER SIZE2              ! Size of STR2

*.

*  Get the size of target string.
      SIZE2 = LEN( STR2 )

*  Check that the pointer is within string.
      IF ( IPOSN .LT. SIZE2 ) THEN

*     Get the length that can be copied.
         SIZE1 = MIN( LEN( STR1 ), SIZE2-IPOSN )

         IF ( SIZE1 .GT. 0 ) THEN

*        Copy the string.
            STR2( IPOSN+1 : IPOSN+SIZE1 ) = STR1( 1 : SIZE1 )

*        Update the pointer value.
            IPOSN = IPOSN + SIZE1
         END IF
      END IF

      END
