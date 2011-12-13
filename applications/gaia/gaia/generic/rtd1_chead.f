      SUBROUTINE RTD1_CHEAD( IN, NEL, OUT, STATUS )
*+
* Name:
*    RTD1_CHEAD

*  Purpose:
*     Writes an output FITS header from an existing one.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL RTD1_CHEAD( IN, NEL, OUT, STATUS )

*  Description:
*     This routine copies a 1D character array of a given size into
*     another (which must be at least as big). The contents are assumed
*     to be FITS cards, which have been created by GAIA. A check is made
*     to ensure that the special NDFID, LBOUND1 and LBOUND2 cards are
*     not written out.

*  Arguments:
*     IN( NEL ) = CHARACTER * ( * ) (Given)
*        The input FITS block (from GAIA).
*     NEL = INTEGER (Given)
*        The number of elements (cards) in IN.
*     OUT( NEL ) = CHARACTER * ( * ) (Returned)
*        The output FITS block.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1997-2001 Central Laboratory of the Research Councils
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
*     PWD: Peter Draper (STARLINK - Durham University)
*     {enter_new_authors_here}

*  History:
*     15-DEC-1997 (PWD):
*        Original version.
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE             ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'         ! Standard SAE constants

*  Arguments Given:
      INTEGER NEL
      CHARACTER * ( * ) IN( NEL )

*  Arguments Returned:
      CHARACTER * ( * ) OUT( NEL )

*  Status:
      INTEGER STATUS            ! Global status

*  Local Variables:
      INTEGER I                 ! Loop variable
*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Copy the whole of the input character array to the output one.
*  Unless the card is one that shouldn't be propagated.
      DO 1 I = 1, NEL
         IF ( IN( I )( 1: 5 ) .NE. 'NDFID' .AND.
     :        IN( I )( 1: 6 ) .NE. 'LBOUND' ) THEN
            OUT( I ) = IN( I )
         ELSE
            OUT( I ) = ' '
         END IF
 1    CONTINUE
      END
