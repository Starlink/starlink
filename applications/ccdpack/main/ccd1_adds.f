
      SUBROUTINE CCD1_ADDS( PART, WHOLE, DEST, DIM1, DIM2, INL, INR,
     :                      STATUS )
*+
*  Name:
*     CCD1_ADDS

*  Purpose:
*     Adds to data arrays missing parts of edge.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL CCD1_ADDS( PART, WHOLE, DEST, DIM1, DIM2, INL, INR,
*                     STATUS )

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
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
*     PDRAPER: Peter Draper (STARLINK)
*     {enter_new_authors_here}

*  History:
*     7-NOV-1991 (PDRAPER):
*        Original version.
*     {enter_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants

*  Arguments Given:
      INTEGER DIM1
      INTEGER DIM2
      REAL PART( DIM1, DIM2 )
      REAL WHOLE( DIM1, DIM2 )
      INTEGER INL
      INTEGER INR

*  Arguments Returned:
      REAL DEST( DIM1, DIM2 )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I
      INTEGER J
      INTEGER UP

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      UP = DIM1 - INR

*  Copy the whole of WHOLE to the destination array, and the parts of
*  PART which lie in the range INL to DIM1-INR
      DO 1 J = 1, DIM2
         DO 2 I = 1, DIM1
            IF ( I .GE. INL .AND. I .LE. UP ) THEN

*  Add WHOLE to PART in these regions.
               DEST( I, J ) = WHOLE( I, J ) + PART( I, J )
            ELSE

*  just copy whole
               DEST( I, J ) = WHOLE( I, J )
            END IF
 2       CONTINUE
 1    CONTINUE

      END
* $Id$
