      SUBROUTINE SUBPAR_BASCOPY ( INSIZE, INVEC, OUTVEC, STATUS )
*+
*  Name:
*     SUBPAR_BASCOPY

*  Purpose:
*     Copies from one byte vector to another.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL SUBPAR_BASCOPY ( INSIZE, INVEC, OUTVEC, STATUS )

*  Description:
*     Copies the contents of a byte vector.

*  Arguments:
*     INSIZE=INTEGER (given)
*        number of elements to be copied
*     INVEC=BYTE(*) (given)
*        input vector
*     OUTVEC=BYTE(*) (returned)
*        output vector
*     STATUS=INTEGER

*  Copyright:
*     Copyright (C) 1984 Science & Engineering Research Council.
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
*     BDK: B D Kelly (ROE)
*     {enter_new_authors_here}

*  History:
*     10-OCT-1984 (BDK):
*        Original
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER INSIZE

      BYTE INVEC(*)


*  Arguments Returned:
      BYTE OUTVEC(*)


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER J


*.


      IF ( STATUS .NE. SAI__OK ) RETURN

      DO J = 1, INSIZE
         OUTVEC(J) = INVEC(J)
      ENDDO

      END
