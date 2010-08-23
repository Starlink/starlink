      SUBROUTINE ZERO1D( SIZE, ARRAY, STATUS )
*+
*  Name:
*     ZERO1D

*  Purpose:
*     Sets all elements of the one-dimensional array to zero.

*  Language:
*     Starlink

*  Invocation:
*     CALL ZERO1D( SIZE, ARRAY, STATUS )

*  Description:
*     All elements of the one-dimensional array, ARRAY, are set equal to
*     zero. An immediate return will occur if STATUS has an error value
*     on entry.

*  Arguments:
*     SIZE   = INTEGER( READ )
*        Number of elements in the array to be set to all zeros.
*     ARRAY( SIZE ) = REAL( UPDATE )
*        All elements of this array will be set to zero.
*     STATUS = INTEGER( READ )
*        This is the global status, if this variable has an error
*        value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
*        For all elements in the array
*           Value of element becomes zero
*        Endfor
*     Endif

*  Copyright:
*     Copyright (C) 1983, 1984, 1986 Science & Engineering Research Council.
*     All Rights Reserved.

*  Licence:
*     This program is free software; you can redistribute it and/or
*     modify it under the terms of the GNU General Public License as
*     published by the Free Software Foundation; either Version 2 of
*     the License, or (at your option) any later version.
*
*     This program is distributed in the hope that it will be
*     useful,but WITHOUT ANY WARRANTY; without even the implied
*     warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
*     PURPOSE. See the GNU General Public License for more details.
*
*     You should have received a copy of the GNU General Public License
*     along with this program; if not, write to the Free Software
*     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
*     02111-1307, USA.

*  Authors:
*     Dave Baines (ROE::ASOC5)
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     12/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 12: Renamed parameters section to arguments and tidied
*        (RL.STAR::CUR)
*     {enter_further_changes_here}

*  Bugs:
*     {note_any_bugs_here}

*-

*  Type Definitions:
      IMPLICIT NONE


*  Global Constants:
      INCLUDE 'SAE_PAR'


*  Arguments Given:
      INTEGER
     :  SIZE


*  Arguments Returned:
      REAL
     :  ARRAY( SIZE )


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :  X            ! pointer to elements of array

*.


*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       do all elements of array

         DO  X = 1, SIZE

*          set value of element to zero

            ARRAY( X ) = 0.0
         ENDDO
      ENDIF

      END
