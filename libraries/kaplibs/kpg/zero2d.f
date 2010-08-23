      SUBROUTINE ZERO2D( DIM1, DIM2, ARRAY, STATUS )
*+
*  Name:
*     ZERO2D

*  Purpose:
*     sets all elements of a two-dimensional array to zero.

*  Language:
*     Starlink

*  Invocation:
*     CALL ZERO2D( DIM1, DIM2, ARRAY, STATUS )

*  Description:
*     All elements of the two-dimensional array, ARRAY, are set equal to
*     zero. An immediate return will occur if STATUS has an error value
*     on entry.

*  Arguments:
*     DIM1 = INTEGER( READ )
*        The first dimension of the two-dimensional array to be set to
*        all zeros.
*     DIM2 = INTEGER( READ )
*        The second dimension of the two-dimensional array to be set to
*        all zeros.
*     ARRAY( DIM1, DIM2 ) = REAL( UPDATE )
*        All elements of this array will be set to zero.
*     STATUS = INTEGER( READ )
*        This is the global status, if this variable has an error
*        value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
*        For all rows of the array
*           For all elements in a row
*              Value of element becomes zero
*           Endfor
*        Endfor
*     Endif

*  Copyright:
*     Copyright (C) 1983, 1984, 1986, 1989 Science & Engineering Research Council.
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
*     Malcolm Currie RAL (UK.AC.RL.STAR)
*     {enter_new_authors_here}

*  History:
*     05/12/1983 : Original version                     (ROE::ASOC5)
*     17/02/1984 : Documentation brought up to standard (ROE::ASOC5)
*     1986 Sep 12: Renamed parameters section to arguments and tidied
*        (RL.STAR::CUR)
*     1989 Aug  7: Passed array dimensions as separate variables
*        (RL.STAR::CUR).
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
     :  DIM1, DIM2


*  Arguments Given and Returned:
      REAL
     :  ARRAY( DIM1, DIM2 )


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :  X,                     ! Pointer to element along first
                               ! dimension of array
     :  Y                      ! Pointer to element along second
                               ! dimension of array

*.


*    Check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       Do all rows of the array

         DO  Y = 1, DIM2

*          Do all elements of each row

            DO  X = 1, DIM1

*             Set value of element to zero

               ARRAY( X, Y ) = 0.0
            END DO
         END DO
      END IF

      END
