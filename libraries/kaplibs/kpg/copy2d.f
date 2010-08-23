      SUBROUTINE COPY2D( DIM1, DIM2, ARRIN, ARROUT, STATUS )
*+
*  Name:
*     COPY2D

*  Purpose:
*     copy one two-dimensional array into another.

*  Language:
*     Starlink

*  Invocation:
*     CALL COPY2D( DIM1, DIM2, ARRIN, ARROUT, STATUS )

*  Description:
*     The input two-dimensional array, ARRIN, of dimension DIM1, DIM2,
*     is copied into the output two-dimensional array, ARROUT, of the
*     same dimensions.  An immediate return will occur if STATUS has
*     an error value on entry.

*  Arguments:
*     DIM1 = INTEGER( READ )
*        The first dimension of the two-dimensional arrays.
*     DIM2 = INTEGER( READ )
*        The second dimension of the two-dimensional arrays.
*     ARRIN( DIM1, DIM2 ) = REAL( READ )
*        Array to be copied.
*     ARROUT( DIM1, DIM2 ) = REAL( WRITE )
*        Will be returned containing a copy of the input array.
*     STATUS = INTEGER( READ )
*        This is the global status, if this variable has an error
*          value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
*        For all lines of the input and output arrays
*           For all the points in a line
*              Output array point is set to the value of input array
*                point
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
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     01/12/1983 : Original version                     (ROE::ASOC5)
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

      REAL
     :  ARRIN( DIM1, DIM2 )


*  Arguments Returned:
      REAL
     :  ARROUT( DIM1, DIM2 )


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :  X,                     ! Index to input/output array elements,
                               ! 1st dimension
     :  Y                      ! Index to input/output array elements,
                               ! 2nd dimension

*.


*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       for all lines of input/output arrays

         DO  Y = 1, DIM2

*          for all points in line

            DO  X = 1, DIM1

*             output array point is set to value of input array point

               ARROUT( X, Y ) = ARRIN( X, Y )
            END DO
         END DO
      END IF

      END
