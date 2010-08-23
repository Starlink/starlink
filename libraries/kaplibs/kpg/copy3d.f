      SUBROUTINE COPY3D( DIM1, DIM2, DIM3, ARRIN, ARROUT, STATUS )
*+
*  Name:
*     COPY3D

*  Purpose:
*     copy one three-dimensional array into another.

*  Language:
*     Starlink

*  Invocation:
*     CALL COPY3D( DIM1, DIM2, DIM3, ARRIN, ARROUT, STATUS )

*  Description:
*     The input three-dimensional array, ARRIN, of dimensions in DIM1,
*     DIM2, DIM3, is copied into the output three-dimensional array,
*     ARROUT, of the same dimensions.  An immediate return will occur
*     if STATUS has an error value on entry.

*  Arguments:
*     DIM1 = INTEGER( READ )
*        The first dimension of the three-dimensional arrays.
*     DIM2 = INTEGER( READ )
*        The second dimension of the three-dimensional arrays.
*     DIM3 = INTEGER( READ )
*        The third dimension of the three-dimensional arrays.
*     ARRIN( DIM1, DIM2, DIM3 ) = REAL( READ )
*        Array to be copied.
*     ARROUT( DIM1, DIM2, DIM3 ) = REAL( WRITE )
*        Will be returned containing a copy of the input array.
*     STATUS = INTEGER( READ )
*        This is the global status, if this variable has an error
*          value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
*        For all bands of the input and output arrays
*           For all the lines of a band
*              For all the points in a line
*                 Output array point is set to value of input array
*                   point
*              Endfor
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
*     02/12/1983 : Original version                     (ROE::ASOC5)
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
     :  DIM1, DIM2, DIM3

      REAL
     :  ARRIN( DIM1, DIM2, DIM3 )


*  Arguments Returned:
      REAL
     :  ARROUT( DIM1, DIM2, DIM3 )


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :  X,                     ! Index to input/output array elements,
                               ! 1st dimension
     :  Y,                     ! Index to input/output array elements,
                               ! 2nd dimension
     :  Z                      ! Index to input/output array elements,
                               ! 3rd dimension

*.


*    check for error on entry

      IF ( STATUS .EQ. SAI__OK ) THEN

*       for all bands of input/output arrays

         DO  Z = 1, DIM3

*          for all lines of band

            DO  Y = 1, DIM2

*             for all points in line

               DO  X = 1, DIM1

*                output array point is set to value of input array point

                  ARROUT( X, Y, Z ) = ARRIN( X, Y, Z )
               END DO
            END DO
         END DO
      END IF

      END
