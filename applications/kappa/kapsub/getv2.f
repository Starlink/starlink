      SUBROUTINE GETV2( INARR, DIM1, DIM2, X, Y, VALUE, STATUS )
*+
*  Name:
*     GETV2

*  Purpose:
*     Returns the value of a specified point in a 2-d array.

*  Language:
*     Starlink Fortran 77

*  Type of Module:
*     SUBROUTINE

*  Invocation:
*     CALL GETV2( INARR, DIM1, DIM2, X, Y, VALUE, STATUS )

*  Description:
*     This routine accepts the co-ordinates of a point
*     on an image and determines the value of the image
*     at that point, which is returned to the calling routine.

*  Arguments:
*     INARR( DIMS1, DIMS2 ) = REAL( READ )
*         The 2-dimensional array.
*     DIM1 = INTEGER( READ )
*         The first dimension of the 2-d array.
*     DIM2 = INTEGER( READ )
*         The second dimension of the 2-d array.
*     X = INTEGER( READ )
*         The X co-ordinate of the point whose value is to be
*           determined.
*     Y = INTEGER( READ )
*         The Y co-ordinate of the point whose value is to be
*           determined.
*     VALUE = REAL( WRITE )
*         The value of the specified pixel.
*     STATUS = INTEGER( READ, WRITE )
*         The status value on entry to this subroutine.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     If input element numbers are outside bounds of the array then
*        Report the error and set bad status
*     Else
*        The value is found by indexing the image array using the
*          co-ordinates given.
*     Endif
*     End

*  Copyright:
*     Copyright (C) 1988-1989 Science & Engineering Research Council.
*     All Rights Reserved.

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
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     1988 Sep  8 : Original (RL.STAR::CUR).
*     1989 Jul 27 : Passed the array dimensions as two variables
*                   (RL.STAR::CUR).
*     {enter_further_changes_here}

*-

*  Type Definitions:

      IMPLICIT NONE

*  Global Constants:

      INCLUDE 'SAE_PAR'

*  Arguments Given:

      INTEGER
     :  DIM1, DIM2,
     :  X, Y

      REAL
     :  INARR( DIM1, DIM2 )

*  Arguments Returned:

      REAL
     :  VALUE

*  Status:

      INTEGER STATUS

*.

*    If the status is bad, then return to the main program.

      IF ( STATUS .NE. SAI__OK ) RETURN

      IF ( X .LT. 1 .OR. X .GT. DIM1 .OR.
     :     Y .LT. 1 .OR. Y .GT. DIM2 ) THEN
         STATUS = SAI__ERROR
         CALL ERR_REP( 'ERR_GETV2_POA',
     :     'GETV2: Element numbers outside array bounds.', STATUS )
      ELSE

*       Reference the image array using the point co-ordinates.

         VALUE = INARR( X, Y )
      END IF

      END
