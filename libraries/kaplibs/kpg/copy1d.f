      SUBROUTINE COPY1D( DIM, ARRIN, ARROUT, STATUS )
*+
*  Name:
*     COPY1D

*  Purpose:
*     copy one one-dimensional array to another.

*  Language:
*     Starlink F77

*  Invocation:
*     CALL COPY1D( DIM, ARRIN, ARROUT, STATUS )

*  Description:
*     The input one-dimensional array, ARRIN, of dimension DIM, is
*     copied into the output one-dimensional array, ARROUT, of the
*     same dimension.  An immediate return will occur if STATUS 
*     has an error value on entry.

*  Arguments:
*     DIM = INTEGER( READ )
*        Dimension of the input and output arrays.
*     ARRIN( DIM ) = REAL( READ )
*        One-dimensional array to be copied.
*     ARROUT( DIM ) = REAL( WRITE )
*        Output array is returned as a copy of the input array.
*     STATUS = INTEGER( READ )
*        This is the global status, if this variable has an error
*        value on entry then an immediate return will occur.

*  Algorithm:
*     If no error on entry then
*        For all points in the input array
*           Output array point is set to value of input array point.
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
*     useful, but WITHOUT ANY WARRANTY; without even the implied
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
     :  DIM

      REAL
     :  ARRIN( DIM )


*  Arguments Returned:
      REAL
     :  ARROUT( DIM )


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :  X              ! index to input/output array elements

*.


*    check for error on entry

      IF( STATUS .EQ. SAI__OK ) THEN

*       for all points in input/output arrays

         DO  X = 1, DIM

*          output array point is set to value of input array point

            ARROUT( X ) = ARRIN( X )
         ENDDO
      ENDIF

      END
