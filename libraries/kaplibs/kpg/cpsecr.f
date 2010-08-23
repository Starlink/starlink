      SUBROUTINE  CPSECR( IMAGE, DIM1, DIM2, X1, Y1, X2, Y2, RXDIM,
     :                      RYDIM, REGION, STATUS )
*+
*  Name:
*     CPSECR

*  Purpose:
*     Copy region of two-dimensional image.

*  Language:
*     Starlink

*  Invocation:
*     CALL  CPSECR( IMAGE, DIM1, DIM2, X1, Y1, X2, Y2, RXDIM, RYDIM,

*  Description:
*     This routine copies the two-dimensional data of a specified region
*     of an image.

*  Arguments:
*     IMAGE( DIM1, DIM2 ) = REAL( READ )
*        The array containing the raw image data.
*     DIM1 = INTEGER( READ )
*        The first dimension of the two-dimensional array.
*     DIM2 = INTEGER( READ )
*        The second dimension of the two-dimensional array.
*     X1 = INTEGER( READ )
*        The X co-ordinate of the first point from which the region
*          is determined.
*     Y1 = INTEGER( READ )
*        The Y co-ordinate of the first point.
*     X2 = INTEGER( READ )
*        The X co-ordinate of the second point.
*     Y2 = INTEGER( READ )
*        The Y co-ordinate of the second point.
*     RXDIM = INTEGER( READ )
*        The X-dimension of the region.
*     RYDIM = INTEGER( READ )
*        The Y-dimension of the region.
*     REGION( RXDIM, RYDIM ) = REAL( WRITE )
*        The array into which the region is copied.
*     STATUS = INTEGER( READ )
*        The status value on entry to this routine.

*  Algorithm:
*     Check for error on entry - return if not o.k.
*     The maximum and minimum co-ordinates of the region are found.
*     The region is copied by a direct assignment of the data elements
*     concerned to the elements of the new array.

*  Copyright:
*     Copyright (C) 1983, 1986, 1989 Science & Engineering Research Council.
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
*     S.Chan ( RGVAD::KFH )
*     Malcolm Currie RAL (UK.AC.RL.STAR::CUR)
*     {enter_new_authors_here}

*  History:
*     28 October 1983:
*     1986 Sep 22: Renamed from KFH_TRANSFER2. Standardised to RAPI2D
*        style; renamed parameters section to arguments and
*        added access; region limits now unaltered within
*        routine; argument reordered (8 -> 10); made generic;
*        removed tabs; relocated 'local' variables to import
*        etc.; stripped of trailing blanks and tidied
*        (RL.STAR::CUR).
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
     :    RXDIM, RYDIM,
     :    DIM1, DIM2,
     :    X1, Y1,
     :    X2, Y2

      REAL IMAGE( DIM1, DIM2 )


*  Arguments Returned:
      REAL REGION( RXDIM, RYDIM )


*  Status:
      INTEGER STATUS


*  Local Variables:
      INTEGER
     :    I, J,                 ! General variables
     :    XX1, YY1,             ! Lower x,y bound of the region
     :    XX2, YY2              ! Upper x,y bound of the region


*.


*    If status value is bad, then return to the main program.

      IF ( STATUS .NE. SAI__OK ) GOTO 999

*    Copy input co-ordinates a pair are transposed

      XX1 = MIN( X1, X2 )
      YY1 = MIN( Y1, Y2 )
      XX2 = MAX( X1, X2 )
      YY2 = MAX( Y1, Y2 )

*    Copy the elements of the region.

      DO  I = YY1, YY2, 1
         DO  J = XX1, XX2, 1

            REGION( J - XX1 + 1, I - YY1 + 1 ) = IMAGE( J, I )

         END DO
      END DO

 999  CONTINUE

      END
