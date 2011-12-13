      SUBROUTINE TEST1( STATUS )
*+
*  Name:
*     test1

*  Purpose:
*     Simple test of IMG

*  Copyright:
*     Copyright (C) 1998, 2004 Central Laboratory of the Research Councils.
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
*     PWD: Peter Draper (Starlink)
*     TIMJ: Tim Jenness (JAC, Hawaii)

*  History:
*     03-JUN-1998 (PWD):
*         Original Version
*     16-AUG-2004 (TIMJ):
*         Use CNF_PVAL

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'CNF_PAR'          ! For CNF_PVAL function

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER NX
      INTEGER NY
      INTEGER IPTR, IPTR2

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

      CALL IMG_IN( 'IN', NX, NY, IPTR, STATUS )
      WRITE(*,*)'NX, NY, IPTR', NX, NY, IPTR

      IF (STATUS .EQ. SAI__OK)
     : CALL SUMIT( NX, NY, %VAL( CNF_PVAL( IPTR ) ), STATUS )

      CALL IMG_OUT( 'IN', 'OUT', IPTR2, STATUS )

      CALL IMG_FREE( '*', STATUS )
      END

      SUBROUTINE SUMIT( NX, NY, ARRAY, STATUS )
      IMPLICIT NONE
      INTEGER NX, NY, STATUS, I, J
      REAL ARRAY( NX, NY ), SUM

      SUM = 0.0
      DO J = 1, NY
        DO I = 1, NX
           SUM = SUM + ARRAY( I, J )
        ENDDO
      ENDDO

      WRITE(*,*)'NX, NY, SUM', NX, NY, SUM
      END
