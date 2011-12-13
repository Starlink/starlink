      SUBROUTINE KPS1_SCAT1( NEL, INA, INB, STATUS )
*+
*  Name:
*     KPS1_SCAT1

*  Purpose:
*     Ensure the two arrays have the same bad pixel mask.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_SCAT1( NEL, INA, INB, STATUS )

*  Description:
*     This routine is called by scatter to ensure that the two arrays
*     have the same bad pixel mask. Each array is set bad if the
*     corresponding pixel in either input array is bad.

*  Arguments:
*     NEL = INTEGER (Given)
*         Number of elements in either of the arrays.
*     INA( NEL ) = REAL (Given and Returned)
*         First array.
*     INB( NEL ) = REAL (Given and Returned)
*         Second array.
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 2009 Science & Technology Facilities Council.
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
*     DSB: David S. Berry (JAC, Hawaii)
*     {enter_new_authors_here}

*  History:
*     15-OCT-2009 (DSB):
*        Original version.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE

*  Global Constants:
      INCLUDE 'SAE_PAR'
      INCLUDE 'PRM_PAR'

*  Arguments Given:
      INTEGER NEL

*  Arguments Given and Returned:
      REAL INA( NEL )
      REAL INB( NEL )

*  Status:
      INTEGER STATUS

*  Internal Variables:
      INTEGER I

*.

*  Check inherited status.
      IF( STATUS .NE. SAI__OK ) RETURN

*  Just do it.
      DO I = 1, NEL
         IF( INA( I ) .EQ. VAL__BADR .OR.
     :       INB( I ) .EQ. VAL__BADR ) THEN
            INA( I ) = VAL__BADR;
            INB( I ) = VAL__BADR;
         END IF
      END DO

      END
