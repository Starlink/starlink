      SUBROUTINE KPS1_MLOFL( NSMP, NDISP, OFFSET, DATA, STATUS )
*+
*  Name:
*     KPS1_MLOFL

*  Purpose:
*     Offset the lines to be drawn in a multi-line plot.

*  Language:
*     Starlink Fortran 77

*  Invocation:
*     CALL KPS1_MLOFL( NSMP, NDISP, DATA, OFFSET, STATUS )

*  Description:
*     This routine offsets each row of the array DATA by a specified
*     amount given by array OFFSET. If the offset for a particular row
*     is invalid, the row will not be offset.

*  Arguments:
*     NSMP = INTEGER (Given)
*        The number of samples in each row.
*     NDISP = INTEGER (Given)
*        The number of rows in the data array.
*     OFFSET( NDISP ) = REAL ( Given )
*        The offset value for each row of the data array.
*     DATA( NSMP, NDISP ) = REAL (Given and Returned)
*        On entry it contains the data array to be offset.
*        On exit it contains the data array after offsetting
*     STATUS = INTEGER (Given and Returned)
*        The global status.

*  Copyright:
*     Copyright (C) 1991 Science & Engineering Research Council. All
*     Rights Reserved.

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
*     WG: Wei Gong (IPMAF)
*     MJC: Malcolm J. Currie (STARLINK)
*     {enter_new_authors_here}

*  History:
*     10-MAY-1991 (WG):
*        Original version.
*     1991 June 18 (MJC):
*        Renamed from OFFLIN.
*     {enter_further_changes_here}

*-

*  Type Definitions:
      IMPLICIT NONE              ! No implicit typing

*  Global Constants:
      INCLUDE 'SAE_PAR'          ! Standard SAE constants
      INCLUDE 'PRM_PAR'          ! PRIMDAT definitions

*  Arguments Given:
      INTEGER NSMP
      INTEGER NDISP
      REAL OFFSET( NDISP )

*  Arguments Given and Returned:
      REAL DATA( NSMP, NDISP )

*  Status:
      INTEGER STATUS             ! Global status

*  Local Variables:
      INTEGER I, J               ! Do loop index

*.

*  Check inherited global status.
      IF ( STATUS .NE. SAI__OK ) RETURN

*  Offset the rows one by one.
      DO J = 1, NDISP

*  If the offset for this row is valid, offset it sample by sample.
         IF ( OFFSET( J ) .NE. VAL__BADR ) THEN
            DO I = 1, NSMP

*  Offset only those valid samples.
               IF ( DATA( I, J ) .NE. VAL__BADR )
     :            DATA( I, J ) = DATA( I, J ) + OFFSET( J )
            END DO
         END IF
      END DO

      END
